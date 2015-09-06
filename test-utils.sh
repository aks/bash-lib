# test-utils.sh
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>

TEST_UTILS_VERSION="test-utils.sh v1.8"
[[ "$TEST_UTILS_SH" = "$TEST_UTILS_VERSION" ]] && return
TEST_UTILS_SH="$TEST_UTILS_VERSION"

export PATH=.:$HOME/lib:$PATH

source list-utils.sh
source help-util.sh

test_help() {
  help_pager <<'EOF'
The `test-utils.sh` library provides an infrasructure for test-driven
development (TDD) of `bash` scripts.

Usage:

    source test-utils.sh

    test_NAME1() {
      start_test
      ... # perform operations and test the results
      end_test
    }

    test_NAME2() {
      start_test
      ... # perform operations and test the results
      end_test
    }

    init_tests [ARGUMENTS]
    run_tests
    summarize_tests

Description:

A *run* is a collection of *tests* (within a single file); each test has a name.

A *test* is a set of related operations with *checks* on the results.

A *check*` tests or compares values, which quietly succeeds, or results in an
error.  The error message can be provided, or a default one is used.

At the end of each test, the number of checks and errors is remembered for
later summarization.

At the end of the run, all checks and error counts are summarized.

While the tests and checks are being performed, output is occuring to show the
progress.  There are three modes of output: terse, errors-only, and detailed.

Terse mode shows each test name followed by the number of checks, and how many
of those checks had errors.  Terse mode is the default.

In errors-only mode, successful tests still show the same as terse mode, but 
tests with error checks show the error message followed by a stack dump
indicating the location of the error.  Errors-mode is indicated by the `-e`
option when invoking the test script.

In details mode, the tests and checks are run in verbose mode, showing both
successful checks and errors.  Details mode is indicated by the `-d` option.

When invoking the test script, the command line argument can be used to pass a
`PATTERN` that is used to match a subset of the test names.  By default, all
tests with the pattern `test_*` are run.  For example, if the pattern `basic`
is used, all tests with the string `basic` will be run, and no others.

In order to be discovered for automatic test runs, the tests functions must
have the function name begin with "test_".

A common technique for test naming is: `test_NN_some_descriptive_name`, where
`NN` is a number.  This allows easy referency by the `NN` to selectively run a
test or tests.

Below are the tests that are currently supported:

      check_value        VAL               ERROR
      check_empty        VAL               ERROR

 Expression tests

      check_true         "EXPR"            ERROR
      check_false        "EXPR"            ERROR

 Array item tests

      check_size         LIST SIZE         ERROR  # same as check_size_eq
      check_size_XX      LIST SIZE         ERROR 

      check_item         LIST INDEX VAL    ERROR
      check_item_equal   LIST INDEX VAL    ERROR
      check_item_unequal LIST INDEX NONVAL ERROR

 Hash tets

      check_key          HASH KEY          ERROR
      check_no_key       HASH KEY          ERROR
      check_key_value    HASH KEY VALUE    ERROR

 String tests

      check_equal        VAL1 VAL2         ERROR
      check_unequal      VAL1 VAL2         ERROR

      check_match        VAL1 REGEXP       ERROR
      check_nomatch      VAL1 REGEXP       ERROR

 Numeric tests

      check_eq           N1 N2             ERROR
      check_ne           N1 N2             ERROR
      check_lt           N1 N2             ERROR
      check_le           N1 N2             ERROR
      check_gt           N1 N2             ERROR
      check_ge           N1 N2             ERROR

Output tests

     check_output [NAME] EXPRESSION [ERROR]

Evaluate `EXPRESSION` and compare its output against a previously collected
reference output.  If the output matches, the test succeeds.  If the output
does not match, print `ERROR` or a default error message.

Use `NAME` as the unique identifier for files in which the `stdout`, `stderr`,
and reference output is identified.

Reference output can be created by the `-k` (`$keep`) option when the test is
run.

The first time a new check_output test is evaluated, there will not be a
collected reference output to compare against, and the test will fail.

NOTE: The following functions are not yet implemented.

     check_out      [NAME] EXPRESSION [ERROR]
     check_out_none [NAME] EXPRESSION [ERROR]
     check_err      [NAME] EXPRESSION [ERROR]
     check_err_none [NAME] EXPRESSION [ERROR]

Check that `STDOUT` or `STDERR` is or is not empty when evaluating
`EXPRESSION`, or show the `ERROR` (or default) message.

     check_out_eq   [NAME] EXPRESSION VALUE [ERROR]
     check_err_eq   [NAME] EXPRESSION VALUE [ERROR]

Check that the `STDOUT`, or `STDERR` of the evaluated `EXPRESSION` matches
`VALUE`, or show the `ERROR` (or a default error message).

     check_out_ne [NAME] EXPRESSION VALUE [ERROR]
     check_err_ne [NAME] EXPRESSION VALUE [ERROR]

Check that the `STDOUT` or `STDERR` of the evaluated `EXPRESSION` does not
contain `VALUE`, or show the `ERROR`.

In all cases, the `ERROR` message is optional.
EOF
}
help_test() { test_help ; }


TEST_usage() {
  help_pager 1>&2 <<EOF
usage: ${0##*/} [opts] [TEST-PATTERN ...]
Run tests with options controlling behavior.

If one or more TEST-PATTERNs are given, those tests not matching the given
patterns are excluded from being run.

All functions beginning with "test_" are included in the list of tests to run.
The tests are run in alphabetic order, unless the -r option is given to cause
them to be run in random order.

The "check_output" function compares stdout/stderr against the reference copies
captured with -k (keep) option.

Options
  -h      show help
  -d      show test status details
  -e      show verbose messages only on errors
  -k      keep test stdout/stderr for future test reference
  -n      don't make any changes (norun mode)
  -r      randomize the order of the tests
  -v      be verbose everywhere
EOF
  exit
}

init_tests() {
  TEST_errors=0
  TEST_checks=0
  TEST_tests=0
  TESTS=()
  TEST_check_status=()
  test_details= verbose_errors= test_randomize= test_verbose= test_keep_ref_output=
  if [[ $# -gt 0 ]]; then
    set -- "$@"
    while getopts 'deknvrh' opt ; do
      case "$opt" in
        d) test_details=1 ;;
        e) verbose_errors=1 ;;
        k) test_keep_ref_output=1 ;;
        h) TEST_usage ;;
        n) norun=1 ;;
        r) test_randomize=1 ;;
        v) test_verbose=1 ;;
      esac
    done
    shift $(( OPTIND - 1 ))
    TEST_patterns=( "$@" )
    if (( test_keep_ref_output )); then
      printf "Saving stdout/stderr for future reference.\n"
    fi
  fi
  gather_tests
}

start_test() {
  TEST_errors_start=$TEST_errors
  TEST_checks_start=$TEST_checks
  if [[ "$TEST_name" != "${FUNCNAME[1]}" ]]; then
    (( TEST_tests++ ))
    TEST_name="${FUNCNAME[1]}"
  fi
}

TEST_check_start() {
  local check_name x
  # find the first function name that does NOT begin with "TEST_"
  for ((x=1; x<${#FUNCNAME}; x++)) ; do
    check_name="${FUNCNAME[$x]}"
    if [[ "$check_name" != TEST_* ]]; then
      break
    fi
  done
  (( TEST_checks++ ))
  TEST_check_status[$TEST_checks]='?'
  TEST_update_status "$check_name" $TEST_checks
}

# checkend OK "ERROR" ["ERROR_ACTION"]
# returns true (0) no error; false (1) for errors

TEST_check_end() {
  if [[ -n "$1" ]]; then
    TEST_check_status[$TEST_checks]='.'
    if (( test_verbose )); then
      echo 1>&2 -n " ok"
    else
      TEST_update_status
    fi
    return 0
  else
    TEST_check_status[$TEST_checks]='!'
    (( TEST_errors++ ))
    if (( test_verbose || verbose_errors )) ; then
      echo 1>&2 " error"
      [[ -n "$3" ]] && eval "$3"      # maybe take action on error
      TEST_error_dump "$2"
    else
      echo -n 1>&2 $'\b'"!"
    fi
    return 1
  fi
}

end_test() {
  (( test_verbose )) || TEST_update_status
  echo 1>&2 ''
}

TEST_print_name() {
  printf 1>&2 "%*s: " $TEST_max_width "${1:-$TEST_name}"
}

TEST_print_status() {
  local checks errors
  (( checks = TEST_checks - TEST_checks_start ))
  (( errors = TEST_errors - TEST_errors_start ))
  printf 1>&2 "%4d checks, %4d errors: " $checks $errors
  if (( ! test_details && ! test_verbose )) ; then
    local x st last_st=' '
    for((x=TEST_checks_start; x<${#TEST_check_status[@]}; x++)) ; do
      st="${TEST_check_status[$x]}"
      if [[ "$st" != "$last_st" ]]; then
        echo 1>&2 -n "$st"
      fi
      last_st="$st"
    done
  elif (( ! test_verbose )) ; then
    local x
    for((x=TEST_checks_start; x<${#TEST_check_status[@]}; x++)) ; do
      echo 1>&2 -n "${TEST_check_status[$x]}"
    done
  fi
}

# TEST_update_status [CHECKNAME CHECKNO]

TEST_update_status() {
  if (( test_verbose )); then
    echo 1>&2 ''
  else
    echo -n 1>&2 $'\r'
  fi
  TEST_print_name
  TEST_print_status
  if [[ $# -gt 0 && -n "$test_verbose" ]]; then
    printf 1>&2 "check %d: %s" $2 "$1"
  fi
}

##############################
#
# These are internal test checking functions.  The prefix "TEST_" keeps them
# from showing up in the error dumps

#  TEST_check EXPR [TRUE-VALUE] [FALSE-VALUE] [ERROR]

TEST_check() {
  TEST_check_start
  local test_ok=$3
  eval "if $1 ; then test_ok=${2:-1} ; fi"
  TEST_check_end "$test_ok" "$4"
}

# TEST_check_expr "EXPR" "ERROR"
TEST_check_expr() { TEST_check "$1" 1 '' "$2" ; }

# TEST_check_size_func VAR FUNC VALUE [ERROR]

TEST_check_size_func() { 
  local insize=`__list_size $1`
  TEST_check_test $insize $2 $3 "${4:-"Size check failed; got: $insize; should be: $3"}"
}

# TEST_check_item_func VAR INDEX OPERATOR VALUE [error]
# Check a specific item of VAR at INDEX for OPERATOR VALUE

TEST_check_item_func() { 
  local val
  eval "val=\"\${$1[$2]}\""
  TEST_check_test "$val" $3 "$4" "${5:-"Item check failed; got '$val', should be '$4'"}"
}

# TEST_check_key    VAR KEY [ERROR]
# TEST_check_no_key VAR KEY [ERROR]
# Check that a key exists (with a non-empty value), or does not exist, in a hash VAR

TEST_check_key() {    TEST_check_test2 -n "\${$1[$2]}" "$3" ; }
TEST_check_no_key() { TEST_check_test2 -z "\${$1[$2]}" "$3" ; }

# TEST_check_key_value VAR KEY VALUE [ERROR]

TEST_check_key_value() { TEST_check_test "\${$1['$2']}\"" '==' "$3" "$4" ; }

# TEST_check_test  LVAL OP RVAL [ERROR]
# TEST_check_test2      OP  VAL [ERROR]

TEST_check_test()  { TEST_check_expr "test \"$1\" $2 \"$3\"" "$4" ; }
TEST_check_test2() { TEST_check_expr "test        $1 \"$2\"" "$3" ; }
TEST_check_test3() { TEST_check_test "$@" ; }

########

# These are the "customer" check funcs

# check_true EXPR [ERROR]
check_true() { TEST_check "$1" 1 '' "$2" ; }

# check_false EXPR [ERROR]
check_false() { TEST_check "$1" '' 1 "$2" ; }

# check_size_eq VAR VAL [ERROR]
# check_size_ne VAR VAL [ERROR]
# check_size_ge VAR VAL [ERROR]
# check_size_gt VAR VAL [ERROR]
# check_size_le VAR VAL [ERROR]
# check_size_lt VAR VAL [ERROR]

check_size_eq() { TEST_check_size_func "$1" -eq $2 "$3" ; }
check_size_ne() { TEST_check_size_func "$1" -ne $2 "$3" ; }
check_size_ge() { TEST_check_size_func "$1" -ge $2 "$3" ; }
check_size_gt() { TEST_check_size_func "$1" -gt $2 "$3" ; }
check_size_le() { TEST_check_size_func "$1" -le $2 "$3" ; }
check_size_lt() { TEST_check_size_func "$1" -lt $2 "$3" ; }

# check_size VAR VAL ERROR
#
# Check that the array VAR has size VAL

check_size()    { check_size_eq   "$@" ; }

# check_item_equal    VAR INDEX VAL ERROR
# check_item_unequal  VAR INDEX VAL ERROR

check_item_equal()   { TEST_check_item_func $1 "$2" '='  "$3" "$4" ; }
check_item_unequal() { TEST_check_item_func $1 "$2" '!=' "$3" "$4" ; }

check_item() { check_item_equal "$@" ; }

# check_key VAR KEY [ERROR]
check_key()    { TEST_check_key "$@" ; }
check_no_key() { TEST_check_no_key "$@" ; }

# check_key_value VAR KEY VALUE [ERROR]
check_key_value() { TEST_check_key_value "$@" ; }

# check_value VALUE [ERROR]
# check_empty VALUE [ERROR]
#
# Check that VALUE is empty or not empty.

check_value() { TEST_check_test2 -n "$1" "$2" ; }
check_empty() { TEST_check_test2 -z "$1" "$2" ; }

# TEST_check_func VALUE FUNC VALUE2 [ERROR]

# TEST_check_func() {
#   TEST_check_start
#   local test_ok=0
#   eval "if [[ \"$1\" $2 \"$3\" ]]; then test_ok=1 ; fi"
#   if (( ! test_ok )) && [[ -z "$4" ]]; then
#     echo 1>&2 "Check failed for \"$2\": '$1' vs '$3'"
#   fi
#   TEST_check_end "$ok" "$4"
# }

# These are the string tests

# check_equal   VAL1 VAL2   [ERROR]
# check_unequal VAL1 VAL2   [ERROR]
# check_match   VAL  REGEXP [ERROR]
# check_nomatch VAL  REGEXP [ERROR]

check_equal()   {   TEST_check_test "$1" =  "$2" "$3" ; }
check_unequal() {   TEST_check_test "$1" != "$2" "$3" ; }
check_match()   {   TEST_check_test "$1" =~ "$2" "$3" ; }
check_nomatch() { ! TEST_check_test "$1" =~ "$2" "$3" ; }

# check_OP      VAL0  VAL2  [ERROR]
# These are the numeric tests

check_lt()      {   TEST_check_test "$1" -lt "$2" "$3" ; }
check_le()      {   TEST_check_test "$1" -le "$2" "$3" ; }
check_eq()      {   TEST_check_test "$1" -eq "$2" "$3" ; }
check_ne()      {   TEST_check_test "$1" -ne "$2" "$3" ; }
check_ge()      {   TEST_check_test "$1" -ge "$2" "$3" ; }
check_gt()      {   TEST_check_test "$1" -gt "$2" "$3" ; }

# check_output [NAME] EXPRESSION [ERROR]
#
# Run EXPRESSION and capture the both stdout & stderr, under NAME.  Compare
# them against previously stored output under the same NAME, if any.  Report
# differences.
#
# If there is no previously stored output, save it if -k (keep) is set.
#
# Be wary of comparying time-varying output, such as dates & times: they will
# always cause differences.

check_output() {
  local name expr errm
  if (( $# == 1 )); then
    expr="$1" name="${1//[^a-zA-Z0-9_-]/}"
  elif (( $# > 1 )) ; then
    name="$1" expr="$2"
  fi
  TEST_check_start
  local test_out_ok= test_err_ok= test_ok=1
  local out="test/$name.out"
  local err="test/$name.err"
  local outref="$out.ref"
  local errref="$err.ref"
  local diffout="$out.diff"
  local differr="$err.diff"
  if (( test_keep_ref_output )); then
    out="$outref" err="$errref"
  fi
  if (( $# > 2 )); then
    errm="$3"
  else
    errm="$name test failed; diffs in $diffout and $differr"
  fi
  eval "$expr 1>$out 2>$err"
  [[ -f "$outref" ]] || touch "$outref"
  [[ -f "$errref" ]] || touch "$errref"
  TEST_compare_output $outref $out $diffout "test_out_ok=1" "test_ok="
  TEST_compare_output $errref $err $differr "test_err_ok=1" "test_ok="
  TEST_check_end "$test_ok" "$errm"
}

# TEST_compare_output ref out diff GOODEXPR ERROREXPR
#
# Used by "check_output" to compare current and reference output.  If the
# comparison is successful (no changes), evaluate GOODEXPR, otherwise, evaluate
# ERROREXPR.

TEST_compare_output() {
  local ref="$1"
  local out="$2"
  local diff="$3"
  if \diff -w -U 0 $ref $out >$diff ; then
    eval "$4"
    if (( ! test_keep_ref_output )); then
      \rm -f "$out" # remove temp files
    fi
    \rm "$diff"
  else
    eval "$5"
    # show diffs on errors with -d
    if (( $test_details )); then
      echo 1>&2 "\n$diff"
      \cat 1>&2 $diff
      echo 1>&2 ""
    fi
  fi
}

# check_out NAME EXPR ERROR
check_out() {
  :
}
# check_out_none NAME EXPR ERROR
check_out_none() {
  :
}
# check_err NAME EXPR ERROR
check_err() {
  :
}
# check_err_none NAME EXPR ERROR
check_err_none() {
  :
}
# check_out_eq NAME EXPR VALUE ERROR
check_out_eq() {
  :
}
# check_out_ne NAME EXPR VALUE ERROR
check_out_ne() {
  :
}
# check_err_eq NAME EXPR VALUE ERROR
check_err_eq() {
  :
}
# check_err_ne NAME EXPR VALUE ERROR
check_err_ne() {
  :
}


# TEST_error_dump ERROR
#
# Dump the function stack (but not those beginning with "check_")

TEST_error_dump() {
  local func source lineno stacksize
  if [[ -n "$1" ]]; then
    echo 1>&2 "Error: $1:"
  else
    echo 1>&2 "Error at:"
  fi
  stacksize=${#FUNCNAME[*]}
  for (( i=1; i < stacksize; i++ )); do
    func="${FUNCNAME[$i]}"
    source="${BASH_SOURCE[$i]}"
    lineno="${BASH_LINENO[$i]}"
    case "$func" in
      TEST_*) continue ;;    # don't process TEST_ funcs
    esac
    printf 1>&2 "  %s:%s:%s()\n" "$source" "$lineno" "$func"
  done
}

TESTS=()

# Filter the TESTS array with the TEST_patterns array.  If names from the
# former aren't matched by any patterns from the latter, remove it from the
# TESTS array.
#
# Do NOT allow the `test_help` function to be included -- it is a help
# function.

filter_tests() {
  local deletes=()
  local name nx
  for ((nx=0; nx<${#TESTS[@]}; nx++)) ; do
    name="${TESTS[nx]}"
    local delete=    # assume the name will NOT be deleted
    if [[ "$name" = 'test_help' ]]; then
      delete=1
    elif (( ${#TEST_patterns[@]} > 0 )); then
      # we have patterns to match against.  Now assume we didn't match it
      delete=1
      local pat
      for pat in "${TEST_patterns[@]}" ; do
        if [[ "$name" =~ $pat ]]; then
          delete= ; break
        fi
      done
    fi
    if (( delete )) ; then
      deletes+=( $nx )
    fi
  done
  if (( ${#deletes[@]} > 0 )) ; then
    # deletions must be done in descending index order
    local x
    for ((x=${#deletes[@]} - 1; x >= 0; x--)) ; do
      nx=${deletes[x]}
      unset TESTS[$nx]
    done
  fi
}

# gather_tests -- find all tests with the prefix "test_"
# filter out those not matching TEST_patterns (if any)
# in alphabetic order, or random order (if -r).

gather_tests() {
  if [[ "${#TESTS[@]}" -eq 0 ]]; then
    TESTS=( `compgen -A function test_` )
    filter_tests
    local clause
    case ${#TEST_patterns[@]} in 
      0) clause= ;;
      1) clause=" matching pattern '${TEST_patterns[0]}'" ;;
      *) clause=" matching given patterns: ${TEST_patterns[@]}" ;;
    esac
    printf 1>&2 "%d tests discovered%s\n" ${#TESTS[@]} "$clause"
    TEST_max_width=0
    local tname
    for tname in "${TESTS[@]}" ; do
      if (( ${#tname} > TEST_max_width )); then
        TEST_max_width=${#tname}
      fi
    done
    if (( $test_randomize )) ; then
      randomize_tests
      printf 1>&2 "The tests will be run in random order.\n"
    fi
  fi
}

# randomize_tests -- place the items in random order
randomize_tests() {
  local newtests=()
  local x
  while (( ${#TESTS[*]} > 0 )) ; do
    x=`jot -r 1 0 $(( ${#TESTS[*]} - 1 ))`
    newtests=( "${newtests[@]}" "${TESTS[$x]}" )
    unset TESTS[$x]
    TESTS=( "${TESTS[@]}" )
  done
  TESTS=( "${newtests[@]}" )
}

run_tests() {
  gather_tests
  local a_test
  for a_test in "${TESTS[@]}" ; do
    eval "$a_test"
  done
}

summarize_tests() {
  echo 1>&2 ''
  printf 1>&2 "%d tests, %d checks, %d errors\n" $TEST_tests $TEST_checks $TEST_errors
}

