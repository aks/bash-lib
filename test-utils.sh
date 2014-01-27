# test-utils.sh
#
# Copyright 2006-2014 Alan K. Stebbens <aks@stebbens.org>
#
# infrasructure for test-driven development of Bash scripts
#
# A run is a collection of tests, each test has a name.
# A test is a set of related operations with checks on the results
# A check compares values, which can result in an error.
#
# At the end of each test, there are a number of checks and errors.
#
# The tests to be run must have the function name begin with "test_".
#
# The general structure of a test suite:
#
#  source test-utils.sh
#
#  init_tests [ARGUMENTS]
#
#  test_01_NAME1() {
#     start_test
#     ... do some operations to be tested
#
#     check_equal 'bar' `my_func foo` "Func on 'foo' did not match 'bar'"
#
#     end_test
#   }
#  ...
#  test_NN_NAME() {
#     ...
#   }
#  ...
#  run_tests
#  summarize_tests
#
# These are the kinds of tests that can be done:
#
#     check_value        VAL               ERROR
#     check_empty        VAL               ERROR
#
#  Expression tests
#
#     check_true         "EXPR"            ERROR
#     check_false        "EXPR"            ERROR
#
#  Array item tests
#
#     check_size         LIST SIZE         ERROR  # same as check_size_eq
#     check_size_XX      LIST SIZE         ERROR 
#
#     check_item         LIST INDEX VAL    ERROR
#     check_item_equal   LIST INDEX VAL    ERROR
#     check_item_unequal LIST INDEX NONVAL ERROR
#
#  String tests 
#
#     check_equal        VAL1 VAL2         ERROR
#     check_unequal      VAL1 VAL2         ERROR
#
#     check_match        VAL1 REGEXP       ERROR
#     check_nomatch      VAL1 REGEXP       ERROR
#
#  Numeric tests
#
#     check_eq           N1 N2             ERROR
#     check_ne           N1 N2             ERROR
#     check_lt           N1 N2             ERROR
#     check_le           N1 N2             ERROR
#     check_gt           N1 N2             ERROR
#     check_ge           N1 N2             ERROR
#
#     ERROR is optional.
#     XX above can be: eq, ne, lt, le, gt, ge
#
#  Alan K. Stebbens <aks@stebbens.org>

TEST_UTILS_VERSION="test-utils.sh v1.6"
[[ "$TEST_UTILS_SH" = "$TEST_UTILS_VERSION" ]] && return
export TEST_UTILS_SH="$TEST_UTILS_VERSION"

export PATH=.:$HOME/lib:$PATH

source list-utils.sh

TEST_usage() {
  cat 1>&2 <<EOF
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
        h) TEST_usage;;
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

# checkend OK "ERROR"

TEST_check_end() {
  if [[ -n "$1" ]]; then
    TEST_check_status[$TEST_checks]='.'
    if (( test_verbose )); then
      echo 1>&2 -n " ok"
    else
      TEST_update_status
    fi
  else
    TEST_check_status[$TEST_checks]='!'
    (( TEST_errors++ ))
    if (( test_verbose || verbose_errors )) ; then
      echo 1>&2 " error"
      TEST_error_dump "$2"
    else
      echo -n 1>&2 $'\b'"!"
    fi
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
  printf 1>&2 "%3d checks, %3d errors: " $checks $errors
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
TEST_check_size_func() { TEST_check_expr "test `list_size $1` $2 $3" "$4" ; }

# TEST_check_item_func VAR INDEX OPERATOR VALUE [error]
# Check a specific item of VAR at INDEX for OPERATOR VALUE

TEST_check_item_func() { TEST_check_expr "test \"\${$1[$2]}\" $3 \"$4\"" "$5" ; }

# TEST_check_test LVAL OP RVAL [ERROR]
# Implied "test" function
TEST_check_test() { TEST_check_expr "test \"$1\" $2 \"$3\"" "$4" ; }

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

check_size()    { check_size_eq   "$1"     $2 "$3" ; }

# check_item_equal    VAR INDEX VAL ERROR
# check_item_unequal  VAR INDEX VAL ERROR

check_item_equal()   { TEST_check_item_func $1 "$2" '='  "$3" "$4" ; }
check_item_unequal() { TEST_check_item_func $1 "$2" '!=' "$3" "$4" ; }

check_item() { check_item_equal "$@" ; }

# check_value VALUE [ERROR]
#
# Check that VALUE is not empty.

check_value() { TEST_check_expr "test -n \"$1\"" "\"$2\"" ; }

# check_empty VALUE [ERROR]
#
# Check that VALUE is empty

check_empty() { TEST_check_expr "test -z \"$1\"" "\"$2\"" ; }

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
  if (( $# > 2 )); then
    errm="$3"
  else
    errm="$name test failed; diffs in $diffout and $differr"
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
  if diff -w -U 0 $ref $out >$diff ; then
    eval "$4"
    if (( ! test_keep_ref_output )); then
      rm "$out" # remove temp files
    fi
    rm "$diff"
  else
    eval "$5"
  fi
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

# Filter the TESTS array with the TEST_patterns array.  if names from the
# former aren't matched by any patterns from the latter, remove it from the
# TESTS array.

filter_tests() {
  if (( ${#TEST_patterns[@]} > 0 )); then
    local nomatches=()
    local name nx pat
    for ((nx=0; nx<${#TESTS[@]}; nx++)) ; do
      name="${TESTS[nx]}"
      local nomatch=1
      for pat in "${TEST_patterns[@]}" ; do
        if [[ "$name" =~ $pat ]]; then
          nomatch= ; break
        fi
      done
      if (( nomatch )) ; then
        nomatches=( "${nomatches[@]}" $nx )
      fi
    done
    if (( ${#nomatches[@]} > 0 )) ; then
      # deletions must be done in descending index order
      local x
      for ((x=${#nomatches[@]} - 1; x >= 0; x--)) ; do
        nx=${nomatches[x]}
        unset TESTS[$nx]
      done
    fi
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

