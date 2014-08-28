bash-lib
========

Library of bash scripts.

Author: Alan K. Stebbens <aks@stebbens.org>

Usage:
------

    export PATH=$PATH:$HOME/lib
    source bash-lib.sh

Replace `$HOME/lib` with wherever these files are installed

This bash library is modular, and the individual utilities can be independently
sourced, as needed.

Each library has a corresponding test script to ensure proper operation before 
installation.  These test scripts are also the basis for regression tests, after
new features are added (or bugs are fixed).

For example, the `text-utils.sh` library has a test script called
`test-text-utils.sh`.  The `test-utils.sh` library is used to operate all the
tests and makes a very good example of how to implement TDD in bash scripts.

Installation:
-------------

The installation is managed with `make`, using `Makefile` which, in turn,
sources `Makefile.inc`.  If any changes are needed to support your
installation, the changes should be made within the `Makefile`.

    make

Show the various make targets.

    make tests

Run all the tests to confirm proper operation.  Some of the tests can take a
few minutes.  Progress will be shown, so there is no guessing.

    make install

Install the bash library into `$HOME/lib` (the default).

    make install libdirs=/usr/local/lib

Install into `/usr/local/lib`.

If this bash library is installed into an alternative path, e.g., `/opt/lib`,
then any scripts that wish to make use of them will need to modify the `PATH`
environment variable, in order to source the library files without explicit
paths.

This library is available at [https://github.com/aks/bash-lib.git].

If you wish to make improvements, feel free to fork this repo, make and test
your changes, and the issue a pull request.

As part of your testing, you'll probably need to source `reset-util-vars.sh`,
which defines `reset_util_vars`, which you can then invoke to reset the shell
variables that prevent redundant sourcings.  Alternatively, you can increment
the version number in the utility libraries that you are modifying.


Follow the links below for detailed descriptions of each module.

* [date-util.sh](#date_utils)
* [list-utils.sh](#list_utils)
* [hash-utils.sh](#hash_utils)
* [real-utils.sh](#real_utils)
* [text-utils.sh](#text_utils)
* [test-utils.sh](#test_utils)

date-util.sh <a name="date_utils" id="date_utils">
-------------

The `date-utils` library enables easy management of dates and its year, month,
and day components.  A variety of date formats are supported both on input
parsing, and on output formatting.  The envar `EUROPEAN_DATES` controls how the
format `NN/NN/NNNN` is interpreted: if set to 1, that format is taken to mean
`DD/MM/YYYY`, where DD is the day of the month; otherwise, it is parsed as
`MM/DD/YYYY`.

    date_parse [DATESTRING]
    date_arg   [DATESTRING]

Parse `DATESTRING` in one of several recognized formats: `YYYY-MM-DD`,
`YYYY.MM.DD`, `YYYY/MM/DD`, `YYYY MM DD`, `MM DD YYYYY`, `DD.MM.YYYY`, and `DD
MM YYYYY` (if `EUROPEAN_DATES` is set).  If the `DATESTRING` is successfully
parsed, the variables `year`, `month`, and `day` are set to their respective
numbers.  `date_arg` is another name for the same function.

If `DATESTRING` is empty, a line of input from STDIN is read and used instead.
This makes the script handy in a pipe.  Example:

    extract_first_date_from_log /var/log/messages | date_parse

will set `year`, `month`, and `day` from the date extracted.


    month_number MONTHNAME
    month_num    MONTHNAME

Given a month name, output it's index.

    days_in_month MONTH

The `days_in_month` function converts a month number or name (spelled out or
abbreviated) into a number of days corresponding to that month (not including
leap-year effects).  Example: `days_in_month Feb` ==> 28

    days_in_month[M]

Array of integers, indexed by month number, corresponding to the number of
days in the given month `M`.

    days_before_month[M]

Array of integers representing the number of days from the beginning of the
year up to the month `M`.

    is_leap_year YEAR

Return 0 (true) if `YEAR` is a leap year; return 1 (false) otherwise.

    last_day_of_month YYYY MM

Return the last day of the month corresponding to year `YYYY` and month `MM`.

    date_to_adays YYYY MM DD
    date_to_adays YYYY-MM-DD

Returns the number of absolute days from the beginning of the Gregorian
calendar for the given date, which can be specified with three numeric
arguments, or a single string argument, which must be parseable by
`date_parse`.

    jdays_to_date JDAYS

Converts `JDAYS` (a Julian day number) into the corresponding date.  If the
date is greater than October 10, 1584, then the Gregorian calendar is used,
otherwise the Julian calendar is used for earlier dates.

    adays_to_date ABSDAYS

Converts `ABSDAYS` into a date formatted by `print_date`.

    adays_to_jdays ADAYS
    jdays_to_adays JDAYS

These functions convert from absolute days to Julian day number, and vice-versa.

    week_number [DATESTRING | YYYY MM DD]

Returns the week number for the given `DATESTRING` or date components.

    date_to_weekday_name [DATESTRING | YYYY MM DD]

Returns the weekday name for the given `DATESTRING` or date components.

    date_to_weekday_num [DATESTRING | YYYY MM DD]

Returns the wekday number (0..6) for the given `DATESTRING` or date components.

    date_day_num [DATESTRING | YYYY MM DD]

Returns the day number for the given `DATESTRING` or date components.

    date_format [FORMAT] YYYY MM DD
    date_format [FORMAT] YYYY-MM-DD

The `format_date` function accepts an optional format string, followed by
three numeric arguments, for the year, month, and day, or a single string
argument in any of the parsable date formats, and reformats into the default
date format, given by `DATE_FORMAT`.  If `DATE_FORMAT` is not defined, the format
`%F` is used.  (See `man strftime` for details).

list-utils.sh <a name="list_utils" id="list_utils">
-------------

bash script utilities for managing lists of things

In the descriptions below, `VAR` is an array variable; `VAL`, `VAL1`, .. are values.

These are the list utilities:

    list_init VAR                        # initialize VAR as an empty list

    list_add      VAR VAL1 [VAL2 ...]    # add VAL1.. to the end of VAR

    list_add_once VAR  VAL1 [VAL2 ..]    # add VAL1.. uniquely to the end of VAR

    add_list      VAR VAL1 [VAL2 ...]    # alias to list_add

    add_list_once VAR VAL ...            # alias to list_add_once

    list_push VAR VAL ...                # alias to list_add

    push_list VAR VAL ...                # alias to list_add

    list_insert      VAR VAL ...         # insert VAL.. at the front of VAR

    list_insert_once VAR VAL ...         # insert VAL.. at the front of VAR; 

    insert_list      VAR VAL ...         # alias to list_insert

    insert_list_once VAR VAL ...         # alias to list_insert_once

    list_pop VAR                         # removes top VAL on VAR and returns in variable "item"

    pop_list VAR                         # an alias to list_pop 

    list_get  VAR N                      # get the Nth item of VAR to stdout

    list_item VAR N                      # set 'item' to the Nth item of VAR

    list_set  VAR N VAL                  # set the Nth item of VAR to VAL

    list_items VAR [START [END]]         # return list items from START to END (or all)

    list_copy LIST NEWLIST [START [END]] # copy list LIST to NEWLIST, from START to END

    in_list VAR  [-any|-all] VAL ...     # return true if one or more values are in a list

    list_size VAR                        # returns the number of items

    sort_str VAL ...                     # sort the space-separated words of VAL ..

    sort_list VAR                        # sort the contents of VAR (a list) in place

    sorted_list VAR                      # output the items of VAR sorted

    sort_str2lines                       # sort STR with each item in a separate line

    sort_list2lines                      # sort LIST with each item in a separate line

    split_into  VAR "STRING" SEP         # split "STRING" by SEP into VAR

    split_str   "STRING" [SEP]           # split "STRING" by SEP

    join_list VAR [SEP] ..               # join the items in VAR into a list, separated by SEP,
      SEP can be 
        AND    -- separate with " and "
        OR     -- separate with " or "
        KEYS   -- enclose each item with X' and ', follwed by ','
        TAB    -- use tabs to separate items
        NL     -- separate each item with newline (and some spaces)
        NOWRAP -- do not auto-wrap long lines (default is WRAP)
        ','    -- separate items with a comma (default)
        str    -- separate each item with an given string.

    join_lines                           # read STDIN and catenate lines; remove trailing NL

    lookup_list LISTVAR KEY              # lookup KEY in LISTVAR

    grep_list   LISTVAR PAT              # grep PAT across LISTVAR

    map_list    LISTVAR EXPR             # create a list of EXPR applied to each item in LISTVAR

    reduce_list LISTVAR EXPR [INIT]      # reduce LISTVAR using EXPR, with initial value INIT

    sum_list LISTVAR                     # sum the items in LISTVAR

    max_list LISTVAR                     # return the maximum item in LISTVAR

    min_list LISTVAR                     # return the minimum item in LISTVAR

    avg_list LISTVAR                     # return the average of the items in LISTVAR

    print_list LISTVAR [indent=INDENT] [width=WIDTH] [sep=SEP] [cols=COLS]

    print_list LISTVAR [i=INDENT] [w=WIDTH] [s=SEP]  [c=COLS]

        print the items in LIST in vertically-sorted columns.  Use COLS if given,
        otherwise the number of columns is computed from WIDTH (defaults to 80) and
        the maximum width of all the items in LISTVAR

    list_help                             # describe the list functions

Splitting
---------

    split_into  VAR "STRING" SEP

splits a `STRING` into parts using separator (`SEP`) (default is ',')
and assigns the resulting separated, quoted strings to the `VAR`.

    split_str   "STRING" [SEP]

outputs the split of `STRING` into parts using a separator `SEP`
(defaulting to space/tab).

    split_input [SEP]

splits the input text into parts using separator (`SEP`) (default is tab).

For the split functions:

If `SEP` does not include a space (" "), care is taken to avoid removing
whitespace from the split values.

`SEP` can be multiple characters; for example ' ,' (a space, comma)
will split using both space and comma.  By default, splitting is
done by tabs.

Lookup functions
----------------

    lookup_list LIST "WORD"

Looks up `WORD` in the array `LIST` for the uniquely matching item, using
disambiguating case-insensitive matching.  If no match, return empty string and
code 1; if 2 or more matches, return empty string, and error code 2.

    grep_list LIST PATTERN

Look up items matching `PATTERN` in the array `LIST`.  Return all matching items,
and return code 0.  If no matching items, return empty string and return code
1.

    lookup_error CODE WORD [NOTFOUNDMSG [AMBIGMSG]]

A utility function to be used in conjuction with a `lookup_list` or `grep_list`
invocation.  `CODE` is an error code returned from `lookup_list` or `grep_list`.
`WORD` is the word used on the search, and is used as the "%s" argument in either
error message.  `NOTFOUNDMSG` is the error message used in the case of error 
code 1.  `AMBIGMSG` is the error message used in the case of error code 2.

`lookup_error` is used like this:

    read -p "What word do you want to use?" word
    words=( a list of words to search from )
    found=`lookup_list words $word` || lookup_error $? $word \
          "'%s' is not a valid word" \
          "'%s" is an ambiguous word"

hash-utils.sh <a name="hash_utils" id="hash_utils">
-------------

Hashes are associative arrays. Hashes have __keys__ and associated
__values__.   Use this library to simplify and ease your use of
associated arrays.

These are the hash utilities:

    hash_init VAR [DEFAULT]           # initialize VAR as an empty hash

    hash_default VAR                  # return the default value for HASH

    hash_set_default VAR DEFAULT      # set the default value for HASH

    hash_put VAR KEY VAL ...          # insert KEY=>VAL into the hash
    hash_set VAR KEY VAL              # alias to "hash_put"

    hash_get  VAR KEY                 # get the Nth item of VAR to stdout

    hash_delete VAR KEY               # delete VAR[KEY]

    hash_delete_if VAR KEY CONDITION  # delete VAR[KEY} if CONDITION is true

    hash_keys VAR                     # return all the keys in hash
    hash_values VAR                   # return all the values in hash

    hash_each VAR KEYVAR EXPR         # eval EXPR, setting KEYVAR to each key in VAR

    hash_copy HASH NEWHASH KEY1 ...   # copy items at KEY1, .. from HASH1 to NEWHASH

    in_hash VAR KEY                   # test if KEY is in the hash VAR
    has_key VAR KEY
    hash_member VAR KEY
    hash_include VAR KEY

    hash_size VAR                     # returns the number of key=>value pairs

    hash_merge VAR1 VAR2              # merge key/value pairs from VAR2 with VAR1

    hash_print HASHVAR [indent=INDENT] [width=WIDTH] [gutter=GUTTER] [cols=COLS] [sep=SEP]

          print the items in `HASHVAR` in vertically-sorted columns.  The
          number of columns is determined by `COLS`, if given, or by `WIDTH`
          (defaults to 80) divided by the maximum width of all items in
          `HASHVAR`.

          Use `GUTTER` blanks (default 2) to separate columns.

          If `SEP` is given, it is used to delimit columns intead of blanks.

          Each option may be abbreviated to its leading character (e.g., "g"
          for "gutter").

    hash_help                             # describe the list functions


real-utils.sh <a name="real_utils" id="real_utils">
-------------
real-utils.sh is a bash library that enables real number arithmetic in bash
scripts.  Real numbers are managed as floating point strings in the format
"X.Y", where X is the integer portion, and "Y" is the fractional part.

Usage:

    source real-utils.sh

    real_compute "EXPRESSIN"  [SCALE]

    real_eval    "EXPRESSION" [SCALE]

    real_cond     EXPRESSION  [SCALE]

    real_int   REAL

    real_frac  REAL

Descriptions:

    real_compute "EXPRESSION" [SCALE]

The `real_compute` bash function evaluates `EXPRESSION` using syntax, operators
and functions as described in the `bc` manual.  All numbers and variables
within `EXPRESSION` are interpreted by `bc`.  The result of the computation is
output to `STDOUT`.

If an error occurs, there is no indication.  This function does not set a
return code, nor does it set the shell status variable `$?`.  Use `real_eval`
for those effects.

In addition to the operators and functions defined by `bc`, the following
additional functions are also made available within the `EXPRESSION`:

    abs(x)           deg(x)           log10(x)         rad(x)
    acos(x)          exp(x)           logn(x)          round(x,s)
    asin(x)          frac(x)          ndeg(x)          sin(x)
    atan(x)          int(x)           pi()             tan(x)
    cos(x)           log(x)           pow(x,y)

To see the `bc` definitions of these functions, use the `real_functions`
function.

    real_eval "EXPRESSION" [SCALE]

The `real_eval` bash function invokes `real_compute` on the arguments, prints
the result on `STDOUT`, and returns with the `bc` return code `$?` (0 or 1, for
success or error, respectively).

    real_cond "EXPRESSION" [SCALE]

`EXPRESSION` is a real number conditional which should evaluate to 1 or 0.  The
return status is 0 for true, 1 for false.  Example usage:

    if real_cond "$num < $max" 2 ; then
       ...
    fi


    real_scale=NUM

Set the precision of subsequent real number arithmetic results.   The
default is 2.

    real_int   REAL         -- outputs the integer portion of a REAL number
    real_frac  REAL         -- outputs the fractional portion of a REAL number

    sin R, cos R, tan R     -- trig functions on radians R
    asin X, acos X, atan X  -- inverse trig functions
    cotan X, sec X, cosec X -- cotangent, secant, cosecant
    arccot X                -- arc-cotangent
    hypot X Y               -- hypotenuse X, Y [sqrt(X^2 + Y^2)]
    sqrt X                  -- square-root of X
    logn X, log X           -- natural log, log base 10
    exp X                   -- exponent X of E (e.g., e^X)
    pow X Y                 -- power function [X^Y]
    rad D                   -- convert degrees D to radians
    deg R                   -- convert radians R to degrees
    ndeg R                  -- convert radians R to natural degrees (0..360)
    round X S               -- Round X to S decimals.  When S=0, rounds to the nearest integer.
    real_int X              -- outputs integer portion of X
    real_frac X             -- outputs fractional portion of X
    abs X                   -- Return the absolute value of X.

    PI   = 3.141592653589793
    TAU  = 6.283185307179586   # 2*PI
    E    = 2.718281828459045


sh-utils.sh <a name="sh_utils" id="sh_utils">
-----------
handy functions for writing bash-based scripts

Copyright 2006-2014 Alan K. Stebbens <aks@stebbens.org>

Shell utility functions:

       talk MSG ..              Print all arguments on `STDERR`.
      vtalk MSG ..              If `$norun` or `$verbose` is set, print all args on `STDERR`.
     nvtalk MSG                 Print all arguments on `STDERR` only if `$verbose` is not set.
     nqtalk MSG                 Print all arguments on `STDERR` only if `$quiet` isn not set.
     error [CODE] "MSG"         Print `MSG` on `STDERR`, then exit with code `CODE` (or 2)
       die "MSG"                Print `MSG` on `STDERR`, then die (with `kill -ABRT`)

      talkf FMT ARGS ..         Printf `FMT` `ARGS` on `STDERR`
     vtalkf FMT ARGS ..         Printf `FMT` `ARGS` on `STDERR` if `$norun` or `$verbose` set
    nvtalkf FMT ARGS ..         Printf `FMT` `ARGS` on `STDERR` unless `$verbose` set
    nqtalkf FMT ARGS ..         Printf `FMT` `ARGS` on `STDERR` unless `$quiet` set
     errorf [CODE] FMT ARGS ..  Printf `FMT` `ARGS` on `STDERR`, then exit `$CODE` [2]
       dief FMT ARGS ..         Printf `FMT` `ARGS` on `STDERR`, then die (with `kill -ABRT`)

    run COMMAND ARGS ..         Show `COMMAND` `ARGS` if `$norun` or `$verbase`;
                                run `COMMAND` unless `$norun`.

    rm_file_later FILE          Cause `FILE` to be removed upon program exit.

    add_trap "CMD" SIGNAL ..    Add `CMD` to the trap list for `SIGNAL`

    fn_exists FUNCTION          Return 0 (true) if `FUNCTION` exists; 1 (false) otherwise

    numarg_or_input "$1"        Return a numeric argument or read it from `STDIN`

    args_or_input "$@"          Return arguments or read them from `STDIN`

    arg_or_input "$1"           Return the argument or read it from `STDIN`

    append_args "$@"            Append the arguments to the next line from `STDIN`

    append_arg "$1"             Append the argument to the next line from `STDIN`


text-utils.sh <a name="text_utils" id="text_utils">
-------------
Copyright 2006-2014 Alan K. Stebbens <aks@stebbens.org>

Text processing utilities for bash scripts.

usage:

    export PATH=.:$HOME/lib:$PATH
    source text-utils.sh

The following functions are provided by this library:

    lowercase STRING          # return the lowercase string
    uppercase STRING          # return the uppercase string
    trim STRING               # trim blanks surrounding string
    ltrim STRING              # trim left-side blanks from STRING
    rtrim STRING              # trim right-side blanks from STRING
    squeeze STRING            # squeeze multiple blanks in string
    split_str STRING SEP      # split STRING using SEP [default: \t]
    url_encode  [STRING]      # encode STRING (or STDIN) for URLs
    url_decode  [STRING]      # decode STRING (or STDIN) from URL encoding
    html_encode [STRING]      # encode STRING (or STDIN) for HTML presentation
    html_decode [STRING]      # decode sTRING (or STDIN) from HTML presentation


test-utils.sh <a name="test_utils" id="test_utils">
-------------
Copyright 2006-2014 Alan K. Stebbens <aks@stebbens.org>

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
tests with the pattern "test_" are run.  For example, if the pattern "basic"
is used, all tests with the string "basic"` will be run, and no others.

In order to be discovered for automatic test runs, the tests functions must
have the function name begin with `test_`.

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

The first time a new `check_output` test is evaluated, there will not be a
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

