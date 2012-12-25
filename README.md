bash-lib
========

Library of bash scripts.

Author: Alan K. Stebbens <aks@stebbens.org>

* [date-util.sh](#date-utils.sh)
* [list-utils.sh](#list-utils.sh)
* [test-utils.sh](#test-utils.sh)

date-util.sh
------------

This file is a list of functions that make it easier to manage dates and conversions in bash scripts.
For example, if your bash script reads a date argument on the command line, the `date_arg` function is
very handy to convert the string date format into an integer representing the day of the Epoch.

Usage:

    source date-util.sh

Functions: 
----------

    date_arg YYYY-MM-DD 
    date_arg YYYY MM DD

set year, month, and days

    days_in_month MM 

where MM = 01 .. 12 , or Jan, Feb, ...,  or Ja, Fe, .. 

    is_leap_year YYYY 

Returns 0 (true) if YYYY is a leap year; 1 (false) otherwise

    last_day_of_month yyyy mm
    date_to_abs_days YYYY MM DD
    date_to_abs_days YYYY-MM-DD

Date must be in YYYY-MM-DD (or YYYY/MM/DD or YYYY.MM.DD) format

    abs_days_to_date  DAYS

returns YYYY-MM-DD

    abs_days_to_date ABSDAYS

date is returned in YYYY-MM-DD format

Algorithm from "Calendrical Calculations", by Nachum Dershowitz and Edward M. Reingold

```lisp
  (let* ((d0 (1- date))
         (n400 (/ d0 146097))
         (d1 (% d0 146097))
         (n100 (/ d1 36524))
         (d2 (% d1 36524))
         (n4 (/ d2 1461))
         (d3 (% d2 1461))
         (n1 (/ d3 365))
         (day (1+ (% d3 365)))
         (year (+ (* 400 n400) (* 100 n100) (* n4 4) n1)))
    (if (or (= n100 4) (= n1 4))
        (list 12 31 year)
      (let ((year (1+ year))
            (month 1))
        (while (let ((mdays (calendar-last-day-of-month month year)))
                 (and (< mdays day)
                      (setq day (- day mdays))))
          (setq month (1+ month)))
        (list month day year)))))
```

    format_date YYYY-MM-DD or Y-M-D or Y/M/D or YYYY M D

    print_date YYYY MM DD

Print the date in the YYYY-MM-DD format.

    days_at_epoch =  a constant for the epoch

    date_to_days_since_epoch YYYY-MM-DD

    get_date_5_years-since [YYYY-MM-DD]

    get_date_last_quarter_end YYYY-MM-DD

Both the above routines output a date string, in YYYY-MM-DD format.
Both accept a date as input, the absence of which defaults to now.

    get_date_5_years_since [YYYY-MM-DD]

    get_date_x_years_since [YEARSOFFSET] [YYYY-MM-DD] 

Get the date X years before the given date

    get_date_last_quarter_end YYYY-MM-DD

given a date, get the previous quarter end date plus one.
If no date, use the current date.

Variables 
---------

    days_in_month (array)
    days_before_moth (array)

list-utils.sh
-------------

bash script utilities for managing lists of things

In the descriptions below, `VAR` is an array variable; `VAL`, `VAL1`, .. are values.

    list_add VAR VAL1 [VAL2 ...]        # add VAL1.. to the end of VAR

    list_add_once VAR  VAL1 [VAL2 ..]   # add VAL1.. uniquely to the end of VAR

    list_insert VAR  VAL ...            # insert VALUE at the front of VAR

    list_insert_once VAR VAL ..         # insert VALUE.. at the front of VAR; 

    in_list VAR  [-any|-all] VAL ...    # return true if one or more values are in a list

    list_size VAR                       # returns the number of items

    sort_str VAL ...                    # sort the space-separated words of VAL ..
  
    sort_list VAR                       # sort the contents of VAR (a list) in place

    join_list VAR [SEP] ..

Join the items in `VAR` into a list, separated by `SEP`, which can be:

`AND`    -- separate with `" and "`

`OR`     -- separate with `" or "`

`KEYS`   -- enclose each item with `X'` and `'`, follwed by `','`

`TAB`    -- use tabs to separate items

`NL`     -- separate each item with newline (and some spaces)

`NOWRAP` -- do not auto-wrap long lines (default is `WRAP`)

`','`    -- separate items with a comma (default)

`str`    -- separate each item with an given string.

    split_into  VAR "STRING" SEP

splits a STRING into parts using separator (`SEP`) (default is ',')
and assigns the resulting separated, and quoted strings to the `VAR`.

    split_str   "STRING" [SEP]

outputs the split of STRING into parts using a separator SEP (defaulting
to space/tab).

For the split functions:

If `SEP` is anything but " " (a space), care is taken to avoid removing whitespace from
the split values.

`SEP` can be multiple characters; for example ' ,' (a space, comma) will split using both space
and comma.  By default, splitting is done by tabs.


sh-utils.sh
-----------
handy functions for writing bash-based scripts

Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>

    chat MSG ..

If `$norun` or `$verbose` is set, print all args on `STDERR`.

    talk MSG ..

Print all arguments on `STDERR`.

    nvtalk MSG 

Print all arguments on `STDERR` only if $verbose is not set.

    error [CODE] "MSG" 

Print `MSG` on `STDERR`, then exit with code CODE (or 2)

    run COMMAND ARGS ..

If `$verbose` is set, show the command and args before running it.
If `$norun` is not set, run the command with args and examine the resulting status.

    rm_file_later FILE

Add FILE to a list of files that will be automatically removed upon program exit.

    add_trap "CMD" SIGNAL ..

Add `CMD` to the trap list for `SIGNAL`, while ensuring that it is not repeated.


test-utils.sh
-------------

Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>

Infrasructure for test-driven development of Bash scripts

* A _*run*_ is a collection of tests, each test has a name.
* A _*test*_ is a set of related operations with checks on the results
* A _*check*_ compares values, which can result in an error.

At the end of each test, there are a number of checks and errors.

The tests to be run must have the function name begin with "`test_`".

The general structure of a test suite:

    source test-utils.sh

    init_tests [ARGUMENTS]

    test_01_NAME1() {
        start_test	# start the tests
        # ... do some operations to be tested

        check_equal 'bar' \`my_func foo\` "Func on 'foo' did not match 'bar'"
        
        #... do some other tests
        end_test	# end the tests
    }
    ...
    test_NN_NAME() {
        start_test
        ...
        end_test
    }
    ...
    run_tests
    summarize_tests

These are the kinds of tests that can be done:

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

ERROR is optional.  `XX` above can be: `eq`, `ne`, `lt`, `le`, `gt`, `ge`.


