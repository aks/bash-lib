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

    make clean

This will remove any temporary output files (from testing).  It will also
remove `prompt-colors.sh` because it is a _generated_ file.

If this bash library is installed into an alternative path, e.g., `/opt/lib`,
then any scripts that wish to make use of them will need to modify the `PATH`
environment variable, in order to source the library files without explicit
paths.

This library is available at [https://github.com/aks/bash-lib.git].

There is a script called `maybe-install-bash-lib` that can be incorporated into
the installation process of other bash libraries that depend on `bash-lib`.  It
will check to see if the named utilities are installed, and if not, perform an
installation from the repository.

If you wish to make improvements, feel free to fork this repo, make and test
your changes, and the issue a pull request.

As part of your testing, you'll probably need to source `reset-util-vars.sh`,
which defines `reset_util_vars`, which you can then invoke to reset the shell
variables that prevent redundant sourcings.  Alternatively, you can increment
the version number in the utility libraries that you are modifying.

Many of these utility functions have helpful argument checking.  In order to
avoid unnecessary overhead, each function name that provides argument checking
also has a more efficient, non-argument checking name, prefixed with `__`.

Follow the links below for detailed descriptions of each module.

* [arg-utils.sh](#arg_utils)
* [date-utils.sh](#date_utils)
* [hash-utils.sh](#hash_utils)
* [help-util.sh](#help_util)
* [list-utils.sh](#list_utils)
* [prompt-colors.sh](#prompt_colors)
* [real-utils.sh](#real_utils)
* [run-utils.sh](#run_utils)
* [sh-utils.sh](#sh_utils)
* [talk-utils.sh](#talk_utils)
* [text-utils.sh](#text_utils)
* [test-utils.sh](#test_utils)
* [time-utils.sh](#time_utils)


arg-utils.sh <a name="arg_utils" id="arg_utils">
============

The `arg-utils.sh` library is a collection of bash functions that enable
flexible argument handling on functions that need to be able to accept
arguments on the command-line or on `STDIN`.

When writing a bash function that can accept input on the command line or from
`STDIN`, the function should begin with an invocation of one of the following
functions.

For example, if we had a function that needed a numeric argument, the following
invocation would be used:

    local f=`numarg_or_input "$1"`

If a text argument is needed:

    local txtarg=`arg_or_input "$1"`

For those cases where two or more arguments can be accepted, either on the
command-line or from `STDIN`:

    local args=( `args_or_input "$@"` )

The following are the `arg-util` functions:

    numarg_or_input "$1"

Return a numeric argument or read it from `STDIN`

    arg_or_input "$1"

Return the argument or read it from `STDIN`

    args_or_input "$@"

Return arguments or read them from `STDIN`

    args_or_stdin "$@"
    
Return the arguments or read all of `STDIN`

    append_args "$@"
    
Append the arguments to the next line from `STDIN`

    append_arg "$1"

Append the argument to the next line from `STDIN`

Example
-------

Let's say we have two bash functions to convert Celsius to Fahrenheit and
vice-versa.  Let's call them `c2f` and `f2c`.  With these functions, they can
be used in two ways:

Typical functions with arguments:

    c2f 69              # convert 69C to F
    f2c 10              # convert 10F to C

Or, accepting their input on `STDIN`, as in a pipe:

     echo 69 | c2f      # convert 69C to F
     echo 10 | f2c      # convert 10F to C

The advantage of the latter approach is that the functions can be fitted into
a pipe where the data can come from another process directly on its `STDOUT`.

The definition of these two functions would be:

    # f2c -- convert F to C via: (°F  -  32)  x  5/9 = °C
    function f2c() {
      local f=`numarg_or_input "$1"`
      echo "scale=1; ($f - 32)*5/9' | bc
    }
    # c2f -- convert C to F via °C  x  9/5 + 32 = °F
    function c2f() {
      local c=`numarg_or_input "$1"`
      echo "scale=1; $c * 9/5 + 32" | bc
    }


help-util.sh <a nam="help_util" id="help_util">
============

This utility makes it easy to provide helpful responses for shell functions
that are missing arguments.

Each collection of related shell functions can share a common `help_FUNC`
function, which is then filtered for the specific function name for which
help is being sought.

Each function that can be used by a user should start with a call to
`help_args_func`, passing the `HELPFUNC`, `$#`, and the minimum number of
arguments.

If the using function is called with less than the required arguments the
`HELPFUNC` is invoked and the output filtered through a simple filter that does
not print until the calling function name is found and then prints only until
the next empty line of test.

Each collection of functions that wish to make use of this utility should
have a `HELPFUNC` that prints a brief description of each command (function),
where each function name begins an unindented comment line, with exactly one
blank after the comment character.  A description may follow -- as a bash
comment, indented or not.  Finally, the doc entry for the given function is
an empty comment line.

For reference examples, please see either `list-utils.sh` or `hash-utils.sh`.

    help_pager <<END_OF_MESSAGE
    some message
    ...
    END_OF_MESSAGE


date-utils.sh <a name="date_utils" id="date_utils">
=============

The `date-utils` library enables easy management of dates and its year, month,
and day components.  A variety of date formats are supported both on input
parsing, and on output formatting.  

The envar `EUROPEAN_DATES` controls how the format `NN/NN/NNNN` is interpreted:
if set to 1, that format is taken to mean `DD/MM/YYYY`, where DD is the day of
the month; otherwise, it is parsed as `MM/DD/YYYY`.

    date_parse [DATESTRING]
    date_arg   [DATESTRING]

Parse `DATESTRING` in one of several recognized formats: `YYYY-MM-DD`,
`YYYY.MM.DD`, `YYYY/MM/DD`, `YYYY MM DD`, `MM DD YYYYY`, `DD.MM.YYYY`, and `DD
MM YYYYY` (if `EUROPEAN_DATES` is set).  If the `DATESTRING` is successfully
parsed, the variables `year`, `month`, and `day` are set to their respective
numbers.  `date_arg` is another name for the same function.

If `DATESTRING` is empty, a line of input from `STDIN` is read and used
instead.  This makes the script handy in a pipe.  Example:

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
arguments, or a single string argument, which must be parsable by
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

Returns the weekday number (0..6) for the given `DATESTRING` or date components.

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
==============

`bash` script utilities for managing lists of things.

In the descriptions below, `VAR` is an array variable and `VAL`\*, are
values.

The functions that modify a list variable (e.g., `list_push`, `list_pop`)
cannot be used within a sub-shell (e.g., a command substitution).

Commands executed within a sub-shell are incapable of affecting variables in
the parent shell.  In other words, the expression `var=$(list_pop list)` looks
nice but won't work.    Instead, do:

    list_pop some_list    # sets "$item" with the popped value

List Functions
--------------
These are the list utilities:

    list_init VAR

Initialize `VAR` as an empty list.

    list_add      VAR VAL1 [VAL2 ...]

Add one or more values (`VAL1..`) to the end of `VAR`.  There is no check for
duplicates.

    list_add_once VAR  VAL1 [VAL2 ..]

Add one or more values (`VAL1..`) uniquely to the end of `VAR`.

    list_push VAR VAL ...

Alias to `list_add`.

    list_insert      VAR VAL ...

Insert `VAL..` at the front of `VAR`.

    list_insert_once VAR VAL ...

Insert one or more values (`VAL..`) at the front of `VAR`. 

    list_pop VAR

Removes `VAL` from the list `VAR` and returns it in the variable `item`.

    list_remove VAR VAL ...

Remove one or more given values (`VAL..`) from the list `VAR`.

    list_get  VAR N

Get the `N`th item of `VAR` to `STDOUT`.

    list_item VAR N

Set the variable `item` to the `N`th item of `VAR`.

    list_set  VAR N VAL

Set the `N`th item of `VAR` to `VAL`.

    list_items VAR [START [END]]

Return list items from `START` to `END` (or all).

    list_copy LIST NEWLIST [START [END]]

Copy list `LIST` to `NEWLIST,` from `START` to `END`.

    in_list VAR  [-any|-all] VAL ...

Return true if one or more values are in list `VAR`.

    list_size VAR

Returns the number of items in `VAR`.

    list_dump VAR

Output the contents of `VAR`, with indexes.

    sort_str VAL ...

Sort the space-separated words of `VAL ..`.

    list_sort VAR

Sort the contents of `VAR` (a list) in place.

    list_sorted VAR

Output the items of `VAR` sorted.

    sort_str2lines STRING

Sort `STRING` with each item in a separate line.

    sort_list2lines VAR

Sort list `VAR` with each item in a separate line.

    split_into  VAR "STRING" [SEP]

Split `STRING` by `SEP` into `VAR`.

    split_str   "STRING" [SEP]

Split `STRING` by `SEP`.

    list_join VAR [SEP] ..

Join the items in `VAR` into a list, separated by `SEP`.
`SEP` can be:

* `AND   ` - separate with `" and "` 
* `OR    ` - separate with `" or "` 
* `KEYS  ` - enclose each item with `X'` and `'`, followed by `,` 
* `TAB   ` - use tabs to separate items 
* `NL    ` - separate each item with newline (and some spaces) 
* `NOWRAP` - do not auto-wrap long lines (default is `WRAP`) 
* `','   ` - separate items with a comma (default) 
* `str   ` - separate each item with an given string. 


    join_lines

Read `STDIN` and catenate lines; remove trailing `NL`.

    list_lookup LISTVAR KEY

Lookup and return matching `KEY` in `LISTVAR`.  `KEY` can be an abbreviation.

    list_grep   LISTVAR PAT

Perform a `grep` using pattern `PAT` across the contents of `LISTVAR`.

    list_map    LISTVAR EXPR [JOINSTR]

Create a list of `EXPR` applied to each item in `LISTVAR`.

    list_reduce LISTVAR EXPR [INIT]

Reduce `LISTVAR` using `EXPR,` with initial value `INIT`.

    list_sum LISTVAR

Sum the items in `LISTVAR`.

    list_max LISTVAR

Return the maximum item in `LISTVAR`.

    list_min LISTVAR

Return the minimum item in `LISTVAR`.

    list_avg LISTVAR

Return the average of the items in `LISTVAR`.

    list_print LISTVAR [indent=INDENT] [width=WIDTH] [sep=SEP] [cols=COLS]

    list_print LISTVAR [i=INDENT] [w=WIDTH] [s=SEP]  [c=COLS]

Print the items in `LIST` in vertically-sorted columns.  Use `COLS` if given,
otherwise the number of columns is computed from `WIDTH` (defaults to 80) and
the maximum width of all the items in `LISTVAR`.

    list_help

Describe the list functions.

There are convenient aliases for most `list_XXX` functions as `XXX_list`.  For
example, `join_list` => `list_join`, `list_pop` => `pop_list`, `map_list` =>
`list_map`, etc.  This allows people who think _VERB-NOUN_ to use the functions
like `grep_list`, while other people who think _NOUN-VERB_ can use `list_grep`.
The canonical function name begins with `list_`.

Programmers wanting to make use of the list functions can use any of the list
names prefixed with `__` to avoid the argument checking that is more helpful
for interactive usage.  For example, within a script, the size of a list `FOO`
is obtained with `__list_size FOO`.

Splitting
---------

    split_into  VAR "STRING" SEP

Splits a `STRING` into parts using separator (`SEP`) (default is ',')
and assigns the resulting separated, quoted strings to the `VAR`.

    split_str   "STRING" [SEP]

Outputs the split of `STRING` into parts using a separator `SEP`
(defaulting to space/tab).

    split_input [SEP]

Splits the input text into parts using separator (`SEP`) (default is tab).

For the split functions:

If `SEP` does not include a space (`" "`), care is taken to avoid removing
whitespace from the split values.

`SEP` can be multiple characters; for example `' ,'` (a space, comma) will
split using both space and comma.  By default, splitting is done by tabs.

Lookup functions
----------------

    list_lookup LIST "WORD"

Looks up `WORD` in the array `LIST` for the uniquely matching item, using
disambiguating case-insensitive matching.  If no match, return empty string and
code 1; if 2 or more matches, return empty string, and error code 2.

    list_grep LIST PATTERN

Look up items matching `PATTERN` in the array `LIST`.  Return all matching items,
and return code 0.  If no matching items, return empty string and return code 1.

    lookup_error CODE WORD [NOTFOUNDMSG [AMBIGMSG]]

A utility function to be used in conjunction with a `lookup_list` or
`grep_list` invocation.  `CODE` is an error code returned from `lookup_list` or
`grep_list`.  `WORD` is the word used on the search, and is used as the `"%s"`
argument in either error message.  `NOTFOUNDMSG` is the error message used in
the case of error code 1.  `AMBIGMSG` is the error message used in the case of
error code 2.

`lookup_error` is used like this:

    read -p "What word do you want to use?" word
    words=( a list of words to search from )
    found=`lookup_list words $word` || lookup_error $? $word \
          "'%s' is not a valid word" \
          "'%s" is an ambiguous word"

hash-utils.sh <a name="hash_utils" id="hash_utils">
=============

Hashes are associative arrays. Hashes have __keys__ and associated
__values__.   Use this library to simplify and ease your use of
associated arrays.

These are the hash utilities:

    hash_init VAR [DEFAULT]

Initialize `VAR` as an empty hash.

    hash_default VAR

Return the default value for `HASH.`

    hash_set_default VAR DEFAULT

Set the default value for `HASH`.

    hash_put VAR KEY VAL ...

Insert `[KEY]=VAL` into the hash.

    hash_set VAR KEY VAL

Alias to `hash_put`.

    hash_get  VAR KEY

Output the item associated with `KEY` in `VAR` to `STDOUT`.

    hash_delete VAR KEY

Delete `VAR[KEY]`.

    hash_delete_if VAR KEY CONDITION

Delete `VAR[KEY]` if `CONDITION` is true.

    hash_keys VAR

Return all the keys in hash `VAR`.

    hash_values VAR

Return all the values in hash `VAR`.

    hash_each VAR KEYVAR EXPR

Evaluate `EXPR`, setting `KEYVAR` to each key in `VAR`.

    hash_copy HASH NEWHASH KEY1 ...

Copy items at `KEY1`, .. from `HASH1` to `NEWHASH`.

    in_hash      VAR KEY
    has_key      VAR KEY
    hash_member  VAR KEY
    hash_include VAR KEY

Test if `KEY` is in the hash `VAR`.

    hash_size VAR

Returns the number of `[key]=value` pairs

    hash_merge VAR1 VAR2

Merge key/value pairs from `VAR2` with `VAR1`.

    hash_print HASHVAR [indent=INDENT] [width=WIDTH] [gutter=GUTTER] [cols=COLS] [sep=SEP]

Print the items in `HASHVAR` in vertically-sorted columns.  The
number of columns is determined by `COLS`, if given, or by `WIDTH`
(defaults to 80) divided by the maximum width of all items in
`HASHVAR`.

Use `GUTTER` blanks (default 2) to separate columns.

If `SEP` is given, it is used to delimit columns instead of blanks.

Each option may be abbreviated to its leading character (e.g., "g"
for "gutter").

    hash_help                             # describe the list functions

option-utils.sh <a name="option_utils" id="options_utils">
===============

The `option-util.sh` library is a small set of functions to manage
building options and arguments, which is often needed in the
development of command-line utilities.  

These functions use two global variables: `option_pairs` and
`options`.  The `option_pairs` variable is used to accumulate pairs
of options and arguments, e.g.: `-F FILE`, while `options` is used to
accumulate single character options that can be clustered behind a
single dash "-".

All of the accumulated options and arguments can be output with
`all_opts`.

    init_opts                # empty "option_pairs" and "options"

    reset_opts               # same as init_opts

    add_optarg OPTION ARG .. # add OPTION and ARG to the option_pairs list

    add_option OPTION ..     # add OPTION to the single options list
    add_opt    OPTION ..     # eg: add_arg -c -d ..  or add_arg c d ..

    all_opts                 # outputs both option_pairs and options


prompt-colors.sh <a name="prompt_colors" id="prompt_colors">
================

`prompt-colors.sh` is a bash script that creates two functions:
`define_color_names` and `reset_color_names`, and then invokes the former.  The
`define_color_names` function creates a bunch of color variable names, setting
them to the corresponding `bash` prompt escape sequences.  This allows the
`bash` `PS1` and related prompts to be easily colored using color names, like
`${Red}` and `${BoldCyan}`.  The function `reset_color_names` removes all the
color names from the current bash session.

Usage:
    
    source prompt-colors.sh


The file `prompt-colors.sh` is actually dynamically generated from the script
`generate-color-names`, and is not even part of this repository.  To create it, you
must run `make`, or invoke `generate-prompt-colors` manually.


real-utils.sh <a name="real_utils" id="real_utils">
=============
The `real-utils.sh` bash library provides real number arithmetic in bash
scripts.  Real numbers are managed as floating point strings in the format
`"X.Y"`, where `X` is the integer portion, and `Y` is the fractional part.

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

run-utils.sh <a name="run_utils" id="run_utils">
============
Shell utility functions for running system commands:

    run COMMAND ARGS ..       Show `COMMAND` `ARGS` if `$norun` or `$verbase`;
                              run `COMMAND` unless `$norun`.

    safe_run COMMAND ARGS ... Same as "run", but always executes.

    rm_file_later FILE        Cause `FILE` to be removed upon program exit.

    add_trap "CMD" SIGNAL ..  Add `CMD` to the trap list for `SIGNAL`


sh-utils.sh <a name="sh_utils" id="sh_utils">
===========
Handy functions for writing bash-based scripts

The shell command utility functions consist of several groups
of functions which collectively are quite useful in development
command-line utilities and other system scripts.

The following are separate modules that are included with `sh-utils`:

- `arg-utils   ` - help with arguments or STDIN
- `help-util   ` - help with help on selected functions
- `option-utils` - manage option and argument lists
- `run-utils   ` - run system commands, with $norun and $verbose 
- `talk-utils  ` - conditional output to STDERR

There are also some miscellaneous functions:

    rm_file_later FILE          Cause `FILE` to be removed upon program exit.

    add_trap "CMD" SIGNAL ..    Add `CMD` to the trap list for `SIGNAL`

    fn_exists FUNCTION          Return 0 (true) if `FUNCTION` exists; 1 (false) otherwise


talk-utils.sh <a name="talk_utils" id="talk_utils">
=============
All of the `talk`, `error`, and `die` functions conditionally print the
arguments on `STDERR`.  The conditions that are available include: `$verbose`,
`$quiet`, `$norun`, and are indicated by the prefixes: `v`, `q`, and `nr`,
respectively.  The additional prefixes of `vo`, `nv`, and `nq` test for "verbose only", "no-verbose", and "no-quiet", respectively.

       talk MSG ..              Print all args on `STDERR`
      vtalk MSG ..              If `$norun` or `$verbose` is set, print all args.
     votalk MSG ..              If `$verbose` only (no `$norun`) is set, print all args.
     nrtalk MSG ..              if `$norun` set, print all args
     nvtalk MSG                 Unless `$verbose` is set, print all args
     nqtalk MSG                 Unless `$quiet` is set, print all args

      talkf FMT ARGS ..         Printf `FMT` `ARGS`
     vtalkf FMT ARGS ..         If `$norun` or `$verbose` set, printf `FMT, `ARGS`
    votalkf FMT ARGS ..         If `$verbose` only (no `$norun`) is set, printf `FMT`, `ARGS`
    nrtalkf FMT ARGS ..         If `$norun` set, printf `FMT`, `ARGS`
    nvtalkf FMT ARGS ..         Unless `$verbose` is set, printf `FMT` `ARGS`
    nqtalkf FMT ARGS ..         Unless `$quiet` is set, printf `FMT` `ARGS`

       warn MSG                 Print all args on `STDERR`
      error [CODE] "MSG"        Print `MSG` on `STDERR`, then exit with code `CODE` (or 2)
        die "MSG"               Print `MSG` on `STDERR`, then die (with `kill -ABRT`)

      warnf FMT ARGS ..         Printf `FMT` `ARGS` on `STDERR`
     errorf [CODE] FMT ARGS ..  Printf `FMT` `ARGS` on `STDERR`, then exit `$CODE` [2]
       dief FMT ARGS ..         Printf `FMT` `ARGS` on `STDERR`, then die (with `kill -ABRT`)


text-utils.sh <a name="text_utils" id="text_utils">
=============
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
=============
The `test-utils.sh` library provides an infrastructure for test-driven
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
------------

A *run* is a collection of *tests* (within a single file); each test has
a name.

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

When invoking the test script, the command line argument can be used to pass
a `PATTERN` that is used to match a subset of the test names.  By default, all
tests with the pattern "test_" are run.  For example, if the pattern "basic" is
used, all tests with the string "basic"` will be run, and no others.

In order to be discovered for automatic test runs, the tests functions must
have the function name begin with `test_`.

A common technique for test naming is: `test_NN_some_descriptive_name`, where
`NN` is a number.  This allows easy reference by the `NN` to selectively run a
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

Hash tests

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

Examples of Tests:
------------------

Please carefully review the various test files in this repository:

    test-date-utils.sh    -- test the functions in date-utils.sh
    test-hash-utils.sh    -- test the functions in hash-utils.sh
    test-list-utils.sh    -- test the functions in list-utils.sh
    test-real-utils.sh    -- test the functions in real-utils.sh
    test-sh-utils.sh      -- test the functions in sh-utils.sh
    test-text-utils.sh    -- test the functions in text-utils.sh
    test-template.sh      -- a template for future tests
    test-utils.sh         -- the functions described here

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

time-utils.sh <a name="time_utils" id="time_utils">
=============
The `time-utils` library enables easy management of timestamps, with hour,
minute, seconds, and timezone components.  A variety of time formats are
supported both on input parsing, and on output formatting. 

    time_parse [TIMESTRING]
    time_arg   [TIMESTRING]

Parse `TIMESTRING` in one of several recognized formats: `HH:MM:SS`,
`HH:MM:SS.ssss`, If the `TIMESTRING` is successfully parsed, the variables
`hours`, `mins`, and `secs` are set the corresponding numbers.  `time_arg` is
another name for the same function.

If `TIMESTRING` is empty, a line of input from `STDIN` is read and used instead.

    time2secs [TIMESTRING]

Parse `TIMESTRING` (or `STDIN)` and convert to seconds.

    time_format [FORMAT] HOURS MINS SECS
    time_format [FORMAT] TIMESTRING

The `time_format` function accepts an optional format string, followed by three
numeric arguments, for the hour, minutes, and seconds, or a single string
argument in any of the parsable date formats, and reformats into the default
time format, given by `TIME_FORMAT`.  If `TIME_FORMAT` is not defined, the
format `%T` is used.  (See `man strftime` for details).

    time_add TIME1 TIME2

Add `TIME1` to `TIME2` and produce a `time_format` result.

    time_sub TIME1 TIME2

Subtract `TIME2` from `TIME1` and produce a `time_format` result.

# end of README
# vim: set ai sw=2
