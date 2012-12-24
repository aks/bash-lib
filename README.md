bash-lib
========

Library of bash scripts

date-util.sh
------------

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

