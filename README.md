bash-lib
========

Library of bash scripts

date-util.sh
============

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

