# bash 
# date-utils.sh
# Copyright 2011 Alan K. Stebbens <aks@stebbens.org>
# 
# functions for date management
#
# date_arg YYYY-MM-DD 
# date_arg YYYY MM DD
#    set year, month, and days
#
date_arg() {
  if (( $# == 3 )); then
    year=$(( 10#$1 ))  month=$(( 10#$2 ))  day=$(( 10#$3 ))
  else
    local date="${1:-`date +%F`}"
    year=$((  10#${date%-*-*} ))
    mmdd="${date#*-}"
    month=$(( 10#${mmdd%-*}   ))
    day=$((   10#${mmdd#*-}   ))
  fi
}
#
# days_in_month MM 
#
# (where MM = 01 .. 12 , or Jan, Feb, ...,  or Ja, Fe, .. )

days_in_month() {
  case "$1" in
    1|01|January|Jan|Ja|january|jan|ja)     echo 31 ;;
    2|02|February|Feb|Fe|february|feb|fe)   echo 28 ;;
    3|03|March|Mar|Ma|march|mar|ma)         echo 31 ;;
    4|04|April|Apr|Ap|april|apr|ap)         echo 30 ;;
    5|05|May|Ma|may|ma)                     echo 31 ;;
    6|06|June|Jun|Ju|june|jun|ju)           echo 30 ;;
    7|07|July|Jul|jul)                      echo 31 ;;
    8|08|August|Aug|Au|august|aug|au)       echo 31 ;;
    9|09|September|Sep|Se|september|sep|se) echo 30 ;;
    10|10|October|Oct|Oc|october|oct|se)    echo 31 ;;
    11|11|November|Nov|No|november|nov|no)  echo 31 ;;
    12|12|December|Dec|De|december|dec|de)  echo 31 ;;
    *) echo "Bad month name or index: $1" ; kill -ABRT $$ ; exit 2 ;;
  esac
}

# Index                1  2  3  4   5   6   7   8   9  10  11  12
    days_in_month=( - 31 28 31 30  31  30  31  31  30  31  30  31 )
days_before_month=( -  0 31 59 90 120 151 181 212 243 273 304 334 )

# is_leap_year YYYY 
#
# Returns 0 (true) if YYYY is a leap year; 1 (false) otherwise

is_leap_year() {
  local year=$(( 10#${1:?'Missing year'} ))
  if (( year < 0 )) ; then
    (( year++ ))
  fi
  if (( year % 4 == 0 && ( year % 100 != 0 || year % 400 == 0 ) )) ; then
    return 0
  fi
  return 1
}

# last_day_of_month yyyy mm
#
# Gregorian calendar

last_day_of_month() {
  local  year=$(( 10#${1:?'Missing year'} ))
  local month=$(( 10#${2:?'Missing month'} ))
  if (( month == 2 )) && is_leap_year $year ; then
    echo 29
  else
    echo $(( days_in_month[month] ))
  fi
}

# date_to_abs_days YYYY MM DD
# date_to_abs_days YYYY-MM-DD
#
# Date must be in YYYY-MM-DD (or YYYY/MM/DD or YYYY.MM.DD) format

date_to_abs_days() {
  local year month day
  date_arg "$@"
  (( day += days_before_month[month] ))           # add in the days preceding the current month
  (( month > 2 )) && is_leap_year $year && { (( days++ )) ; }
  (( year-- ))                                    # the following are relative to the prior year
  (( day += ( 365 * year ) ))                     # days in prior years
  (( day += ( year / 4   ) ))                     # Julian leap days in prior years
  (( day -= ( year / 100 ) ))                     # less prior century years
  (( day += ( year / 400 ) ))                     # plus leap quad-centuries
  echo $day
}

# abs_days_to_date  DAYS
# returns YYYY-MM-DD

slow_abs_days_to_date() {
  local adays=$(( 10#${1:-'Missing absolute days'} ))
  local approx=$(( adays / 366 ))
  local year=$approx yd
  for ((y=approx; ; y++)) ; do
    (( year++ ))
    yd=`date_to_abs_days $(( y + 1 )) 1 1` 
    if (( adays < yd )) ; then break ; fi
  done
  local month=1 md d
  for ((m=1; ; m++)) ; do
    d=`last_day_of_month $year $m`
    md=`date_to_abs_days $year $m $d`
    if (( adays <= md )); then break ; fi
    (( month++ ))
  done
  local days=$(( `date_to_abs_days $year $month 1` - 1 ))
  local day=$(( adays - days  ))
  print_date $year $month $day
}

# abs_days_to_date ABSDAYS
#
# date is returned in YYYY-MM-DD format
#
# Algorithm from "Calendrical Calculations", by Nachum Dershowitz and Edward M. Reingold
#
#   (let* ((d0 (1- date))
#          (n400 (/ d0 146097))
#          (d1 (% d0 146097))
#          (n100 (/ d1 36524))
#          (d2 (% d1 36524))
#          (n4 (/ d2 1461))
#          (d3 (% d2 1461))
#          (n1 (/ d3 365))
#          (day (1+ (% d3 365)))
#          (year (+ (* 400 n400) (* 100 n100) (* n4 4) n1)))
#     (if (or (= n100 4) (= n1 4))
#         (list 12 31 year)
#       (let ((year (1+ year))
#             (month 1))
#         (while (let ((mdays (calendar-last-day-of-month month year)))
#                  (and (< mdays day)
#                       (setq day (- day mdays))))
#           (setq month (1+ month)))
#         (list month day year)))))

abs_days_to_date() {
  local date=$(( 10#${1:?'Missing days'} ))
  local d0=$(( date - 1 ))              # one day ago
  local n400=$(( d0 / 146097 ))         # number of completed 400 year cycles
  local d1=$((   d0 % 146097 ))         # remaining days not in n400
  local n100=$(( d1 / 36524 ))          # number of 100 year cycles not included in n400
  local d2=$((   d1 % 36524 ))          # days not included in n400 or n100
  local n4=$((   d2 / 1461 ))           # number of 4 year cycles not included in n400 or n100
  local d3=$((   d2 % 1461 ))           # days not included in n400, n100, or n4
  local n1=$((   d3 / 365 ))            # number of years not included in n400, n100, or n4
  local day=$(( 1 + ( d3 % 365 ) ))     # days not included in n400, n100, n4, or n1
  local year=$(( (400 * n400) + (100 * n100) + (4 * n4) + n1 )) # Gregorian year
  local month
  if (( n100 == 4 || n1 == 4 )); then
    month=12 day=31
  else
    local mdays
    (( year++ ))
    for ((month=1; month<=12; month++)) ; do
      mdays=`last_day_of_month $year $month`
      if (( day <= mdays )); then break ; fi
      (( day -= mdays ))
    done
  fi
  print_date $year $month $day
}

# format_date YYYY-MM-DD or Y-M-D or Y/M/D or YYYY M D
format_date() {
  local year month day
  if [[ $# -eq 3 ]]; then
    date_arg "$@"
  else
    case "$1" in
      *-*-*) year="${1%%-*}" mmdd="${1#*-}" month="${mmdd%-*}" day="${mmdd#*-}" ;;
      */*/*) year="${1%%/*}" mmdd="${1#*/}" month="${mmdd%/*}" day="${mmdd#*/}" ;;
      *.*.*) year="${1%%.*}" mmdd="${1#*.}" month="${mmdd%.*}" day="${mmdd#*.}" ;;
      *) echo 1>&2 "Unknown date format! $1" ; exit 2 ;;
    esac
    date_arg $year $month $day
  fi
  print_date $year $month $day
}

# print_date YYYY MM DD
#
# Print the date in the YYYY-MM-DD format.

print_date() {
  local year month day
  date_arg "$@"
  printf "%04d-%02d-%02d\n" $year $month $day
}
printd() { print_date "$@" ; }

# Set a constant for the epoch
days_at_epoch=`date_to_abs_days 1970-01-01`

# date_to_days_since_epoch YYYY-MM-DD

date_to_days_since_epoch() {
  local date="${1:-`date +%F`}"
  local adays=`date_to_abs_days "$date"`
  echo $(( adays - days_at_epoch ))
}

# get_date_5_years-since [YYYY-MM-DD]
#
# get_date_last_quarter_end YYYY-MM-DD
#
# Both the above routines output a date string, in YYYY-MM-DD format.
# Both accept a date as input, the absence of which defaults to now.

# get_date_5_years_since [YYYY-MM-DD]
#
# Get the date five years before the given date

get_date_5_years_since() {
  get_date_x_years_since 5
}

# get_date_x_years_since [YEARSOFFSET] [YYYY-MM-DD] 
#
# Get the date X years before the given date

get_date_x_years_since() {
  local yoffset="${1:?'Missing number of years'}"
  shift
  local year month day
  date_arg "$@"
  (( year -= yoffset )) # get X years ago
  print_date $year $month $day
}


# get_date_last_quarter_end YYYY-MM-DD
#
# given a date, get the previous quarter end date plus one.
# If no date, use the current date

get_date_last_quarter_end() {
  local year month day
  date_arg "$@"
  month=$(( ( ( ( month - 1 ) / 3 ) * 3 ) + 1 ))    # get previous quarter month
  print_date $year $month 1
}

