# bash 
# date-utils.sh  -- date management utility for bash
#
# Copyright 2009-2014 Alan K. Stebbens <aks@stebbens.org>

DATE_UTILS_VERSION="date-utils.sh v1.8"
[[ "$DATE_UTILS_SH" = "$DATE_UTILS_VERSION" ]] && return
DATE_UTILS_SH="$DATE_UTILS_VERSION"

export DATE_FORMAT="%F"

source sh-utils.sh

help_date() {
  cat <<'EOF'
The `date-utils` library enables easy management of dates and its year, month,
and day components.  A variety of date formats are supported both on input
parsing, and on output formatting.  The envar "EUROPEAN_DATES" controls how the
format NN/NN/NNNN is interpreted: if set to 1, that format is taken to mean
"DD/MM/YYYY", where DD is the day of the month; otherwise, it is parsed as
'MM/DD/YYYY'.

    date_parse [DATESTRING]
    date_arg   [DATESTRING]

Parse DATESTRING in one of several recognized formats: `YYYY-MM-DD`,
`YYYY.MM.DD`, `YYYY/MM/DD`, `YYYY MM DD`, `MM DD YYYYY`, `DD.MM.YYYY`, and `DD
MM YYYYY` (if `EUROPEAN_DATES` is set).  If the DATESTRING is successfully
parsed, the variables `year`, `month`, and `day` are set to their respective
numbers.  `date_arg` is another name for the same function.

If DATESTRING is empty, a line of input from STDIN is read and used instead.

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

Converts JDAYS (a Julian day number) into the corresponding date.  If the date
is greater than October 10, 1584, then the Gregorian calendar is used,
otherwise the Julian calendar is used for earlier dates.

    adays_to_date ABSDAYS

Converts ABSDAYS into a date formatted by `print_date`.

    adays_to_jdays ADAYS
    jdays_to_adays JDAYS

These functions convert from absolute days to Julian day number, and vice-versa.

    weeknumber [DATESTRING | YYYY MM DD]

Returns the week number for the given DATESTRING or DATE.

    date_to_weekday_name [DATESTRING | YYYY MM DD]

Returns the weekday name for the given DATESTRING or DATE.

    date_to_weekday_num [DATESTRING | YYYY MM DD]

Returns the wekday number (0..6) for the given DATESTRING or DATE.

    date_day_num [DATESTRING | YYYY MM DD]

Returns the day number for the given DATESTRING or DATE.

    date_format [FORMAT] YYYY MM DD
    date_format [FORMAT] YYYY-MM-DD

The `format_date` function accepts an optional format string, followed by
three numeric arguments, for the year, month, and day, or a single string
argument in any of the parsable date formats, and reformats into the default
date format, given by DATE_FORMAT.  If DATE_FORMAT is not defined, the format
`%F` is used.  (See `man strftime` for details).

EOF

}
date_help() { help_date ; }

## FIXME -- maybe don't need this
## source real-utils.sh

# ensure EUROPEAN_DATES is unset by default
if [[ -z "$EUROPEAN_DATES" ]]; then
  export EUROPEAN_DATES=0
fi

# This application assumes that the changeover from the Julian calendar to the
# Gregorian calendar occurred in October of 1582, according to the scheme
# instituted by Pope Gregory XIII. Specifically, for dates on or before 4
# October 1582, the Julian calendar is used; for dates on or after 15 October
# 1582, the Gregorian calendar is used. Thus, there is a ten-day gap in
# calendar dates, but no discontinuity in Julian dates or days of the week: 4
# October 1582 (Julian) is a Thursday, which begins at JD 2299159.5; and 15
# October 1582 (Gregorian) is a Friday, which begins at JD 2299160.5. The
# omission of ten days of calendar dates was necessitated by the astronomical
# error built up by the Julian calendar over its many centuries of use, due to
# its too-frequent leap years.▫

GREGORIAN_START_DATE='1582-10-4'

GREGORIAN_START_YEAR=1582
GREGORIAN_START_MONTH=10
GREGORIAN_START_DAY=4

GREGORIAN_START_JDAY=2299150     # Julian Day Number for GREGORIAN_START_DATE

# functions for date management
#
# date_parse YYYY MM DD
#            MM DD YYYY
#            DD MM YYYY       if EUROPEAN_DATES is set
#            MMMM DD, YYYY
#            DD MMM YYYY
#            WDAY MMM DD HH:MM:SS TZONE YEAR    (Unix date format)
#
# Wherever there is a space, the parser accepts '-', '/', or '.'  In other
# words, all of these dates are equally parsable: YYYY/MM/DD, YYYY-MM-DD,
# YYYY.MM.DD.
#
# If EUROPEAN_DATES is not set at all, then `NN.NN.YYYY` is interpreted as
# `DD.MM.YYYY`.
#
# parse out year, month, and days into these vars: year, month, day.  When the
# Unix date format is parsed, the variables weekday_name, timestr, and tzone
# are also set.

date_parse() {
  case ${#@} in
    0|1) date_parse_str "$1" ;;
    *)   date_parse_ymd "$@" ;;
  esac
}
# older names -- keep for compatibility
parse_date() { date_parse "$@" ; }
date_arg()   { date_parse "$@" ; }

# date_parse_str DATESTRING
#
# parse the DATESTRING.  It can be in one of several formats: YYYY-MM-DD,
# YYYY.MM.DD, YYYY/MM/DD, YYYY MM DD, DD MMM YYYY, MMM DD, YYYY, and DD/MM/YYYY
# (if EUROPEAN_DATES is set).
#
# Sets the variables: year, month, day  --- unless there was a parser failure

date_parse_str() {
  local date="${1:-`date +%F`}"
  local euro_format=
  weekday_name= tzone= timestr= year= month= day=
  case "$date" in
    *-*-*) date="${date//-/ }"                  ;;  # replace '-' with blanks
    *.*.*) date="${date//./ }"  ; euro_format=1 ;;  # replace '.' with blanks
    */*/*) date="${date//\// }"                 ;;  # repace '/' with blanks
  esac

  # YYYY MM DD
  if [[ "$date" =~ ([0-9]{4})\ ([ 0-9]{1,2})\ ([ 0-9]{1,2}) ]]; then # yyyy mm dd
    date_parse_ymd ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]}

  # MMM DD, YYYY   or MMMM DD, YYYY
  elif [[ "$date" =~ ([[:alpha:]]+)\ +([0-9]{1,2}),?\ +([0-9]{4}) ]]; then
    date_parse_mmmdy ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]}

  #  DD MMM YYYY
  elif [[ "$date" =~ ([0-9]{1,2})\ +([[:alpha:]]+)\ +([0-9]]{4}) ]]; then
    date_parse_mmmdy ${BASH_REMATCH[2]} ${BASH_REMATCH[1]} ${BASH_REMATCH[3]}

  # MM DD YYYY  or  DD MM YYYY
  elif [[ "$date" =~ ([0-9]{1,2})\ ([ 0-9]{1,2})\ ([0-9]{4}) ]] ; then # mm dd yyyy   or  dd mm yyyy
    if (( EUROPEAN_DATES || euro_format )) ; then
      date_parse_dmy ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]}
    else
      date_parse_mdy ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]}
    fi

  # WDAY MMM DD HH:MM:SS TZONE YYYY
  elif [[ "$date" =~ ([[:alpha:]]{2,9})\ ([[:alpha:]]{3,8})\ ([ 0-9]{2})\ ([0-9]{2}:[0-9]{2}:[0-9]{2})\ ([[:alnum:]]{3,9})\ ([0-9]{4}) ]] ; then
    date_parse_mmmdy ${BASH_REMATCH[2]} ${BASH_REMATCH[3]} ${BASH_REMATCH[6]}
    weekday_name=${BASH_REMATCH[1]} timestr="${BASH_REMATCH[4]}" tzone="${BASH_REMATCH[5]}"

  else  # failure -- leave no variables set
    :
  fi
}

# date_parse_ymd   YEAR [MONTH] [DAY]
# date_parse_mdy   MONTH [DAY] [YEAR]
# date_parse_dmy   DAY [MONTH] [YEAR]
# date_parse_mmmdy MONTHNAME DAY YEAR
#
# Set three global variables 'year', 'month', 'day'
#
# The default for a missing MONTH or DAY is 1.  The default for a missing year
# is the current year.

check_parsed_date_values() {
  (( month >= 1 && month <= 12 )) || error "Bad month value: $month"
  (( day >= 1 && day <= 31 ))     || error "Bad day value: $day"
  [[ -n "year" ]]                 || error "Bad year value: $year"
}

date_parse_ymd() {
  year=$(( 10#$1 ))  month=$(( 10#${2:-1} ))  day=$(( 10#${3:-1} ))
  check_parsed_date_values
}

date_parse_mdy() {
  month=$(( 10#$1 ))  day=$(( 10#$2 )) year=$(( 10#$3 ))
  check_parsed_date_values
}

date_parse_dmy() {
  day=$(( 10#$1 ))  month=$(( 10#$2 )) year=$(( 10#$3 ))
  check_parsed_date_values
}

date_parse_mmmdy() {
  month=`month_number $1` day=$(( 10#$2 ))  year=$(( 10#$3 ))
  check_parsed_date_values
}

# month_number MONTHNAME
#
# Given a monthname or its abbrevation, return its index

month_number() {
  local mon="`echo "$1" | tr 'A-Z' 'a-z'`"
  case "$mon" in
    january|jan|ja)   echo 1 ;;
    february|feb|fe)  echo 2 ;;
    march|mar|ma)     echo 3 ;;
    april|apr|ap)     echo 4 ;;
    may|ma)           echo 5 ;;
    june|jun|ju)      echo 6 ;;
    july|jul)         echo 7 ;;
    august|aug|au)    echo 8 ;;
    september|sep|se) echo 9 ;;
    october|oct|se)   echo 10 ;;
    november|nov|no)  echo 11 ;;
    december|dec|de)  echo 12 ;;
  *) error "month_number: Bad monthname: $1" ;;
  esac
}
month_num() { month_number "$@" ; }


# days_in_month [MM | MONTHNAME]
#
# (where MM = 01 .. 12 , or Jan, Feb, ...,  or Ja, Fe, .. )

days_in_month() {
  local mon=`echo "$1" | tr 'A-Z' 'a-z'`
  case "$mon" in
    1|01|january|jan|ja)   echo 31 ;;
    2|02|february|feb|fe)  echo 28 ;;
    3|03|march|mar|ma)     echo 31 ;;
    4|04|april|apr|ap)     echo 30 ;;
    5|05|may|ma)           echo 31 ;;
    6|06|june|jun|ju)      echo 30 ;;
    7|07|july|jul)         echo 31 ;;
    8|08|august|aug|au)    echo 31 ;;
    9|09|september|sep|se) echo 30 ;;
    10|10|october|oct|se)  echo 31 ;;
    11|11|november|nov|no) echo 31 ;;
    12|12|december|dec|de) echo 31 ;;
    *) error "Bad month name or index: $1" ;;
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

# is_julian_date [YYYY MM DD]
#
# Check DATE to see if it is a Julian date; that is, it precedes
# GREGORIAN_START_DATE.  If DATE is omitted, use current values of YEAR MONTH
# and DAY.

is_julian_date() {
  if (( $# > 0 )); then
    local year month day
    parse_date "$@"
  fi
  if (( year < GREGORIAN_START_YEAR || 
        (year == GREGORIAN_START_YEAR && 
          (month < GREGORIAN_START_MONTH ||
            (month == GREGORIAN_START_MONTH && 
              day < GREGORIAN_START_DAY ) ) ) )) ; then
    return 0
  else
    return 1
  fi
}

is_gregorian_date() {
  if is_julian_date "$@" ; then
    return 1
  else
    return 0
  fi
}


# date_to_jdays [DATESTRING | YYYY MM DD]
# echo [DATESTRING | YYYY MM DD] | date_to_jdays
#
# Julian calculation algorithms borrowed from:
# http://www.tondering.dk/claus/cal/julperiod.php
# http://aa.usno.navy.mil/faq/docs/JD_Formula.php
# http://my.safaribooksonline.com/book/xml/0596009747/dates-and-times/xsltckbk2-chp-4-sect-4

date_to_jdays() {
  local year month day
  date_parse `args_or_input "$@"`
  local a y m jdays
  (( a = (14 - month) / 12 ))
  (( y = year + 4800 - a ))
  (( m = month + 12*a - 3 ))
  if is_julian_date ; then
    (( jdays = day + (153*m + 2)/5 + y*365 + y/4                 - 32083 ))
  else
    (( jdays = day + (153*m + 2)/5 + y*365 + y/4 - y/100 + y/400 - 32045 ))
  fi
  echo $jdays
}

# jdays_to_date -- convert julian days into gregoriate date

jdays_to_date() {
  local jdays=`numarg_or_input $1`
  local a b c d e m 
  if (( jdays < GREGORIAN_START_JDAY )) ; then
    (( b = 0 ))
    (( c = jdays + 32082 ))
  else
    (( a = jdays + 32044 ))
    (( b = (4*a + 3)/146097 ))
    (( c = a - (b*146097)/4 ))
  fi
  jdatefunc_to_date $b $c
}

# jdays_to_jdate -- convert julian days into a julian date

jdays_to_jdate() {
  local jdays=`numarg_or_input $1`
  jdatefunc_to_date 0 $(( jdays + 32082 ))
}

# julian date function to return a date YYYY-MM-DD

jdatefunc_to_date() {
  local b c d e m
  (( b = $1 , c = $2                 ))
  (( d = (4*c + 3)/1461              ))
  (( e = c - (1461*d)/4              ))
  (( m = (5*e + 2)/153               ))
  (( day   = e - (153*m + 2)/5 + 1   ))
  (( month = m + 3 - 12*(m/10)       ))
  (( year  = b*100 + d - 4800 + m/10 ))
  printd $year $month $day
}

# The difference, in days, between a julian day and an absolute day
JDAY_ADAY_OFFSET=1721424

# jday_to_aday [JDAY]
# echo JDAY | jday_to_aday

jdays_to_adays() {
  local jdays=`numarg_or_input "$@"`
  local adays=$(( jdays - JDAY_ADAY_OFFSET + 1 ))
  echo $adays
}

adays_to_jdays() {
  local adays=`numarg_or_input "$@"`
  local jdays=$(( adays + JDAY_ADAY_OFFSET - 1 ))
  echo $jdays
}

adays_to_date() {
  local jdays=`adays_to_jdays "$@"`
  jdays_to_date "$jdays"
}

date_to_adays() {
  local jdays=`date_to_jdays "$@"`
  jdays_to_adays "$jdays"
}

# week_number [DATESTRING | YYYY MM DD]
#
# See 
# http://my.safaribooksonline.com/book/xml/0596009747/dates-and-times/xsltckbk2-chp-4-sect-4

week_number() {
  local jdays=`date_to_jday "$@"`
  local d4=$(( ( ( ( jdays + 31741 - (jdays % 7) ) % 146097) % 36524) % 1461 ))
  local l=$(( d4 / 1460 ))
  local d1=$(( ( (d4 - l) % 365 ) + l ))
  local week_no=$(( ( d1 / 7 ) + 1 ))
  echo $week_no
}

# ISO dates
# TBD


# date_day_num YYYY MM DD
#
# Return the day of year for the given date

date_day_num() {
  local year month day
  date_parse `args_or_input "$@"`
  local year_jdays=`date_to_jdays $year 1 1`
  local jdays=`date_to_jdays $year $month $day`
  echo $(( jdays - $year_jdays + 1 ))
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

### (defsubst calendar-absolute-from-gregorian (date)
###   "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
### The Gregorian date Sunday, December 31, 1 BC is imaginary.
### DATE is a list of the form (month day year).  A negative year is
### interpreted as BC; -1 being 1 BC, and so on.  Dates before 12/31/1 BC
### return negative results."
###   (let ((year (calendar-extract-year date))
###         offset-years)
###     (cond ((zerop year)
###            (error "There was no year zero"))
###           ((> year 0)
###            (setq offset-years (1- year))
###            (+ (calendar-day-number date) ; days this year
###               (* 365 offset-years)       ; + days in prior years
###               (/ offset-years 4)         ; + Julian leap years
###               (- (/ offset-years 100))   ; - century years
###               (/ offset-years 400)))     ; + Gregorian leap years
###           (t
###            ;; Years between date and 1 BC, excluding 1 BC (1 for 2 BC, etc).
###            (setq offset-years (abs (1+ year)))
###            (- (calendar-day-number date)
###               (* 365 offset-years)
###               (/ offset-years 4)
###               (- (/ offset-years 100))
###               (/ offset-years 400)
###               (calendar-day-number '(12 31 -1))))))) ; days in year 1 BC

# gregorian_date_to_adays DATESTRING
# gregorian_date_to_adays YEAR MONTH DAY

# The absolute days from Gregorian 12/31/1 BC and DATESTRING/YEAR-MONTH-DAY
# DATE is either a DATESTRING, or YEAR MONTH DAY.  A negative year is
# interpreted as BC; -1 is 1 BC, etc.  Dates before 12/31/1 BC return negative
# results.

gregorian_date_to_adays() {
  local year month day
  date_parse `args_or_input "$@"`
  if (( year == 0 )) ; then
    echo 1>&2 "There was no year zero" ; exit 1
  fi
  local day_num=`date_day_num $year $month $day`
  if (( year > 0 )); then
    local offset_years=$(( year - 1 ))
    local days_in_1BC=0
  else
    # years between date and 1 BC, excluding 1 BC (1 of 2 BC, etc.)
    local offset_years=`abs $year + 1`
    local days_in_1BC=`date_day_num -1 $month $day`
  fi
  local      prior_year_days=$(( offset_years * 365 ))
  local    julian_leap_years=$(( offset_years / 4 ))
  local        century_years=$(( -( offset_years / 100 ) ))
  local gregorian_leap_years=$(( offset_years / 400 ))
  if (( year > 0 )) ; then
    echo $(( day_num + prior_year_days + julian_leap_years - century_years + gregorian_leap_years ))
  else
    echo $(( day_num - prior_year_days - julian_leap_years + century_years - gregorian_leap_years - days_in_1BC ))
  fi
}

# compute absolute value from X
abs() { echo $(( $1 < 0 ? -$1 : $1 )) ; }

# date_format  FORMAT
# date_format [FORMAT] [DATESTRING]
# date_format [FORMAT] [YYYY MM DD]
#
# The FORMAT is a string of characters, some of which are special format
# characters, as defined by `strftime` (see the `man` page).
#
# If the FORMAT is omitted, the default is `DATE_FORMAT`, which itself
# defaults to `%F`.

date_format() {
  local format year month day
  case "$#" in
    1|2|4) format="$1" ; shift ;;
    *)     format="${DATE_FORMAT:-%F}" ;;
  esac
  case "$#" in
    3) date_parse "$@" ;;
    *) date_parse "${1:-`date +%F`}" ;;
  esac
  # year, month, day have values
  #date -jn -f '%Y-%m-%d' "$year-$month-$day" +"$format"
  local of new
  of=''
  while [[ format =~ ^([^%]*)%(.)(.*)$ ]]; do
    of="$of$1" format="$3" new=''
    case "$2" in
      A) new=`df_weekday_name`   ;;
      a) new=`df_weekday_abbrev` ;;
      B) new=`df_month_name`     ;;
      b) new=`df_month_abbrev`   ;;
      C) new=`df_century`        ;;
      c) new=`df_date_time`      ;;
      D) new=`df_mmddyy`         ;;
      d) new=`df_day`            ;;
      e) new=`df_month`          ;;
      F) new=`df_fin_date`       ;;
      G) new=`df_year4`          ;;
      g) new=`df_year2`          ;;
      j) new=`df_day_of_year`    ;;
      n) new=$'\n'               ;;
      t) new=$'\t'               ;;
      U) new=`df_week_num4`      ;;
      u) new=`df_weekday_num1`   ;;
      V) new=`df_week_num_ISO`   ;;
      v) new=`df_eby4`           ;;
      W) new=`df_week_num`       ;;
      w) new=`df_weekday_num0`   ;;
      x) new=`df_date`           ;;
      Y) new=`df_year4`          ;;
      '%') new='%'               ;;
      *) new="$2"                ;;
    esac
    of="$of$new"
  done
  echo "$of"
}
format_date() { date_format "$@" ; }

#print_date()  { date_format "$@" ; }
#printd()      { date_format "$@" ; }


# weekday_names can be indexed with 0..6 (Sun..Sat) or 1..7 (Mon..Sun)
# Index          0   1   2   3   4   5   6   7
weekday_names=( Sun Mon Tue Wed Thu Fri Sat Sun )

df_weekday_name() {		# A
  echo "`df_weekday_abbrev`day"
}

df_weekday_abbrev() {		# a
  local wd=`df_weekday_num0`
  echo "${weekday_names[$wd]}"
}

# date_to_weekday_num DATESTRING
# date_to_weekday_num YEAR MONTH DAY

date_to_weekday_num() {
  days=`date_to_adays "$@"`
  echo $(( days % 7 ))
}

date_to_weekday_name() {
  local wd=`date_to_weekday_num "$@"`
  echo "${weekday_names[$wd]}"
}

# Index           1     2        3      4     5   6    7    8      9         10      11       12
month_names=( - January February March April May June July August September October November December )

df_month_name() {		# B
  echo "${month_names[$month]}"
}
df_month_abbrev() {		# b
  local name=`df_month_name`
  echo "${name:0:3}"
}

df_century() {		        # C
  echo "$(( year / 100 ))"
}

df_date_time() {		# c
  local dow=`df_weekday_abbrev`
  local mmm=`df_month_abbrev`
  echo "$dow $mmm $day $year"
}

df_mmddyy() {		        # D
  printf "%02d/%02d/%4d\n" $month $day $year
}

df_day() {		        # d - the day of the month as a decimal number (01-31).
  printf "%02d\n" $day
}

df_month() {		        # e - day of the month as a decimal number (1-31); 
  printf "%2d\n" $month         #     single digits are preceded by a blank.
}

df_fin_date() {		        # F - quivalent to ``%Y-%m-%d''.
  printf "%4d-%02d-%02d\n" $year $month $day
} 

df_year4() {		        # G - year as a decimal number with century.
  printf "%4d\n" $year
}

df_year2() {		        # g - year as in ``%G'', but as a decimal number without century (00-99).
  printf "%02d\n" $(( year % 100 ))
}

df_day_of_year() {		# j - the day of the year as a decimal number (001-366).
  echo "$(( days_before_month[$month] + $day ))"
}

n() { echo $'\n' ; }            # n

t() { echo $'\t' ; }            # t

df_weekday_num1() {             # u
  local wd=`date_to_weekday_num $year $month $day`
  echo $(( wd == 0 ? 7 : $wd )) # return 1..7 (Mon .. Sun)
}

df_week_num_ISO() {		# U
  : 
}

df_week_num() {		        # V
  local wn=`week_number "$@"`
  echo $wn
}

df_eby4() {		        # v
  local mname=`df_month_abbrev`
}
df_week_num() {		        # W
  :
}
df_weekday_num0() {		# w
  date_to_weekday_num $year $month $day   # output 0..6 for Sun..Sat
}
df_date() {		        # x
  printf "%02d/%02d/%04d\n" $month $day $year
}
df_year4() {		        # Y
  printf "%04d\n" $year
}


# print_date [FORMAT] YYYY MM DD
# print_date [FORMAT] DATESTRING
#
# Print the date in the YYYY-MM-DD format.

print_date() {
  local year month day
  date_parse "$@"
  printf "%04d-%02d-%02d\n" $year $month $day
}
printd() { print_date "$@" ; }

# Set a constant for the epoch
days_at_epoch=`date_to_adays 1970-01-01`

# date_to_days_since_epoch YYYY-MM-DD

date_to_days_since_epoch() {
  local date="${1:-`date +%F`}"
  local adays=`date_to_adays "$date"`
  echo $(( adays - days_at_epoch ))
}

# get_date_5_years_before [YYYY-MM-DD]
#
# get_date_last_quarter_end YYYY-MM-DD
#
# Both the above routines output a date string, in YYYY-MM-DD format.
# Both accept a date as input, the absence of which defaults to now.

# get_date_5_years_before [YYYY-MM-DD]
#
# Get the date five years before the given date

get_date_5_years_before() {
  get_date_x_years_before 5 $1
}

# get_date_x_years_before YEARSOFFSET [YYYY-MM-DD]
# get_date_x_years_after  YEARSOFFSET [YYYY-MM-DD]
# get_date_x_years_since  YEARSOFFSET [YYYY-MM-DD]
#
# Get the date X years before or after the given date

get_date_x_years_after() {
  local yoffset="${1:?'Missing number of years'}"
  shift
  local year month day
  date_parse "$@"
  (( year += yoffset )) # get X years later/before
  print_date $year $month $day
}
get_date_x_years_since()  { get_date_x_years_after "$@" ; }
get_date_x_years_before() { get_date_x_years_after "-$@" ; }

# get_date_last_quarter_end YYYY-MM-DD
#
# given a date, get the previous quarter end date plus one.
# If no date, use the current date

get_date_last_quarter_end() {
  local year month day
  date_parse "$@"
  month=$(( ( ( ( month - 1 ) / 3 ) * 3 ) + 1 ))    # get previous quarter month
  print_date $year $month 1
}

# end of date-utils.sh
# vim: set ai sw=2
