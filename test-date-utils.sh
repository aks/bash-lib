#!/usr/bin/env bash
# Copyright 2006-2014, Alan K. Stebbens <aks@stebbens.org>
#
# test date-utils.sh
#
# uses test-utils.sh

PATH=.:$PATH:$HOME/lib

source test-utils.sh

. date-utils.sh

known_dates=( 0001-1-1       1
              1492-1-1  544577
              1492-3-1  544637
              1776-1-1  648306
              1776-3-1  648366
              1776-7-4  648491 
            )

test_01_conversion_equivalencies() {
  start_test
  last_days=
  for ((year=1; year<=2082; year+=100)) ; do
    for month in 1 3 ; do
      mdays=`last_day_of_month $year $month`
      for day in 1 15 $mdays ; do
        date1=`printd $year $month $day`
        days=`date_to_abs_days $date1`
        check_value "$days"         "Empty days!?"              # should never be empty
        if [[ -n "$last_days" ]]; then
          check_gt $days $last_days "Days failed to increase"   # should keep incrementing
        fi
        last_days=$days
        date2=`abs_days_to_date $days`
        check_equal "$date1" "$date2" "Bad conversion"
      done
    done
    # echo ''
  done
  end_test
}

test_02_known_dates() {
  start_test
  check_gt "$dates_count" 0
  limit="${#known_dates[@]}"
  for ((x=0; x<${#known_dates[@]}; x+=2)); do
    date_in="${known_dates[$x]}" 
    days_in="${known_dates[$x+1]}"
    if (( verbose )) ; then
      echo 1>&2 $'\n'"testing $date_in and $days_in .."
    fi
    days_out=`date_to_abs_days $date_in`
    check_eq $days_out $days_in "Incorrect conversion of $date_in; should be: $days_in ; was $days_out"
    date_out=`abs_days_to_date $days_in`
    fmt_date_in=`print_date $date_in`
    check_equal $date_out $fmt_date_in "Incorrect conversion of $days_in; should be: $fmt_date_in; was $date_out"
  done
  end_test
}

# yyyy-mm-dd
# 0123 56 89

test_03_years_offset() {
  start_test
  start_date="2008-03-05"
  year=${start_date:0:4}
  mm=${start_date:5:2}
  for ((i=1; i<=5; i++)) ; do
    date_then=`get_date_x_years_before $i $start_date`
    check_value "$date_then"
    year2=${date_then:0:4}
    check_eq $(( year - i )) $year2
    check_eq $mm ${date_then:5:2}

    date_since=`get_date_x_years_since $i $start_date`
    check_value "$date_since"
    year3=${date_since:0:4}
    check_eq $(( year + i )) $year3
    check_eq $mm ${date_since:5:2}
  done
  today=`date +%F`
  odate1=`get_date_x_years_before 1`
  odate2=`get_date_x_years_before 1 $today`
  check_equal "$odate1" "$odate2" "get_date_x_years_before default doesn't match explicit"
  odate1=`get_date_x_years_after 10`
  odate2=`get_date_x_years_after 10 $today`
  check_equal "$odate1" "$odate2" "get_date_x_years_after default doesn't match explicit"
  end_test
}

# check_date 'mm/dd/yyyy' yyyy mm dd

check_date() {
  parse_date "$1"
  check_eq $year $2
  check_eq $month $3
  check_eq $day   $4
}

test_04_parse_date() {
  start_test
  check_date '1/1/2014'  2014 1  1
  check_date '2/28/1981' 1981 2 28
  check_date '3-31-1985' 1985 3 31
  check_date '4-15-2016' 2016 4 15
  check_date '2016-4-15' 2016 4 15
  check_date '2016/8/25' 2016 8 25
  check_date '8/25/2016' 2016 8 25
  end_test
}

check_dim() { 
  local x y
  x=`days_in_month $1`
  check_eq "$x" "$2"
}

test_05_days_in_month_mmm() {
  start_test
  check_dim Jan 31
  check_dim Feb 28
  check_dim Mar 31
  check_dim Apr 30
  check_dim May 31
  check_dim Jun 30
  check_dim Jul 31
  check_dim Aug 31
  check_dim Sep 30
  check_dim Oct 31
  check_dim Nov 31
  check_dim Dec 31
  end_test
}

test_06_days_in_month_mmmm() {
  start_test
  check_dim January   31
  check_dim February  28
  check_dim March     31
  check_dim April     30
  check_dim May       31
  check_dim June      30
  check_dim July      31
  check_dim August    31
  check_dim September 30
  check_dim October   31
  check_dim November  31
  check_dim December  31
  end_test
}
test_07_days_in_month_num() {
  start_test
  check_dim 1 31
  check_dim 2 28
  check_dim 3 31
  check_dim 4 30
  check_dim 5 31
  check_dim 6 30
  check_dim 7 31
  check_dim 8 31
  check_dim 9 30
  check_dim 10 31
  check_dim 11 31
  check_dim 12 31
  end_test
}

check_leap_year() {
  local ly=n
  is_leap_year "$1" && { ly=y ; }
  check_equal "$ly" "$2" "is_leap_year failed for '$1'; should be '$2'"
}

test_08_leap_year() {
  start_test
  check_leap_year 2004 y
  check_leap_year 2003 n
  check_leap_year 2002 n
  check_leap_year 2001 n
  check_leap_year 2000 y
  end_test
}

test_09_last_day_of_month() {
  start_test
  x=`last_day_of_month 2000 1`
  check_eq $x 31
  x=`last_day_of_month 2001 1`
  check_eq $x 31
  x=`last_day_of_month 2000 2`
  check_eq $x 29
  x=`last_day_of_month 2001 2`
  check_eq $x 28
  end_test
}

# check_date_x_years_before X STARTDATE RESULTDATE

check_date_x_years_before() {
  local dt
  dt=`get_date_x_years_before $1 "$2"`
  check_equal $dt `format_date "$3"` "get_date_x_years_before failed: $1 years from $2 should be \"$3\", got \"$dt\""
}
check_date_x_years_after() {
  local dt
  dt=`get_date_x_years_after $1 "$2"`
  check_equal $dt `format_date "$3"` "get_date_x_years_after failed: $1 years after $2 should be \"$3\", got \"$dt\""
}

test_10_date_x_years_before() {
  start_test
  check_date_x_years_before 1 2001/11/1 2000/11/1
  check_date_x_years_before 5 2011/8/5  2006/8/5
  check_date_x_years_after  1 2001/11/1 2002/11/1
  check_date_x_years_after  5 2011/8/5  2016/8/5
  end_test
}

(( dates_count=${#known_dates[@]} / 2 ))
echo 1>&2 "$dates_count date-day pairs defined."

init_tests "$@"
run_tests
summarize_tests
exit
