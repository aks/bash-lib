#!/bin/bash
# Copyright 2006-2013, Alan K. Stebbens <aks@stebbens.org>
#
# test-template.sh -- a template on which to create new test cases

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
  for ((year=1; year<=2012; year+=100)) ; do
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
  for ((i=i; i<=5; i++)) ; do
    date_then=`get_date_x_years_since $i $start_date`
    check_value "$date_then"
    year2=${date_then:0:4}
    check_eq $(( year - i )) $year2
    check_eq $mm ${date_then:5:2}
  done
  end_test
}

(( dates_count=${#known_dates[@]} / 2 ))
echo 1>&2 "$dates_count date-day pairs defined."

init_tests "$@"
run_tests
summarize_tests
exit
