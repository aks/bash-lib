#!/usr/bin/env bash
# Copyright 2006-2014, Alan K. Stebbens <aks@stebbens.org>
#
# test date-utils.sh
#
# uses test-utils.sh

PATH=.:$PATH:$HOME/lib

source test-utils.sh

. date-utils.sh

testdata='test-dates.dat'


test_01_conversion_equivalencies() {
  start_test
  last_adays='-1'
  for ((year=1; year<=2082; year+=500)) ; do
    for month in 1 2 3 6 9 12; do
      mdays=`last_day_of_month $year $month`
      for day in 1 15 25 $mdays ; do
        date1=`printd $year $month $day`
        adays=`date_to_adays $date1`
        check_value "$adays"         "Empty days!?"              # should never be empty
        check_lt $last_adays $adays  "ADays should increase"
        last_adays=$adays
        date2=`adays_to_date $adays`
        check_value "$date1" "$date2" "ADays conversion failed: $date1 vs. $date2"
        jdays=`date_to_jdays $date1`
        date3=`jdays_to_date $jdays`
        check_equal "$date1" "$date3" "Bad conversion: $date1 vs. $date3"
        date4=`date_to_jdays $date1 | jdays_to_date`
        check_equal "$date1" "$date4" "Bad pipe conversion: $date1 vs. $date4"
      done
    done
    # echo ''
  done
  end_test
}

test_02_test_dates() {
  start_test
  check_true "test -f $testdata" "Test file '$testdata' does not exist"
  local tdate tadays tjdays
  while read tdate tadays tjdays ; do
    if [[ -z "$tdate" ]]; then break ; fi
    if (( verbose )) ; then
      echo ''
      echo "ref date  = $tdate"
      echo "ref adays = $tadays"
      echo "ref jdays = $tjdays"
    fi
    jdays=`date_to_jdays $tdate`
    adays=`date_to_adays $tdate`
    check_equal "$jdays" "$tjdays" "Wrong jdays for $tdate: got $jdays; should be $tjdays"
    check_equal "$adays" "$adays"  "Wrong adays for $tdate: got $adays; should be $tadays"
  done <$testdata
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

# check_date_time 'yyyymmddhhmm' yyyy mm dd hh mm

check_date_time() {
  check_date "$1" $2 $3 $4
  check_eq $hour   $5
  check_eq $minute $6
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
  check_equal $dt `print_date "$3"` "get_date_x_years_before failed: $1 years from $2 should be \"$3\", got \"$dt\""
}
check_date_x_years_after() {
  local dt
  dt=`get_date_x_years_after $1 "$2"`
  check_equal $dt `print_date "$3"` "get_date_x_years_after failed: $1 years after $2 should be \"$3\", got \"$dt\""
}

test_10_date_x_years_before() {
  start_test
  check_date_x_years_before 1 2001/11/1 2000/11/1
  check_date_x_years_before 5 2011/8/5  2006/8/5
  check_date_x_years_after  1 2001/11/1 2002/11/1
  check_date_x_years_after  5 2011/8/5  2016/8/5
  end_test
}

test_11_date_parse_serials() {
  start_test
  check_date  20140101   2014 1   1
  check_date  20161231   2016 12 31
  end_test
}

test_12_datetime_parse_serials() {
  start_test
  check_date_time  201401010809   2014 1   1  8  9
  check_date_time  201612310708   2016 12 31  7  8
  check_date_time  201612312359   2016 12 31 23 59
  end_test
}

date_parts() {
  echo "${1:0:4} ${1:5:2} ${1:8:2}"
}

test_13_today() {
  start_test
  local tdate=`date +%F`
  local date=`today`
  check_date "$date" `date_parts $tdate`
  end_test
}

test_14_yesterday() {
  start_test
  local ydate=`date -j -v-1d +%F`
  local date=`yesterday`
  check_date "$date" `date_parts $ydate`
  end_test
}

test_15_tomorrow() {
  start_test
  local tdate=`date -j -v+1d +%F`
  local date=`tomorrow`
  check_date "$date" `date_parts $tdate`
  end_test
}

test_16_get_date_x_days_before() {
  start_test
  for offset in 1 3 5 7 15 ; do
    local tdate=`date -j -v-${offset}d +%F`
    local date=`get_date_x_days_before $offset`
    check_date "$date" `date_parts $tdate`
  done
  end_test
}

test_17_get_date_x_days_since() {
  start_test
  for offset in 1 3 5 7 15 ; do
    local tdate=`date -j -v+${offset}d +%F`
    local date=`get_date_x_days_since $offset`
    check_date "$date" `date_parts $tdate`
  done
  end_test
}

init_tests "$@"
run_tests
summarize_tests
exit
