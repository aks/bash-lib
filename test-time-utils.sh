#!/usr/bin/env bash
# Copyright 2014, Alan K. Stebbens <aks@stebbens.org>
#
# test time-utils.sh
#
# uses test-utils.sh

PATH=.:$PATH:$HOME/lib

source test-utils.sh

. time-utils.sh

testdata='test-times.dat'

# check_time TIMESTRING HH MM SS

check_time() {
  time_parse     "$1"
  check_eq $hours $2
  check_eq $mins  $3
  check_eq $secs  $4
}

test_01_parse_time() {
  start_test
  check_time '00:00:00'   0   0   0
  check_time '12:00:00'  12   0   0
  check_time '12:01:02'  12   1   2
  check_time '23:22:11'  23  22  11
  check_time '23:59:59'  23  59  59
  check_time '12:01'     12  01  00
  end_test
}

# check_t2secs TIMESTRING SECS

check_t2secs() {
  local secs=`time2secs "$1"`
  check_eq $secs        "$2"
}

test_10_time2secs() {
  start_test
  check_t2secs    '00:00:01'      1
  check_t2secs    '00:00:59'     59
  check_t2secs    '00:01:00'     60
  check_t2secs    '00:01:59'    119
  check_t2secs    '00:11:22'    682  # 11*60+22
  check_t2secs    '00:59:59'   3599
  check_t2secs    '01:00:00'   3600
  check_t2secs    '01:02:03'   3723
  check_t2secs    '12:34:56'  45296
  check_t2secs    '23:59:59'  86399
  end_test
}

init_tests "$@"
run_tests
summarize_tests
exit
