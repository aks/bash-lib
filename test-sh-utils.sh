#!/usr/bin/env bash
# test-sh-utils.sh
#
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>
#
# Test module for sh-utils.sh
#
export PATH=.:$PATH

source sh-utils.sh
source test-utils.sh

test_01_fn_exists() {
  start_test
  check_false "fn_exists TEST_test_func"
  # create the test func
  TEST_test_func() { my_test_ok=1 ; }
  check_true 'fn_exists TEST_test_func'
  unset -f TEST_test_func
  check_false 'fn_exists TEST_test_func'
  end_test
}

test_10_add_trap() {
  start_test
  local x=$( my_test_ok=2
             add_trap 'my_test_ok=3' USR1
             kill -USR1 $BASHPID
             echo "$my_test_ok"
           )
  check_equal $x 3
  local y=$( my_test_ok=4 my_new_test=1
             add_trap 'my_test_ok=3'  USR1
             add_trap 'my_new_test=2' USR1
             kill -USR1 $BASHPID
             sleep .5
             echo "$my_test_ok $my_new_test"
           )
  check_equal "$y" "3 2"
  end_test
}

init_tests "$@"
run_tests
summarize_tests
exit
