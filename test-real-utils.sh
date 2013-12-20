#!/bin/bash
# Copyright 2013, Alan K. Stebbens <aks@stebbens.org>
#
# test-real-utils.sh -- test the real-utils script

PATH=.:$PATH:$HOME/lib

source test-utils.sh
source real-utils.sh

echo "Testing real-utils .."

# test_cond EXPR [TRUE|FALSE] [SCALE]
function TEST_cond() {
  local scale=$real_scale  expect=true        # set defaults
  local expr="$1" ; shift
  while [[ $# -gt 0 ]]; do
    case "$1" in
      true|false) expect=$1 ;;
      [0-9]*)     scale="$1" ;;
    esac
    shift
  done
  if real_cond "$expr" $scale ; then
    res=true
  else
    res=false
  fi
  check_equal "$res" "$expect" "'$expr' did not evaluate to $expect"
  #if [[ "$res" = "$expect" ]]; then
  #  printf "ok: [ %5s ]: %s\n" "$expect" "$expr"
  #else 
  #  printf "no: [ %5s ]: %s\n" "$expect"  "$expr"
  #fi
}

# TEST_math 'MATH_EXPRESSION'  ANSWER

function TEST_math() {
  local expr="$1"
  local ans=`real_eval "$1"`
  check_equal "$ans" "$2"  "'$1' did not evaluate to '$2'"
}

# Use command line arguments if there are any.

test_basic_arithmetic() {
  start_test
  TEST_math "scale=3; 21.5 / 6.4"       "3.359"
  TEST_math "200.5 / 5.3 + 3.6 * 7.2"   "63.75"
  TEST_math "scale=1; (12.0 / 3.0)"     "4.0"
  end_test
}

test_cond_evals() {
  start_test
  TEST_cond "10.1 > 10.0"
  TEST_cond "10.1 >= 10.1"
  TEST_cond "10.1 <  10.1" false
  TEST_cond "10.0 < 9.9"   false
  TEST_cond "10.0 > 10.01" false
  TEST_cond "0.1 >= 0.1"
  TEST_cond "0.1 > 0.09"
  TEST_cond "0.1 < 0.11"
  end_test
}

test_powers() {
  start_test
  TEST_cond "2^2 == 4"
  TEST_cond "2^3 == 8"
  TEST_cond "2^4 == 16"
  TEST_cond "2^5 == 32"
  TEST_cond "2.2^2 == 4.84"     3
  TEST_cond "5.2^3 == 140.608"  3
  end_test
}

init_tests "$@"
run_tests
summarize_tests

exit

# vim: ts=2: sw=2: expandtab

