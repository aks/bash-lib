#!/usr/bin/env bash
# Copyright 2014, Alan K. Stebbens <aks@stebbens.org>
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
  check_equal "$res" "$expect" "'$expr' evaluated to '$res', not '$expect'"
}

# TEST_math 'MATH_EXPRESSION'  ANSWER

function TEST_math() {
  local expr="$1"
  local ans=`real_eval "$1"`
  check_equal "$ans" "$2"  "'$1' evaluated to '$ans', not '$2'"
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

test_int_frac_funcs() {
  start_test
  TEST_math "int(3)"        "3"
  TEST_math "int(3.1)"      "3"
  TEST_math "int(3.99)"     "3"
  TEST_math "frac(3.99)"    ".99"
  TEST_math "frac(123.456)" ".456"
  TEST_math "scale=1;int(3)/2"      "1.5"
  TEST_math "scale=1;int(3.2)/2"    "1.5"
  TEST_math "scale=1;frac(3.2)*2"   ".4"
  end_test
}

test_trig_funcs() {
  start_test
  export real_scale=8
  TEST_math  "sin(0)"       "0"
  TEST_math  "sin(0.1)"     ".09983341"
  TEST_math  "sin(0.2)"     ".19866933"
  TEST_math  "sin(0.3)"     ".29552020"
  TEST_math  "sin(0.4)"     ".38941834"
  TEST_math  "sin(0.5)"     ".47942553"
  TEST_math  "sin(0.6)"     ".56464247"
  TEST_math  "sin(0.7)"     ".64421768"
  TEST_math  "sin(0.8)"     ".71735609"
  TEST_math  "sin(0.9)"     ".78332690"
  TEST_math  "sin(1.0)"     ".84147098"
  TEST_math  "sin(1.1)"     ".89120736"
  TEST_math  "sin(1.2)"     ".93203908"
  TEST_math  "sin(1.3)"     ".96355818"
  TEST_math  "sin(1.4)"     ".98544973"
  TEST_math  "sin(1.5)"     ".99749498"
  TEST_math  "sin(1.57)"    ".99999968"
  TEST_math  "sin(1.6)"     ".99957360"
  end_test
}

test_conversions() {
  start_test
  for ((deg = 0; deg <= 360; deg += 5)); do
    # test conversion functions
    rad=`rad $deg 2`
    TEST_math "scale=2 ; rad($deg)" "$rad"
    deg2=`deg $rad 2`
    TEST_math "scale=2 ; deg($rad)" "$deg2"
    diff=`real_eval '$deg - $deg2'`
    # we must allow for a single digit rounding error
    TEST_cond "abs($deg - $deg2) <= 1"
    # Now test for normalized degrees
    # Not ready yet
    #rad=`rad $deg 10`     # get radians
    #sin=`sin $rad 10`     # get sine
    #asin=`asin $sin 10`   # arcsine of sine
    #deg3=`ndeg $asin`     # convert to normalized degrees
    #TEST_cond "abs($deg3 - $deg) < 1"
  done
  end_test
}

test_round() {
  start_test
  TEST_math  "round(1.4,0)"   "1"
  TEST_math  "round(1.5,0)"   "2"
  TEST_math  "round(1.4,1)"   "1.4"
  TEST_math  "round(1.5,1)"   "1.5"
  TEST_math  "round(1.99,1)"  "2.0"
  TEST_math  "round(1.99,2)"  "1.99"
  end_test
}


init_tests "$@"
run_tests
summarize_tests

exit

# vim: ts=2: sw=2: expandtab

