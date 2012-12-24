#!/bin/bash
# Copyright 2006-2011, Alan K. Stebbens <aks@stebbens.org>
#
# test-template.sh -- a template on which to create new test cases

PATH=.:$PATH:$HOME/lib

source test-utils.sh

test_1_NAME1() {
  start_test

  # ... do some operations to be tested
  # ... now test the results of the operations 

  # check_value        VAR              ERROR
  # check_empty        VAR              ERROR
  #
  # array size and item tests
  #
  # check_size         LIST SIZE        ERROR  # same as check_size_eq
  # check_size_XX      LIST SIZE        ERROR 
  # check_item         VAR INDEX VAL    ERROR
  # check_item_equal   VAR INDEX VAL    ERROR
  # check_item_unequal VAR INDEX NONVAL ERROR
  #
  # String value tests
  #
  # check_equal        VAL1 VAL2        ERROR
  # check_unequal      VAL1 VAL2        ERROR
  # check_match        VAL1 REGEXP      ERROR
  # check_nomatch      VAL1 REGEXP      ERROR
  #
  # Numeric value tests
  #
  # check_lt          N1 N2             ERROR
  # check_le          N1 N2             ERROR
  # check_eq          N1 N2             ERROR
  # check_ne          N1 N2             ERROR
  # check_ge          N1 N2             ERROR
  # check_gt          N1 N2             ERROR
  #
  # ERROR is optional
  #... 
  #... more operations

  end_test
}

test_2_NAME2() {
   start_test
   #....
   end_test
}

# ... more tests

# If you pass arguments to init_tests, they will be processed just like
# command-line args (ie? -v == verbose, -e == verbose_errors, -n == norun, 
# -h == help)

init_tests  "$@"
run_tests
summarize_tests
exit
