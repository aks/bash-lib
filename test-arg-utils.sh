#!/usr/bin/env bash
# test-arg-utils.sh
#
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>
#
# Test module for arg-utils.sh
#
export PATH=.:$PATH
source arg-utils.sh
source test-utils.sh


wordlist=( the time has come to talk of ceiling wax and cabbages and kings )

numarg_test_func() {
  local arg=`numarg_or_input "$1"`
  echo "$arg"
}

args_test_func() {
  local args=( `args_or_input "$@"` )
  echo "${args[@]}"
}

test_10_num_args_or_input() {
  start_test
  local num=`numarg_test_func 62`
  check_eq "$num" 62 "numarg_test_func failed on arg 62"
  local num=`echo 229 | numarg_test_func`
  check_eq "$num" 229 "numarg_test_func failed on stdin 229"
  local args=()
  testdata="foo bar bif baf"
  args=( `args_test_func $testdata ` )
  check_size args 4 "args is the wrong size"
  check_equal "${args[*]}" "$testdata" "args_test_func failed on arg input"
  args=( )
  args=( `echo foo bar bif baf | args_test_func` )
  check_equal "${args[*]}" "$testdata" "args_test_func failed on stdin"
  end_test
}

test_11_append_args() {
  start_test
  local result=`echo 'foo' 'bar' | append_args 'bif' 'baf'`
  check_equal "$result" "foo bar bif baf"
  local result=`echo '' | append_arg 'bam'`
  check_equal "$result" 'bam' "Should be 'bam'; got '$result'"
  local result=`echo 'foo bar' | append_args ''`
  check_equal "$result" 'foo bar ' "Should be 'foo bar'; got '$result'"
  end_test
}

init_tests "$@"
run_tests
summarize_tests
exit
