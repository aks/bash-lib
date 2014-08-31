#!/usr/bin/env bash
# test-sh-utils.sh
#
# Copyright 2006-2014 Alan K. Stebbens <aks@stebbens.org>
#
# Test module for sh-utils.sh
#
export PATH=.:$PATH
source sh-utils.sh
source test-utils.sh


test_01_talk_basic() {
  start_test
  check_output talk1 'talk "one"'
  check_output talk2 'talk "two"'
  check_output talk3 'talk "three"'
  end_test
}

test_02_talk_advanced() {
  start_test
  check_output talk_w_2_args 'talk "one two three"'
  check_output talk_w_2_args2 'talk one two three words'
  end_test
}

test_03_talk_varying_norun_verbose_quiet() {
  check_output talk_functions talk_with_varying_norun_verbose_quiet
}

talk_with_varying_norun_verbose_quiet() {
  local norun verbose quiet func
  for norun in '' 1 ; do
    for verbose in '' 1 ; do
      for quiet in '' 1 ; do
        for func in vtalk votalk nrtalk nqtalk ; do
          echo -n 1>&2 "norun=$norun verbose=$verbose quiet=$quiet : $func says: "
          $func "There is output" || echo 1>&2 ""
        done
        echo 1>&2 ""  # separate function test groups
      done
    done
  done
}

test_04_talkf_basic() {
  start_test
  check_output talkf_basic_test
  end_test
}

wordlist=( the time has come to talk of ceiling wax and cabbages and kings )

talkf_basic_test() {
  local x=0
  for (( x=0; x < ${#wordlist[*]}; x++ )) ; do
    word=${wordlist[$x]}
    talkf "The %d word is '%s'\n" $x "$word"
  done
}

test_05_vtalkf_basic() {
  start_test
  check_output vtalkf_basic_test
  end_test
}

vtalkf_basic_test() {
  verobse= norun=
  for verbose in '' 1 ; do
    vtalkf "Verbose = %s\n" $verbose
  done
}

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
