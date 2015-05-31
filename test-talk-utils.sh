#!/usr/bin/env bash
# test-talk-utils.sh
#
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>
#
# Test module for talk-utils.sh
#
export PATH=.:$PATH
source talk-utils.sh
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

init_tests "$@"
run_tests
summarize_tests
exit
