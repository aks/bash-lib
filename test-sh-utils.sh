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


wordlist=( the time has come to talk of ceiling wax and cabbages and kings )

test_01_first_sh_test() {
  start_test
  end_test
}

test_10_second_sh_test() {
  start_test
  end_test
}

init_tests "$@"
run_tests
summarize_tests
exit
