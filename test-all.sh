#!/usr/bin/env bash
# test-all.sh [options]
# 
# Run all tests with the name "test-*-*.sh"
#
# Copyright 2014, Alan K. Stebbens <aks@stebbens.org>

lines="--------------"
for test in test-*-*.sh ; do
  printf "\n%s %s %s\n" $lines $test $lines
  $test "$@"
done
exit
