#/bin/sh
# Copyright 2006-2013, Alan K. Stebbens <aks@stebbens.org>
# 
# Test module for list-utils.sh
#

export PATH=.:$HOME/lib:$PATH

source list-utils.sh
source test-utils.sh

test_10_print_list() {
  start_test
  words=( 
  apple banana cherry dog elephant fox giraffe hawk indigo manzana milk november
  october december january february march april may june july august
  )
  print_list words
  echo ''
  print_list words i=1
  echo ''
  print_list words i=2 c=5
  echo ''
  print_list words i=3 c=4
  echo ''
  print_list words c=3
  echo ''
  print_list words c=2
  echo ''
  end_test
}


init_tests "$@"
run_tests
summarize_tests

exit
