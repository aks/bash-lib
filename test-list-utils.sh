#/bin/sh
# Copyright 2006-2011, Alan K. Stebbens <aks@stebbens.org>
# 
# Test module for list-utils.sh
#
source list-utils.sh
source test-utils.sh

test_01_list_add() {
  start_test
  list1=()

  list_add list1 foo

  check_item list1 0  foo "list_add failed"
  check_size list1 1      "list_add size test failed"

  list_add list1 bar

  check_size list1 2
  check_item list1 0 foo
  check_item list1 1 bar

  end_test
}

test_02_list_sort() {
  start_test
  list2=()

  words="now is the time for all good men to come to the aid of their country"
  list_add list2  $words
  check_size list2 16

  sorted_words=`sort_str "$words"`
  check_unequal "$sorted_words" "$words"

  sort_list list2
  check_size list2 16
  sorted_words2=`join_list list2 nowrap ' '`
  check_equal "$sorted_words" "$sorted_words2"
  check_unequal "$words" "$sorted_words2"
  end_test
}

test_03_list_add_once() {
  start_test
  tlist=()

  words="now is the time for all good men to come to the aid of their country"
  list_add_once tlist $words
  check_size tlist 14

  list_add_once tlist $words
  check_size tlist 14
  end_test
}

test_04_list_insert() {
  start_test
  tlist=()
  list_add tlist banana cherry
  check_size tlist 2
  list_insert tlist apple
  check_size tlist 3
  check_item tlist 0 apple
  check_item tlist 1 banana
  check_item tlist 2 cherry
  end_test
}

test_05_list_insert_once() {
  start_test
  tlist=()
  list_add_once tlist banana cherry
  check_size tlist 2
  list_insert_once tlist apple
  check_size tlist 3
  check_item tlist 0 apple
  check_item tlist 1 banana
  check_item tlist 2 cherry
  list_insert_once tlist apple banana cherry
  check_size tlist 3
  list_insert_once tlist aardvark
  check_size tlist 4
  check_item tlist 0 aardvark
  check_item tlist 1 apple
  end_test
}

test_06_in_list() {
  start_test
  tlist=()
  list_add_once tlist foo bar baf
  check_size tlist 3
  check_true  "in_list tlist foo"
  check_false "in_list tlist foo2"
  check_true  "in_list tlist -all foo bar"
  check_false "in_list tlist -any foo1 bar2"
  check_true  "in_list tlist -all foo bar baf"
  check_true  "in_list tlist -any foo1 baf bar2"
  check_false "in_list tlist -all foo bar baf gonzo"
  check_true  "in_list tlist      foo1 baf bar2"
  check_false "in_list tlist      foo1 baf2 bar2"
  end_test
}

init_tests "$@"
run_tests
summarize_tests

exit



tstlst=()

limit=100
start=0
while [[ $start -lt $limit ]]; do
  aword="word${start}"
  list_add tstlist "$aword"
  start=$(( start + 1 ))
done
echo $'Test join_list tstlist'
join_list tstlist

echo $'\nTest join_list tstlist KEYS'
join_list tstlist KEYS

echo $'\nTest join_list tstlist AND'
join_list tstlist AND

namelist=( script1 script2 script3 script4 script5 )
echo $'\nTest join_list namelist "|\\n"'
join_list namelist $'|\n'

echo $'\nTesting in_list tstlist ..'
x=0
while [[ $x -lt ${#tstlist[@]} ]] ; do
  if (( (x % 10) == 0 )) ; then
    printf "%d.." $x
  fi
  word="${tstlist[$x]}"
  x=$(( x + 1 ))
  if ! in_list tstlist "$word" ; then
    echo "huh: $word not found in tstlist!!"
    exit 2
  fi
done
echo ''

echo $'\nTest long list of column titles with default wrapping'
col_names=( Manager Fund Status Opened Closed Action Stock Shares Price Net Comm Fees Comment )
export COLUMNS=9999
join_list col_names 

echo $'\nTest long list of column titles without wrapping'
join_list col_names NOWRAP

exit
