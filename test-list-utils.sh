#/bin/sh
# Copyright 2006-2013, Alan K. Stebbens <aks@stebbens.org>
# 
# Test module for list-utils.sh
#

export PATH=.:$HOME/lib:$PATH

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

test_07_lookup_list() {
  start_test
  tlist1=( now is the time for all good men to come to the aid of their country )

  item=`lookup_list tlist1 now`
  code=$?
  check_eq $code 0
  check_equal "$item" 'now'

  item=`lookup_list tlist1 time`
  code=$?
  check_eq $code 0
  check_equal $item 'time'

  item=`lookup_list tlist1 notfound`
  code=$?
  check_eq $code 1
  check_equal "$item" ''

  item=`lookup_list tlist1 the`
  code=$?
  check_eq $code 2 
  check_equal "$item" ''

  item=`lookup_list tlist1 to`
  code=$?
  check_eq $code 2
  check_equal "$item" ''

  item=`lookup_list tlist1 goo`
  code=$?
  check_eq $code 0
  check_equal "$item" 'good'

  item=`lookup_list tlist1 go`
  code=$?
  check_eq $code 0
  check_equal "$item" 'good'

  item=`lookup_list tlist1 t`
  code=$?
  check_eq $code 2
  check_equal "$item" ''

  end_test
}

test_08_grep_list() {
  start_test
  tlist1=( now is the time for all good men to come to the aid of their country )

  items=( `grep_list tlist1 now` )      # 1
  code=$?
  check_eq $code 0
  check_size items 1
  check_item_equal items 0 'now'

  items=( `grep_list tlist1 time` )     # 2
  code=$?
  check_eq $code 0
  check_size items 1
  check_item_equal items 0 'time'

  items=( `grep_list tlist1 notfound` ) # 3
  code=$?
  check_eq $code 1
  check_size items 0

  items=( `grep_list tlist1 the` )      # 4
  code=$?
  check_eq $code 0 
  check_size items 3
  check_item_equal items 0 'the'
  check_item_equal items 1 'the'
  check_item_equal items 2 'their'

  items=( `grep_list tlist1 to` )       # 5
  code=$?
  check_eq $code 0
  check_size items 2
  check_item_equal items 0 'to'
  check_item_equal items 1 'to'

  items=( `grep_list tlist1 goo` )      # 6
  code=$?
  check_eq $code 0
  check_size items 1
  check_item_equal items 0 'good'

  items=( `grep_list tlist1 go` )       # 7
  code=$?
  check_eq $code 0
  check_size items 1
  check_item_equal items 0 'good'

  items=( `grep_list tlist1 t` )        # 8
  code=$?
  check_eq $code 0
  check_size items 7
  check_item_equal items 0 'the'
  check_item_equal items 1 'time'
  check_item_equal items 2 'to'
  check_item_equal items 3 'to'
  check_item_equal items 4 'the'
  check_item_equal items 5 'their'
  check_item_equal items 6 'country'

  items=( `grep_list tlist1 e` )
  code=$?
  check_eq $code 0
  check_size items 6
  check_item_equal items 0 'the'
  check_item_equal items 1 'time'
  check_item_equal items 2 'men'
  check_item_equal items 3 'come'
  check_item_equal items 4 'the'
  check_item_equal items 5 'their'

  end_test
}

test_09_push_pop_list() {
  start_test
  stack=()
  check_size stack 0
  push_list stack fubar
  check_size stack 1
  push_list stack tarfu
  check_size stack 2
  push_list stack snafu
  check_size stack 3

  pop_list stack
  check_size stack 2
  check_equal "$item" 'snafu'

  pop_list stack
  check_size stack 1
  check_equal "$item" 'tarfu'

  pop_list stack
  check_size stack 0
  check_equal "$item" 'fubar'

  pop_list stack
  code=$?
  check_eq $code 1
  check_equal "$item" ''

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
