#!/usr/bin/env bash
# test-list-utils.sh
#
# Copyright 2006-2014, Alan K. Stebbens <aks@stebbens.org>
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

  sorted_words=`sort_str $words`
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
  tlist2=()
  list_add_once tlist foo bar baf
  list_add_once tlist2 "foo bar" "bif baf" "+" "-" "/" "*"
  check_size tlist 3
  check_true  "in_list tlist foo"
  check_false "in_list tlist foo2"
  check_true  "in_list tlist -all foo bar"
  check_false "in_list tlist -any foo1 bar2"
  check_true  "in_list tlist -all foo bar baf"
  check_true  "in_list tlist -any foo1 baf bar2"
  check_false "in_list tlist -all foo bar baf gonzo"
  check_true  "in_list tlist      foo1 baf bar2"
  check_true  "in_list tlist -any foo1 baf bar2"
  check_false "in_list tlist -all foo1 baf bar2"
  check_false "in_list tlist      foo1 baf2 bar2"
  # check weird matches
  check_true  "in_list tlist2 '+'"
  check_false "in_list tlist2 '?'"
  check_false "in_list tlist2 'bif'"
  check_true  "in_list tlist2 'bif baf'"
  end_test
}

test_07_lookup_list() {
  start_test
  tlist1=( now is the 'time' 'for' all good men to come to the aid of their country )

  item=`lookup_list tlist1 now`
  code=$?
  check_eq $code 0
  check_equal "$item" 'now'

  item=`lookup_list tlist1 'time'`
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
  tlist1=( now is the 'time' 'for' all good men to come to the aid of their country )

  items=( `grep_list tlist1 now` )      # 1
  code=$?
  check_eq $code 0
  check_size items 1
  check_item_equal items 0 'now'

  items=( `grep_list tlist1 'time'` )     # 2
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

test_10_print_list() {
  start_test
  words=(
    apple banana cherry dog elephant fox giraffe hawk indigo manzana milk november
    october december january february march april may june july august
  )
  check_output plout1 "print_list words"
  check_output plout2 "print_list words i=1"
  check_output plout3 "print_list words i=2 c=4"
  check_output plout4 "print_list words i=3 c=3"
  check_output plout5 "print_list words i=1 c=2"
  end_test
}
test_10a_print_list() {
  start_test
    workfiles=( '.environment*' '.cshrc*' '.aliases*' '.prompts*' '.bashrc*' '.inputrc'
                '.profile' '.vim*' '.git-bash-prompt' 'src/github/aks/maximum-awesome' )
    check_output plouta1 "print_list workfiles"
    check_output plouta2 "print_list workfiles i=1"
    check_output plouta3 "print_list workfiles i=1 c=2"
    check_output plouta4 "print_list workfiles i=1 c=3"
    check_output plouta5 "print_list workfiles i=2 c=1"
    check_output plouta6 "print_list workfiles i=4 c=1"
  end_test
}
test_11_join_list() {
  start_test
  list_init tlist
  list_add tlist apple banana cherry
  check_size tlist 3
  check_output join_list_3    "join_list tlist"
  check_output join_list_3sp  "join_list tlist ' '"
  check_output join_list_3tab "join_list tlist TAB"
  check_output join_list_3and "join_list tlist AND"
  check_output join_list_3or  "join_list tlist OR"
  check_output join_list_3nl  "join_list tlist NL"
  words=(
    apple banana cherry dog elephant fox giraffe hawk indigo manzana milk november
    october december january february march april may june july august
  )
  check_output join_list_words        "join_list words"
  check_output join_list_words_nowrap "join_list words NOWRAP"
  end_test
}

test_12_map_list() {
  start_test
  words=( now is the 'time' 'for' all good men to come to the aid of their country )
  num_words=`list_size words`

  items=( `map_list words 'echo ${#item}'` )
  check_size items $num_words
  check_item_equal items 0 3
  check_item_equal items 1 2
  check_item_equal items 2 3
  check_item_equal items 3 4
  check_item_equal items 4 3
  check_item_equal items 5 3
  check_item_equal items 14 5
  check_item_equal items 15 7
  end_test
}

test_12a_map_list_func_expr() {
  start_test
  #        0  1  2    3      4    5   6    7   8   9   10 11  12  13  14   15
  words=( now is the 'time' 'for' all good men to come to the aid of their country )
  num_words=`list_size words`

  function count_vowels() {
    local word="$1"
    echo "$word" | sed -Ee $'s/(.)/\\1\\\n/g' | egrep -c '[aeiouy]'
  }

  items=( `map_list words count_vowels` )
  check_size items $num_words
  check_item_equal items 0 1
  check_item_equal items 1 1
  check_item_equal items 2 1
  check_item_equal items 3 2
  check_item_equal items 4 1
  check_item_equal items 5 1
  check_item_equal items 6 2
  check_item_equal items 7 1
  check_item_equal items 8 1
  check_item_equal items 9 2
  check_item_equal items 10 1
  check_item_equal items 12 2
  check_item_equal items 14 2
  check_item_equal items 15 3
  end_test

}

test_12b_map_list_joinstr() {
  start_test
  words=( now is the "time" "for" all good men to come to the aid of their country )
  check_output map_list_joinstr "map_list words \"echo \\\"unset \\\$item\"\\\" \"\$CHAR_NL\" "
  end_test
}

test_14_reductions() {
  start_test
  words=( now is the 'time' 'for' all good men to come to the aid of their country )
  num_words=`list_size words`
  items=( `map_list words 'echo ${#item}'` )
  count_chars_orig=`echo "${words[@]}" | wc -c`
  count_chars=$(( `sum_list items` + num_words ))
  check_eq $count_chars $count_chars_orig 'sum_list count error; got $count_chars'
  min=`min_list items`
  check_eq $min 2  "min test failed; got $min, should be 2"
  max=`max_list items`
  check_eq $max 7  "max test failed; got $max, should be 7"
  avg=`avg_list items`
  check_eq $avg 3 "avg test failed; got $avg, should be 3"
  end_test
}

test_19_list_help_func() {
  start_test
  check_output list_help          "list_help"
  check_output list_init_help     "list_init"
  check_output list_init_nohelp   "list_init foo"
  check_output list_add_help      "list_add"
  check_output list_add_nohelp    "list_add foo bar"
  check_output list_add_once_help "list_add_once"
  check_output list_get_help      "list_get"
  check_output list_get2_help     "list_get nolist"
  check_output list_item_help     "list_item"
  check_output list_item2_help    "list_item nolist"
  check_output list_push_help     "list_push"
  check_output list_push2_help    "list_push nolist"
  end_test
}

# test to make sure IFS is not changed by list_items and list_join, which alters it
test_20_list_IFS_check() {
  start_test
  saveIFS="$IFS"
  list_init foo
  list_add foo now is the 'time' 'for' all good men to come to the aid
  tmpfile="/tmp/foo.$$"
  list_items foo >$tmpfile
  check_equal "$IFS" "$saveIFS"
  join_list foo >$tmpfile
  check_equal "$IFS" "$saveIFS"
  end_test
}


init_tests "$@"
run_tests
summarize_tests

exit
