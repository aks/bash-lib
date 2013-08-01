#!/bin/bash
# Copyright 2006-2013, Alan K. Stebbens <aks@stebbens.org>
#
# test-template.sh -- a template on which to create new test cases

PATH=.:$PATH:$HOME/lib

source test-utils.sh
source text-utils.sh

# lowercase STRING              # return the lowercase string
# uppercase STRING              # return the uppercase string
# trim STRING                   # trim blanks surrounding string
# ltrim STRING                  # trim left-side blanks from STRING
# rtrim STRING                  # trim right-side blanks from STRING
# squeeze STRING                # squeeze multiple blanks in string
# split_str STRING SEP          # split STRING using SEP

test_1_lowercase() {
  start_test

  out=`lowercase ABC`
  check_equal "$out" 'abc'      "Failed ABC test"

  out=`lowercase AbC`
  check_equal "$out" 'abc'      "Failed AbC test"

  out=`lowercase Now Is The Time For All Good Men`
  check_equal "$out" 'now is the time for all good men' "Failed long sentence test"

  end_test
}

test_2_uppercase() {
   start_test

  out=`uppercase ABC`
  check_equal "$out" 'ABC'      "Failed ABC test"

  out=`uppercase AbC`
  check_equal "$out" 'ABC'      "Failed AbC test"

  out=`uppercase Now Is The Time For All Good Men`
  check_equal "$out" 'NOW IS THE TIME FOR ALL GOOD MEN' "Failed long sentence test"

  end_test
}

# trim STRING                   # trim blanks surrounding string
test_3_trim() {
  start_test

  out=`trim "no blanks to trim"`
  check_equal "$out" "no blanks to trim"

  out=`trim " one blank in front test"`
  check_equal "$out" "one blank in front test"

  out=`trim "one blank at end test "`
  check_equal "$out" "one blank at end test"

  out=`trim " blanks at either end test "`
  check_equal "$out" "blanks at either end test"

  end_test
}

# ltrim STRING                  # trim left-side blanks from STRING
test_4_ltrim() {
  start_test
  out=`ltrim "no blanks to trim"`
  check_equal "$out" "no blanks to trim"

  out=`ltrim " one blank in front test"`
  check_equal "$out" "one blank in front test"

  out=`ltrim "one blank at end test "`
  check_equal "$out" "one blank at end test "

  out=`ltrim " blanks at either end test "`
  check_equal "$out" "blanks at either end test "

  out=`ltrim "        blanks  all  over       "`
  check_equal "$out" "blanks  all  over       "

  end_test
}

# rtrim STRING                  # trim right-side blanks from STRING
test_5_rtrim() {
  start_test

  out=`rtrim "no blanks to trim"`
  check_equal "$out" "no blanks to trim"            "Failed rtrim no blanks to trim"

  out=`rtrim " one blank in front test"`
  check_equal "$out" " one blank in front test"     "Failed rtrim one blank in front test"

  out=`rtrim " one blank at end test "`
  check_equal "$out" " one blank at end test"       "Failed rtrim one blank at end test"

  out=`rtrim " blanks at either end test "`
  check_equal "$out" " blanks at either end test"   "Failed rtrim blanks at either end test"

  out=`rtrim "        blanks  all  over       "`
  check_equal "$out" "        blanks  all  over"    "Failed rtrim blanks all over test"

  end_test
}

# squeeze STRING                # squeeze multiple blanks in string
test_6_squeeze() {
  start_test
  out=`squeeze "no extra blanks to squeeze"`
  check_equal "$out" "no extra blanks to squeeze"  "Failed squeeze no extra blanks test"

  out=`squeeze "  blank  in  front  test"`
  check_equal "$out" " blank in front test"   "Failed squeeze blank in front test"

  out=`squeeze "blank  at  end  test  "` 
  check_equal "$out" "blank at end test "     "Failed squeeze blank at end test"

  out=`squeeze " blanks  at  either  end  test "`
  check_equal "$out" " blanks at either end test "    "Failed squeeze blanks at both ends test"

  out=`squeeze "        blanks     all     over       "`
  check_equal "$out" " blanks all over "              "Failed squeeze blanks all over test"

  end_test
}

# split_str STRING SEP          # split STRING using SEP
test_7_split_str() {
  start_test

  out=( `split_str "A list of words to split" ' '` )

  check_split_data() {
    check_size out 6
    check_item out 0 'A'
    check_item out 1 'list'
    check_item out 2 'of'
    check_item out 3 'words'
    check_item out 4 'to'
    check_item out 5 'split'
    out=()
  }
  check_split_data

  out=( `split_str "A,list,of,words,to,split" ','` )
  check_split_data

  out=( `split_str "A:list:of:words:to:split" ':'` )
  check_split_data

  out=( `split_str "A, list, of, words, to, split" ", "` )
  check_split_data

  end_test
}

init_tests  "$@"
run_tests
summarize_tests
exit
