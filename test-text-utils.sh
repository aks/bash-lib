#!/usr/bin/env bash
# test-text-utils.sh
#
# Copyright 2006-2014, Alan K. Stebbens <aks@stebbens.org>
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
# html_encode STRING            # encode STRING for HTML

# run_filter_tests FUNC INPUT OUTPUT ERROR

do_test() {
  local func="$1"
  local input="$2"
  local output="$3"
  local error="$4"

  if [[ -n "$error" ]]; then
    error="Failed $func test with $error"
  else
    error="Failed $func test with '$input'"
  fi
  # test function arg call first
  local out="`$func \"$input\"`"
  check_equal "$out" "$output" "$error"

  # test pipe with no arg call next
  out="`echo \"$input\" | $func`"
  check_equal "$out" "$output" "$error in pipe"
}

test_01_lowercase() {
  start_test
  do_test lowercase 'ABC' 'abc'
  do_test lowercase 'AbC' 'abc'
  do_test lowercase 'Now Is The Time For All Good Men' \
                     'now is the time for all good men' \
                     "long sentence"
  end_test
}

test_02_uppercase() {
  start_test
  do_test uppercase 'abc' 'ABC'
  do_test uppercase 'AbC' 'ABC'
  do_test uppercase 'Now Is The Time For All Good Men' \
                     'NOW IS THE TIME FOR ALL GOOD MEN' \
                     "long sentence"
  end_test
}

# trim STRING                   # trim blanks surrounding string
test_03_trim() {
  start_test
  do_test trim "no blanks to trim"       "no blanks to trim"
  do_test trim " one blank in front"     "one blank in front"
  do_test trim "one blank at end "       "one blank at end"
  do_test trim " blanks at either end "  "blanks at either end"
  end_test
}

# ltrim STRING                  # trim left-side blanks from STRING
test_04_ltrim() {
  start_test
  do_test ltrim "no blanks to trim"        "no blanks to trim"
  do_test ltrim " one blank in front"      "one blank in front"
  do_test ltrim "  two blanks in front"    "two blanks in front"
  do_test ltrim "one blank at end "        "one blank at end "
  do_test ltrim " blanks at either end "   "blanks at either end "
  do_test ltrim "    blanks   all  over  " "blanks   all  over  "
  end_test
}

# rtrim STRING                  # trim right-side blanks from STRING
test_05_rtrim() {
  start_test
  do_test rtrim   "no blanks to trim"           "no blanks to trim"
  do_test rtrim   " one blank in front test"    " one blank in front test"
  do_test rtrim   " one blank at end test "     " one blank at end test"
  do_test rtrim   " blanks at either end test " " blanks at either end test"
  do_test rtrim   "        blanks  all  over  " "        blanks  all  over"
  end_test
}

# squeeze STRING                # squeeze multiple blanks in string
test_06_squeeze() {
  start_test
  do_test squeeze "no extra blanks to squeeze"      "no extra blanks to squeeze"
  do_test squeeze "  blank  in  front  test"        " blank in front test"
  do_test squeeze "blank  at  end  test  "          "blank at end test "
  do_test squeeze " blanks  at  either  end  test " " blanks at either end test "
  do_test squeeze "     blanks     all     over   " " blanks all over "
  end_test
}

# split_str STRING SEP          # split STRING using SEP
test_07_split_str() {
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

test_08_url_encode() {
  start_test
  do_test url_encode "this_is_a_plain_string"     "this_is_a_plain_string"
  do_test url_encode "this is a plain string"     "this%20is%20a%20plain%20string"
  do_test url_encode "this(is)[a]{plain}'string'" "this%28is%29%5Ba%5D%7Bplain%7D%27string%27"
  end_test
}

test_09_url_decode() {
  start_test

  for string in "this_is_a_plain_string" \
                "this is a plain string" \
                "this(is)[a]{plain}'string'" \
                "~!@#$%^&*()_+1234567890-=qwertyuiop[]\\{}|asdfghjkl;':zxcvbbbnnm,./<>?" ; do
    enc_out=`url_encode "$string"`
    dec_out=`url_decode "$enc_out"`
    check_equal "$string" "$dec_out"                    "Failed encode/decode test on '$string'"
  done

  end_test
}

test_10_html_encode() {
  start_test
  do_test html_encode "this_is_a_plain_string" "this_is_a_plain_string"
  do_test html_encode "<this is a token>"      "&lt;this is a token&gt;"
  do_test html_encode "<token1><token2>"       "&lt;token1&gt;&lt;token2&gt;"
  end_test
}

test_11_html_decode() {
  start_test

  for string in "this_is_a_plain_string" \
                "this is a plain string" \
                "this(is)[a]{plain}'string'" \
                "~!@#$%^&*()_+1234567890-=qwertyuiop[]\\{}|asdfghjkl;':zxcvbbbnnm,./<>?" ; do
    enc_out=`html_encode "$string"`
    dec_out=`html_decode "$enc_out"`
    check_equal "$string" "$dec_out"                    "Failed html encode/decode test on '$string'"
  done

  end_test
}
init_tests  "$@"
run_tests
summarize_tests
exit
