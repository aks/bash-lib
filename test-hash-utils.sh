#!/usr/bin/env bash
# Copyright 2006-2022, Alan K. Stebbens <aks@stebbens.org>
#
# Test module for hash-utils.sh
#

export PATH=.:$HOME/lib:$PATH

source hash-utils.sh
source test-utils.sh

test_01_hash_init() {
  start_test
  hash_init hash1a
  check_output hash1a_init_test 'hash_info hash1a' "Hash1a init info not correct"
  hash_init hash1b NONE
  check_output hash1b_init_test 'hash_info hash1b' "Hash1b init info not correct"
  def=`hash_default hash1b`
  check_equal "$def" 'NONE'  "Hash1b default result ($def) is incorrect; should be 'NONE'"
  # make sure hash1a and hash1b are distinct
  hash_set hash1a key1a val1a
  hash_set hash1b key1b val1b
  check_size hash1a 1
  check_size hash1b 1
  end_test
}

wordlist1="apple one banana two cherry three mango four pear five"

test_02_hash_set() {
  start_test
  hash_init hash2
  hash_set hash2 $wordlist1

  check_output hash2_info "hash_info hash2"

  check_size hash2 5 "hash size is incorrect; `hash_size hash2` vs. 5"

  check_key hash2 apple
  check_key hash2 banana
  check_key hash2 cherry
  check_key hash2 mango
  check_key hash2 pear
  check_no_key hash2 foobar

  # add the same words again
  oldsize=`hash_size hash2`
  hash_set hash2 $wordlist1
  newsize=`hash_size hash2`
  check_size hash2 $oldsize "hash_set with same data changes size (old=$oldsize, new=$newsize)"
  end_test
}

test_03_hash_get() {
  start_test
  hash_init hash3

  words=( key1 val1 key2 val2 key3 val3 key4 val4 key5 val5 key6 val6 )
  hash_set hash3 "${words[@]}"
  num_words=`list_size words`
  num_pairs=$(( num_words / 2 ))
  hsize=`hash_size hash3`
  check_size hash3 $num_pairs "hash3 size is wrong; is $num_pairs, should be: $hsize"

  for ((i=0; i<${#words}; i += 2)); do
    val1="${words[i+1]}"
    val2=`hash_get hash3 ${words[i]}`
    check_equal "$val1" "$val2"
  done

  end_test
}

test_04_hash_delete() {
  start_test
  words=( key1 val1 key2 val2 key3 val3 key4 val4 key5 val5 key6 val6 )
  hash_init hash4
  hash_set hash4 "${words[@]}"
  check_size hash4  $(( ${#words[@]} / 2 ))

  # first check that the items are there
  for ((i=1; i<=6; i++)) ; do
    check_item hash4 "key$i" "val$i"
  done
  # now delete them and confirm their deletion
  count=`hash_size hash4`
  for ((i=1; i<=6; i++)); do
    hash_delete hash4 "key$i"
    check_no_key hash4 "key$i"
    (( count-- ))
    check_size hash4 $count
  done
  end_test
}

test_05_in_hash() {
  start_test
  hash_init hash5
  hash_set hash5 key1 val1 key2 val2 key3 val3 key4 val4
  check_size hash5 4
  check_true  "in_hash hash5 key1"
  check_false "in_hash hash5 key9"
  check_true  "in_hash hash5 key2"
  check_false "in_hash hash5 val2"
  check_true  "in_hash hash5 key4"
  check_true  "in_hash hash5 key3"
  check_false "in_hash hash5 key5"
  end_test
}

test_06_keys() {
  start_test
  hash_init hash6
  hash_set  hash6 key1 val1 key2 val2 key3 val3 key4 val4 key5 val5 key6 val6
  keys=( `hash_keys hash6` )
  for key in "${keys[@]}" ; do
    check_true "[[ \"$key\" =~ key[1-6] ]]"
  done
  end_test
}

test_07_values() {
  start_test
  hash_init hash7
  hash_set  hash7 key1 val1 key2 val2 key3 val3 key4 val4 key5 val5 key6 val6
  values=( `hash_values hash7` )
  for val in "${values[@]}" ; do
    check_true "[[ \"$val\" =~ val[1-6] ]]"
  done
  end_test
}

test_10_print_hash() {
  start_test
  words=(
    apple banana cherry dog elephant fox giraffe hawk indigo manzana milk november
    october december january february march april may june july august
  )
  hash_init hash10
  for ((i=0; i<${#words[@]}; i++)) ; do
    hash_set hash10 ${words[i]} $i
  done
  check_output phout1 "hash_print hash10"
  check_output phout2 "hash_print hash10 i=1"
  check_output phout3 "hash_print hash10 i=2 c=4"
  check_output phout4 "hash_print hash10 i=3 c=3"
  check_output phout5 "hash_print hash10 i=1 c=2"
  end_test
}

if (( BASH_VERSINFO[0] < 4 )); then
  echo "The hash-utils library needs bash version 4 or greater"
  exit
fi

init_tests "$@"
run_tests
summarize_tests

exit
