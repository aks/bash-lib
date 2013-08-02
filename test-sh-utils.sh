#!/bin/sh
# Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>
#
# Test module for sh-utils.sh
#
export PATH=.:$PATH
source sh-utils.sh

echo "Testing basic functions"

echo ">> talk with one argument"

talk "one"
talk "two"
talk "three"

echo ">> talk with more args"

talk "one two three"

for norun in '' 1 ; do
  for verbose in '' 1 ; do
    echo -n 1>&2 "norun=$norun verbose=$verbose : vtalk says: "
    vtalk "this is a test"
    echo 1>&2
  done
done

echo "Testing talkf .."

wordlist=( the time has come to talk of ceiling wax and cabbages and kings )

x=0
while [[ $x -lt ${#wordlist[*]} ]]; do
  word=${wordlist[$x]}
  talkf "The %d word is '%s'\n" $x "$word"
  x=$((x + 1))
done

echo $'\nTesting vtalkf ..'

verobse= norun=
for verbose in '' 1 ; do
  vtalkf "Verbose = %s\n" $verbose
done
