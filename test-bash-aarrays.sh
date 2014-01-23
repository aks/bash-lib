#!/usr/local/bin/bash

declare -A aa
aa[foo]=bar
aa[fee]=baz
aa[fie]=tar
for key in "${!aa[@]}" ; do
  printf "key: '%s'    val: '%s'\n" $key "${aa[$key]}"
done
echo "${aa[@]}"
exit

