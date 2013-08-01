#!/bin/bash
# text-utils.sh -- utilities for processing text
#
# Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>

[[ -z "$TEXT_UTILS_SH" ]] || return

help_test_utils() {
  cat 1>&2 <<EOF
lowercase STRING              # return the lowercase string
uppercase STRING              # return the uppercase string
trim STRING                   # trim blanks surrounding string
ltrim STRING                  # trim left-side blanks from STRING
rtrim STRING                  # trim right-side blanks from STRING
squeeze STRING                # squeeze multiple blanks in string
split_str STRING SEP          # split STRING using SEP [default: \t]
EOF
  exit
}

lowercase() { echo "$*" | tr 'A-Z' 'a-z' ; }
uppercase() { echo "$*" | tr 'a-z' 'A-Z' ; }

ltrim()   { echo "$*" | sed -Ee 's/^[[:space:]]*//' ; }
rtrim()   { echo "$*" | sed -Ee 's/[[:space:]]*$//' ; }
trim()    { echo "$*" | sed -Ee 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' ; }
squeeze() { echo "$*" | tr '\t' ' ' | tr -s ' ' ; }

# split_str   "STRING" [SEP]
#
#   outputs the split of STRING into parts using a separator SEP (defaulting
#   to space/tab).
#
# If SEP is anything but " ", care is taken to avoid removing whitespace from
# the split values.
#
# SEP can be multiple characters; for example ' ,' will split using both space
# and comma.  By default, splitting is done by tabs.

split_str() {
  local sep="${2:-$'\t'}"
  echo "$1"                         |
  tr "$sep" '\n'                    |
  sed -Ee 's/^(.*[ ,?*].*)$/"\1"/'  |
  tr '\n' ' '
}

# define TEXT_UTILS_SH as the source file that brought us in
export TEXT_UTILS_SH="${BASH_SOURCE[0]}"

# end text-utils.sh
# vim: set sw=2 ai
