#!/bin/bash
# text-utils.sh -- utilities for processing text
#
# Copyright 2006-2022 Alan K. Stebbens <aks@stebbens.org>

TEXT_UTILS_VERSION="text-utils.sh v1.8"
[[ "$TEXT_UTILS_SH" = "$TEXT_UTILS_VERSION" ]] && return
TEXT_UTILS_SH="$TEXT_UTILS_VERSION"

source help-util.sh
source arg-utils.sh

text_help() {
  help_pager <<'EOF'
str_len STRING                # string length (multi-byte unaware)

mstr_len STRING               # string length (multi-byte aware)

byte_len STRING               # byte length (multi-byte unaware)

char_len STRING               # character length (possibly multi-byte)

lowercase STRING              # return the lowercase string

uppercase STRING              # return the uppercase string

trim STRING                   # trim blanks surrounding string

ltrim STRING                  # trim left-side blanks from STRING

rtrim STRING                  # trim right-side blanks from STRING

squeeze STRING                # squeeze multiple blanks in string

split_str STRING [SEP]        # split STRING using SEP [default: ' \t']

split_input [SEP]             # split STDIN using SEP [default: ' \t']

args2lines [ARG ..]           # echo each ARG (or STDIN) on a separate line

sort_str2lines "STRING .."    # output the sorted words in STRING on separate lines

join_lines                    # join lines on STDIN together with newlines

sort_str [WORDS TO BE SORTED] # return the sorted list of WORDS
str_sort [WORDS TO BE SORTED] # an alias for 'sort_str'

html_encode [STRING]          # encode STRING (or STDIN) for html

url_encode  [STRING]          # encode STRING (or STDIN) for url

html_decode [STRING]          # decode STRING (or STDIN) from HTML encoding

url_decode  [STRING]          # decode STRING (or STDIN) from URL encoding

Most functions, except 'split_str' and 'sort_str', can be used in a pipe
without an argument.  For example:

    echo "This is a string" | uppercase   => "THIS IS A STRING"
    echo ""
    html_encode <input-file >html-file
EOF
}

help_text() { text_help ; }

str_len()   { __args_or_stdin "$@" | wc -c ; }
mstr_len()  { __args_or_stdin "$@" | wc -m ; }

byte_len()  { __args_or_stdin "$@" | wc -c ; }
char_len()  { __args_or_stdin "$@" | wc -m ; }


lowercase() { __args_or_stdin "$@" | tr 'A-Z' 'a-z' ; }
uppercase() { __args_or_stdin "$@" | tr 'a-z' 'A-Z' ; }

ltrim()     { __args_or_stdin "$@" | sed -Ee 's/^[[:space:]]*//' ; }
rtrim()     { __args_or_stdin "$@" | sed -Ee 's/[[:space:]]*$//' ; }
trim()      { __args_or_stdin "$@" | sed -Ee 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' ; }
squeeze()   { __args_or_stdin "$@" | tr '\t' ' ' | tr -s ' ' ; }

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
  echo "$1" | split_input "$2"
}

# split_input [SEP]
#
#   Splits STDIN by SEP, or by whitespace if SEP not given
#
split_input() {
  local sep="${1:-$' \t'}"
  tr "$sep" '\n'                    |
  sed -Ee 's/^(.*[ ,?*].*)$/"\1"/'  |
  tr '\n' ' '
}

# args2lines ARG ..
#
# echo each ARG on a separate line

args2lines()   { __args2lines "$@" ; }

__args2lines() { args_or_stdin "$@" | tr ' \t' '\n' ; }

# sort_str2lines "STRING ..."  -- output the words of STRING on separate lines, sorted

sort_str2lines()  {
  help_args_func text_help $# 1 || return
  __sort_str2lines "$@"
}

__sort_str2lines() { __args2lines "$@" | sort -f ; }

# join_lines -- join lines on STDIN together with newlines

join_lines() { __join_lines "$@" ; }

__join_lines() { tr '\n' ' ' | sed -e 's/ $//' ; }

# sort_str [WORDS TO BE SORTED]
# str_sort

sort_str() {
  help_args_func text_help $# 1 || return
  __sort_str "$@"
}

str_sort()   {   sort_str "$@" ; }

__str_sort() { __sort_str "$@" ; }

__sort_str() { __sort_str2lines "$@" | __join_lines ; }

# url_encode [STRING] -- encode STRING for URLs
# url_encode          -- encode STDIN for URLs

# Note: must convert % first, otherwise the other entities will
# get double-converted.

url_encode() {
  __args_or_stdin "$@" |
  sed -Ee "\
           s/\%/%25/g  ;
           s/ /%20/g   ; s/\\\$/%24/g ;	s/\>/%3E/g  ;\
           s/#/%23/g   ;              ; s/\[/%5B/g  ;\
           s/'/%27/g   ; s/\&/%26/g   ; s/\]/%5D/g  ;\
           s/,/%2C/g   ; s/\(/%28/g   ; s/\^/%5E/g  ;\
           s/-/%2D/g   ; s/\)/%29/g   ; s/\`/%60/g  ;\
           s/=/%3D/g   ; s/\*/%2A/g   ; s/\{/%7B/g  ;\
           s/[\]/%5C/g ; s/\+/%2B/g   ; s/\|/%7C/g  ;\
           s/\!/%21/g  ; s/\//%2F/g   ; s/\}/%7D/g  ;\
           s/\"/%22/g  ; s/\</%3C/g   ; s/\~/%7E/g"
}

# url_decode STRING -- decode STRING for urls
# url_decode        -- decode STDIN for urls

url_decode() {
  __args_or_stdin "$@" |
  sed -e "\
           s/%20/ /g  ;  s/%29/)/g   ;   s/%5B/[/g    ;\
           s/%21/\!/g ;  s/%2A/*/g   ;   s/%5C/\\\\/g ;\
           s/%22/\"/g ;  s/%2B/+/g   ;   s/%5D/]/g    ;\
           s/%23/\#/g ;  s/%2C/,/g   ;   s/%5E/^/g    ;\
           s/%24/\$/g ;  s/%2D/-/g   ;   s/%60/\`/g   ;\
           s/%25/%/g  ;  s/%2F/\//g  ;   s/%7B/{/g    ;\
           s/%26/\&/g ;  s/%3C/</g   ;   s/%7C/|/g    ;\
           s/%27/'/g  ;  s/%3D/=/g   ;   s/%7D/}/g    ;\
           s/%28/(/g  ;  s/%3E/>/g   ;   s/%7E/~/g"
}

# html_encode STRING -- encode STRING for HTML presentation
# html_encode        -- encode STDIN  for HTML presentation
#
# converts '&' => &amp;  '>' => &gt;  '<' => &lt;

html_encode() {
  __args_or_stdin "$@" |
  sed -e "s/\&/\&amp;/g ; s/[<]/\&lt;/g  ; s/[>]/\&gt;/g"
}

# html_decode STRING  -- decode STRING from HTML presentation
# html_decode         -- decode STDIN from HTML presentation

html_decode() {
  __args_or_stdin "$@" |
  sed -Ee "s/\&lt;/</g ; s/\&gt;/>/g ; s/\&amp;/\&/g"
}


# end text-utils.sh
# vim: set sw=2 ai
