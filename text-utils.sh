#!/bin/bash
# text-utils.sh -- utilities for processing text
#
# Copyright 2006-2014 Alan K. Stebbens <aks@stebbens.org>

TEXT_UTILS_VERSION="text-utils.sh v1.3"
[[ "$TEXT_UTILS_SH" = "$TEXT_UTILS_VERSION" ]] && return
export TEXT_UTILS_SH="$TEXT_UTILS_VERSION"

export TEXT_UTILS_SH="${BASH_SOURCE[0]}"

help_test_utils() {
  cat 1>&2 <<EOF
lowercase STRING              # return the lowercase string
uppercase STRING              # return the uppercase string
trim STRING                   # trim blanks surrounding string
ltrim STRING                  # trim left-side blanks from STRING
rtrim STRING                  # trim right-side blanks from STRING
squeeze STRING                # squeeze multiple blanks in string
split_str STRING SEP          # split STRING using SEP [default: \t]
html_encode [STRING]          # encode STRING (or STDIN) for html
url_encode  [STRING]          # encode STRING (or STDIN) for url
html_decode [STRING]          # decode STRING (or STDIN) from HTML encoding
url_decode  [STRING]          # decode STRING (or STDIN) from URL encoding
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

# url_encode [STRING] -- encode STRING for URLs
# url_encode          -- encode STDIN for URLs

# Note: must convert % first, otherwise the other entities will
# get double-converted.

url_encode() {
  ( [[ $# -gt 0 ]] && echo -n "$*" || cat ) |
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
  ( [[ $# -gt 0 ]] && echo -n "$*" || cat ) |
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
  ( [[ $# -gt 0 ]] && echo -n "$*" || cat ) |
  sed -e "s/\&/\&amp;/g ; s/[<]/\&lt;/g  ; s/[>]/\&gt;/g"
}

# html_decode STRING  -- decode STRING from HTML presentation
# html_decode         -- decode STDIN from HTML presentation

html_decode() {
  ( [[ $# -gt 0 ]] && echo -n "$*" || cat ) |
  sed -Ee "s/\&lt;/</g ; s/\&gt;/>/g ; s/\&amp;/\&/g"
}


# end text-utils.sh
# vim: set sw=2 ai
