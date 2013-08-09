# list-utils.sh
# Copyright 2009-2013 Alan K. Stebbens <aks@stebbens.org>
#
# sh script utilities for managing lists of things

LIST_UTILS_VERSION='list-utils.sh v1.2'
#[[ "$LIST_UTILS" = "$LIST_UTILS_VERSION" ]] && return
export LIST_UTILS="$LIST_UTILS_VERSION"

export CHAR_NL=$'\n'
export CHAR_TAB=$'\t'
export CHAR_WS=$'\t '

list_help() {
  cat 1>&2 <<EOF
These are the list utilities:

list_add VAR VAL1 [VAL2 ...]        # add VAL1.. to the end of VAR
list_add_once VAR  VAL1 [VAL2 ..]   # add VAL1.. uniquely to the end of VAR

list_insert VAR  VAL ...            # insert VALUE at the front of VAR
list_insert_once VAR VAL ..         # insert VALUE.. at the front of VAR; 

list_push VAR VAL                   # same as "list_add VAR VAL"
list_pop  VAR                       # removes top VAL on VAR and returns in variable "item"

in_list VAR  [-any|-all] VAL ...    # return true if one or more values are in a list
list_size VAR                       # returns the number of items

sort_str VAL ...                    # sort the space-separated words of VAL ..
sort_list VAR                       # sort the contents of VAR (a list) in place

split_into  VAR "STRING" SEP        # split "STRING" by SEP into VAR
split_str   "STRING" [SEP]          # split "STRING" by SEP

join_list VAR [SEP] ..              # join the items in VAR into a list, separated by SEP,
  SEP can be 
    AND    -- separate with " and "
    OR     -- separate with " or "
    KEYS   -- enclose each item with X' and ', follwed by ','
    TAB    -- use tabs to separate items
    NL     -- separate each item with newline (and some spaces)
    NOWRAP -- do not auto-wrap long lines (default is WRAP)
    ','    -- separate items with a comma (default)
    str    -- separate each item with an given string.

lookup_list LISTVAR KEY             # lookup KEY in LISTVAR
grep_list LISTVAR PAT               # grep PAT across LISTVAR

EOF
}
help_list() { list_help ; }

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
  local sep="${2:-$CHAR_TAB}"
  echo "$1"			      | 
  tr "$sep" '\n'		      | 
  sed -Ee 's/^(.*[ ,?*].*)$/"\1"/'  | 
  tr '\n' ' '
}
split_list() { split_str "$@" ; }

# split_into  VAR "STRING" SEP
#
#   splits a STRING into parts using separator (SEP) (default is ',')
#   and assigns the resulting separated, and quoted strings to the VAR.
#   
#   See split_str for details on SEP

split_into() {
  local var="$1"
  local sep="${3:-' 	'}"
  eval "$var=( `split_str \"$2\" \"$sep\"` )"
}

# list_add VAR VALUE ...
#  Supports insertion of multiple values into the list

list_add() { 
  local var="$1"
  shift
  eval "$var=( \"\${$var[@]}\" \"\$@\" )" 
}
add_list()  { list_add "$@" ; }
list_push() { list_add "$@" ; }
push_list() { list_add "$@" ; }

# list_add_once VAR VALUE ...
#
# Add each VALUE only once to VAR
list_add_once() {
  local var="$1"
  shift
  while [[ $# -gt 0 ]]; do
    if ! in_list $var "$1" ; then
      list_add $var "$1"
    fi
    shift
  done
}
add_list_once() { list_add_once "$@" ; }

# list_insert VAR VALUE ..
# Insert VALUE at the front of the list

list_insert() {
  local var="$1"
  shift
  eval "$var=( \"\$@\" \"\${$var[@]}\" )"
}
insert_list() { list_insert "$@" ; }

# list_insert_once VAR VALUE(s)...
# Insert VALUE(s)... into the list only if it is not already in the list.

list_insert_once() {
  local var="$1"
  shift
  while [[ $# -gt 0 ]]; do
    if ! in_list $var "$1" ; then
      list_insert $var "$1"
    fi
    shift
  done
}
insert_list_once() { list_insert_once "$@" ; }

# list_pop VAR
#
# Pop the last item off the list, and return it in a variable
# named "item".
#
# If there are no items to pop, return nothing, and error code 1

list_pop() {
  local var=${1:?'Missing list name'}
  local x=`list_size $var`
  if (( --x < 0 )); then
    item=
    return 1
  fi
  local val
  eval "val=\"\${$var[$x]}\""
  unset $var[$x]
  eval "$var=( \"\${$var[@]}\" )"
  #echo "$val"
  item="$val"
}
pop_list() { list_pop "$@" ; }

# in_list LISTNAME [-any] VALUE ..
# in_list LISTNAME -all V2 ...
#
# Returns 0 if any of VALUE(s) are one of LISTNAME's values
# Returns 1 otherwise.
#
# If "-all" given, require all values to be in the list in order
# to return 0.

in_list () 
{ 
  local vals val1 val2 match_all=
  eval "vals=( \"\${$1[@]}\" )"
  shift
  case "$2" in
    -all|-ALL|-and|-AND) match_all=1 ; shift ;;
    -any|-ANY|-or|-OR)   match_all=  ; shift ;;
  esac
  if (( match_all )); then
    # when matching all, the first non-match causes a failure
    # success is achieved only after all matches succeed
    for val1 in "${vals[@]}" ; do
      for val2 in "$@" ; do
        if [[ "x$val1" != "x$val2" ]]; then return 1 ; fi
      done
    done
    return 0
  else
    # match matching for any, the first match succeeds right away.
    # otherwise, failure occurs only when all matches fail.
    for val1 in "${vals[@]}" ; do
      for val2 in "$@" ; do
        if [[ "x$val1" = "x$val2" ]] ; then return 0 ; fi
      done
    done
    return 1
  fi
}


# list_size NAME -- return list size
list_size() { eval "echo \"\${#$1[@]}\"" ; }

# sort_str [WORDS TO BE SORTED]
#
# list_sort NAME -- sort the items in the list
# sort_list NAME
#
sort_str() {
  echo $( echo "$@" | tr -s ' ' '\n' | sort )
}
str_sort() { sort_str "$@" ; }

sort_list() { 
  local vals
  eval "vals=( \"\${$1[@]}\" )"
  eval "$1=( `sort_str \"${vals[@]}\"` )"
}
list_sort() { sort_list "$@" ; }


# join_list ARRAYNAME[, [AND|OR|KEYS|STR|TAB|NL|,|<sep>|NOWRAP]]
# 
# make a list of ARRAYNAME.  If KEYS given, normalize the keys
#
# Args
# ----
# and    - then separate each list item with an 'AND'
# keys   - then normalize the items as keys, and separate with ','
# str    - quote each item as a string, and separate with ','
# tab    - separate each list item with a TAB
# nl     - separate each list item with a newline,
# nowrap - do not wrap long lists (they are wrapped by default)
# ','    - separate items with a comma (default)
# <other> - use <other> as a separator (e.g., ',')
#
# If no other arg, ',' is used as a separator.
# All keywords are case-insensitive.

join_list() {
  local var="$1"
  local varsize="\${#${var}[*]}"
  local count val key
  local list
  list=()
  eval "count=$varsize"
  local sep=', '
  local val=
  local strings=
  local nowrap=0
  shift
  while [[ $# -gt 0 ]]; do
    case "$1" in
      keys|KEYS)     key=1 ;;
      and|AND)       sep=$'\n       and ' ;;
      or|OR)         sep=$'\n	     or '  ;;
      str|strings|STR|STRINGS) strings=1 ;;
      tab|TAB)       sep=$'\t' ;;
      nl|NL)         sep=$'\n' ;;
      nowrap|NOWRAP) nowrap=1 ;;
      *)             sep="$1" ;;
    esac
    shift
  done
  local index=0
  local limit=10
  while [[ $index -lt $count ]]; do
    eval "val=\"\${${var}[$index]}\""
    if [[ -n "$val" ]]; then		            # not an empty value?
      if [[ -n "$key" ]] ; then		    # key value?
        val="`normal_key \"$val\"`"	            # yes, quote as a key
      elif [[ -n "$strings" ]]; then	            # string value?
        val="`quoted_string \"$val\"`"	    # quote as a string
      fi
      if [[ ${#list[@]} -gt 0 ]] ; then	    # list not empty?
        list=( "${list[@]}" "$sep" "$val" )       # append new items
        if (( $nowrap == 0 && ( ( $index % 10 ) == 0 ) )) ; then    # every 10 items, cause a newline
          list=( "${list[@]}" "$CHAR_NL" )	    # append a new line
        fi
      else
        list=( "$val" )			    # start the list
      fi
    fi
    index=$(( index + 1 ))
  done
  (IFS= ; /bin/echo "${list[*]}")
}
list_join() { join_list "$@" ; }


# lookup_list LISTVAR KEY
#
#   Lookups up KEY in the items of LISTVAR, returns the matching LISTVAR, using
#   a case-insensitive, disambiguating search.  If there is no match, returns
#   the empty string, with return code of 1.  If there is a match, returns the
#   matching item, and a return code of 0.  If there are two or more matches,
#   returns empty string, and return code of 2.
#
# item=`lookup_list LIST KEY`
# returns item matching key, with return code 0
# return code 1: KEY not found
# return code 2: KEY ambiguous

lookup_list() {
  local listvar="$1"
  local key="$2"
  local list=()
  eval "list=( \"\${$listvar[@]}\" )"      # get a copy of the list
  local items=${#list[*]}                  # number of items
  local x=0
  local found=
  while (( x < items )) ; do
    if eval "[[ '${list[x]}' =~ ^$key ]]" ; then
      if [[ -n "$found" ]]; then
        return 2 
      else
        found="${list[x]}"
      fi
    fi
    : $(( x++ ))
  done
  if [[ -n "$found" ]]; then
    echo "$found"         # stdout is found keyword
    return 0              # found one match
  fi
  return 1                # not found
}

# lookup_error $CODE WORD ["NOT-FOUND-MSG" ["AMBIGUOUS-MSG"]]
#
# Usage: 
#  foundword=`lookup_list list $someword`
#  [[ $? != 0 ]] && lookup_error $? $someword    
#
# or:
#  foundword=`lookup_list list $someword` || lookup_error $? $someword`
#
# or:
#  foundword=`lookup_list list $someword` || \
#       lookup_error $? $someword "%s was not found" "%s is ambiguous"
#
# You can redefine "lookup_error" to use your own messages.

lookup_error() {
  if [[ $1 -eq 1 ]]; then
    errorf "${3:-'%s' was not found}" "$2"
  elif [[ $1 -eq 2 ]]; then
    errorf "${4:-'%s' is ambiguous.}" "$2"
  fi
}

# grep_list LISTVAR PAT
#
#   Like lookup_list but returns all partially matching entries on multiple
#   matches.

grep_list() {
  local listvar="$1"
  local pat="$2"
  local list=()
  eval "list=( \"\${$listvar[@]}\" )"      # get a copy of the list
  local items=${#list[*]}                  # number of items
  local x
  local found=()
  while (( x < items )) ; do
    if eval "[[ '${list[x]}' =~ $pat ]]" ; then
      add_list found "${list[x]}"
    fi
    : $(( x++ ))
  done
  if (( ${#found[*]} > 0 )) ; then
    echo "${found[@]}"
    return 0
  fi
  return 1                                # not found
}

# Legacy name
make_list() { join_list "$@" ; }

# end of list-utils.sh
# vim: sw=2 ai

