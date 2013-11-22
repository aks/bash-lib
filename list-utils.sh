# list-utils.sh
# Copyright 2009-2013 Alan K. Stebbens <aks@stebbens.org>
#
# sh script utilities for managing lists of things

LIST_UTILS_VERSION='list-utils.sh v1.2'
#[[ "$LIST_UTILS" = "$LIST_UTILS_VERSION" ]] && return
export LIST_UTILS="$LIST_UTILS_VERSION"

export CHAR_NL=$'\n'
# export CHAR_TAB=$'\t'
# export CHAR_WS=$'\t '

list_help() {
  cat 1>&2 <<EOF
These are the list utilities:

list_init VAR                        # initialize VAR as an empty list

list_add VAR VAL1 [VAL2 ...]         # add VAL1.. to the end of VAR
list_add_once VAR  VAL1 [VAL2 ..]    # add VAL1.. uniquely to the end of VAR

list_insert VAR  VAL ...             # insert VALUE at the front of VAR
list_insert_once VAR VAL ..          # insert VALUE.. at the front of VAR; 

list_push VAR VAL                    # same as "list_add VAR VAL"
list_pop  VAR                        # removes top VAL on VAR and returns in variable "item"

list_get  VAR N                      # get the Nth item of VAR to stdout
list_item VAR N                      # set 'item' to the Nth item of VAR

list_set  VAR N VAL                  # set the Nth item of VAR to VAL

list_copy LIST NEWLIST [START [END]] # copy list LIST to NEWLIST, from START to END

in_list VAR  [-any|-all] VAL ...     # return true if one or more values are in a list
list_size VAR                        # returns the number of items

sort_str VAL ...                     # sort the space-separated words of VAL ..
sort_list VAR                        # sort the contents of VAR (a list) in place
sorted_list VAR                      # output the items of VAR sorted

split_into  VAR "STRING" SEP         # split "STRING" by SEP into VAR
split_str   "STRING" [SEP]           # split "STRING" by SEP

join_list VAR [SEP] ..               # join the items in VAR into a list, separated by SEP,
  SEP can be 
    AND    -- separate with " and "
    OR     -- separate with " or "
    KEYS   -- enclose each item with X' and ', follwed by ','
    TAB    -- use tabs to separate items
    NL     -- separate each item with newline (and some spaces)
    NOWRAP -- do not auto-wrap long lines (default is WRAP)
    ','    -- separate items with a comma (default)
    str    -- separate each item with an given string.

lookup_list LISTVAR KEY              # lookup KEY in LISTVAR
grep_list LISTVAR PAT                # grep PAT across LISTVAR

reduce_list LISTVAR EXPR [INIT]      # reduce LISTVAR using EXPR, with initial value INIT

print_list LISTVAR [indent=INDENT] [width=WIDTH] [sep=SEP] [cols=COLS]
print_list LISTVAR [i=INDENT] [w=WIDTH] [s=SEP]  [c=COLS]

    print the items in LIST in vertically-sorted columns.  Use COLS if given,
    otherwise the number of columns is computed from WIDTH (defaults to 80) and
    the maximum width of all the items in LISTVAR

list_help                             # describe the list functions

EOF
}
help_list() { list_help ; }

error()  { echo "$*" 1>&2   ; exit 1 ; }
errorf() { printf "$@" 1>&2 ; exit 1 ; }

# list_init VAR
#
# Initialize VAR as an empty list

list_init() {
  local var=${1:?'Missing list name'}
  eval "$var=()"
}

# list_add VAR VALUE ...
#  Supports insertion of multiple values into the list

list_add() { 
  local var="$1"
  shift
  local w len
  for w in "$@" ; do
    eval "len=\${#$var[*]}"
    eval "$var[$len]=\"$w\""
  done
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
  while (( $# > 0 )); do
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
#
# Note: because this function modifies the list, it won't work correctly
# if it is invoked within back-quotes.  This is why its return value
# inserted into a variable.

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

# val=`list_get VARLIST N`
#
# Echo the Nth item from VARLIST to output

list_get() {
  local var="${1:?'Missing list name'}"
  local n=${2:?'Missing index'}
  eval "echo \"\${$var[$n]}\""
}

# list_item VARLIST N
#
# Set the variable 'item' to the Nth item from VARLIST

list_item() {
  local var="${1:?'Missing list name'}"
  local n=${2:?'Missing index'}
  eval "item=\"\${$var[$n]}\""
}

# list_set VARLIST N VAL
#
# Set the Nth item in VARLIST to VAL

list_set() {
  local var="${1:?'Missing list name'}"
  local n=${2:?'Missing index'}
  local val="${3:?'Missing value'}"
  eval "let $var[$n]=\"$val\""
}

# list_copy LIST NEWLIST [START [END]]
#
# Copy LIST to NEWLIST, entirely if START and END omitted.  Copy a part of LIST
# to NEWLIST, starting at START, and ending at END (or the end if END is
# omitted).

list_copy() {
  local srclist="${1:?'Missing name for source list'}"
  local dstlist="${2:?'Missing name for new list'}"
  if (( $# == 2 )); then
    eval "$dstlist=( \"\${$srclist[@]}\" )"
  else
    local len=`list_size $srclist`
    local start=${3}
    local end=${4:-$len}
    local x
    list_init $dstlist
    for ((x=$start; x < $end; x++)) ; do
      list_add $dstlist "`list_get $srclist $x`"
    done
  fi
}

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
# str_sort
#
# sort_list NAME -- sort the list items inplace
# list_sort NAME
#
# sorted_list NAME -- output the list items sorted
# list_sorted NAME
#
sort_str() {
  ( for w in "$@" ; do echo "$w" ; done ) | sort -f | tr '\n' ' ' | sed -e 's/ $//'
}
str_sort() { sort_str "$@" ; }

sorted_list() { 
  eval "sort_str \"\${$1[@]}\""
}
list_sorted() { sorted_list "$@" ; }

sort_list() {
  eval "$1=( `sorted_list $1` )"
}
list_sort() { sort_list "$@" ; }

#############################
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
  local count=`list_size $var`
  local val key index
  local list
  list=()
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
  local limit=10
  for ((index=0; index < count; index++)); do
    val="`list_get $var $index`"
    if [[ -n "$val" ]]; then		        # not an empty value?
      if [[ -n "$key" ]] ; then		        # key value?
        val="`normal_key \"$val\"`"	        # yes, quote as a key
      elif [[ -n "$strings" ]]; then	        # string value?
        val="`quoted_string \"$val\"`"	        # quote as a string
      fi
      if [[ ${#list[@]} -gt 0 ]] ; then	        # list not empty?
        list=( "${list[@]}" "$sep" "$val" )     # append new items
        if (( $nowrap == 0 && ( ( $index % 10 ) == 0 ) )) ; then    # every 10 items, cause a newline
          list=( "${list[@]}" "$CHAR_NL" )	# append a new line
        fi
      else
        list=( "$val" )			        # start the list
      fi
    fi
  done
  (IFS= ; echo "${list[*]}")
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
  local nitems=`list_size $listvar`        # number of items
  local found x item
  for ((x=0; x < nitems; x++)) ; do
    list_item $listvar $x
    if [[ "$item" =~ ^$key ]]; then
      if [[ -n "$found" ]]; then
        return 2                          # too many items found
      else
        found="$item"
      fi
    fi
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
  local nitems=`list_size $listvar`
  local found=()
  local x
  for ((x=0; x<nitems; x++)); do
    local val="`list_get $listvar $x`"
    if [[ "$val" =~ $pat ]]; then
      add_list found "$val"
    fi
  done
  if (( `list_size found` > 0 )); then
    list_join found ' '
    return 0
  fi
  return 1                                # not found
}

# reduce_list LISTVAR EXPR INIT
#
#     For each consecutive pair of items in LISTVAR, perform ITEM EXPR ITEM,
#     starting with INIT EXPR ITEM.  If EXPR is an operator (e.g., +, -, etc.)
#     invoke it using arithmetic evaluation, with two variables 'a' and 'b'
#     representing the values being evaluated.  Otherwise, invoke it using
#     function call notation, with the previous value (starting with INIT)
#     preceding the next item value
#
#     Examples:  
#       sum=`reduce_list list_of_nums 'a + b' 0`
#       prod=`reduce_list list_of_nums 'a * b' 1`

reduce_list() {
  local listvar="${1:-Missing list variable name}"
  local expr="${2:-Missing expression}"
  local init="${3}"
  local start=0
  if [[ -z "$init" ]]; then
    init=`list_get $listvar 0`      # use the 1st item as the initial value
    start=1                         # start the loop on the 2nd item
  fi
  local x
  local val="$init"
  local item_count=`list_size $listvar`
  case "$expr" in
    [a-zA-Z_]*) funccall=1 ;;
    *)          funccall=0 ;;
  esac
  for ((x=start; x<item_count; x++)) ; do
    if (( funcall )) ; then
      eval "val=\"\`$func \"$val\" \"`list_get $listvar $x`\"\`"
    else
      local a="$val"
      local b="`list_get $listvar $x`"
      eval "val=\$(( $expr ))"
    fi
  done
  echo "$val"
}

# newlist=( `map_list LISTVAR EXPR` )
#
#     Invoke EXPR on each item in LISTVAR, collecting the results into a list,
#     returned as the result.  If EXPR is a bash expression, the current item
#     is referenced as "$item".  Otherwise, "EXPR $item"

map_list() {
  local listvar="${1:-Missing list variable name}"
  local expr="${2:-Missing expression}"
  local newlist=()
  local x item
  local item_count=`list_size $listvar`
  for ((x=0; x<item_count; x++)) ; do
    item="`list_get $listvar $x`"
    item="`eval \"$expr\"`"
    add_list newlist "$item"
  done
  join_list newlist ' '
}

# max_list listname

max_list() {
  local listvar="${1:-Missing list variable name}"
  reduce_list $listvar 'a >= b ? a : b'
}

min_list() {
  local listvar="${1:-Missing list variable name}"
  reduce_list $listvar 'a >= b ? b : a'
}

avg_list() {
  local listvar="${1:-Missing list variable name}"
  local size=`list_size $listvar`
  if (( size > 0 )); then
    local sum=`reduce_list $listvar 'a + b'`
    echo $(( sum / size ))
  fi
}

# print_list list [indent=INDENT] [width=WIDTH] [sep=SEP] [cols=COLS]
#
#   Print list in vertically sorted columns, optionally indented,
#   limit output to `maxwidth`
#   separate columns by `sep` blanks (defaults to 2)

print_list() {
  local listvar="$1"
  shift
  _set_args "indent width sep cols" "$@"
  # set defaults
  width=${width:-${COLUMNS:-80}}
  sep="${sep:-2}"
  local ourlist
  list_copy $listvar ourlist
  # sort the items
  sort_list ourlist
  local total=`list_size ourlist`

  # determine the maximum column width
  local item_sizes=( `map_list $listvar 'echo ${#item}'` )
  cw=`max_list item_sizes`
  local fmt="%-${cw}s%*s"
  if [[ -n "$indent" ]]; then
    local spaces="`printf \"%*s\" $indent ' '`"
    fmt="$spaces$fmt"
  fi

  # determine the number of columns
  if [[ -z "$cols" ]]; then
    # no explicit # of columns; compute from column width and line width
    local nc=$(( width / (cw + 1) ))
    if (( nc == 0 )) ; then
      nc=1
    fi
  else
    nc=$cols
  fi
  local nr=$(( total / nc))
  if (( total % nc > 0 )); then let nr++ ; fi

  #  R C| 0   1   2
  # ----+-------------
  #  0  | 0   3   6  
  #  1  | 1   4   7
  #  2  | 2   5

  # c=C*NR+R

  # iterate across the values
  local x r c
  for ((r=0; r<nr; r++)); do
    for ((c=0 , x=(c*nr)+r; c<nc && x<total; c++, x=(c*nr)+r)); do
      printf "$fmt" "${ourlist[x]}" $sep ' '
    done
    printf "\n"
  done
}

# _set_args "OPT1 OPT2 ..." "$@"
#
# Scan the arguments looking for OPTn=VAL, and set each OPTn to the
# value.

_set_args() {
  local optlist=()
  add_list optlist $1
  shift
  while (( $# > 0 )) ; do
    local opt="${1%=*}"   # get opt part of 'opt=val'
    local val="${1#*=}"    # get val part of 'opt=val'
    shift
    local var=`lookup_list optlist $opt`
    [[ -n "$var" ]] || error "No such option: '$opt'"
    case "$?" in
      0) eval "$var=\"$val\"" ;;
      1) error "No such option: '$opt'" ;;
      2) error "'$opt' is ambiguous" ;;
    esac
  done
}

# Legacy name
make_list() { join_list "$@" ; }

# end of list-utils.sh
# vim: sw=2 ai

