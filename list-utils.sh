# list-utils.sh
# Copyright 2011 Alan K. Stebbens <aks@stebbens.org>
#
# sh script utilities for managing lists of things
#
# In the descriptions below, "VAR" is an array variable; VAL, VAL1, .. are values.
#
# list_add VAR VAL1 [VAL2 ...]        # add VAL1.. to the end of VAR
#
# list_add_once VAR  VAL1 [VAL2 ..]   # add VAL1.. uniquely to the end of VAR
#
# list_insert VAR  VAL ...            # insert VALUE at the front of VAR
#
# list_insert_once VAR VAL ..         # insert VALUE.. at the front of VAR; 
#
# in_list VAR  [-any|-all] VAL ...    # return true if one or more values are in a list
#
# list_size VAR                       # returns the number of items
#
# sort_str VAL ...                    # sort the space-separated words of VAL ..
#   
# sort_list VAR                       # sort the contents of VAR (a list) in place
#
# join_list VAR [SEP] ..
#   join the items in VAR into a list, separated by SEP,
#   which can be:
#     AND    -- separate with " and "
#     OR     -- separate with " or "
#     KEYS   -- enclose each item with X' and ', follwed by ','
#     TAB    -- use tabs to separate items
#     NL     -- separate each item with newline (and some spaces)
#     NOWRAP -- do not auto-wrap long lines (default is WRAP)
#     ','    -- separate items with a comma (default)
#     str    -- separate each item with an given string.
#
# split_into  VAR "STRING" SEP
#
#   splits a STRING into parts using separator (SEP) (default is ',')
#   and assigns the resulting separated, and quoted strings to the VAR.
#
# split_str   "STRING" [SEP]
#
#   outputs the split of STRING into parts using a separator SEP (defaulting
#   to space/tab).
#
# For the split functions:
#
# If SEP is anything but " ", care is taken to avoid removing whitespace from
# the split values.
#
# SEP can be multiple characters; for example ' ,' will split using both space
# and comma.  By default, splitting is done by tabs.

if [[ -z "$LIST_UTILS" ]]; then

  LIST_UTILS=1

  CHAR_NL=$'\n'
  CHAR_TAB=$'\t'
  CHAR_WS=$'\t '

  split_str() {
    local sep="${2:-$CHAR_TAB}"
    echo "$1"			      | 
    tr "$sep" '\n'		      | 
    sed -Ee 's/^(.*[ ,?*].*)$/"\1"/'  | 
    tr '\n' ' '
  }
  split_list() { split_str "$@" ; }

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
  add_list() { list_add "$@" ; }

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
  # Pop the last item off the list

  list_pop() {
    local var=${1:?'Missing list name'}
    local x=`list_size $var`
    local val=`echo "\${${var}[$x]}"`
    eval "unset ${var}[$x]"
    echo "$val"
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


  # join_list ARRAYNAME[, [AND|OR|KEYS|STR|TAB|NL|<sep>|NOWRAP]]
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

  # Legacy name
  make_list() { join_list "$@" ; }

fi # $LIST_UTILS

# end of list-utils.sh

