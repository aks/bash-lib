# list-utils.sh
# Copyright 2009-2014 Alan K. Stebbens <aks@stebbens.org>
#
# sh script utilities for managing lists of things

LIST_UTILS_VERSION='list-utils.sh v2.7'

source bash-check.sh                # make sure we're running bash >= 3.3

# Don't source this list more than once per session (per version)
[[ "$LIST_UTILS_SH" = "$LIST_UTILS_VERSION" ]] && return
LIST_UTILS_SH="$LIST_UTILS_VERSION"

source help-util.sh
source arg-utils.sh
source talk-utils.sh
source text-utils.sh

export CHAR_NL=$'\n'
export CHAR_TAB=$'\t'
export CHAR_WS=$'\t '

list_help() {
  help_pager <<EOF
These are the list utilities:

list_init VAR                        # initialize VAR as an empty list

list_add      VAR VAL1 [VAL2 ...]    # add VAL1.. to the end of VAR

add_list      VAR VAL1 [VAL2 ...]    # alias to list_add

list_add_once VAR  VAL1 [VAL2 ..]    # add VAL1.. uniquely to the end of VAR

add_list_once VAR VAL ...            # alias to list_add_once

list_remove   VAR VAL1 [VAL2 ..]     # remove VAL1 .. from the VAR list

remove_list_item VAR VAL1 [VAL2 ..]  # alias to list_remove

list_push VAR VAL ...                # alias to list_add

push_list VAR VAL ...                # alias to list_add

list_insert      VAR VAL ...         # insert VAL.. at the front of VAR

list_insert_once VAR VAL ...         # insert VAL.. at the front of VAR

insert_list      VAR VAL ...         # alias to list_insert

insert_list_once VAR VAL ...         # alias to list_insert_once

list_pop VAR                         # removes top VAL on VAR and returns in variable "item"

pop_list VAR                         # alias to list_pop

list_get      VAR N                  # get the Nth item of VAR to stdout

get_list_item VAR N                  # alias to list_get

list_item VAR N                      # set 'item' to the Nth item of VAR

list_set      VAR N VAL              # set the Nth item of VAR to VAL

list_set_item VAR N VAL              # alias to list_set

set_list_item  VAR N VAL             # alias to "list_set"

list_items VAR [START [END]]         # return list items from START to END (or all)

list_copy LIST NEWLIST [START [END]] # copy list LIST to NEWLIST, from START to END

copy_list LIST NEWLIST [START [END]] # alias to list_copy

in_list VAR  [-any|-all] VAL ...     # return true if one or more values are in a list

list_member VAR [-any|-all] VAL ...  # same as in_list

list_size VAR                        # returns the number of items

list_dump VAR                        # dump the list VAR, showing indexes and values

dump_list VAR                        # alias to list_dump

list_sort VAR                        # sort the contents of VAR (a list) in place

sort_list VAR                        # alias to list_sort

list_sorted VAR                      # output the items of list VAR sorted

sorted_list VAR                      # alias to list_sorted

sort_list2lines LIST                 # sort LIST with each item in a separate line

split_into  VAR "STRING" SEP         # split "STRING" by SEP into VAR

list_join VAR [SEP] ..               # join the items in VAR into a list, separated by SEP,
  SEP can be
    AND    -- separate with " and "
    OR     -- separate with " or "
    KEYS   -- enclose each item with X' and ', follwed by ','
    TAB    -- use tabs to separate items
    NL     -- separate each item with newline (and some spaces)
    NOWRAP -- do not auto-wrap long lines (default is WRAP)
    ','    -- separate items with a comma (default)
    str    -- separate each item with an given string.

join_list VAR [SEP] ..               # alias to list_join

join_lines                           # read STDIN and catenate lines; remove trailing NL

list_lookup LISTVAR KEY              # lookup KEY in LISTVAR

lookup_list LISTVAR KEY              # alias to list_lookup

list_grep   LISTVAR PAT              # grep PAT across the contents of LISTVAR

grep_list   LISTVAR PAT              # alias to list_grep

list_map    LISTVAR EXPR [JOINSTR]   # create a list of EXPR applied to each item in LISTVAR

map_list    LISTVAR EXPR [JOINSTR]   # alias to list_map

list_reduce LISTVAR EXPR [INIT]      # reduce LISTVAR using EXPR, with initial value INIT

reduce_list LISTVAR EXPR [INIT]      # alias to list_reduce

list_sum LISTVAR                     # sum the items in LISTVAR

sum_list LISTVAR                     # alias to list_sum

list_max LISTVAR                     # return the maximum item in LISTVAR

max_list LISTVAR                     # alias to list_max

list_min LISTVAR                     # return the minimum item in LISTVAR

min_list LISTVAR                     # alias to list_min

list_avg LISTVAR                     # return the average of the items in LISTVAR

avg_list LISTVAR                     # alias to list_avg

list_print LISTVAR [indent=INDENT] [width=WIDTH] [sep=SEP] [cols=COLS]

list_print LISTVAR [i=INDENT] [w=WIDTH] [s=SEP]  [c=COLS]

    print the items in LIST in vertically-sorted columns.  Use COLS if given,
    otherwise the number of columns is computed from WIDTH (defaults to 80) and
    the maximum width of all the items in LISTVAR

print_list LISTVAR [indent=INDENT] [widht=WIDTH] [sep=SEP] [cols=COLS]

print_list LISTVAR [i=INDENT] [w=WIDTH] [s=SEP]  [c=COLS]

    aliases to list_print

list_help                             # describe the list functions

There are many aliases to the functions above: for a given "list_XXX" function,
there exist an alias "XXX_list".  For example, "max_list" => "list_max",
"dump_list" => "list_dump", "help_list" => "list_help", etc.

EOF
}
help_list() { list_help ; }

# list_init VAR
#
# Initialize VAR as an empty list

list_init() {
  help_args_func list_help $# 1 || return
  __list_init "$@"
}

__list_init() {
  eval "declare -ga $1"
  eval "${1}=()"
}
init_list() { list_init "$@" ; }

# list_add VAR VALUE ...
#  Supports insertion of multiple values into the list

list_add() {
  help_args_func list_help $# 2 || return
  __list_add "$@"
}

__list_add() {
  local var="$1"
  shift
  local w
  for w in "$@" ; do
    eval "$var+=( \"$w\" )"
  done
}

__add_list() { __list_add "$@" ; }

list_push() { list_add "$@" ; }
add_list()  { list_add "$@" ; }
push_list() { list_add "$@" ; }

# list_add_once VAR VALUE ...
#
# Add each VALUE only once to VAR
list_add_once() {
  help_args_func list_help $# 2 || return
  __list_add_once "$@"
}

__list_add_once() {
  local var="$1"
  shift
  while [[ $# -gt 0 ]]; do
    if ! __in_list $var "$1" ; then
      __list_add $var "$1"
    fi
    shift
  done
}

add_list_once() { list_add_once "$@" ; }

# list_remove VAR VALUE ...
#
# remove VALUEs from VAR

list_remove() {
  help_args_func list_help $# 2 || return
  __list_remove "$@"
}

__list_remove() {
  local var="$1"
  local size=`__list_size $var`
  shift
  local x val1 val2 removed
  for ((x=size; x>=0; x--)); do
    eval "val1=\"\${$var[$x]}\""
    for val2 in "$@" ; do
      if [[ "$val1" == "$val2" ]]; then
        eval "unset $var[$x]"
        removed=1
        break
      fi
    done
  done
  if [[ -z "$removed" && -t 1 ]]; then
    echo 2>&1 "Not found"
  else
    eval "$var=( \"\${$var[@]}\" )"     # this removes the empty fields
  fi
}

remove_list_item() { list_remove "$@" ; }

# list_insert VAR VALUE ..
# Insert VALUE at the front of the list

list_insert() {
  help_args_func list_help $# 2 || return
  __list_insert "$@"
}

__list_insert() {
  local var="$1"
  shift
  eval "$var=( \"\$@\" \"\${$var[@]}\" )"
}
insert_list() { list_insert "$@" ; }

# list_insert_once VAR VALUE(s)...
# Insert VALUE(s)... into the list only if it is not already in the list.

list_insert_once() {
  help_args_func list_help $# 2 || return
  __list_insert_once "$@"
}

__list_insert_once() {
  local var="$1"
  shift
  while (( $# > 0 )); do
    if ! __in_list $var "$1" ; then
      __list_insert $var "$1"
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
# Note: because this function modifies the list, it won't work correctly if it
# is invoked within back-quotes (within a subshell).  This is why its return
# value inserted into a variable ("item").

list_pop() {
  help_args_func list_help $# 1 || return
  __list_pop "$@"
}

__list_pop() {
  local var="$1"
  local x=`__list_size $var`
  if (( --x < 0 )); then
    item=
    return 1
  fi
  local val
  eval "val=\"\${$var[$x]}\""
  unset $var[$x]
  eval "$var=( \"\${$var[@]}\" )"
  item="$val"
}

pop_list() { list_pop "$@" ; }

# val=`list_get VARLIST N`
#
# Echo the Nth item from VARLIST to output

list_get() {
  help_args_func list_help $# 2 || return
  __list_get "$@"
}
__list_get() {
  eval "echo -n \"\${$1[$2]}\""
}
get_list_item() { list_get "$@" ; }

# list_item VARLIST N
#
# Set the variable 'item' to the Nth item from VARLIST

list_item() {
  help_args_func list_help $# 2 || return
  __list_item "$@"
}

__list_item() {
  local var="$1"
  local n=$(( 10#$2 ))
  eval "item=\"\${$var[$n]}\""
}

# list_set VARLIST N VAL
#
# Set the Nth item in VARLIST to VAL

list_set() {
  help_args_func list_help $# 3 || return
  __list_set "$@"
}

__list_set() {
  eval "let $1[$2]=\"$3\""
}

list_set_item() { list_set "$@" ; }
set_list_item() { list_set "$@" ; }

# list_items LIST [START [END]]
#
# Output the items from LIST from START to END.  If END is omitted,
# fetch all the items to the end.  If START is omitted, fetch from
# the beginning items.

list_items() {
  help_args_func list_help $# 1 || return
  __list_items "$@"
}

__list_items() {
  local var="$1"
  if (( $# == 1 || -z "$2$3")); then
    eval "local IFS=' ' ; echo \"\${$var[@]}\""
  else
    local size=`__list_size $var`
    local start=${2:-0}
    local end=${3:-$size}
    local x
    for ((x=start; x<size && x<=end; x++)); do
      eval "echo -n \"\${$var[$x]}\""     # output the xth item
    done
    echo ''
  fi
}

# list_dump VAR
#
# Show the list items, with indexes

list_dump() {
  help_args_func list_help $# 1 || return
  __list_dump "$@"
}

__list_dump() {
  local var="$1"
  local size=`__list_size $var`
  local x val fmt
  if (( size >= 1000 ));  then fmt="[%4d] %s\n"
  elif (( size >= 100 )); then fmt="[%3d] %s\n"
  else                         fmt="[%2d] %s\n" ; fi
  printf "%s:\n" "$var"
  for ((x=0; x< size; x++)); do
    eval "val=\"\${$var[$x]}\""
    printf "$fmt" $x "$val"
  done
}
dump_list() { list_dump "$@" ; }

# list_copy LIST NEWLIST [START [END]]
#
# Copy LIST to NEWLIST, entirely if START and END omitted.  Copy a part of LIST
# to NEWLIST, starting at START, and ending at END (or the end if END is
# omitted).

list_copy() {
  help_args_func list_help $# 2 || return
  __list_copy "$@"
}

__list_copy() {
  local srclist="$1"
  local dstlist="$2"
  shift 2
  __list_init $dstlist
  __list_add $dstlist `__list_items $srclist "$@"`
}
copy_list() { list_copy "$@" ; }

# in_list LISTNAME [-any] VALUE ..
# in_list LISTNAME -all V2 ...
#
# Returns 0 if any of VALUE(s) are one of LISTNAME's values
# Returns 1 otherwise.
#
# If "-all" given, require all values to be in the list in order
# to return 0.

in_list () {
  help_args_func list_help $# 2 || return
  __in_list "$@"
}

__in_list() {
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
list_member() { in_list "$@" ; }


# list_size NAME -- return list size
list_size() {
  help_args_func list_help $# 1 || return
  __list_size "$@"
}

__list_size() {
  eval "echo \"\${#$1[@]}\""
}

# sort_list2lines LISTVAR -- output the items of LISTVAR, sorted, one per line

sort_list2lines() {
  help_args_func list_help $# 1 || return
  __sort_list2lines "$@"
}

__sort_list2lines() {
  eval "args2lines \"\${$1[@]}\"" | sort -f
}

# list_sorted LISTVAR -- output the list items sorted; does not modify LISTVAR
# sorted_list LISTVAR

list_sorted() {
  help_args_func list_help $# 1 || return
  __list_sorted "$@"
}

__list_sorted() {
  sort_list2lines $1  | join_lines
}

sorted_list() { list_sorted "$@" ; }

# list_sort LISTVAR -- sort the list items inplace; modifies LISTVAR
# sort_list LISTVAR

list_sort() {
  help_args_func list_help $# 1 || return
  __list_sort "$@"
}

__list_sort() {
  eval "$1=( `sorted_list $1` )"
}

sort_list() { list_sort "$@" ; }


# split_into  VAR "STRING" SEP
#
#   splits a STRING into parts using separator (SEP) (default is ',')
#   and assigns the resulting separated, and quoted strings to the VAR.
#
#   See split_str for details on SEP

split_into() {
  help_args_func list_help $# 2 || return
  __split_into "$@"
}

__split_into() {
  local sep="${3:-' 	'}"
  eval "$1=( `__split_str \"$2\" \"$sep\"` )"
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

list_join() {
  help_args_func list_help $# 1 || return
  __list_join "$@"
}

__list_join() {
  local var="$1"
  local count=`__list_size $var`
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
    val="`__list_get $var $index`"
    if [[ -n "$val" ]]; then		        # not an empty value?
      if [[ -n "$key" ]] ; then		        # key value?
        val="`normal_key \"$val\"`"	        # yes, quote as a key
      elif [[ -n "$strings" ]]; then	        # string value?
        val="`quoted_string \"$val\"`"	        # quote as a string
      fi
      if [[ ${#list[@]} -gt 0 ]] ; then	        # list not empty?
        list+=( "$sep" "$val" )
        if (( $nowrap == 0 && ( ( $index % 10 ) == 0 ) )) ; then    # every 10 items, cause a newline
          list+=( "$CHAR_NL" )	                # append a new line
        fi
      else
        list=( "$val" )			        # start the list
      fi
    fi
  done
  (local IFS= ; echo "${list[*]}")
}

join_list() { list_join "$@" ; }

# normal_key KEYSTRING
normal_key() { echo "E'\\\\x$1'" ; }

# quoted_string "SOMESTRING"
quoted_string() { echo "\"${*//\"/\\\"}\"" ; }


# list_lookup LISTVAR KEY
#
#   Lookups up KEY in the items of LISTVAR, returns the matching LISTVAR, using
#   a case-insensitive, disambiguating search.  If there is no match, returns
#   the empty string, with return code of 1.  If there is a match, returns the
#   matching item, and a return code of 0.  If there are two or more matches,
#   returns empty string, and return code of 2.
#
# item=`list_lookup LIST KEY`
# returns item matching key, with return code 0
# return code 1: KEY not found
# return code 2: KEY ambiguous

list_lookup() {
  help_args_func list_help $# 2 || return
  __list_lookup "$@"
}

__list_lookup() {
  local listvar="$1"
  local key="$2"
  local nitems=`__list_size $listvar`        # number of items
  local found x item
  for ((x=0; x < nitems; x++)) ; do
    __list_item $listvar $x
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

lookup_list() { list_lookup "$@" ; }

# lookup_error $CODE WORD ["NOT-FOUND-MSG" ["AMBIGUOUS-MSG"]]
#
# Usage:
#  foundword=`list_lookup list $someword`
#  [[ $? != 0 ]] && lookup_error $? $someword
#
# or:
#  foundword=`list_lookup list $someword` || lookup_error $? $someword`
#
# or:
#  foundword=`list_lookup list $someword` || \
#       lookup_error $? $someword "%s was not found" "%s is ambiguous"
#
# You can redefine "lookup_error" to use your own messages.

lookup_error() {
  help_args_func list_help $# 2 || return
  __lookup_error "$@"
}

__lookup_error() {
  if [[ $1 -eq 1 ]]; then
    errorf "${3:-'%s' was not found}" "$2"
  elif [[ $1 -eq 2 ]]; then
    errorf "${4:-'%s' is ambiguous.}" "$2"
  fi
}

# list_grep LISTVAR PAT
#
#   Like lookup_list but returns all partially matching entries on multiple
#   matches.

list_grep() {
  help_args_func list_help $# 2 || return
  __list_grep "$@"
}

__list_grep() {
  local listvar="$1"
  local pat="$2"
  local nitems=`__list_size $listvar`
  local found=()
  local x
  for ((x=0; x<nitems; x++)); do
    local val="`__list_get $listvar $x`"
    if [[ "$val" =~ $pat ]]; then
      __list_add found "$val"
    fi
  done
  if (( `__list_size found` > 0 )); then
    __list_join found ' '
    return 0
  fi
  return 1                                # not found
}

grep_list() { list_grep "$@" ; }

# list_reduce LISTVAR EXPR [INIT]
#
#     For each consecutive pair of items in LISTVAR, evaluate EXPR.  If EXPR
#     begins with a function name, the EXPR (function) is evaluated.  If EXPR
#     begins with a non-alphabetic character (e.g., '(expr)'), the EXPR is
#     evaluated in an arithmetic context.  In either case, the EXPR should
#     reference two variables '$a' and '$b', or, in the arithmetic case: 'a'
#     and 'b'.
#
#     If both variables are not referenced within EXPR, then the EXPR is
#     evaluated either as a function call: "EXPR $a $b", or as an infix
#     operation between the two values: "a EXPR b".
#
#     If INIT is omitted, use the first item in LISTVAR.
#
#     Examples:
#
#       sum=`reduce_list list_of_nums '+'`
#
#       prod=`reduce_list list_of_nums '*'`
#
#       min=`reduce_list list_of_nums '(a <= b ? a : b)'`
#
#       squares=`list_map list_of_nums '(x * x)'`
#
#       sum_of_squares=`list_reduce squares '+'`

list_reduce() {
  help_args_func list_help $# 2 || return
  __list_reduce "$@"
}

__list_reduce() {
  local _listvar="$1"
  local _expr="$2"
  local _init="$3"
  local _start=0
  if [[ -z "$_init" ]]; then
    _init=`__list_get $_listvar 0`    # use the 1st item as the initial value
    _start=1                          # start the loop on the 2nd item
  fi
  local _x
  local _val="$_init"
  local _item_count=`__list_size $_listvar`
  local _exprtype=0                   # assume infix operator
  if [[ "$_expr" =~ ^[a-zA-Z_] ]] ; then
    (( _exprtype += 1 ))              # function call
  fi
  if [[ "$_expr" =~ \$a[^a-zA-Z0-9_]|(^|[^a-zA-Z0-9_])a([^a-zA-Z0-9_]|$) && \
        "$_expr" =~ \$b[^a-zA-Z0-9_]|(^|[^a-zA-Z0-9_])b([^a-zA-Z0-9_]|$) ]] ; then
    (( _exprtype += 2 ))              # explicit variables
  fi
  for ((_x=_start; _x<_item_count; _x++)) ; do
    local a="$_val"
    local b="`__list_get $_listvar $_x`"
    case $_exprtype in
      0) eval "_val=\$(( $a $_expr $b ))" ;;      # EXPR
      1) eval "_val=\"`$_expr '$a' '$b' `\"" ;;   # FUNC
      2) eval "_val=\$(( $_expr ))" ;;            # EXPR (with $a $b)
      3) eval "_val=\"`$_expr`\"" ;;              # FUNC (with $a $b)
    esac
  done
  echo "$_val"
}

reduce_list() { list_reduce "$@" ; }

# newlist=( `list_map LISTVAR EXPR [JOINSTR]` )
#
#     Evaluate EXPR on each item in LISTVAR, collecting the results into a
#     list, returned as the result.  The EXPR can refer to the current value
#     using the name 'x' or 'item'.   If EXPR contains neither 'x' nor 'item',
#     then it is treated as a function call applied to the current value.  If
#     EXPR begins with an alphabetic character, then it is treated as a
#     function name.  If the EXPR begins with '(', then the evaluation occurs
#     in an arithmetic context.
#
#     Empty items in the LISTVAR are ignored.
#
#     The  results are joined (with `join_list`) using JOINSTR which defaults
#     to ' '.

list_map() {
  help_args_func list_help $# 2 || return
  __list_map "$@"
}

__list_map() {
  local _listvar="$1"
  local _expr="$2"
  local _joinstr="${3:- }"               # join string (or a blank)
  local _newlist=()
  local _x x item
  local _item_count=`__list_size ${_listvar}`

  # expression types:
  #  0 -  EXPR [ITEM]                   # expression with implicit argument
  #  1 -  EXPR                          # expression with explicit argument
  #  2 -  ( EXPR )                      # arithmetic expression (must reference x)

  local _exprtype=0                     # assume expression with implicit argument
  if [[ "$_expr" =~ ^[^a-zA-Z0-9] ]]; then  # if doesn't start with an alphabetic
    _exprtype=2                         # arithmetic context eval
  elif [[ "$_expr" =~ \$(\{\#?)?(item|x)([^a-zA-z0-9_]|$) ]] ; then
    _exprtype=1                         # explicit argument
  fi
  for ((_x=0; _x<_item_count; _x++)) ; do
    item="`__list_get $_listvar $_x`"   # get the current value
    [[ -z "$item" ]] && continue        # ignore empty items
    x="$item"                           # x = item
    case $_exprtype in
      0) item="`eval \"$_expr \\\"$item\\\"\"`" ;;  # eval expr with implicit arg
      1) item="`eval \"$_expr\"`" ;;                # eval expr with explicit arg
      2) eval "item=\"\$(( $_expr ))\"" ;;          # eval arithmetic expr with explicit arg
    esac
    __list_add _newlist "$item"           # accumulate the result
  done
  __list_join _newlist NOWRAP "$_joinstr"
}

map_list() {
  list_map "$@"
}

# list_max listname
list_max() {
  help_args_func list_help $# 1 || return
  __list_max "$@"
}

__list_max() {
  __list_reduce $1 '(a >= b ? a : b)'
}

max_list() { list_max "$@" ; }

# list_min LISTNAME
list_min() {
  help_args_func list_help $# 1 || return
  __list_min "$@"
}

__list_min() {
  __list_reduce $1 '(a >= b ? b : a)'
}

min_list()   { list_min "$@" ; }

# list_sum LISTNAME
list_sum() {
  help_args_func list_help $# 1 || return
  __list_sum "$@"
}

__list_sum() {
  __list_reduce $1 '+'
}

sum_list()   { list_sum "$@" ; }

# list_avg LISTNAME
list_avg() {
  help_args_func list_help $# 1 || return
  __list_avg "$@"
}

__list_avg() {
  local listvar="$1"
  local size=`__list_size $listvar`
  if (( size > 0 )); then
    local sum=`__list_sum $listvar`
    echo $(( sum / size ))
  fi
}

avg_list() { list_avg "$@" ; }

# list_print list [indent=INDENT] [width=WIDTH] [sep=SEP] [cols=COLS]
#
#   Print list in vertically sorted columns, optionally indented, limit output
#   to `maxwidth`, separate columns by `sep` blanks (defaults to 2)

list_print() {
  help_args_func list_help $# 1 || return
  __list_print "$@"
}

__list_print() {
  local listvar="$1"
  shift
  __set_args "indent width sep cols" "$@"
  # set defaults
  sep="${sep:-2}"
  widtharg=
  if [[ -n "$width" ]]; then
    widtharg="-w$width "
  fi
  if [[ -n "$sep" ]]; then
    separg="-g$sep"
  fi
  # sort & shape the items
  if [[ -z "$indent" ]]; then
    __sort_list2lines $listvar | rs -t $widtharg $separg 0 $cols
  else
    prefix=`printf "%*s" $indent ' '`
    __sort_list2lines $listvar | rs -t $widtharg $separg 0 $cols | (
      while read line ; do
        echo -n "$prefix"
        echo "$line"
      done
    )
  fi
}

print_list()   {   list_print "$@" ; }
__print_list() { __list_print "$@" ; }

# The original columnizing formatter, in bash (without rs)
#
# local total=`list_size ourlist`
#
# # determine the maximum column width
# local item_sizes=( `map_list $listvar 'echo ${#item}'` )
# cw=`max_list item_sizes`
# local fmt="%-${cw}s%*s"
# if [[ -n "$indent" ]]; then
#   local spaces="`printf \"%*s\" $indent ' '`"
#   fmt="$spaces$fmt"
# fi

# # determine the number of columns
# if [[ -z "$cols" ]]; then
#   # no explicit # of columns; compute from column width and line width
#   local nc=$(( width / (cw + 1) ))
#   if (( nc == 0 )) ; then
#     nc=1
#   fi
# else
#   nc=$cols
# fi
# local nr=$(( total / nc))
# if (( total % nc > 0 )); then let nr++ ; fi

  #  R C| 0   1   2
# # ----+-------------
# #  0  | 0   3   6
# #  1  | 1   4   7
# #  2  | 2   5

# # c=C*NR+R

# # iterate across the values
# local x r c
# for ((r=0; r<nr; r++)); do
#   for ((c=0 , x=(c*nr)+r; c<nc && x<total; c++, x=(c*nr)+r)); do
#     printf "$fmt" "${ourlist[x]}" $sep ' '
#   done
#   printf "\n"
# done
#}

# __set_args "OPT1 OPT2 ..." "$@"
#
# Scan the arguments looking for OPTn=VAL, and set each OPTn to the
# value.

__set_args() {
  local optlist=()
  __list_add optlist $1
  shift
  while (( $# > 0 )) ; do
    local opt="${1%=*}"   # get opt part of 'opt=val'
    local val="${1#*=}"    # get val part of 'opt=val'
    shift
    local var=`__list_lookup optlist $opt`
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
