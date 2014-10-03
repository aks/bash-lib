# list-utils.sh
# Copyright 2009-2014 Alan K. Stebbens <aks@stebbens.org>
#
# sh script utilities for managing lists of things

LIST_UTILS_VERSION='list-utils.sh v2.4'

source bash-check.sh                # make sure we're running bash >= 3.3

# Don't source this list more than once per session (per version)
[[ "$LIST_UTILS_SH" = "$LIST_UTILS_VERSION" ]] && return
LIST_UTILS_SH="$LIST_UTILS_VERSION"

export CHAR_NL=$'\n'
export CHAR_TAB=$'\t'
export CHAR_WS=$'\t '

list_help() {
  local prog
  for prog in less more cat ; do
    prog=`which $prog 2>/dev/null`
    if [[ -n "$prog" ]]; then
      break
    fi
  done
  $prog <<EOF
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

list_insert_once VAR VAL ...         # insert VAL.. at the front of VAR;

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

sort_str VAL ...                     # sort the space-separated words of VAL ..

str_sort VAL ...                     # an alias to sort_str

list_sort VAR                        # sort the contents of VAR (a list) in place

sort_list VAR                        # alias to list_sort

list_sorted VAR                      # output the items of list VAR sorted

sorted_list VAR                      # alias to list_sorted

sort_str2lines "STRING"              # sort STR with each item in a separate line

sort_list2lines LIST                 # sort LIST with each item in a separate line

split_into  VAR "STRING" SEP         # split "STRING" by SEP into VAR

split_str   "STRING" [SEP]           # split "STRING" by SEP

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

error()  { echo "$*" 1>&2   ; exit 1 ; }
errorf() { printf "$@" 1>&2 ; exit 1 ; }

# list_init VAR
#
# Initialize VAR as an empty list

list_init() {
  list_help_func $# 1 || return
  eval "${1}=()"
}
init_list() { list_init "$@" ; }

# list_add VAR VALUE ...
#  Supports insertion of multiple values into the list

list_add() {
  list_help_func $# 2 || return
  local var="$1"
  shift
  local w
  for w in "$@" ; do
    eval "$var+=( \"$w\" )"
  done
}
list_push() { list_add "$@" ; }
add_list()  { list_add "$@" ; }
push_list() { list_add "$@" ; }

# list_add_once VAR VALUE ...
#
# Add each VALUE only once to VAR
list_add_once() {
  list_help_func $# 2 || return
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

# list_remove VAR VALUE ...
#
# remove VALUEs from VAR

list_remove() {
  list_help_func $# 2 || return
  local var="$1"
  local size=`list_size $var`
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
  list_help_func $# 2 || return
  local var="$1"
  shift
  eval "$var=( \"\$@\" \"\${$var[@]}\" )"
}
insert_list() { list_insert "$@" ; }

# list_insert_once VAR VALUE(s)...
# Insert VALUE(s)... into the list only if it is not already in the list.

list_insert_once() {
  list_help_func $# 2 || return
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
# Note: because this function modifies the list, it won't work correctly if it
# is invoked within back-quotes (within a subshell).  This is why its return
# value inserted into a variable ("item").

list_pop() {
  list_help_func $# 1 || return
  local var="$1"
  local x=`list_size $var`
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
  list_help_func $# 2 || return
  eval "echo -n \"\${$1[$2]}\""
}
get_list_item() { list_get "$@" ; }

# list_item VARLIST N
#
# Set the variable 'item' to the Nth item from VARLIST

list_item() {
  list_help_func $# 2 || return
  local var="$1"
  local n=$(( 10#$2 ))
  eval "item=\"\${$var[$n]}\""
}

# list_set VARLIST N VAL
#
# Set the Nth item in VARLIST to VAL

list_set() {
  list_help_func $# 3 || return
  local var="${1:?'Missing list name'}"
  local n=${2:?'Missing index'}
  local val="${3:?'Missing value'}"
  eval "let $var[$n]=\"$val\""
}
list_set_item() { list_set "$@" ; }
set_list_item() { list_set "$@" ; }

# list_items LIST [START [END]]
#
# Output the items from LIST from START to END.  If END is omitted,
# fetch all the items to the end.  If START is omitted, fetch from
# the beginning items.

list_items() {
  list_help_func $# 1 || return
  local var=${1:?'Missing list name'}
  if (( $# == 1 || -z "$2$3")); then
    eval "local IFS=' ' ; echo \"\${$var[@]}\""
  else
    local size=`list_size $var`
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
  list_help_func $# 1 || return
  local var=${1:?'Missing list name'}
  local size=`list_size $var`
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
  list_help_func $# 2 || return
  local srclist="$1"
  local dstlist="$2"
  shift 2
  list_init $dstlist
  list_add $dstlist `list_items $srclist "$@"`
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
  list_help_func $# 2 || return
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
  list_help_func $# 1 || return
  eval "echo \"\${#$1[@]}\""
}

# args2lines ARG ..
#
# echo each ARG on a separate line

args2lines() { local w ; for w in "$@" ; do echo "$w" ; done ; }

# sort_str2lines "STRING ..."  -- output the words of STRING on separate lines, sorted

sort_str2lines()  {
  list_help_func $# 1 || return
  args2lines "$@"                 | sort -f
}

# sort_list2lines LISTVAR -- output the items of LISTVAR, sorted, one per line

sort_list2lines() {
  list_help_func $# 1 || return
  eval "args2lines \"\${$1[@]}\"" | sort -f
}

# join_lines -- join lines on STDIN together with newlines

join_lines()      { tr '\n' ' ' | sed -e 's/ $//' ; }

# sort_str [WORDS TO BE SORTED]
# str_sort

sort_str() {
  list_help_func $# 1 || return
  sort_str2lines "$@" | join_lines
}
str_sort() { sort_str "$@" ; }

# list_sorted LISTVAR -- output the list items sorted; does not modify LISTVAR
# sorted_list LISTVAR

list_sorted() {
  list_help_func $# 1 || return
  sort_list2lines $1  | join_lines
}
sorted_list() { list_sorted "$@" ; }

# list_sort LISTVAR -- sort the list items inplace; modifies LISTVAR
# sort_list LISTVAR

list_sort() {
  list_help_func $# 1 || return
  eval "$1=( `sorted_list $1` )"
}
sort_list() { list_sort "$@" ; }

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
  list_help_func $# 1 || return
  local sep="${2:-$CHAR_TAB}"
  echo "$1"			      |
  tr "$sep" '\n'		      |
  sed -Ee 's/^(.*[ ,?*].*)$/"\1"/'    |
  tr '\n' ' '
}

str_split() { split_str "$@" ; }

# split_into  VAR "STRING" SEP
#
#   splits a STRING into parts using separator (SEP) (default is ',')
#   and assigns the resulting separated, and quoted strings to the VAR.
#
#   See split_str for details on SEP

split_into() {
  list_help_func $# 2 || return
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

list_join() {
  list_help_func $# 1 || return
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
  list_help_func $# 2 || return
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
  list_help_func $# 2 || return
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
  list_help_func $# 2 || return
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
grep_list() { list_grep "$@" ; }

# list_reduce LISTVAR EXPR INIT
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

list_reduce() {
  list_help_func $# 2 || return
  local listvar="$1"
  local expr="$2"
  local init="$3"
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
reduce_list() { list_reduce "$@" ; }

# newlist=( `list_map LISTVAR EXPR [JOINSTR]` )
#
#     Invoke EXPR on each item in LISTVAR, collecting the results into a list,
#     returned as the result.  If EXPR contains a reference to "\$item" or
#     ${#item} (for length), then it is evaluated as is, otherwise, "EXPR
#     $item" is evaluated.  The results are joined (with `join_list`) using
#     JOINSTR which defaults to ' '.

list_map() {
  list_help_func $# 2 || return
  local listvar="$1"
  local expr="$2"
  local joinstr="${3:- }"               # join string (or a blank)
  local newlist=()
  local x item
  local item_count=`list_size $listvar`
  for ((x=0; x<item_count; x++)) ; do
    item="`list_get $listvar $x`"
    [[ -z "$item" ]] && continue        # ignore empty items
    if [[ "$expr" =~ \$(\{\#?)?item ]]; then    # if $item referenced, then eval expr as is
      item="`eval \"$expr\"`"
    else                                # otherwise,
      item="`eval \"$expr $item\"`"     # treat expr as a function with $item argument
    fi
    list_add newlist "$item"
  done
  list_join newlist NOWRAP "$joinstr"
}
map_list() { list_map "$@" ; }

# list_max listname
list_max() {
  list_help_func $# 1 || return
  list_reduce $1 'a >= b ? a : b'
}
max_list() { list_max "$@" ; }

# list_min LISTNAME
list_min() {
  list_help_func $# 1 || return
  list_reduce $1 'a >= b ? b : a'
}
min_list() { list_min "$@" ; }

# list_sum LISTNAME
list_sum() {
  list_help_func $# 1 || return
  list_reduce $1 'a + b'
}
sum_list() { list_sum "$@" ; }

# list_avg LISTNAME
list_avg() {
  list_help_func $# 1 || return
  local listvar="$1"
  local size=`list_size $listvar`
  if (( size > 0 )); then
    local sum=`list_reduce $listvar 'a + b'`
    echo $(( sum / size ))
  fi
}
avg_list() { list_avg "$@" ; }

# list_print list [indent=INDENT] [width=WIDTH] [sep=SEP] [cols=COLS]
#
#   Print list in vertically sorted columns, optionally indented, limit output
#   to `maxwidth`, separate columns by `sep` blanks (defaults to 2)

list_print() {
  list_help_func $# 1 || return
  local listvar="$1"
  shift
  _set_args "indent width sep cols" "$@"
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
    sort_list2lines $listvar | rs -t $widtharg $separg 0 $cols
  else
    prefix=`printf "%*s" $indent ' '`
    sort_list2lines $listvar | rs -t $widtharg $separg 0 $cols | (
      while read line ; do
        echo -n "$prefix"
        echo "$line"
      done
    )
  fi
}
print_list() { list_print "$@" ; }

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

# list_help_func GIVEN [NEED] || return
#
# If GIVEN is less than NEED, provide the help for the function named NAME.
#
# Typical call:
#
#    list_help_func $# 2 || return

list_help_func() {
  local func="${FUNCNAME[1]}"         # who called us?
  if (( $1 < ${2:-1} )) ; then
    list_help 2>&1 | awk "/^$func/{p=1; print;next}
                          /^[^ ▸]/{if(p){exit}}
                          {if(p){print}}" 1>&2
    return 1
  fi
  return 0
}

# end of list-utils.sh
# vim: sw=2 ai
