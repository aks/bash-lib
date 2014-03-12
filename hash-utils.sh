# hash-utils.sh
# Copyright 2014 Alan K. Stebbens <aks@stebbens.org>
#
# bash script utilities for managing hashes (associative arrays)

HASH_UTILS_VERSION='hash-utils.sh v1.1'
[[ "$HASH_UTILS" = "$HASH_UTILS_VERSION" ]] && return
HASH_UTILS="$HASH_UTILS_VERSION"

source list-utils.sh

hash_help() {
  cat 1>&2 <<EOF

Hashes are associative arrays. Hashes have _keys_ and associated
_values_.  Bash version 4 supports associative arrays, but bash
ver 3 does not.  Use this library to get more hash-like functions
regardless of which bash version you are using.

These are the hash utilities:

hash_init VAR [DEFAULT]              # initialize VAR as an empty hash

hash_default VAR                     # return the default value for HASH

hash_set_default VAR DEFAULT         # set the default value for HASH

hash_put VAR KEY VAL ...             # insert KEY=>VAL into the hash
hash_set VAR KEY VAL                 # alias to "hash_put"

hash_get  VAR KEY                    # get the Nth item of VAR to stdout

hash_delete VAR KEY                  # delete VAR[KEY]

hash_delete_if VAR KEY CONDITION     # delete VAR[KEY} if CONDITION is true

hash_keys VAR                        # return all the keys in hash
hash_values VAR                      # return all the values in hash

hash_each VAR KEYVAR EXPR            # eval EXPR, setting KEYVAR to each key in VAR

hash_copy HASH NEWHASH KEY1 ...      # copy items at KEY1, .. from HASH1 to NEWHASH

in_hash VAR KEY                      # test if KEY is in the hash VAR
has_key VAR KEY
hash_member VAR KEY
hash_include VAR KEY

hash_size VAR                        # returns the number of key=>value pairs

hash_merge VAR1 VAR2                 # merge key/value pairs from VAR2 with VAR1

hash_print HASHVAR [indent=INDENT] [width=WIDTH] [gutter=GUTTER] [cols=COLS] [sep=SEP]

    print the items in HASHVAR in vertically-sorted columns.  The number of
    columns is determined by COLS, if given, or by WIDTH (defaults to 80)
    divided by the maximum width of all items in HASHVAR.

    Use GUTTER blanks (default 2) to separate columns.

    If SEP is given, it is used to delimit columns intead of blanks.

    Each option may be abbreviated to its leading character (e.g., "g" for "gutter").

hash_help                             # describe the list functions

EOF
}
help_hash() { hash_help ; }

error()  { echo "$*" 1>&2   ; exit 1 ; }

# hash_init VAR [DEFAULT]
#
# Initialize VAR as an empty hash
# if DEFAULT given, set as the default value on non-existant keys

hash_init() {
  hash_help_func $# || return
  local var="$1"
  eval "unset $var"
  eval "declare -gA $var"                 # declare the global associative array
  (( $# > 1 )) && { hash_set_default $var "$2" ; }
}

# hash_set_default VAR DEFAULT
#
# Set the default value for the hash VAR

hash_set_default() {
  hash_help_func $# 2 || return
  eval "declare -g ${1}_default"
  eval "${1}_default=\"$2\""
}

# hash_default VAR
#
# Return the default value for the hash VAR

hash_default() {
  hash_help_func $# || return
  eval "echo \"\${${1}_default}\""
}

# hash_info VAR
# 
# Display information about the hash VAR:
#    hash: somehash
#    size: 4
#    default: (empty)

hash_info() {
  hash_help_func $# || return
  local var="$1"
  local type=`declare -p $var 2>/dev/null`
  if [[ ! ( "$type" =~ -A ) ]]; then
    printf "%s is not a hash\n" $var
    return 1
  fi
  local size=`hash_size $var`
  local def=`hash_default $var`
  printf   "   hash: %s\n" $var
  printf   "   size: %d\n" $size
  if [[ -n "$def" ]]; then
    printf "default: %s\n" $def
  fi
}


# hash_set VAR KEY VALUE [KEY2 VAL2 ...]
#  Supports insertion of multiple values into the hash

hash_set() { 
  hash_help_func $# 3 || return
  local var="$1"
  shift
  local key val
  while (( $# > 0 )) ; do
    key="$1" val="$2" ; shift 2
    eval "$var[\"$key\"]=\"$val\""
  done
}
hash_put() { hash_set "$@" ; }

# hash_get_keys HASHVAR
#
# Set "keys" to the list of keys in HASHVAR

hash_get_keys() {
  eval "keys=( \"\${!$1[@]}\" )"
}

# hash_reset VAR
#
# Reset the hash VAR to its initial empty state.

hash_reset() {
  hash_help_func $# || return
  local var="$1" keys k
  hash_get_keys $var
  for k in "${keys[@]}" ; do
    unset $var['$k']
  done
}


# hash_get VAR KEY
#
# Get the value associated with KEY.  If there is none, use any defined default value.

hash_get() {
  hash_help_func $# 2 || return
  local val
  eval "val=\"\${$1['$2']}\""
  [[ -z "$val" ]] && { eval "val=\"\${${1}_default}\"" ; }
  echo "$val"
}

# hash_delete VAR KEY
#
# Delete VAR[KEY]

hash_delete() {
  hash_help_func $# 2 || return
  unset $1["$2"]
}

# hash_delete_if VAR KEY COND
#
# Delete VAR[KEY] if COND is true

hash_delete_if() {
  hash_help_func $# 3 || return
  if [[ -n "$3" ]]; then
    unset $1["$2"]
  fi
}

# hash_keys VAR
# hash_values VAR

hash_keys() {
  hash_help_func $# || return
  eval "echo \"\${!$1[@]}\""
}

hash_values() {
  hash_help_func $# || return
  eval "echo \"\${$1[@]}\""
}

# hash_each      VAR KEY     EXPR
# hash_each_pair VAR KEY VAL EXPR
#
# Iterate over the key/value pairs in VAR, seting KEY to each key, and then
# evaluating EXPR.
#
# When "hash_each_pair" is invoked, the current key is assigned to key, and the
# current value is assigned to VAL on each iteration.

hash_each() {
  hash_help_func $# 3 || return
  local val
  hash_each_pair $1 $2 val "$3"
}

hash_each_pair() {
  hash_help_func $# 4 || return
  local name key keyn valn expr
  local -a keys
  name="${1:?'Missing hash variable name'}"
  keyn=${2:-key} valn=${3:-val} expr="$4"
  hash_get_keys $name
  for key in "${keys[@]}" ; do
    eval "${keyn}=\"$key\""
    eval "${valn}=`hash_get $name \"$key\"`"
    eval "$expr"
  done
}


# in_hash HASH KEY
#
# Returns 0 if KEY is defined in the HASH
# Returns 1 otherwise.

in_hash() {
  hash_help_func $# 2 || return
  local val=`hash_get $1 "$2"`
  [[ -n "$val" ]] && return 0 || return 1
}
has_key()      { in_hash "$@" ; }     # aliases
hash_member()  { in_hash "$@" ; }
hash_include() { in_hash "$@" ; }


# hash_size NAME -- return list size
hash_size() { 
  hash_help_func $# || return
  eval "echo \"\${#$1[@]}\"" 
}

# hash_merge VAR1 VAR2
#
# Merge hash VAR2 into VAR1

hash_merge() {
  hash_help_func $# 2 || return
  local key val
  hash_each_pair $2 key val "hash_put $1 \"\$key\" \"\$val\""
}

# print_hash hash [indent=INDENT] [width=WIDTH] [gw=GUTTER] [cols=COLS] [sep=SEP]
#
#   Print list in vertically sorted columns, optionally indented, limit output
#   to `maxwidth` separate columns by `GUTTER` blanks (defaults to 2).  Use SEP
#   as separator character between columns.
#
#   hash=( [KEY1]=VAL1
#          [KEY2]=VAL2
#           ...
#        )

hash_print() {
  hash_help_func $# || return
  local var="$1"
  shift
  local indent width gutter sep cols widtharg gutterarg separg prefix 
  _set_args "indent width gutter sep cols" "$@"
  # set defaults
  gutter="${gutter:-2}"
  widtharg=
  [[ -n "$width" ]]   && {  widtharg="-w$width " ; }
  [[ -n "$gutter" ]]  && { gutterarg="-g$gutter" ; }
  [[ -n "$sep" ]]     && {    separg="-C'$sep'"  ; }
  # sort & shape the items
  if [[ -z "$indent" ]]; then
    hash_print_items $var | rs -t $widtharg $gutterarg $separg 0 $cols
  else
    prefix=`printf "%*s" $indent ' '`
    hash_print_items $var | rs -t $widtharg $gutterarg $separg 0 $cols | (
      while read line ; do
        printf "%s%s\n" "$prefix" "$line"
      done
    )
  fi
}
print_hash() { "$@" ; }

# hash_print_items VAR
hash_print_items() {
  hash_help_func $# || return
  local var="$1" k
  local -a keys
  hash_get_keys $var
  for k in "${keys[@]}" ; do
    hash_print_item $var "$k"
    echo ''
  done
}

# hash_print_item VAR KEY

hash_print_item() {
  local var="$1" key="$2"
  local val=`hash_get $var "$key"`
  printf "%s['%s']='%s'" $var "$key" "$val"
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

# hash_help_func GIVEN [NEED] || return
#
# If GIVEN is less than NEED, provide the help for the function named NAME.
#
# Typical call:
#
#    hash_help_func $# 2 || return

hash_help_func() {
  local func="${FUNCNAME[1]}"         # who called us?
  if (( $1 < ${2:-1} )) ; then
    hash_help 2>&1 | awk "/^$func/{p=1; print;next}
                          /^[^ 	]/{if(p){exit}}
                          {if(p){print}}"
    return 1
  fi
  return 0
}

# end of hash-utils.sh
# vim: sw=2 ai

