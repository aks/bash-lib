# arg-utils.sh
#
# handy functions for flexibly managing function arguments in bash scripts
#
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>
#

ARG_UTILS_VERSION="arg-utils.sh v2.0"

[[ "$ARG_UTILS_SH" = "$ARG_UTILS_VERSION" ]] && return
ARG_UTILS_SH="$ARG_UTILS_VERSION"

source help-util.sh

arg_utils_help() {
    help_pager <<'EOF'
The `arg-utils.sh` library is a collection of bash functions that
enable flexible argument handling on functions that need to be able
to accept arguments on the command-line or on STDIN.

When writing a bash function that can accept input on the command line or from
STDIN, the function should begin with an invocation of one of the following
functions.

For example, if we had a function that needed a numeric argument, the following
  invocation would be used:

  local f=`numarg_or_input "$1"`

If a text argument is needed:

  local txtarg=`arg_or_input "$1"`

For those cases where two or more arguments can be accepted, either on the
command-line or from STDIN:

  local args=( `args_or_input "$@"` )

The following are the arg-util functions:

numarg_or_input "$1"     Return a numeric argument or read it from `STDIN`

arg_or_input "$1"        Return the argument or read it from `STDIN`

args_or_input "$@"       Return arguments or read them from `STDIN`

args_or_stdin "$@"       Return the arguments or read all of `STDIN`

append_args "$@"         Append the arguments to the next line from `STDIN`

append_arg "$1"          Append the argument to the next line from `STDIN`
EOF
}

help_arg_utils() { arg_utils_help ; }

# The following functions, "numarg_or_input", "arg_for_input", and
# "args_or_input" enable bash functions using them to flexibly accept an
# argument, or arguments, on their call, or on STDIN.
#
# For example, let's say we have two bash functions to convert Celsius to
# Farheneit and vice-versa.  Let's call them "c2f" and "f2c".  With these
# functions, they can be used in two ways:
#
# Typical functions with arguments:
#
# c2f 69              # convert 69C to F
# f2c 10              # convert 10F to C
#
# Or, accepting their input on STDIN, as in a pipe:
#
#  echo 69 | c2f      # convert 69C to F
#  echo 10 | f2c      # convert 10F to C
#
# The advantage of the latter appraoch is that the functions can be fitted into
# a pipe where the data can come from another process directly, on its STDOUT.
#
# The definition of these two functions would be:
#
#  # f2c -- convert F to C via: (°F  -  32)  x  5/9 = °C
#  function f2c() {
#    local f=`numarg_or_input "$1"`
#    echo "scale=1; ($f - 32)*5/9' | bc
#  }
#  # c2f -- convert C to F via °C  x  9/5 + 32 = °F
#  function c2f() {
#    local c=`numarg_or_input "$1"`
#    echo "scale=1; $c * 9/5 + 32" | bc
#  }


# local arg=`numarg_or_input $1`
#
# Return the numeric argument or read from stdin

numarg_or_input() {
  __numarg_or_input "$@"
}

__numarg_or_input() {
  local -i arg
  if [[ $# -eq 0 || -z "$1" ]] ; then
    local func=`__calling_funcname`
    local -a args
    while (( ${#args[*]} == 0 )) ; do
      read -p "${func}? " args
    done
    arg=$(( 10#${args[0]} ))
  else
    arg=$(( 10#$1 ))
  fi
  echo $arg
}

# func=`__calling_funcname`
#
# Obtain the first function name not prefixed with "__"

__calling_funcname() {
  local _x=1
  local func="${FUNCNAME[1]}"
  for (( _x=1; _x <= ${#FUNCNAME[*]}; _x++ )); do
    [[ "$func" =~ ^__ ]] && continue
    func="${FUNCNAME[$_x]}"
    break
  done
  echo "$func"
}

# local arg=`arg_or_input "$1"`
#
# Return the argument given, or the first non-empty line from STDIN

arg_or_input() {
  __arg_or_input "$@"
}

__arg_or_input() {
  local arg
  if [[ $# -eq 0 || -z "$1" ]]; then
    local func=`__calling_funcname`
    local -a args
    while (( ${#args[*]} == 0 )) ; do
      read -p "${func}? " args
    done
    arg="${args[0]}"
  else
    arg="$1"
  fi
  echo "$arg"
}


# local args=( `args_or_input "$@"` )
#
# Return the arguments or read a line of non-empty input

args_or_input() {
  __args_or_input "$@"
}

__args_or_input() {
  if (( $# == 0 )) ; then
    local -a args
    local func=`__calling_funcname`
    while (( ${#args[*]} == 0 )); do
      read -p "$func? " -a args
    done
    echo "${args[@]}"
  else
    echo "$@"
  fi
}

# args_or_stdin "$@" | some-pipe
#
# return the given arguments, or read & return STDIN until EOF

args_or_stdin() {
  __args_or_stdin "$@"
}

__args_or_stdin() {
  if [[ $# -gt 0 ]] ; then
    echo "$*"
  else
    cat
  fi
}


# append_arg  ARG
# append_args ARGS
#
# appends ARGS to the next line of input, and return the entire string
#
#    echo SOMEDATA | input_with_arg SOMEARG ==> SOMEDATA SOMEARG

append_arg() {
  local -a data
  read -a data
  echo "${data[@]}" "$@"
}
append_args() { append_arg "$@" ; }


# end of arg-utils.sh
# vim: sw=2 ai
