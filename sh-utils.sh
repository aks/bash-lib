# sh-utils.sh
#
# handy functions for writing bash-based scripts
#
# Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>
#
#

# Don't read ourselves multiple times
#[[ -n "$SH_UTILS" ]] && return 0

SH_UTILS_VERSION="sh-utils.sh v1.5"

export SH_UTILS="$SH_UTILS_VERSION"

sh_utils_help() {
    cat 1>&2 <<EOF
Shell utility functions:

  talk MSG ..       Print all arguments on STDERR.
 vtalk MSG ..       If \$norun or \$verbose is set, print all args on STDERR.
nvtalk MSG          Print all arguments on STDERR only if \$verbose is not set.
nqtalk MSG          Print all arguments on STDERR only if \$quiet isn not set.
 error [CODE] "MSG" Print MSG on STDERR, then exit with code CODE (or 2)
   die "MSG"        Print MSG on STDERR, then die (with 'kill -ABRT')

  talkf FMT ARGS ..         Printf FMT ARGS on STDERR
 vtalkf FMT ARGS ..         Printf FMT ARGS on STDERR if \$norun or \$verbose set
nvtalkf FMT ARGS ..         Printf FMT ARGS on STDERR unless \$verbose set
nqtalkf FMT ARGS ..         Printf FMT ARGS on STDERR unless \$quiet set
 errorf [CODE] FMT ARGS ..  Printf FMT ARGS on STDERR, then exit \$CODE [2]
   dief FMT ARGS ..         Printf FMT ARGS on STDERR, then die (with 'kill -ABRT')

run COMMAND ARGS .. Show COMMAND ARGS if \$norun or \$verbase; run command unless \$norun.

rm_file_later FILE  Cause FILE to be removed upon program exit.

add_trap "CMD" SIGNAL ..   Add CMD to the trap list for SIGNAL

fn_exists FUNCTION         return 0 (true) if FUNCTION exists; 1 (false) otherwise

args=\`numarg_or_input "\$1"\`   Return a numeric argument or read it from STDIN

args=( \'args_or_input "\$@"\` )  Return arguments or read them from STDIN

EOF
}
help_sh_utils() { sh_utils_help ; }

#   talk MSG      - show MSG on STDERR
#  vtalk MSG      - show MSG on STDERR if $norun or $verbose
# nvtalk MSG      - show MSG on STDERR unless $verbose is set
# nqtalk MSG      - show MSG on STDERR unless $quiet is set

talk()        { echo 1>&2 "$@" ; }
warn()        { talk "$@" ; }
vtalk()	      { [[ -n "$norun$verbose" ]] && talk "$@" ; }
nvtalk()      { [[ -z "$verbose" ]]       && talk "$@" ; }
nqtalk()      { [[ -z "$quiet" ]]         && talk "$@" ; }

# error [CODE] MSG - show MSG on STDERR then exit with error CODE [default 2]

error()       { 
  local code=2
  case "$1" in [0-9]*) code=$1 ; shift ;; esac
  talk "$@"
  exit $code
}

#   talkf FMT ARGS...   printf FMT ARGS on STDERR
#   warnf FMT ARGS...   alias for talkf
#  vtalkf FMT ARGS..    printf FMT ARGS on STDERR if $norun or $verbose are set
# nvtalkf FMT ARGS..    printf FMT ARGS on STDERR unless $verbose set

talkf()       { printf 1>&2 "$@" ; }
warnf()       { talkf "$@" ; }
vtalkf()      { [[ -n "$norun$verbose" ]] && talkf "$@" ; }
nvtalkf()     { [[ -z "$verbose" ]]       && talkf "$@" ; }
nqtalkf()     { [[ -z "$quiet" ]]         && talkf "$@" ; }

# errorf [CODE] FMT ARGS .. print FMT ARGS on STDERR, then exit with CODE[2]

errorf()      { 
  local code=2
  case "$1" in [0-9]*) code=$1 ; shift ;; esac
  talkf "$@"
  exit $code
}

# die "Error message"
# dief FMT ARGS .. 
#
# These functions are designed to be used within other bash scripts.  Simply
# exiting with an error code is not sufficient because many bash scripts don't
# have very good exception handling.  So.. our "die" function prints its error
# message on STDERR, and then causes the current process to abort.

die() { dief "%s\n" "$@" ; }

dief() {
  talkf "$@"
  talk "Call stack:"
  for ((i=1; i<${#BASH_SOURCE[@]}; i++)); do
    case "${FUNCNAME[$i]}" in die|dief) continue ;; esac
    talkf "%s <%s:%s>\n" "${FUNCNAME[$i]}" "${BASH_SOURCE[$i]}"  "${BASH_LINENO[$i-1]}"
  done
  kill -ABRT $$ 
  exit 2
}

# rm_file_later FILE
#
# schedule file for later removal on program exit

rm_file_later() {
  local rmfiles="/tmp/rmfiles-$$.sh"
  touch $rmfiles
  ( fgrep -v $rmfiles $rmfiles ; echo "/bin/rm -f '$1'" ; echo "/bin/rm -f $rmfiles" ) > $rmfiles.new
  mv -f $rmfiles.new $rmfiles
  add_trap "/bin/sh $rmfiles"  EXIT HUP INT TERM
  add_trap "exit"              EXIT HUP INT TERM
}

# add_trap "Command" SIGNAL ..

add_trap() {
  local cmd="$1" ; shift
  for sig in "$@" ; do
    traps="`trap -p $sig | tr ';' '\n' | grep -v \"$cmd\" | tr '\n' ' ; '`"
    if [[ -n "$traps" ]]; then
      trap "$traps ; $cmd" $sig
    else
      trap "$cmd" $sig
    fi
  done
}

# run COMMAND ARGS ...

run() {
  if [[ $norun ]]; then
    talk "(norun) $@"
  else
    safe_run "$@"
  fi
  return 0
}

# safe_run COMMAND ARGS
# Safe run -- run command even in "$norun" mode

safe_run() {
  if [[ -n "$verbose$norun" ]]; then
    talk ">> $@"
  fi
  if ! eval "$@" ; then
    code=$?
    return $code
  fi
  return 0
}

# fn_exists FUNCTION
#
# Return 0 (true) or 1 (false) if FUNCTION is defined.

fn_exists() { declare -f "$1" >/dev/null ; }

# The following functions, "numarg_or_input", "arg_for_input", and
# "args_or_input" enable bash functions using them to flexibly accept an
# argument, or arguments, on their call, or on STDIN.  
#
# For example, let's say we have two bash functions to convert Celsius to
# Fareigheit and vice-versa.  Let's call them "c2f" and "f2c".  With these
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
#

# local arg=`numarg_or_input $1`
#
# Return the numeric argument or read from stdin

numarg_or_input() {
  local -i arg
  if [[ $# -eq 0 || -z "$1" ]] ; then
    local func="${FUNCNAME[1]}"
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
_numarg_or_input() { numarg_or_input "$1" ; }

arg_or_input() {
  local arg
  if [[ $# -eq 0 || -z "$1" ]]; then
    local func="${FUNCNAME[1]}"
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
_arg_or_input() { arg_or_input "$1" ; }


# local args=( `args_or_input "$@"` )
#
# Return the arguments or read input

args_or_input() {
  local -a args
  if (( $# == 0 )) ; then
    local func="${FUNCNAME[1]}"
    while (( ${#args[*]} == 0 )); do
      read -p "$func? " -a args
    done
  else
    args=( "$@" )
  fi
  echo "${args[@]}"
}
_args_or_input() { args_or_input "$@" ; }

# end of sh-utils.sh
# vim: sw=2 ai
