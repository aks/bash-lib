# sh-utils.sh
#
# handy functions for writing bash-based scripts
#
# Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>
#
#

[[ -z "$SH_UTILS" ]] || return

export SH_UTILS="${BASH_SOURCE[0]}"

sh_utils_help() {
    cat 1>&2 <<EOF
Shell utility functions:

talk MSG ..         Print all arguments on STDERR.
chat MSG ..         If $norun or $verbose is set, print all args on STDERR.
nvtalk MSG          Print all arguments on STDERR only if $verbose is not set.
error [CODE] "MSG"  Print MSG on STDERR, then exit with code CODE (or 2)

talkf FMT ARGS ..           printf FMT ARGS on STDERR
chatf FMT ARGS ..           printf FMT ARGS on STDERR if $norun or $verbose set
nvtalkf FMT ARGS ..         printf FMT ARGS on STDERR unless $verbose set
errorf [CODE] FMT ARGS ..   printf FMT ARGS on STDERR, then exit $CODE [2]

run COMMAND ARGS .. Show COMMAND ARGS if $norun or $verbase; run command unless $norun.

rm_file_later FILE  Cause FILE to be removed upon program exit.

add_trap "CMD" SIGNAL ..   Add CMD to the trap list for SIGNAL

fn_exists FUNCTION         return 0 (true) if FUNCTION exists; 1 (false) otherwise

EOF
}
help_sh_utils() { sh_utils_help ; }

# chat MSG        - show MSG on STDERR if $norun or $verbose
# talk MSG        - show MSG on STDERR
# warn MSG        - show MSG on STDERR
# nvtalk MSG      - show MSG on STDERR unless $verbose is set

talk()        { echo 1>&2 "$@" ; }
warn()        { talk "$@" ; }
chat()	{ [[ -n "$norun$verbose" ]] && talk "$@" ; }
nvtalk()      { [[ -z "$verbose" ]] && talk "$@" ; }

# error [CODE] MSG - show MSG on STDERR then exit with error CODE [default 2]

error()       { 
  local code=2
  case "$1" in [0-9]*) code=$1 ; shift ;; esac
  talk "$@"
  exit $code
}

# chatf FMT ARGS..    printf FMT ARGS on STDERR if $norun or $verbose are set
# talkf FMT ARGS...   printf FMT ARGS on STDERR
# warnf FMT ARGS...   alias for talkf
# nvtalkf FMT ARGS..  printf FMT ARGS on STDERR unless $verbose set

talkf()       { printf 1>&2 "$@" ; }
warnf()       { talkf "$@" ; }
chatf()       { [[ -n "$norun$verbose" ]] && talkf "$@" ; }
nvtalkf()     { [[ -z "$verbose" ]] && talkf "$@" ; }

# errorf [CODE] FMT ARGS .. print FMT ARGS on STDERR, then exit with CODE[2]

errorf()      { 
  local code=2
  case "$1" in [0-9]*) code=$1 ; shift ;; esac
  talkf "$@"
  exit $code
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
fn_exists() { declare -f "$1" >/dev/null ; }

# end of sh-utils.sh
# vim: sw=2 ai
