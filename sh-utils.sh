# sh-utils.sh
#
# handy functions for writing bash-based scripts
#
# Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>
#
#
# chat MSG ..
#
#   If $norun or $verbose is set, print all args on STDERR.
#
# talk MSG ..
#
#   Print all arguments on STDERR.
#
# nvtalk MSG 
#
#   Print all arguments on STDERR only if $verbose is not set.
#
# error [CODE] "MSG" 
#
#   Print MSG on STDERR, then exit with code CODE (or 2)
#
# run COMMAND ARGS ..
# 
# If $verbose is set, show the command and args before running it.
# if $norun is not set, run the command with args and examine the resulting status.
#
#
# rm_file_later FILE
#
#   Add FILE to a list of files that will be automatically removed
#   upon program exit.
#
# add_trap "CMD" SIGNAL ..
#
#   Add CMD to the trap list for SIGNAL, while ensuring that it is
#   not repeated.

if [[ "$SH_UTILS" != 'true' ]]; then
  SH_UTILS=true

  chat()	{ if [[ -n "$norun$verbose" ]]; then echo 1>&2 "$@" ; fi ; }
  talk()        { echo 1>&2 "$@" ; }
  nvtalk()      { if [[ -z "$verbose" ]]; then echo 1>&2 "$@" ; fi ; }

  error()       { 
    local code=2
    case "$1" in [0-9]*) code=$1 ; shift ;; esac
    echo 1>&2 "$@"
    exit $code
  }

  chatf()       { if [[ -n "$norun$verbose" ]]; then printf 1>&2 "$@" ; fi ; }
  talkf()       { printf 1>&2 "$@" ; }
  nvtalkf()     { if [[ -z "$verbose" ]]; then printf 1>&2 "$@" ; fi ; }

  errorf()      { 
    local code=2
    case "$1" in [0-9]*) code=$1 ; shift ;; esac
    printf 1>&2 "$@"
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
      if [[ $verbose ]]; then
        talk ">> $@"
      fi
      if ! eval "$@" ; then
        local code=$?
        return $code
      fi
    fi
    return 0
  }

fi

# end of sh-utils.sh
# vim: sw=2 ai
