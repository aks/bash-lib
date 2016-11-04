# run-utils.sh
#
# handy functions for running system commands in bash scripts, with
# optional support for $norun and $verbose modes.
#
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>
#

RUN_UTILS_VERSION="sh-utils.sh v1.8"

[[ "$RUN_UTILS_SH" = "$RUN_UTILS_VERSION" ]] && return
RUN_UTILS_SH="$RUN_UTILS_VERSION"

source help-util.sh

run_utils_help() {
    cat 1>&2 <<'EOF'
Shell utility functions for running system commands:

run COMMAND ARGS ..         Show `COMMAND` `ARGS` if `$norun` or `$verbose`;
                            run `COMMAND` unless `$norun`.

safe_run COMMAND ARGS ...   Same as "run", but always executes.

rm_file_later FILE          Cause `FILE` to be removed upon program exit.

add_trap "CMD" SIGNAL ..    Add `CMD` to the trap list for `SIGNAL`

EOF
}
help_run_utils() { run_utils_help ; }

# rm_file_later FILE
#
# schedule file for later removal on program exit

rm_file_later() {
  help_args_func run_utils_help $# || return 1
  local rmfiles="/tmp/rmfiles-$$.sh"
  local lockfile=$rmfiles.lock
  touch $rmfiles
  lockfile $lockfile      # lock the semaphore
  ( fgrep -v $rmfiles $rmfiles ; echo "/bin/rm -f '$1'" ; echo "/bin/rm -f $rmfiles" ) > $rmfiles.new
  mv -f $rmfiles.new $rmfiles
  rm -f $lockfile         # release the semaphore
  add_trap "/bin/sh $rmfiles"  EXIT HUP INT TERM
  add_trap "exit"              EXIT HUP INT TERM
}

# add_trap "Command" SIGNAL ..

# add_trap CMD [SIGNAL ...]
add_trap() {
  local cmd="$1" ; shift
  local sig cmds
  for sig in "$@" ; do
    cmds=`trap_cmd $sig`
    cmds="$cmds${cmds:+; }$cmd"
    trap "$cmds" $sig
  done
}

# trap_cmd SIGNAL
trap_cmd() {
  local cmds=`trap -p $1`
  cmds="${cmds#*\'}"
  cmds="${cmds%\'*}"
  echo "$cmds"
}

# run COMMAND ARGS ...

run() {
  help_args_func run_utils_help $# 1 || return 1
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
  help_args_func run_utils_help $# 1 || return 1
  if [[ -n "$verbose$norun" ]]; then
    talk ">> $@"
  fi
  if ! eval "$@" ; then
    code=$?
    return $code
  fi
  return 0
}

# end of run-utils.sh
# vim: sw=2 ai
