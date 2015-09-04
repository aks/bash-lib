# sh-utils.sh
#
# handy functions for writing bash-based scripts
#
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>
#

SH_UTILS_VERSION="sh-utils.sh v2.2"

[[ "$SH_UTILS_SH" = "$SH_UTILS_VERSION" ]] && return
SH_UTILS_SH="$SH_UTILS_VERSION"

# Need to bring these in with sh-utils -- because I have many
# scripts that were developed before the refactoring.

source talk-utils.sh
source run-utils.sh
source option-utils.sh

sh_utils_help() {
    cat 1>&2 <<'EOF'
The shell command utility functions consist of several groups
of functions which collectively are quite useful in development
command-line utilities and other system scripts.

The following are separate modules that are included with sh-utils:

- arg-utils     - help with arguments or STDIN
- help-util     - help with help on selected functions
- option-utils  - manage option and argument lists
- run-utils     - run system commands, with $norun and $verbose 
- talk-utils    - conditional output to STDERR

These are some miscellaneous functions:

rm_file_later FILE          Cause `FILE` to be removed upon program exit.

add_trap "CMD" SIGNAL ..    Add `CMD` to the trap list for `SIGNAL`

fn_exists FUNCTION          Return 0 (true) if `FUNCTION` exists; 1 (false) otherwise

EOF
}
help_sh_utils() { sh_utils_help ; }

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

# fn_exists FUNCTION
#
# Return 0 (true) or 1 (false) if FUNCTION is defined.

fn_exists() { declare -f "$1" >/dev/null ; }


# end of sh-utils.sh
# vim: sw=2 ai
