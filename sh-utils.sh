# sh-utils.sh
#
# handy functions for writing bash-based scripts
#
# Copyright 2006-2022 Alan K. Stebbens <aks@stebbens.org>
#

SH_UTILS_VERSION="sh-utils.sh v2.2"

[[ "$SH_UTILS_SH" = "$SH_UTILS_VERSION" ]] && return
SH_UTILS_SH="$SH_UTILS_VERSION"

# Need to bring these in with sh-utils -- because I have many
# scripts that were developed before the refactoring.

source help-util.sh
source talk-utils.sh
source run-utils.sh
source option-utils.sh

help_sh_utils() {
    help_pager <<'EOF'
The `sh-utils.sh` includes several groups of functions which collectively are
quite useful in the development of command-line utilities and other system
scripts.

The following are separate modules that are included with sh-utils:

- arg-utils     - help with arguments or STDIN
- help-util     - help with help on selected functions
- option-utils  - manage option and argument lists
- run-utils     - run system commands, with $norun and $verbose
- talk-utils    - conditional output to STDERR

In addition, `sh-utils.sh` defines some additional functions:

rm_file_later FILE          Cause `FILE` to be removed upon program exit.

add_trap "CMD" SIGNAL ..    Add `CMD` to the trap list for `SIGNAL`

rm_trap "CMD" SIGNAL ..     Remove 'CMD' from the trap list for `SIGNAL`

get_traps SIGNAL            Output (on STDOUT) the trap commands for SIGNAL

filter_traps CMD            Filter CMD out of the newline-separated commands on STDIN

reset_traps SIGNAL ..       Reset (remove) all traps on `SIGNAL`

fn_exists FUNCTION          Return 0 (true) if `FUNCTION` exists; 1 (false) otherwise

EOF
}

sh_utils_help() { help_sh_utils ; }

# rm_file_later FILE
#
# schedule file for later removal on program exit

rm_file_later() {
  help_args_func help_sh_utils $# 1 || return 1
  __rm_file_later "$@"
}

__rm_file_later() {
  local rmfiles="/tmp/rmfiles-$$.sh"
  touch $rmfiles
  ( fgrep -v $rmfiles $rmfiles ; echo "/bin/rm -f '$1'" ; echo "/bin/rm -f $rmfiles" ) > $rmfiles.new
  mv -f $rmfiles.new $rmfiles
  add_trap "/bin/sh $rmfiles"  EXIT HUP INT TERM
  add_trap "exit"              EXIT HUP INT TERM
}

# add_trap "Command" SIGNAL ..

add_trap() {
  help_args_func help_sh_utils $# 2 || return 1
  __add_trap "$@"
}

__add_trap() {
  local cmd="$1" ; shift
  local sig traps
  for sig in "$@" ; do
    traps="`get_traps $sig | filter_traps \"$cmd\"`"
    if [[ -n "$traps" ]]; then
      trap "$traps ; $cmd" $sig
    else
      trap "$cmd" $sig
    fi
  done
}

# rm_trap "CMD" SIGNAL

rm_trap() {
  help_args_func help_sh_utils $# 2 || return 1
  __rm_trap "$@"
}

__rm_trap() {
  local cmd="$1" ; shift
  local sig traps
  for sig in "$@" ; do
    traps="`__get_traps $sig | __filter_traps \"$cmd\"`"
    if [[ -n "$traps" ]]; then
      trap "$cmd" $sig
    else
      trap - $sig
    fi
  done
  trap -p $sig 1>&2
}

# echo "some traps" | filter_traps CMD

filter_traps() {
  help_args_func help_sh_utils $# 1 || return 1
  __filter_traps "$@"
}

__filter_traps() {
  local cmd="$1" ; shift
  fgrep -v "$cmd" | sed -e "s/$'\n'/ ; /g"
}

# reset_traps SIG

reset_traps() { 
  help_args_func help_sh_utils $# 1 || return 1
  __reset_traps "$@"
}

__reset_traps() {
  local sig
  for sig in "$@" ; do
    trap - $sig
  done
}

# get_traps SIGNAL

get_traps() {
  help_args_func help_sh_utils $# 1 || return 1
  __get_traps "$@"
}

__get_traps() {
  trap -p $1 | sed -E "s/^trap -- '([^']*)' SIG.*$/\1/"
}

# fn_exists FUNCTION
#
# Return 0 (true) or 1 (false) if FUNCTION is defined.

fn_exists() { declare -f "$1" >/dev/null ; }


# end of sh-utils.sh
# vim: sw=2 ai
