# talk-utils.sh
#
# handy output functions for writing bash-based scripts
#
# Copyright 2006-2015 Alan K. Stebbens <aks@stebbens.org>
#

TALK_UTILS_VERSION="talk-utils.sh v1.8"

[[ "$TALK_UTILS_SH" = "$TALK_UTILS_VERSION" ]] && return
TALK_UTILS_SH="$TALK_UTILS_VERSION"

source help-util.sh

talk_help() {
    help_pager <<'EOF'
Shell output utility functions:

The `talk`, `error`, and `die` functions print their arguments on STDERR.  The
`talk` and `talkf` functions print unconditionally to STDERR, and return
success (0).  The related functions with prefixes of 'v', 'vo', 'nr', 'nv',
'nq' print conditionally and return success (0) if they printed, and failure
(1) otherwise.  This allows them to be used on conditionals.

The `warn` function is just another name for `talk`: it prints its output on
STDERR.  The `error` function does the same, accepting an optional error CODE,
and then exits.  The `die` function sends a `SIGABRT` signal to the parent
process id, forcing an abort.

   talk MSG ..              Print all args on `STDERR`
  vtalk MSG ..              If `$norun` or `$verbose` is set, print all args.
 votalk MSG ..              If `$verbose` only (no `$norun`) is set, print all args.
 nrtalk MSG ..              If `$norun` set, print all args
 nvtalk MSG ..              Unless `$verbose` is set, print all args
 nqtalk MSG ..              Unless `$quiet` is set, print all args
nrvtalk MSG ..              If `$norun` or `$verbose`, print all args

   talkf FMT ARGS ..        Printf `FMT` `ARGS`
  vtalkf FMT ARGS ..        If `$norun` or `$verbose` set, printf `FMT, `ARGS`
 votalkf FMT ARGS ..        If `$verbose` only (no `$norun`) is set, printf `FMT`, `ARGS`
 nrtalkf FMT ARGS ..        If `$norun` set, printf `FMT`, `ARGS`
 nvtalkf FMT ARGS ..        Unless `$verbose` is set, printf `FMT` `ARGS`
 nqtalkf FMT ARGS ..        Unless `$quiet` is set, printf `FMT` `ARGS`
nrvtalkf FMT ARGS ..        If `$norun` or `$verbose`, printf `FMT` `ARGS` 

   warn        MSG          Print all args on `STDERR`
  error [CODE] MSG          Print `MSG` on `STDERR`, then exit with code `CODE` (or 2)
    die        MSG          Print `MSG` on `STDERR`, then die (with `kill -ABRT`)

  warnf        FMT ARGS ..  Printf `FMT` `ARGS` on `STDERR`
 errorf [CODE] FMT ARGS ..  Printf `FMT` `ARGS` on `STDERR`, then exit `$CODE` [2]
   dief        FMT ARGS ..  Printf `FMT` `ARGS` on `STDERR`, then die (with `kill -ABRT`)

EOF
}

talk_utils_help() { talk_help ; }
help_talk()       { talk_help ; }
help_talk_utils() { talk_help ; }

# All output goes to STDERR
#
#    talk MSG                                   show MSG
#    warn MSG      alias for talk
#   vtalk MSG      if $verbose or $norun,       show MSG
#  votalk MSG      if $verbose only (no $norun) show MSG
#  nrtalk MSG      if $norun,                   show MSG
#  nvtalk MSG      unless $verbose              show MSG
#  nqtalk MSG      unless $quiet                show MSG
# nrvtalk MSG      if $verbose or $norun        show MSG

talk()      { help_args_func talk_help $# 1 || return 1 ; __talk    "$@" ; }
warn()      { help_args_func talk_help $# 1 || return 1 ; __warn    "$@" ; }
vtalk()     { help_args_func talk_help $# 1 || return 1 ; __vtalk   "$@" ; }
votalk()    { help_args_func talk_help $# 1 || return 1 ; __votalk  "$@" ; }
nrtalk()    { help_args_func talk_help $# 1 || return 1 ; __nrtalk  "$@" ; }
nvtalk()    { help_args_func talk_help $# 1 || return 1 ; __nvtalk  "$@" ; }
nqtalk()    { help_args_func talk_help $# 1 || return 1 ; __nqtalk  "$@" ; }
nrvtalk()   { help_args_func talk_help $# 1 || return 1 ; __nrvtalk "$@" ; }
error()     { help_args_func talk_help $# 1 || return 1 ; __error   "$@" ; }

__talk()    { echo 1>&2 "$@" ; return 0 ;}
__warn()    {                                       __talk "$@"             ; }
__vtalk()   { [[ -n "$norun$verbose" ]]          && __talk "$@" || return 1 ; }
__votalk()  { [[ -n "$verbose" && -z "$norun" ]] && __talk "$@" || return 1 ; }
__nrtalk()  { [[ -n "$norun" ]]                  && __talk "$@" || return 1 ; }
__nvtalk()  { [[ -z "$verbose" ]]                && __talk "$@" || return 1 ; }
__nqtalk()  { [[ -z "$quiet" ]]                  && __talk "$@" || return 1 ; }
__nrvtalk() { [[ -n "$verbose$norun" ]]          && __talk "$@" || return 1 ; }

# error [CODE] MSG - show MSG on STDERR then exit with error CODE [default 2]

__error() {
  local code=2
  case "$1" in [0-9]*) code=$1 ; shift ;; esac
  talk "$@"
  exit $code
}

#    talkf FMT ARGS..                                 printf FMT ARGS
#    warnf FMT ARGS..    alias for talkf
#   vtalkf FMT ARGS..    if $norun or $verbose set,   printf FMT ARGS
#  votalkf FMT ARGS..    if $verbose & unless $norun, printf FMT ARGS
#  nrtalkf FMT ARGS..    if $norun,                   printf FMT ARGS
#  nvtalkf FMT ARGS..    unless $verbose is set,      printf FMT ARGS
#  nqtalkf FMT ARGS..    unless $quiet is set,        printf FMT ARGS
# nrvtalkf FMT ARGS..    if $verbose or $norun,       printf FMT ARGS

talkf()      { help_args_func talk_help $# 1 || return 1 ; __talkf    "$@" ; }
warnf()      { help_args_func talk_help $# 1 || return 1 ; __warnf    "$@" ; }
vtalkf()     { help_args_func talk_help $# 1 || return 1 ; __vtalkf   "$@" ; }
votalkf()    { help_args_func talk_help $# 1 || return 1 ; __votalkf  "$@" ; }
nrtalkf()    { help_args_func talk_help $# 1 || return 1 ; __nrtalkf  "$@" ; }
nvtalkf()    { help_args_func talk_help $# 1 || return 1 ; __nvtalkf  "$@" ; }
nqtalkf()    { help_args_func talk_help $# 1 || return 1 ; __nqtalkf  "$@" ; }
nrvtalkf()   { help_args_func talk_help $# 1 || return 1 ; __nrvtalkf "$@" ; }
errorf()     { help_args_func talk_help $# 1 || return 1 ; __errorf   "$@" ; }

__talkf()    { printf 1>&2 "$@" ; return 0                                    ; }
__warnf()    {                                       __talkf "$@"             ; }
__vtalkf()   { [[ -n "$norun$verbose" ]]          && __talkf "$@" || return 1 ; }
__votalkf()  { [[ -n "$verbose" && -z "$norun" ]] && __talkf "$@" || return 1 ; }
__nrtalkf()  { [[ -n "$norun" ]]                  && __talkf "$@" || return 1 ; }
__nvtalkf()  { [[ -z "$verbose" ]]                && __talkf "$@" || return 1 ; }
__nqtalkf()  { [[ -z "$quiet" ]]                  && __talkf "$@" || return 1 ; }
__nrvtalkf() { [[ -n "$verbose$norun" ]]          && __talkf "$@" || return 1 ; }

# errorf [CODE] FMT ARGS .. print FMT ARGS on STDERR, then exit with CODE[2]

__errorf()      {
  local code=2
  case "$1" in [0-9]*) code=$1 ; shift ;; esac
  __talkf "$@"
  exit $code
}

# die "Error message"
# dief FMT ARGS ..
#
# These functions are designed to be used within other bash scripts.  Simply
# exiting with an error code is not sufficient because many bash scripts don't
# have very good exception handling.  So.. our "die" function prints its error
# message on STDERR, and then causes the current process to abort.

die()   { help_args_func talk_help $# 1 || return ; __die  "$@" ; }
dief()  { help_args_func talk_help $# 1 || return ; __dief "$@" ; }

__die() { __dief "%s\n" "$@" ; }

__dief() {
  talkf "$@"
  talk "Call stack:"
  local _i
  for ((_i=1; _i<${#BASH_SOURCE[@]}; _i++)); do
    case "${FUNCNAME[$_i]}" in die|dief|__die|__dief) continue ;; esac
    __talkf "%s <%s:%s>\n" "${FUNCNAME[$_i]}" "${BASH_SOURCE[$_i]}"  "${BASH_LINENO[$_i-1]}"
  done
  kill -ABRT $$
  exit 2
}

# end of talk-utils.sh
# vim: sw=2 ai
