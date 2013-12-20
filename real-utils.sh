#!/bin/bash
# real-utils.sh
#
# Copyright 2013 Alan K. Stebbens <aks@stebbens.org>
#
# enable real number arithmetic in bash scripts
#
#  real_eval "EXPRESSION" [SCALE]
#
#    EXPRESSION is a real number expression using operators as described
#    is the "bc" manual.
#
#  real_cond "EXPRESSION" [SCALE]
#
#    EXPRESSION is a real number conditional which should evaluate to 1 or 0
#
# real_scale=NUM
#
#    Set the precision of real number arithmetic results.   The default is 2.
#
# Default scale used by real functions.
real_scale=2

# result=$(( real_eval 'EXPRESSION' [SCALE] ))
# $? == 0 => bad calculation

function real_eval()
{
  local stat=0 res=0.0 scale="${2:-$real_scale}"
  if (( $# > 0 )) ; then
    res=`echo "scale=$scale; $1" | bc -q 2>/dev/null`
    stat=$?
    if [[ $stat -eq 0 && -z "$result" ]]; then stat=1; fi
  fi
  echo $res
  return $stat
}

# real_cond CONDITION [SCALE]
#
#  if real_cond "10.1 > 9.3" 1
#    ...
#  fi

function real_cond()
{
  local cond=0 scale=${2:-$real_scale}
  if (( $# > 0 )); then
    cond=`echo "scale=$scale; $1" | bc -q 2>/dev/null`
    if [[ -z "$cond" ]]; then cond=0; fi
    if [[ "$cond" != 0  && "$cond" != 1 ]]; then cond=0; fi
  fi
  local stat=$((cond == 0))
  return $stat
}

# vim: sw=2: ai:
