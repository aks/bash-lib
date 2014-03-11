#!/bin/bash
# real-utils.sh
#
# Copyright 2014 Alan K. Stebbens <aks@stebbens.org>

# Don't read ourselves multiple times

REAL_UTILS_VERSION="real-utils.sh v1.1"

[[ "$REAL_UTILS_VERSION" = "$REAL_UTILS" ]] && return 0

export REAL_UTILS="$REAL_UTILS_VERSION"

real_help() {
  cat <<'EOF'
real-utils.sh is a bash library that enables real number arithmetic in bash
scripts.  Real numbers are managed as flaoting point strings in the format
"X.Y", where X is the integer portion, and "Y" is the fractional part.

Usage:

   source real-utils.sh

   real_eval "EXPRESSION" [SCALE]
   real_cond EXPRESSION [SCALE]
   real_int REAL
   real_frac REAL

Function Descriptions:

real_eval "EXPRESSION" [SCALE]

    The `real_eval` bash function evaluates EXPRESSION using syntax, operators and
    functions as described is the "bc" manual.  All numbers and variables within
    EXPRESSION are interpreted by `bc`.  If $? > 0, an error occured.

    In addition to the operators and functions defined by `bc`, the following
    additional functions are also made available:

    abs(x)           deg(x)           log10(x)         rad(x)
    acos(x)          exp(x)           logn(x)          round(x,s)
    asin(x)          frac(x)          ndeg(x)          sin(x)
    atan(x)          int(x)           pi()             tan(x)
    cos(x)           log(x)           pow(x,y)

real_cond "EXPRESSION" [SCALE]

    EXPRESSION is a real number conditional which should evaluate to 1 or 0.  The
    return status is 0 for true, 1 for false.

real_scale=NUM

    Set the precision of subsequent real number arithmetic results.   The
    default is 2.

real_int  REAL          -- outputs the integer portion of a REAL number
real_frac  REAL         -- outputs the fractional portion of a REAL number

sin R, cos R, tan R     -- trig functions on radians R
asin X, acos X, atan X  -- inverse trig functions
cotan X, sec X, cosec X -- cotangent, secant, cosecant
arccot X                -- arc-cotangent
hypot X Y               -- hypotenuse X, Y [sqrt(X^2 + Y^2)]
sqrt X                  -- square-root of X
logn X, log X           -- natural log, log base 10
exp X                   -- exponent X of E (e.g., e^X)
pow X Y                 -- power function [X^Y]
rad D                   -- convert degrees D to radians
deg R                   -- convert radians R to degrees
ndeg R                  -- convert radians R to natural degrees (0..360)
round X S               -- Round X to S decimals.  When S=0, rounds to the nearest integer.
real_int X              -- outputs integer portion of X
real_frac X             -- outputs fractional portion of X
abs X                   -- Return the absolute value of X.

    PI   = 3.141592653589793
    TAU  = 6.283185307179586   # 2*PI
    E    = 2.718281828459045

EOF
}
help_real() { real_help ; }

# Default scale used by real functions.
[[ -n "$real_scale" ]] || export real_scale=2

# real_compute EXPR [SCALE]
#
# Basic computational engine, provides function definitions and evaluation, but
# does not do shell status or return code management.

real_compute() {
  ( cat <<EOF
  define pi()       { auto r,s ; s=scale; scale=10 ; r=4*a(1);         scale=s ; return(r) ; }
  define int(x)     { auto r,s ; s=scale; scale=0  ; r=((x - x%1)/1) ; scale=s ; return(r) ; }
  define frac(x)    { auto r,s ; s=scale; scale=0  ; r=(x%1) ;         scale=s ; return(r) ; }
  define sin(x)     { return(s(x))                     ; }
  define cos(x)     { return(c(x))                     ; }
  define tan(x)     { return(s(x)/c(x))                ; }
  define asin(x)    { return(2*a(x/(1+sqrt(1-(x^2))))) ; }
  define acos(x)    { return(2*a(sqrt(1-(x^2))/(1+x))) ; }
  define atan(x)    { return(a(x))                     ; }
  define logn(x)    { return(l(x))                     ; }
  define log(x)     { return(l(x)/l(10.0))             ; }
  define log10(x)   { return(log(x))                   ; }
  define exp(x)     { return(e(x))                     ; }
  define pow(x,y)   { return(x^y)                      ; }
  define rad(x)     { return(x*pi()/180)               ; }
  define deg(x)     { return(x*180/pi())               ; }
  define ndeg(x)    { return((360 + deg(x))%360)       ; }
  define round(x,s) { auto r,o
                      o=scale(x) ; scale=s+1
                      r = x + 5*10^(-(s+1))
                      scale=s
                      return(r/1)
                    }
  define abs(x)     { if (x<0) return(-x) else return(x) ; }
  scale=${2:-$real_scale}
  $1
EOF
  ) | bc -lq 2>/dev/null
}


# result=`real_eval 'EXPRESSION' [SCALE]`
#
# Performs evaluation of an arithmentic expression, supporting real numbers.
#
# $? == 1 => bad calculation

real_eval()
{
  local stat=0 res=0 scale="${2:-$real_scale}"
  if (( $# > 0 )) ; then
    res=`real_compute "$1" $scale`
    stat=$?
    if [[ $stat -eq 0 && -z "$res" ]]; then stat=1; fi
  fi
  echo $res
  return $stat
}

# real_cond CONDITION [SCALE]
#
# Test a conditional expression using real numbers.
#
#  if real_cond "10.1 > 9.3" 1
#    ...
#  fi

function real_cond()
{
  local cond=0 scale=${2:-$real_scale}
  if (( $# > 0 )); then
    cond=`real_compute "$1" $scale`
    if [[ -z "$cond" ]]; then cond=0; fi
    if [[ "$cond" != 0  && "$cond" != 1 ]]; then cond=0; fi
  fi
  local stat=$((cond == 0))
  return $stat
}

# _args_or_input  [ARG]
#
# Invoke FUNC with the ARGUMENTS or input from STDIN
#
# This is a utility function designed to allow functions to be called with
# arguments given on the command line, but if omitted, to be read from STDIN.

_args_or_input() {
  local func="${FUNCNAME[1]}"
  if (( $# > 0 )); then
    echo "$@"
  else
    local -a args
    while (( ${#args[*]} == 0 )) ; do
      read -p "$func? " -a args
    done
    echo "${args[@]}"
  fi
}

# _with_arg ARGS
#
# Take input and append ARGS to it and emit on stdout.

_with_arg() {
  local -a data
  read -a data
  echo "${data[@]}" "$@"
}

# real_int REAL   # return the integer part of a REAL
# real_frac REAL  # return the fractional part of a REAL
#
# These are simple text functions on the string representation of real numbers.

real_int()  { echo "${1%.*}"  ; }
real_frac() { echo ".${1#*.}" ; }


# Math functions
#
# All math functions operate with scale=8 unless overriden
#
# Some handy trig constants
PI='3.141592653589793'
TAU='6.283185307179586'   # 2*PI
E='2.718281828459045'

# Trig functions
#
# sin REAL [SCALE=8]
# cos REAL 
# tan REAL
#
# cotan REAL  - cotangent
# sec REAL    - secant
# csc REAL    - cosecant
# 
# arcsin REAL - arcsine aka "asin"
# arccos REAL - arcosine aka "acos"
# arctan REAL - arctan aka   "atan"
# 
# pi = 3.141592654
# tau = 2*pi

sin()    { set - `_args_or_input "$@"` ; real_eval "s($1)"         ${2:-8} ; }
cos()    { set - `_args_or_input "$@"` ; real_eval "c($1)"         ${2:-8} ; }
tan()    { set - `_args_or_input "$@"` ; real_eval "(s($1)/c($1))" ${2:-8} ; }

cotan()  { set - `_args_or_input "$@"` ; real_eval "(c($1)/s($1))" ${2:-8} ; }
sec()    { set - `_args_or_input "$@"` ; real_eval "(1/c($1))"     ${2:-8} ; }
cosec()  { set - `_args_or_input "$@"` ; real_eval "(1/s($1))"     ${2:-8} ; }
csc()    { set - `_args_or_input "$@"` ; cosec "$@" ; }

# hypot X Y [SCALE]
hypot()  { set - `_args_or_input "$@"` ; real_eval "sqrt(($1)^2 + ($2)^2)"  ${3:-8} ; }

# Inverse trig funcs
asin()   { set - `_args_or_input "$@"` ; real_eval "asin($1)"        ${2:-8} ; }
acos()   { set - `_args_or_input "$@"` ; real_eval "acos($1)"        ${2:-8} ; }
atan()   { set - `_args_or_input "$@"` ; real_eval "atan($1)"        ${2:-8} ; }

arccot() { set - `_args_or_input "$@"` ; real_eval "(($PI/2)-a($1))" ${2:-8} ; }
arcsin() { set - `_args_or_input "$@"` ; asin "$@" ; }
arccos() { set - `_args_or_input "$@"` ; acos "$@" ; }
arctan() { set - `_args_or_input "$@"` ; atag "$@" ; }

# Log functions
logn()   { set - `_args_or_input "$@"` ; real_eval "l($1)"          ${2:-8} ; }
log10()  { set - `_args_or_input "$@"` ; real_eval "l($1)/l(10.0)"  ${2:-8} ; }
log()    { set - `_args_or_input "$@"` ; log10 "$@" ; }
exp()    { set - `_args_or_input "$@"` ; real_eval "e($1)"          ${2:-8} ; }

# Power function
pow()    { set - `_args_or_input "$@"` ; real_eval "$1^$2"          ${2:-8} ; }

# rad x -- convert degrees to radians
# deg x -- convert radians to degrees
# ndeg x -- convert radians to normalized degrees (0 <= d <= 360)
#
# 1 rad == 180 deg / PI
# 1 deg == PI rad / 180

deg()    { set - `_args_or_input "$@"` ; real_eval "deg($1)"   ${2:-8} ; }
ndeg()   { set - `_args_or_input "$@"` ; real_eval "ndeg($1)"  ${2:-8} ; }
rad()    { set - `_args_or_input "$@"` ; real_eval "rad($1)"   ${2:-8} ; }

# absolute X
abs()    { set - `_args_or_input "$@"` ; real_eval "abs($1)"           ; }

# Round NUM [SCALE] -- round NUM at the SCALE

round()  { set - `_args_or_input "$@"` ; real_eval "round($1, ${2:-(scale($1)-1)})" ${2:-8} ; }


# vim: sw=2: ai:
