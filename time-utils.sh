# bash 
# time-utils.sh  -- time management utility for bash
#
# Copyright 2014-2022  Alan K. Stebbens <aks@stebbens.org>

TIME_UTILS_VERSION="time-utils.sh v1.8"
[[ "$TIME_UTILS_SH" = "$TIME_UTILS_VERSION" ]] && return
TIME_UTILS_SH="$TIME_UTILS_VERSION"

export TIME_FORMAT="%T"

source help-util.sh
source talk-utils.sh

help_time() {
  help_pager 1>&2 <<'EOF'
The `time-utils` library enables easy management of timestamps, with hour,
minute, seconds, and timezone components.  A variety of time formats are
supported both on input parsing, and on output formatting. 

    time_parse [TIMESTRING]
    time_arg   [TIMESTRING]

Parse TIMESTRING in one of several recognized formats: `HH:MM:SS`,
`HH:MM:SS.ssss`, If the TIMESTRING is successfully parsed, the variables
`hours`, `mins`, and `secs` are set the corresponding numbers.  `time_arg` is
another name for the same function.

If TIMESTRING is empty, a line of input from STDIN is read and used instead.

    time2secs [TIMESTRING]

Parse TIMESTRING (or STDIN) and convert to seconds.

    time_format [FORMAT] HOURS MINS SECS
    time_format [FORMAT] TIMESTRING

The `time_format` function accepts an optional format string, followed by three
numeric arguments, for the hour, minutes, and seconds, or a single string
argument in any of the parsable date formats, and reformats into the default
time format, given by TIME_FORMAT.  If TIME_FORMAT is not defined, the format
`%T` is used.  (See `man strftime` for details).

    time_add TIME1 TIME2

Add TIME1 to TIME2 and produce a `time_format` result.

    time_sub TIME1 TIME2

Subtract TIME2 from TIME1 and produce a `time_format` result.

EOF

}
time_help() { help_time ; }

## FIXME -- maybe don't need this
## source real-utils.sh

# functions for time management
#
# time_parse HH:MM:SS
#            HH:MM:SS.SSS
#            HH MM SS
#            HH MM SS SSS

time_parse() {
  case ${#@} in
    0|1) time_parse_str  "$1" ;;
    2|3) time_parse_hms  "$@" ;;
    4)   time_parse_hmss "$@" ;;
  esac
}

# time_parse_str TIMESTRING
#
# parse the TIMESTRING.  It can be in one of several formats: 
# HH:MM:SS
# HH:MM:SS.sss
# HH:MM         ( could also be HH:SS, but his is less common )
#
# Sets the variables: hours, mins, secs, unless there was an error.

time_parse_str() {
  local time="${1:-`date +%T`}"
  hours= mins= secs= msecs=

  # HH:MM:SS.sss
  if [[ "$time" =~ ([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})\.([0-9]{1,3}) ]]; then # hh:mm:ss
    time_parse_hmss ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]} ${BASH_REMATCH[4]}
  elif [[ "$time" =~ ([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2}) ]]; then # hh:mm:ss
    time_parse_hms ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]}
  elif [[ "$time" =~ ([0-9]{1,2}):([0-9]{1,2}) ]]; then # hh:mm
    time_parse_hms ${BASH_REMATCH[1]} ${BASH_REMATCH[2]}
  else  # failure -- leave no variables set
    error "Cannot parse: '$1'; unknown time format"
  fi
}

# time_parse_hms   HH MM SS
# time_parse_hmss  HH MM SS SSS
# Set three global variables 'hours', 'mins', 'secs', and maybe 'msecs'

# checks for appropriate time values.

check_parsed_time_values() {
  (( hours >= 0 && hours < 24 ))   || error "Bad hours value: $hours"
  (( mins  >= 0 && mins  < 60 ))   || error "Bad minutes value: $mins"
  (( secs  >= 0 && secs  < 60 ))   || error "Bad seconds value: $secs"
  (( msecs >= 0 && msecs < 1000 )) || error "Bad milleseconds value: $msecs"
}

time_parse_hms() {
  hours=$(( 10#$1 ))  mins=$(( 10#${2:-0} )) secs=$(( 10#${3:-0} )) msecs=0
  check_parsed_time_values
}

time_parse_hmss() {
  hours=$(( 10#$1 ))  mins=$(( 10#${2:-0} )) secs=$(( 10#${3:-0} )) msecs=$(( 10#${4:-0} ))
  check_parsed_time_values
}

# time2secs TIMESTRING
# time2secs HH MM SS

time2secs() {
  local hours= mins= secs=
  time_parse $1
  echo $(( $secs + ( 60 * ( $mins + ( 60 * $hours ) ) ) ))
}

# time_add TIME1 TIME2

time_add() {
  local t1=`time2secs $1`
  local t2=`time2secs $2`
  local t3=$(( t1 + t2 ))
  time_format $t3
}

# time_sub TIME1 TIME2

time_sub() {
  local t1=`time2secs $1`
  local t2=`time2secs $2`
  if (( t1 > t2 )) ; then
    time_format $(( t1 - t2 ))
  else
    time_format $(( t2 - t1 ))
  fi
}

# time_format SECONDS
#
# If seconds exceeds 24 hours in value, a days value will be included:
# DD:HH:MM:SS. Otherwise, the result is HH:MM:SS

time_format() {
  local format secs_in
  if (( $# > 1 )); then
    format="$1" secs_in="$2"
  else
    format="%T" secs_in="$1"
  fi
  local days hours mins secs
  secs2hms $secs_in
  if (( days > 0 )); then
    printf "%d:%02d:%02d:%02d\n" "$days" "$hours" "$mins" "$secs"
  else
    printf "%02d:%02d:%02d\n" "$hours" "$mins" "$secs"
  fi
}

# convert seconds to HMS values 
# sets days, hours, mins secs

secs2hms() {
  days= hours= mins= secs="$1"
  if (( secs >= 60 )); then
    mins=$(( secs / 60 ))
    secs=$(( secs % 60 ))
  fi
  if (( mins >= 60 )); then
    hours=$(( mins / 60 ))
    mins=$((  mins % 60 ))
  fi
  if (( hours >= 24 )); then
    days=$((  hours / 24 ))
    hours=$(( hours % 24 ))
  fi
}


# end of time-utils.sh
# vim: set ai sw=2
