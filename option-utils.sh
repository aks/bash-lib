# option-utils.sh -- 
# Copyright 2015-2022 Alan K. Stebbens <aks@stebbens.org>

# utility for collecting lists of options and arguments

OPTION_UTILS_VERSION="option-utils.sh v1.1"
[[ "$OPTION_UTILS_SH" = "$OPTION_UTILS_VERSION" ]] && return
OPTION_UTILS_SH="$OPTION_UTILS_VERSIONS"

source help-util.sh

option_help() {
  cat 1>&2 <<'EOF'
The option-util.sh library is a small set of functions to manage
building options and arguments, which is often needed in the
development of command-line utilities.  

These functions use two global variables: `option_pairs` and
`options`.  The `option_pairs` variable is used to accumulate pairs
of options and arguments, eg: "-F FILE", while `options` is used to
accumulate single character options that can be clustered behind a
single dash "-".

All of the accumulated options and arguments can be output with
`all_opts`.

init_opts                # empty "option_pairs" and "options"

reset_opts               # same as init_opts

add_optarg OPTION ARG .. # add OPTION and ARG to the option_pairs list

add_option OPTION ..    # add OPTION to the single options list
add_opt    OPTION ..    # eg: add_arg -c -d ..  or add_arg c d ..

all_opts                 # outputs both option_pairs and options
EOF
}
help_option() { option_help ; }

init_options() {
  declare -g option_pairs=
  declare -g options='-'         # a list of clustered single options
}

reset_options() { init_options ; }

add_optarg() {
  help_args_func option_help $# 2 || return
  __add_optarg "$@"
}

__add_optarg() {
  while (( $# > 0 )); do
    case "$1" in
      -*) option_pairs+=" $1 $2" ;;
      *)  option_pairs+=" -$1 $2" ;;
    esac
    shift 2
  done
}

add_option() {
  help_args_func option_help $# 1 || return
  __add_option "$@"
}

__add_option() {
  while (( $# > 0 )); do
    case "$1" in
      -*) options+="${1#-}" ;;      # append option without leading '-'
      *)  options+="$1" ;;
    esac
    shift
  done
}

add_opt()   {   add_option "$@" ; }
__add_opt() { __add_option "$@" ; }

all_opts() {
  if (( ${#options} == 1 )); then     # don't output empty '-'
    echo "$option_pairs"
  else
    echo "$option_pairs $options"          # output both the option_pairs and options
  fi
}
all_args()   { all_opts "$@" ; }
__all_opts() { all_opts "$@" ; }
__all_args() { all_opts "$@" ; }

# end of option-utils.sh
