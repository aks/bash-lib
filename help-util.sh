# help-util.sh
# Copyright 2015 Alan K. Stebbens <aks@stebbens.org>
#
# This utility makes it easy to provide helpful responses for shell functions
# that are missing arguments.
#
# Each collection of related shell functions can share a common "help_FUNC"
# function, which is then filtered for the specific function name for which
# help is being sought.
#
# Each function that can be used by a user should start with a call to
# "help_args_func", passing the HELPFUNC, $#, and the mininum number of
# arguments.
#
# If the using function is called with less than the required arguments the
# HELPFUNC is invoked and the output filtered through a simple filter that does
# not print until the calling function name is found and then prints only until
# the next empty line of test.
#
# Each collection of functions that wish to make use of this utility should
# have a HELPFUNC that prints a brief description of each command (function),
# where each function name begins a line (without any leading whitespace.

# usage:
#
#   help_args_func HELPFUNC $# [NEEDED]

help_args_func() {
  local func="${FUNCNAME[1]}"         # who called?
  if (( $2 < ${3:-1} )) ; then
    $1 2>&1 | awk "BEGIN { t=\"\" }
                   /^$func[^a-z0-9_]/ {p=1;
                                      if (length(t) > 0){print t};
                                      print; next}
                   /^[ 	]*$/         {if (p) {exit};
                                      t=\"\"; next}
                   /^[^ 	]/   {if(!p){t = t $0; next}}
                                     {if(p){print}}"
    return 1
  fi
  return 0
}

# end of help-util.sh
