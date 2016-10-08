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
# where each function name begins an unindented comment line, with exactly one
# blank after the comment character.  A description may follow -- as a bash
# comment, indented or not.  Finally, the doc entry for the given function is
# an empty comment line.
#
# For reference examples, please see either list-utils.sh or hash-utils.sh.

# help_pager <<END_OF_MESSAGE
#    some message
#    ...
# END_OF_MESSAGE

# Use the user's defined pager, or use a default one.

help_pager() {
  local prog="${HELP_PAGER:-more}"
  if [[ -z "$prog" ]]; then
    for prog in less more cat ; do
      prog=`which $prog 2>/dev/null`
      if [[ -n "$prog" ]]; then
        export HELP_PAGER="$prog"
        break
      fi
    done
  fi
  $prog
}

# usage:
#
#  some_function() {
#    help_args_func HELPFUNC $# [NEEDED]
#    ...
#  }

help_args_func() {
  local func _x
  for (( _x=1; _x <= ${#FUNCNAME[*]} ; _x++ )); do
    func="${FUNCNAME[$_x]}"
    [[ "$func" =~ ^_|^help_ ]] || break
  done
  (( $2 >= ${3:-1} )) && return 0     # return true on sufficient args
                                      # not enough args.  Show some help
  $1 | help_func_filter $func
  return 1                            # return false on help
}

# help_func_filter FUNC

# filters STDIN for FUNC and any following lines starting with a blank

help_func_filter() {
  awk "BEGIN { t=\"\" }
                 /^$func[^a-z0-9_]/ { p=1;
                                      if (length(t) > 0){print t};
                                      print
                                      next 
                                    }
                 /^[ 	]*$/        { if (p) {exit}
                                      t=\"\"
                                      next
                                    }
                 /^[^ 	]/          { if(!p) {
                                        t = t \$0
                                        next
                                      } else {
                                        print
                                      }
                                    }"
}

# end of help-util.sh
