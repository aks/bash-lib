# reset-util-vars.sh
#
# each module checks a variable to avoid recursive sourcing.  These variables
# are not exported, so subshells will always source correctly.
#
# This function definition must be sourcd into bash, and then the function
# invoked in order to clear these variables within the current bash context.

reset_util_vars() {
  unset ARG_UTILS_SH
  unset CALENDAR_UTILS_SH
  unset DATE_UTILS_SH
  unset HASH_UTILS_SH
  unset LIST_UTILS_SH
  unset REAL_UTILS_SH
  unset RUN_UTILS_SH
  unset OPTION_UTILS_SH
  unset SH_UTILS_SH
  unset TALK_UTILS_SH
  unset TEST_UTILS_SH
  unset TEXT_UTILS_SH
  unset TIME_UTILS_SH
}
