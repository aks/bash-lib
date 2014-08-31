# bash-check.sh
#
# this script checks to see if bash is running and if the version is >= 3.3

if [[ "${#BASH_VERSINFO[@]}" -eq 0 ||
        ${BASH_VERSINFO[0]}  -lt 3 ||
      ( ${BASH_VERSINFO[0]}  -eq 3 && ${BASH_VERSINFO[1]} -lt 3 ) ]]
then
  echo 1>&2 "This script can only run with bash version >= 3.3"
  if [[ -n "$BASH_VERSION" ]]; then
    echo 1>&2 "This is bash $BASH_VERSION"
  else
    echo 1>&2 "This is not bash!"
  fi
  exit 75           # EPROGMISMATCH
fi
