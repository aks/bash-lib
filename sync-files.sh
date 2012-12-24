#!/bin/bash
# Copyright 2006-2010 by Alan K. Stebbens <aks@stebbens.orgm>
#
# sync-files.sh -- library for managing file syncs
#
# usage:
#
#  export PATH=$PATH:$HOME/lib/
#  . sync-files.sh
#
#  FILES_DIR=PATH
#	Set this to the directory containing the files to be synchronized
#
#  DEST_INFO=(  USER@HOST/PATH1  USER@HOST/PATH2 ... )
#
#  USE_DATED_FILES=1  
#	set this if the files will have date suffixes; e.g.,
#	FILENAME-YYYY-MM-DD.EXT
#
#  USE_DATE_DIRS=1   
#	set this if the dated files should be filed into dated directories
#
#  FILE_ITERATOR
#	Set this to an expression which evaluates to one or more name parts,
#       to be used in a loop.
#
#	Example:  FILE_ITERATOR='returns aareturns perfstats'
#
#  FILENAME_PREFIX
#	the filename prefix; if not defined will be "file-"
#
#  FILENAME_SUFFIX
#	the filename suffix, occuring after the iterator.  If not defined, 
#       is empty.
#
#  FILENAME_EXT
#	the extension. If not defined, use ".txt".
#
#  FILENAME_BUILDER
#	set this to an expression that evaluates to a filename using
#       the variable "$file_iter" or "$file".
#
#	Example:  FILENAME_BUILDER="mfolio-$file_iter-$(date +%F).html"
#
#  FILE_GENERATOR
#	Set this to an expression which evaluates to a command that generates
#       the current file, using the variables, $file_iter, $file, $date.  
#       The command is invoked using the "run" function, which applies $norun 
#       and $verbose.
#
#	Example: 
#
#        FILE_GENERATOR="dfi -pmf -mfolio-$file_iter -html -csslink Returns.css   -o mfolio-$file_iter-"
#
# Provided functions:
#
# build_files
# sync_files


RSYNC="rsync -e 'ssh -i $(echo ~build)/.ssh/id_rsa' "

# The first destination is the development host -- it receives both the dated
# suffix files and the "plain" files too.
#
# The additional destionation hosts receive only the plain name of the file
# (without a date suffix).

if [[ -z "$DEST_INFO" ]]; then
  DEST_INFO=(   )
fi

# for development

export PATH=/usr/local/bin:~build/bin:/Marketocracy/Scripts/build/bin:$PATH

show_standard_opts() {
  cat 1>&2 <<EOF 
Options:
  -d       Development mode -- the files are copied only to the dev host.
  -D DATE  Set the date of processing
  -f       Only build the files; do not rsync them.
  -F       Force the building of files, even if they already exist.
  -n       no run mode -- show commands but don't do them
  -N       run rsync -n -- don't really make remote changes
  -p       Production mode -- the files are copied to all destinations.
  -s       Sync the files to the destination host.
  -v       Verbose mode
EOF
}

show_extra_info() {
    cat 1>&2 <<EOF
DEST_INFO:
$(echo "${DEST_INFO[@]}" | rs)
EOF
}



build_files() {
  for file_iter in ${FILE_ITERATOR:?'FILE_ITERATOR not defined!'} ; do
    file="$( eval echo \"${FILENAME_BUILDER:?'FILENAME_BUILDER not defined!'}\" )"
    if [[ -e "$file" ]]; then               # already exists?
      if [[ -z "$force" ]]; then            # force build it?
        chat "Not building $file; already exists."
        continue                            # no, skip it
      else
        chat "Rebuilding $file.."
      fi
    fi
    run "$( eval echo \"$FILE_GENERATOR\" )"
  done
}

# get_year filename_TABLE-YYYYMMDD.html
# get_month
# get_day

get_year() {
  case "$1" in
  *[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-([0-9]{4})[0-9]{4}(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-([0-9]{4})-[0-9]{2}-[0-9]{2}(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9]-[0-9][0-9]-[0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-([0-9]{2})-[0-9]{2}-[0-9]{2}(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9][0-9][0-9][0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-([0-9]{2})[0-9]{2}[0-9]{2}(\.[^.]+)?$/\1/p'
    ;;
  esac
}

get_month() {
  case "$1" in
  *[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{4}([0-9]{2})[0-9]{2}(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{4}-([0-9]{2})-[0-9]{2}(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9]-[0-9][0-9]-[0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{2}-([0-9]{2})-[0-9]{2}(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9][0-9][0-9][0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{2}([0-9]{2})[0-9]{2}(\.[^.]+)?$/\1/p'
    ;;
  esac
}

get_day() {
  case "$1" in
  *[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{4}[0-9]{2}([0-9]{2})(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{4}-[0-9]{2}-([0-9]{2})(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9]-[0-9][0-9]-[0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{2}-[0-9]{2}-([0-9]{2})(\.[^.]+)?$/\1/p'
    ;;
  *[0-9][0-9][0-9][0-9][0-9][0-9]*)
    echo "$1" | sed -Ene 's/^.*-[0-9]{2}[0-9]{2}([0-9]{2})(\.[^.]+)?$/\1/p'
    ;;
  esac
}

# sync_a_file SOURCE DESTINATION

sync_a_file() {
  if [[ -z "$verbose" ]]; then
    echo "$1 -> $2"
  fi
  run $RSYNC "$1" "$2"
}

sync_files() {
  local first=1
  for destinfo in "${DEST_INFO[@]}" ; do
    for file_iter in ${FILE_ITERATOR:?'FILE_ITERATOR not defined!'} ; do
      datedfile=$( ls -t1 ${FILENAME_PREFIX:-file-}$file_iter${FILENAME_SUFFIX:-}-*.${FILENAME_EXT:-.txt} | head -1 )
      if [[ -n "$datedfile" && -f "$datedfile" ]]; then

        # the first desition is the development target -- we
        # save the dated copies to that host (and only that host)

        if [[ -n "$first" ]]; then
          sync_a_dated_file "$datedfile" "$destinfo"
        fi

        # save the current file to the "plain" name destination
        plainfile="${FILENAME_PREFIX:-file-}$file_iter${FILENAME_SUFFIX:-}.${FILENAME_EXT:-.txt}"

        sync_a_file       "$datedfile" "$destinfo/$plainfile"

      fi
    done

    # if we are in dev mode, stop here
    [[ -n "$dev_mode" ]] && break
    first=
  done
}

# get_date_dir FILENAME-YYYY-MM-DD.EXT  [DIRECTORY]

get_dated_dir() {
  local file="$1"
  local dest="$2"${2:+/}
  local year=$(  get_year  "$file")
  local month=$( get_month "$file")
  local day=$(   get_day   "$file")
  local thepath="$dest$year/$month/$day"
  echo "$thepath"
}

# sync_a_dated_file FILE-YYYY-MM-DD.html DESTSPEC:DESTPATH/
#
# Using the YYYY, MM, DD parts of the file name, sync the file to a
# dated directory path of: DESTPATH/YYYY/MM/DD/

sync_a_dated_file() {
  local thepath="`get_dated_dir \"$1\" \"$2\"`"
  remote_mkdir "$thepath"
  sync_a_file "$1" "$thepath/"
}

# remote_mkdir PATH
#
# Ensure that PATH exists on the remote host

# mkdir_cache is a cache of filenames that have been made.  We check this to
# avoid remaking files multiple times.

mkdir_cache="/tmp/mkdir_cache.$$"
trap "rm -f $mkdir_cache ; exit" 0 1 2 3 
touch $mkdir_cache

# remote_mkdir HOST:PATH
remote_mkdir() {
  if ! fgrep -q "$1" $mkdir_cache ; then
    local host="${1%%:*}"
    local path="${1#*:}"
    run "ssh $host mkdir -pv $path"
    echo "$1" >>$mkdir_cache
  fi
}

dev_mode()  { export dev_mode=1 ; export prod_mode=  ; }
prod_mode() { export dev_mode=  ; export prod_mode=1 ; }

setup() {
  run "cd `echo ${FILES_DIR:?'No FILES_DIR defined!'}`"
}

parse_opts() {

  [[ $# -eq 0 ]] && usage

  while getopts 'dD:pfFnNhsv' opt ; do
    if ! standard_opt "$opt" ; then
      if [[ -n "$extra_opts" ]]; then
        eval "$extra_opts $opt"
      fi
    fi
  done
}

standard_opt() {
  case "$1" in
    d)  dev_mode ;;
    D)  date=`parseDate.sh -f "$OPTARG"` ;;
    p)  prod_mode ;;
    f)  export build_files=1 ;;
    F)  export force=1 ;;
    n)  export norun=1 ;;
    N)  export NORSYNC=1 ;;
    h)  usage ;;
    s)  export sync_files=1 ;;
    v)  export verbose=1 ;;
    *)  return 0 ;;
  esac
  return 1
}

# process_args "$@"

parse_args() {
  OPTIND=1
  while [[ $# -gt 0 ]]; do
    case "$1" in
      dev|devel|development)  dev_mode ;;
      pr|prod|production)     prod_mode ;;
      *)  error "I have no idea what you want to do! \"$1\"" ;;
    esac
    OPTIND=$(( OPTIND + 1 ))
    shift
  done
}

validate_args() {

  # if production mode, defaults are build and sync
  if [[ -n "$prod_mode" && -z "$build_files" && -z "$sync_files" ]]; then
    build_files=1 sync_files=1
  fi

  # if dev mode, default is build
  if [[ -n "$dev_mode" && -z "$build_files" && -z "$sync_files" ]]; then
    build_files=1 sync_files=
  fi

  # if syncing and neither -d nor -p, default to -d
  if [[ -n "$sync_files" && -z "$dev_mode" && -z "$prod_mode" ]]; then
    dev_mode
  fi

  if [[ -z "$prod_mode" && -z "$dev_mode" && -z "$build_files" && -z "$sync_files" ]]; then
    error "Need an option."
  fi

  if [[ -n "$date" ]]; then
    datearg=" -date $date"
  fi

}

set_rsync_opts() {
  local update='-u --size-only'
  if [[ -n "$forcefiles" ]]; then
    update=
  fi
  RSYNC="$RSYNC -aK ${NORSYNC:+-n} $update --chmod=ugo=rwX ${verbose:+-v}"
}

process_args() {
  [[ -n "$build_files" ]] && build_files
  [[ -n "$sync_files"  ]] && sync_files
}

