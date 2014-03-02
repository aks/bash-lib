#!/usr/bin/env bash
echo "start: $@"
while getopts 'hnv-:' opt ; do
  case "$opt"  in
    h) echo 'help' ;;
    n) echo 'norun' ;;
    v) echo 'verbose' ;;
    -) other="$other $opt" ;;
  esac
done
shift $(( OPTIND - 1 ))
echo "other: $other"
echo "left over: $@"
