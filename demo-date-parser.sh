#!/usr/bin/env bash

date="${1:-`date +%F`}"
case "$date" in
  *-*-*) sep='-' ;;
  *.*.*) sep='.' ;;
  */*/*) sep='/' ;;
  *) echo "Bad format: \"$date\"" ; exit ;;
esac
year=$((  10#${date%$sep*$sep*} ))
mm_dd="${date#*$sep}"
month=$(( 10#${mm_dd%$sep*}      ))
day=$((   10#${mm_dd#*$sep}      ))

printf " year: $year\n"
printf "mm_dd: $mm_dd\n"
printf "month: $month\n"
printf "  day: $day\n"
exit
