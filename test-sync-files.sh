#!/bin/sh
# copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>
#
# Test module for date-utils.sh

export PATH=.:$PATH:$HOME/lib

source sync-files.sh

#              Ja Fe Ma Ap Ma Jn Jl Au Se Oc No De
month_limits=( 31 28 31 30 31 30 31 31 30 31 30 31 )

next_year() {
  month=1
  : $(( ++year ))
}

next_month() {
  if (( ++month > 12 )) ; then
    day=1
    next_year
  fi
}

# next_date 
next_day() {
  if (( day++ >= month_limits[month-1] )) ; then
    if (( month == 2 && ((year % 100) == 0) && ((year % 400) == 0) )) ; then    # Feb?
      if (( day > 29 )); then
        next_month
      fi
    else                # not leap year
      next_month
    fi
  fi
}

test_a_date() {
  local date="$1"
  yr=`get_year  file-$date`
  mo=`get_month file-$date`
  dy=`get_day   file-$date`
  if (( 10#$yr < 100 )) ; then
    if (( 10#$yr != (year % 100) )) ; then
      error "Bad year '$yr' for date: $date"
    fi
  elif (( 10#$yr != 10#$year )); then
    error "Bad year '$yr' for date: $date"
  fi
  if (( 10#$mo != 10#$month )) ; then
    error "Bad month '$mo' for date: $date"
  fi
  if (( 10#$dy != 10#$day )) ; then
    error "Bad day '$dy' for date: $date"
  fi
}

test_get_funs() {
  year=1995
  month=1
  day=1
  last_year=0
  while (( year < 2011 )); do
    if  (( last_year != year )) ; then
      printf " %s" $year
    fi
    last_year=$year
    date1=`printf "%4d-%02d-%02d" $year $month $day`
    date2=`printf "%4d%02d%02d" $year $month $day`
    date3=`printf "%2d%02d%02d" $((year % 100)) $month $day`
    test_a_date "$date1"
    test_a_date "$date2"
    test_a_date "$date3"
    next_day
  done
  echo ""
}

test_get_funs
exit
