#!/usr/bin/env bash
# gen-test-dates.sh
#
# generate random test dates and corresponding jdates and adate
# to be used with date tests.
#
test_date_count=500
#
random_year() {
  local year=0
  while (( year == 0 )) ; do
    year=$(( 1 + (2050 * RANDOM  / 32768) ))
  done
  echo $year
}

random_month() {
  local month=0
  while (( month == 0 )); do
    month=$(( 1 + (12 * RANDOM / 32768) ))
  done
  echo $month
}

#              Ja Fe Mar Apr May Jun Jul Aug Sep Oct Nov Dec 
month_days=( 0 31 28 31  30  31  30  31  31  30  31  30  31 )

random_day() {
  local mx=${1:-$month}
  local max=$(( month_days[$mx] ))
  local day=0
  while (( day == 0 )); do
    day=$(( 1 + (max * RANDOM / 32768) ))
  done
  echo "$day"
}

TESTDATA="test-dates.dat"

echo "Generating $test_data_count test dates to $TESTDATA .."

rm -f $TESTDATA
touch $TESTDATA

declare -A days

for (( i=0; i<test_date_count; i++ )); do
  if (( i % 10 == 0 )); then printf "%d.." $i ; fi
   year=`random_year`
  month=`random_month`
    day=`random_day $month`
  date=`printf "%04d-%02d-%02d" $year $month $day`
  adays=`calfunc a $date`
  jdays=`calfunc j $date`
  if [[ -z "${days[$adays]}" ]]; then
    printf "%s\t%s\t%s\n" "$date" "$adays" "$jdays" >>$TESTDATA
    let days[adays]=1
  fi
done
echo ''
echo "Done"
exit
