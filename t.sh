#!/bin/sh
#
func1() {
  echo $(( ( 30001340 / 1340 ) * 500 / 15 ))
}

func2() {
  if (( $# > 0 )); then
    arg=$(( 10#$1 ))
  else
    local num
    read num
    arg=$(( 10#$num ))
  fi
  echo "arg is $arg"
  echo "100 * arg is $(( arg * 100 ))"
}

func3() {
  func1 | func2
}

func4() {
  arg=`func1`
  func2 "$arg"
}

touch log3

time for ((i=0;i<=1000; i++)) ; do func3 >> log3 ; done

touch log4

time for ((i=0;i<=1000; i++)) ; do func4 >> log4 ; done

exit
