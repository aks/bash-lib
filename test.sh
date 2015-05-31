#!/bin/bash

source list-utils.sh

numbers=( 1 2 3 4 5 6 7 8 )

map_list numbers '( x * x )'

sqr() { echo $(( $1 * $1 )) ; }

map_list numbers 'sqr x'

map_list numbers 'sqr'

exit
