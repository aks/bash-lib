#!/usr/bin/env bash
# my-script some-args
#
# This is a template for a command-line script.
# It supports four options: -n, -v, -h, and -i FILE.
#
# See "help getopts" for more information on adding other options.
#
# Copyright 2019-2022 Alan K. Stebbens <aks@stebbens.org>

# set the name and directory of this program
PROG="${0##*/}"
DIR="${0%/*}"

# include ~/bin and ~/lib to get my bash scripts
export PATH=$PATH:$HOME/bin:$HOME/lib

# include some library scripts
source talk-utils.sh
source run-utils.sh

usage() {
  cat 1>&2 <<USAGE
usage: $PROG [options] ARGUMENTS
This script does this and that, and the other.  You can control it with certain
options.  I usually explain how it works at a high-level and then explain the options.

Options
  -h          show help
  -i FILE   read input from FILE
  -n          norun: don't do anything, but show the commands
  -v          be verbose; talk a lot
USAGE
  exit
}

# define more bash functions here, if needed

# other setup goes here

# start scanning the program options
norun= verbose=  # ensure vars not set

while getopts 'hi:nv' opt ; do
  case "$opt" in
  -h) usage ;;
  -n) norun=1 ;;
  -i) input_file="$OPTARG" ;;
  -v) verbose=1 ;;
  esac
done
shift $(( OPTIND - 1 ))

# if no arguments, show the command usage and exit
[[ $# -gt 0 ]] || usage

# do any processing that depends on the options here, after parsing them

exit
