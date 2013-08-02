#/bin/sh
# Copyright 2006-2013, Alan K. Stebbens <aks@stebbens.org>
# 
# Test module for list-utils.sh
#

export PATH=.:$HOME/lib:$PATH

source list-utils.sh
source test-utils.sh

  tlist1=( now is the time for all good men to come to the aid of their country )

  items=( `grep_list tlist1 the` )

  echo "$?"
  echo `join_list items`
  exit
