# Makefile for libraries 
# $Id$
#

SHELL = /usr/local/bin/bash
bindirs = 
bins =	

libdirs = $(HOME)/lib
libs = 	\
	Makefile.inc 		\
	Makefile-template	\
        date-utils.sh           \
	hash-utils.sh		\
	list-utils.sh		\
	real-utils.sh		\
        sh-utils.sh             \
	sync-files.sh		\
        test-utils.sh           \
        test-template.sh        \
	text-utils.sh		\
    	# end of list

subdirs=

tests= \
	test-date-utils.sh	\
	test-hash-utils.sh	\
	test-list-utils.sh	\
	test-real-utils.sh	\
	test-text-utils.sh	\
	test-sh-utils.sh	\
	#test-sync-files.sh	\
	# end of tests

include  Makefile.inc

# vim: sw=4 ai
