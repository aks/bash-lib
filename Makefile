# Makefile for libraries 
# $Id$
#

bindirs = 
bins =	

libdirs = $(HOME)/lib
libs = 	\
	Makefile.inc 		\
	Makefile-template	\
        date-utils.sh           \
	list-utils.sh		\
        sh-utils.sh             \
	sync-files.sh		\
        test-utils.sh           \
        test-template.sh        \
    	# end of list

subdirs=

include  Makefile.inc

# vim: sw=4 ai
