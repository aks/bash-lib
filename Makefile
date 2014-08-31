# Makefile for bash-lib
#
# Copyright Alan K. Stebbens <aks@stebbens.org>

SHELL = /usr/local/bin/bash
bindirs =
bins =

libdirs = $(HOME)/lib

libs = 	\
	Makefile.inc 		\
	Makefile-template	\
	bash-lib.sh		\
        date-utils.sh           \
	hash-utils.sh		\
	list-utils.sh		\
	prompt-colors.sh	\
	real-utils.sh		\
        sh-utils.sh             \
	sync-files.sh		\
        test-utils.sh           \
        test-template.sh        \
	text-utils.sh		\
    	# end of list

prompt-colors.sh: generate-prompt-colors
	generate-prompt-colors

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
