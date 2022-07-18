# Makefile for bash-lib
#
# Copyright 2006-2022 Alan K. Stebbens <aks@stebbens.org>

SHELL := $(shell which bash)
bindirs =
bins =

libdirs = $(HOME)/lib

libs = 	\
	Makefile.inc 		\
	Makefile-template	\
	arg-utils.sh		\
	bash-check.sh		\
	bash-lib.sh		\
	calendar-utils.sh       \
	cli-template.sh		\
	date-utils.sh		\
	hash-utils.sh		\
	help-util.sh		\
	list-utils.sh		\
	option-utils.sh		\
	prompt-colors.sh	\
	real-utils.sh		\
	reset-util-vars.sh	\
	run-utils.sh		\
	sh-utils.sh		\
	sync-files.sh		\
	talk-utils.sh		\
	test-utils.sh		\
	test-template.sh        \
	text-utils.sh		\
	time-utils.sh		\
	# end of list

prompt-colors.sh: generate-prompt-colors
	./generate-prompt-colors

clean::
	rm -f prompt-colors.sh*
	rm -f test/*.{out,err}

subdirs=

tests= \
        test-arg-utils.sh	\
	test-date-utils.sh	\
	test-hash-utils.sh	\
	test-list-utils.sh	\
	test-real-utils.sh	\
	test-sh-utils.sh	\
	test-talk-utils.sh	\
	test-text-utils.sh	\
	test-time-utils.sh	\
	#test-sync-files.sh	\
	# end of tests

include  Makefile.inc

# vim: sw=4 ai
