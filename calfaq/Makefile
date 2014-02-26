# makefile
#
prefix ?= /usr/local
share_prefix ?= /usr/local/share
bindir ?= $(prefix)/bin
mandir ?= $(share_prefix)/man/man$(mansec)
mansec = 1
manpage = $(prog).$(mansec)

prog = calfunc

$(prog):	calfunc.c calfaq.h Makefile
	cc -o $@ -ggdb -lc -lpcre calfaq.c calfunc.c

$(prog).$(mansec):	calfunc.man

.PHONY: install install-bin install-man

install: install-bin install-man

install-bin: $(bindir) $(bindir)/$(prog) 
$(bindir):
	install -d $@
$(bindir)/$(prog): $(prog)
	install -b $? $(@D)

install-man: $(mandir) $(mandir)/$(manpage)
$(mandir):
	install -d $@
$(mandir)/$(manpage): $(prog).man
	install -b $? $@
