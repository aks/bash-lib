# Makefile.inc
# Copyright 2006-2013 Alan K. Stebbens <aks@stebbens.org>
#
# $Id$
#
# Makefile for managing sets of files 
#
# bins = list of "binaries" (could be scripts)
# bindirs = list of directories into which the binaries can be installed
# libs = list of "libraries" (could be any kind of file)
# libdirs = list of directories in which to install the libraries.
#
# distuser is a userid for remote distribution
# disthost is the default host for remote distribution
# distpath is the path on disthost under which the ZIP distribution of
#          the files will be installed.
#

SHELL=/bin/sh

distuser ?= $$USER
disthost ?= $(distuser)@$$DISTHOST
distpath ?= .

# These should be defined in the "parent" Makefile
# bindirs 
# bins 
# libdirs 
# libs 
# incdirs
# incs
# cssdirs
# cssfiles
# tmpldirs
# tmpls
# tests 
# subdirs 

# This is a list of files which are in development, and which should not be installed
# indev 

# if the target is "diff" or "difflist", use -k to keep going even if "diff" errors

ifneq (,$(findstring diff,${MAKECMDGOALS}))
MAKEFLAGS += -k
endif

# if subdirs is defined, then use -w to show what directory we are in
ifdef subdirs
MAKEFLAGS += -w
endif

allfiles = $(bins) $(libs) $(incs) $(cssfiles) Makefile RCS

afile = dummy

.PHONY: subdirs default help status diff diffs difflist 
.PHONY: fetch install install-bins install-libs install-files zip putzip getzip $(subdirs)

default:	help
help:
	@echo "You can make these things:"
	@echo " help"
	@echo " status	 -- do git/svn/rlog status (if ./.git exists)"
	@echo " diff	 -- show differences with installed versions"
	@echo " difflist -- show different files installed versions"
	@echo " install  -- install"
	@echo " fetch	 -- fetch the installed file(s)"
	@echo " test	 -- run tests"
	@echo " zip	 -- Create a zip file "
	@echo " putzip	 -- distribute zipfile to yoda"
	@echo " getzip	 -- fetch zipfile from yoda"
	@echo ""
	@echo "Use bins='file1 ..' or libs='file1 ..' to work on only the named files"
	@echo ""
	@echo "  bindirs = $(bindirs)"
	@echo "  libdirs = $(libdirs)"
	@echo "  incdirs = $(incdirs)"
	@echo "  cssdirs = $(cssdirs)"
	@echo " tmpldirs = $(tmpldirs)"
	@echo "  subdirs = $(subdirs)"
	@echo ""
	@echo "    bins = $(bins)"
	@echo "    libs = $(libs)"
	@echo "    incs = $(incs)"
	@echo "cssfiles = $(cssfiles)"
	@echo "   tmpls = $(tmpls)"
	@echo "   tests = $(tests)"


test tests: $(tests)
	@for file in $(tests) ; do		\
	    echo "\nTesting $$file .." ;	\
	    case $$file in			\
	    *.rb) ruby -I . $$file ;;		\
	    *.sh) sh -x $$file ;;		\
	    *.pl) perl $$file ;;		\
	    *.py) python -I . $$file ;;		\
	    *)	./$$file ;;			\
	    esac ;				\
	done

zip:	$(zipfile)
$(zipfile):	$(allfiles)
	zip -r $(zipfile) $(allfiles)

putzip: $(zipfile)
	scp $(zipfile) $(disthost):$(distpath)

getzip:
	scp $(disthost):$(distpath)/$(zipfile) .

status:
	@if [[ -d .git ]]; then git status ; \
	elif [[ -d .svn ]]; then svn status ; \
	elif [[ -d RCS ]]; then rlog -R -L RCS/* ; fi

subdirs:	$(subdirs)
$(subdirs): ; $(MAKE) -C $@ $(MAKECMDGOALS)


# $(call check_dirs_for_changed_files,FILES,DIRS,ACTION)
# ACTION can reference $$dir and $$file

check_dirs_for_changed_files = \
	for file in $(1) ; do						\
	    for dir in $(2) ; do					\
		$(call changed_file_action,$$dir/$$file,$$file,$(3)) ;	\
	    done ;							\
	done

# $(call changed_file_action,OLD,NEW,ACTION)

changed_file_action = \
	if [[ ! -f $(1) || -n `diff -q $(1) $(2)` ]]; then	\
	    $(3) ;						\
	fi

action_if_exists = \
	if [[ -f $(1) ]]; then	\
	    $(2) ;		\
	fi
	

# These are the known actions
#   diff_file
#   copy_file
#   install_file

diff_file = \
	echo "========================================" ; \
	diff -uawBN $(1) $(2)

copy_file	= (set -x ; cp $(1) $(2) )
install_file	= (set -x ; install -bSC -m $(mode) $(1) $(2) )
install_py_file = $(call install_file,$(1),$(2)) ;			\
		  for ext in pyc pyo ; do				\
		    if [[ -f $(basename $(2)).$$ext ]]; then		\
			(set -x ; rm -f $(basename $(2)).$$ext ) ;	\
		    fi ;						\
		  done

diff_changed_files  = $(call check_dirs_for_changed_files,$(1),$(2),$(call diff_file,$$dir/$$file,$$file))
list_changed_files  = $(call check_dirs_for_changed_files,$(1),$(2),printf "%30s\t%s\n" $$file $$dir/$$file)
fetch_changed_files = $(call check_dirs_for_changed_files,$(1),$(2),$(call copy_file,$$dir/$$file,$$file))

action_to_targets = \
	$(call $(1),$(bins),$(bindirs)) ; \
	$(call $(1),$(libs),$(libdirs)) ; \
	$(call $(1),$(incs),$(incdirs)) ; \
	$(call $(1),$(cssfiles),$(cssdirs)) ; \
	$(call $(1),$(tmpls),$(tmpldirs))

diff diffs: subdirs ; @$(call action_to_targets,diff_changed_files)
difflist:   subdirs ; @$(call action_to_targets,list_changed_files)
fetch:	    subdirs ; @$(call action_to_targets,fetch_changed_files)

install: install-bins install-libs install-incs install-cssfiles install-tmpls

install_targets = $(foreach the_dir,$(1),$(MAKE) the_dir=$(the_dir) files="$(2)" mode=$(3) install-files ;)
install-files:	$(addprefix $(the_dir)/,$(files))

install-bins:     subdirs ; @$(call install_targets,$(bindirs),$(bins),775)
install-libs:     subdirs ; @$(call install_targets,$(libdirs),$(libs),664)
install-incs:     subdirs ; @$(call install_targets,$(incdirs),$(incs),664)
install-cssfiles: subdirs ; @$(call install_targets,$(cssdirs),$(cssfiles),664)
install-tmpls:    subdirs ; @$(call install_targets,$(tmpldirs),$(tmpls),664)

$(the_dir): ; mkdir -p $@
$(the_dir)/%.py:  %.py $(the_dir) ; @$(call changed_file_action,$@,$<,$(call install_py_file,$<,$@))
$(the_dir)/%.rb:  %.rb $(the_dir) ; @$(call changed_file_action,$@,$<,$(call install_file,$<,$@))
$(the_dir)/%.js:  %.js $(the_dir) ; @$(call changed_file_action,$@,$<,$(call install_file,$<,$@))
$(the_dir)/%.sh:  %.sh $(the_dir) ; @$(call changed_file_action,$@,$<,$(call install_file,$<,$@))
$(the_dir)/%.css: %.sh $(the_dir) ; @$(call changed_file_action,$@,$<,$(call install_file,$<,$@))
$(the_dir)/%:	  %    $(the_dir) ; @$(call changed_file_action,$@,$<,$(call install_file,$<,$@))


# vim: sw=4 ai noexpandtab sts=8
