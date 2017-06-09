
at_franz = $(shell if test -d /fi/cl/8.2/acl; then echo t; else echo nil; fi)

# Before Makefile.local include so it can be used there
ARCH=$(shell uname -i)

use_dcl = $(shell if test -f ../dcl.dxl; then echo yes; else echo no; fi)
ifeq ($(use_dcl),yes)
LISP = ../lisp -I dcl.dxl
endif

Makefile_local = \
	$(shell if test -f Makefile.local; then echo Makefile.local; fi)

ifneq ($(Makefile_local),)
include $(Makefile_local)
endif

DEBUG = nil

ifeq ($(at_franz),t)
LISPROOT ?= /fi/cl/8.2
LISP ?= $(LISPROOT)/bin/$(shell if [ $(ARCH) = x86_64 ]; then echo mlisp-64; else echo mlisp; fi)
DESTDIR = /usr/fi
else
DESTDIR = /usr/local
endif

LISP ?= mlisp

runlisp = $(LISP) -q -batch -L build.tmp -kill

version := $(shell grep 'checklinks-version' checklinks.cl | sed -e 's,.*"\(.*\)".*,\1,')

ifeq ($(FI_APPS_COMMON),t)
release ?= $(shell . fi-apps-common/rpm-utils.sh && \
	rpm_next_release_number \
	   /net/$(REPOHOST)$(REPOBASE)/$(ARCH)/checklinks-$(version)-*.$(ARCH).rpm)
else
release ?= 1
endif

tardir = checklinks-$(version)
tarball = checklinks-$(version).tar.gz

# We use FI_APPS_COMMON instead of at_franz because the CL builds don't need
# build the rpm and don't need the fi-apps-common repo.
ifeq ($(FI_APPS_COMMON),t)
ALL_EXTRA = repo_check
endif

all:	clean
	rm -fr checklinks build.tmp
	echo '(setq *debug* $(DEBUG))' >> build.tmp
	cat deliver.cl >> build.tmp
	$(runlisp)
	rm -f build.tmp

ifeq ($(FI_APPS_COMMON),t)
repo_check: FORCE
	@if test ! -d fi-apps-common; then \
	    echo fi-apps-common does not exist.; \
	    exit 1; \
	fi
endif

install: FORCE
ifdef DESTDIR
	rm -rf $(DESTDIR)/bin/checklinks $(DESTDIR)/lib/checklinks
	cp -rp checklinks $(DESTDIR)/lib/checklinks
	(cd $(DESTDIR)/bin; ln -s $(DESTDIR)/lib/checklinks/checklinks .)
else
	@echo There is no DESTDIR defined in Makefile.
	@exit 1
endif

clean: FORCE
	rm -fr checklinks BUILD RPMS SRPMS BUILDROOT SPECS
	rm -f *.fasl */*.fasl *.out build.in *.debug build.tmp

tarball: FORCE
	mkdir $(tardir)
	cp Makefile *.cl $(tardir)
	tar zcf $(tarball) $(tardir)
	rm -fr $(tardir)

SIGN ?= --sign

rpm:	$(ALL_EXTRA) tarball
	mkdir -p BUILD RPMS/$(ARCH) SRPMS
	rpmbuild --define "_sourcedir $(CURDIR)" \
		--define "_topdir $(CURDIR)" \
		--define "version $(version)" \
		--define "release $(release)" \
		--define "mlisp $(LISP)" \
		$(SIGN) \
		--target $(ARCH) \
		-ba checklinks.spec
	rm $(tarball)

REMOVE_PREVIOUS_VERSIONS ?= no
REPOHOST                 ?= cobweb
REPOBASE                 ?= /www/sites/yum

REPODIR=$(REPOBASE)/$(ARCH)

install-repo: FORCE
ifeq ($(REMOVE_PREVIOUS_VERSIONS),yes)
	ssh root@$(REPOHOST) "rm -f $(REPODIR)/checklinks-*"
endif
	scp -p RPMS/$(ARCH)/checklinks-$(version)*.$(ARCH).rpm root@$(REPOHOST):$(REPODIR)
	ssh root@$(REPOHOST) "createrepo -s sha -q --update $(REPODIR)"

###############################################################################

test: FORCE
	checklinks/checklinks -a \
	  -r /home/layer/l/htdocs.work/ \
	  -c /cgi-bin=/home/httpd/cgi-bin \
	  -X random/,movie-data/,Starr/,source/ \
	  -x cat.jpg,Distfile \
	  /index.htm /family.htm

test2: FORCE
	checklinks/checklinks -v -r http://www.dnai.com/~layer/ \
	  http://www.dnai.com/~layer/family.htm

test3: FORCE
	checklinks/checklinks -v -r http://www.known.net/ \
	  -A www.dnai.com \
	  http://www.known.net/index.htm

SITE = wwwlayer

testnew: FORCE
	checklinks/checklinks -i /icons=foo -l \
	  -r http://$(SITE).franz.com/ \
	  -X 5.0.1/,5.0/,support/documentation/5.0.1/,support/documentation/5.0/ \
	  http://$(SITE).franz.com/index.php3

testnew2: FORCE
	checklinks/checklinks -i /icons=foo \
	  -r http://$(SITE).franz.com/ \
	  -X 5.0.1/,5.0/,support/documentation/5.0.1/,support/documentation/5.0/ \
	  http://$(SITE).franz.com/index.php3

testnew3: FORCE
	checklinks/checklinks -i /icons=foo -l -D \
	  -r http://$(SITE).franz.com/ \
	  http://$(SITE).franz.com/index.php3

testnew4: FORCE
	checklinks/checklinks -i /icons=foo \
	  -r http://$(SITE).franz.com/ \
	  http://$(SITE).franz.com/index.php3

testnew5: FORCE
	checklinks/checklinks -l -v \
		-r /net/cobweb/www/franz/users/layer/www/htdocs/ \
		/support/documentation/5.0.1/readme.htm

testfi: FORCE
	checklinks/checklinks -l -r http://www.franz.com/ \
	  http://www.franz.com/index.html

###############################################################################
FORCE:
