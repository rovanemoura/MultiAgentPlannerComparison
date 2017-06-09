# $Id: Makefile,v 1.4 1999/05/04 01:19:38 layer Exp $

SHELL = sh

# `nil' is faster, `t' is smaller (for production)
purify = nil

# `nil' or `:standard'
runtime = :standard

runlisp_sh = $(shell if test -f ../src/runlisp.sh; then echo ../src/runlisp.sh; else echo ../../runlisp.sh; fi)
lisp = $(shell if test -f ../src/lisp; then echo ../src/lisp; else echo ../../lisp; fi)
destdir = $(shell if test -d /usr/fi; then echo /usr/fi; else echo /usr/local; fi)

# "build it and they will come"
all:	FORCE
	rm -fr snpp build.tmp
	echo '(setq *purify* $(purify))' >> build.tmp
	echo '(setq *runtime-type* $(runtime))' >> build.tmp
	cat deliver.cl >> build.tmp
	echo '(exit 0)' >> build.tmp
	sh $(runlisp_sh) -f build.tmp $(lisp) -qq

install: FORCE
	rm -rf $(destdir)/snpp $(destdir)/lib/snpp
	cp -rp snpp $(destdir)/lib/snpp
	ln -s $(destdir)/lib/snpp/snpp $(destdir)/snpp

clean: FORCE
	rm -fr snpp
	rm -f *.fasl *.out build.in build.tmp

FORCE:
