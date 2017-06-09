# $Id: Makefile,v 1.4 2007/03/28 23:43:56 layer Exp $

on_windows := $(shell if test -d c:/; then echo yes; else echo no; fi)
on_macosx  := $(shell if test `uname -s` = "Darwin"; then echo yes; else echo no; fi)

ifeq ($(on_windows),yes)
all: opengl.cl wgl.cl
else
all: opengl.cl
endif

clean:	FORCE
	rm -f opengl.cl wgl.cl
	find . -name "*.fasl" -print | xargs rm -f

opengl.cl: opengl.i
ifeq ($(on_windows),yes)
	swig -DACL_WINDOWS -allegrocl -nocwrap -isolate opengl.i
else
ifeq ($(on_macosx),yes)
	swig -DAPIENTRY -allegrocl -nocwrap -isolate opengl.i
else
	swig -DACL_LINUX -DAPIENTRY -allegrocl -nocwrap -isolate opengl.i
endif
endif

wgl.cl: wgl.i
	swig -allegrocl -nocwrap -isolate wgl.i

### not currently used:
#glx.cl: glx.i
#	swig -allegrocl glx.i

source_files := Makefile *.cl *.c *.txt *.i
source_files +=	cggl/*.cl cggl/*.txt
source_files += glx/*.cl 
source_files +=	gtkgl/*.cl gtkgl/*.txt
source_files +=	linux-1.3/*.cl
source_files += macosx-2.0/*.cl
source_files +=	win32-1.1/*.cl

echo_source_files: FORCE
	@echo $(source_files)

FORCE:
