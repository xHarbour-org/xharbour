#
# $Id$
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),w32)
	ifeq ($(HB_COMPILER),mingw32)
		C_MAIN = mainwin.c
		DIRS = mainstd
	else
		ifeq ($(HB_COMPILER),cemgw)
			C_MAIN = mainwin.c
			DIRS = mainstd mainwin
		else
			C_MAIN = mainstd.c mainwin.c
		endif
	endif
else
      ifeq ($(HB_ARCHITECTURE),os2)
            C_MAIN := mainstd.c maindll.c
	    DIRS = mainstd
      else
            C_MAIN := main.c
      endif
endif

C_SOURCES=\
	hvm.c        \
	arrays.c     \
	arrayshb.c   \
	asort.c      \
	break.c      \
	classes.c    \
	classesc.c   \
	cmdarg.c     \
	dbgentry.c   \
	debug.c      \
	do.c         \
	dynlibhb.c   \
	dynsym.c     \
	codebloc.c   \
	errorapi.c   \
	estack.c     \
	eval.c       \
	evalhb.c     \
	extend.c     \
	fm.c         \
	garbage.c    \
	hash.c       \
	hbi18n.c     \
	inet.c       \
	initexit.c   \
	initsymb.c   \
	itemapi.c    \
	fastitem.c   \
	macro.c      \
	$(C_MAIN)    \
	memvars.c    \
	memvclip.c   \
	pcount.c     \
	proc.c       \
	pvalue.c     \
	runner.c     \
	set.c        \
	thread.c     \
	throw.c      \

PRG_SOURCES=\
	harbinit.prg \
	trpcvmmt.prg \
	lognetmt.prg \
	errlogmt.prg \


ifeq ($(HB_MT),MT)
   MT_LIBNAME=vmmt
endif

LIBNAME=vm

DIRS+=\
	fmstat \

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
