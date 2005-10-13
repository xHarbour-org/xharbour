#
# $Id: Makefile,v 1.21 2005/09/23 21:55:06 likewolf Exp $
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),w32)
    C_MAIN := mainstd.c mainwin.c
    ifeq ($(HB_COMPILER),mingw32)
        ifeq ($(HB_MULTI_GT),yes)
            C_MAIN := mainwin.c
        endif
    endif
else
      ifeq ($(HB_ARCHITECTURE),os2)
            C_MAIN := mainstd.c maindll.c
      else
            C_MAIN := main.c
      endif
endif

C_SOURCES=\
	arrays.c     \
	arrayshb.c   \
	asort.c      \
	break.c      \
	classes.c    \
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
	hvm.c        \
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
	thread.c     \
	throw.c      \

PRG_SOURCES=\
	harbinit.prg \

ifeq ($(HB_MT),MT)
   MT_LIBNAME=vmmt
endif

LIBNAME=vm

DIRS=\
	optcon  \
	optgui  \

ifeq ($(HB_COMPILER),mingw32)
   DIRS += mainstd
endif

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
