#
# $Id: Makefile,v 1.6 2003/01/05 06:50:36 ronpinkas Exp $
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),w32)
	C_MAIN := mainstd.c mainwin.c
else
	ifeq ($(HB_ARCHITECTURE),os2)
		C_MAIN := mainstd.c mainpm.c
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

PRG_SOURCES=\
	harbinit.prg \

ifeq ($(HB_MT),MT)
LIBNAME=vmmt
ARCH_SUFFIX=_mt
else
LIBNAME=vm
endif

include $(TOP)$(ROOT)config/lib.cf
