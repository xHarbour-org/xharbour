#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:43:06 ronpinkas Exp $
#

ROOT = ../../../

C_SOURCES=\
	dbfcdx1.c \

PRG_SOURCES=\
	dbfcdx0.prg \

ifeq ($(HB_MT),MT)
LIBNAME=dbfcdxmt
ARCH_SUFFIX=_mt
else
LIBNAME=dbfcdx
endif

include $(TOP)$(ROOT)config/lib.cf
