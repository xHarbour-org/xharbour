#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:43:00 ronpinkas Exp $
#

ROOT = ../../../

C_SOURCES=\
	dbfntx1.c \

PRG_SOURCES=\
	dbfntx0.prg \

ifeq ($(HB_MT),MT)
LIBNAME=dbfntxmt
ARCH_SUFFIX=_mt
else
LIBNAME=dbfntx
endif

include $(TOP)$(ROOT)config/lib.cf
