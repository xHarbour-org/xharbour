#
# $Id: Makefile,v 1.6 2004/05/10 10:38:06 mauriliolongo Exp $
#

ROOT = ../../../

C_SOURCES=\
	dbfcdx1.c \

PRG_SOURCES=\
	dbfcdx0.prg \

ifeq ($(HB_SIXCDX),yes)
C_USR += -DHB_SIXCDX
PRG_USR += -DHB_SIXCDX
LIBNAME = sixcdx
else
LIBNAME=dbfcdx
endif

ifeq ($(HB_MT),MT)
   MT_LIBNAME=$(LIBNAME)mt
endif

include $(TOP)$(ROOT)config/lib.cf
