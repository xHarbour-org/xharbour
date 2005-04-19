#
# $Id: Makefile,v 1.7 2005/04/18 19:26:44 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	dbfcdx1.c \

PRG_SOURCES=\
	dbfcdx0.prg \

ifeq ($(HB_SIXCDX),yes)
LIBNAME = sixcdx
else
LIBNAME=dbfcdx
endif

ifeq ($(HB_MT),MT)
   MT_LIBNAME=$(LIBNAME)mt
endif

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_SIXCDX),yes)
CFLAGS += -DHB_SIXCDX
HB_FLAGS += -DHB_SIXCDX
endif
