#
# $Id: Makefile,v 1.8 2005/04/19 18:49:57 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	dbfcdx1.c \
	sixcdx1.c \

PRG_SOURCES=\
	dbfcdx0.prg \
	sixcdx0.prg \

ifeq ($(HB_MT),MT)
   MT_LIBNAME=$(LIBNAME)mt
endif

LIBNAME=dbfcdx

include $(TOP)$(ROOT)config/lib.cf
