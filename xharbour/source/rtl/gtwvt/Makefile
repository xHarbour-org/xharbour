#
# $Id: Makefile,v 1.1 2003/12/21 21:31:52 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtwvt.c \
	wvtcore.c \
	wvtutils.c

LIBNAME=gtwvt

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
