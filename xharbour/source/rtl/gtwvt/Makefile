#
# $Id: Makefile,v 1.2 2003/05/16 19:52:12 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtwvt.c \

LIBNAME=gtwvt

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
