#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:42:25 ronpinkas Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtwin.c \
	mousewin.c \

LIBNAME=gtwin

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
