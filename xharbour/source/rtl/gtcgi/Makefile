#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:42:44 ronpinkas Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtcgi.c \
	mousecgi.c \

LIBNAME=gtcgi

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
