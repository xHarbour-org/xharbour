#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:42:28 ronpinkas Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtstd.c \
	mousestd.c \

LIBNAME=gtstd

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
