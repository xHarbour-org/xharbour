#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:42:35 ronpinkas Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtpca.c \
	mousepca.c \

LIBNAME=gtpca

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
