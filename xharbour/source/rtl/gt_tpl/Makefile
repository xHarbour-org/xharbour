#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:42:24 ronpinkas Exp $
#

ROOT = ../../../

C_SOURCES=\
	gt_tpl.c \
	mousetpl.c \

LIBNAME=gt_tpl

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
