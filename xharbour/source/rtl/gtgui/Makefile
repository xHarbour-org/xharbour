#
# $Id: Makefile,v 1.2 2006/06/28 14:30:22 guerra000 Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtgui.c \
	gtdef.c \

LIBNAME=gtgui

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif

ifneq ($(HB_GT_DEFAULT),)
CFLAGS += -DHB_GT_DEFAULT=$(HB_GT_DEFAULT:gt%=%)
endif
ifneq ($(HB_GT_LIB),)
CFLAGS += -DHB_GT_LIB=$(HB_GT_LIB:gt%=%)
endif
