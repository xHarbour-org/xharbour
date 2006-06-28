#
# $Id: Makefile,v 1.1 2006/06/26 11:27:06 druzus Exp $
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

ifneq ($(HB_DEFAULT_GT),)
CFLAGS += -DHB_DEFAULT_GT=$(HB_DEFAULT_GT:gt%=%)
endif
ifneq ($(HB_GT_LIB),)
CFLAGS += -DHB_GT_LIB=$(HB_GT_LIB:gt%=%)
endif
