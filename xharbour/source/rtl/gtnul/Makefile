#
# $Id: Makefile,v 1.3 2003/06/19 00:07:59 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtnul.c \
	gtext.c \

LIBNAME=gtnul

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
