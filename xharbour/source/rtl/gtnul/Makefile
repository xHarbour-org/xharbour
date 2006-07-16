#
# $Id: Makefile,v 1.5 2004/04/01 22:00:42 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtnul.c \
	gtext.c \
	gtsys.c \

LIBNAME=gtnul

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
