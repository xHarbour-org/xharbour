#
# $Id: Makefile,v 1.4 2003/07/31 18:56:12 druzus Exp $
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

ifneq ($(HB_DEFAULT_GT),)
CFLAGS += -DHB_DEFAULT_GT=$(HB_DEFAULT_GT:gt%=%)
endif
ifneq ($(HB_GT_LIB),)
CFLAGS += -DHB_GT_LIB=$(HB_GT_LIB:gt%=%)
endif
