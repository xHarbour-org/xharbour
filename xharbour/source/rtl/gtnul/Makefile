#
# $Id: Makefile,v 1.2 2003/05/21 09:35:36 druzus Exp $
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
#CFLAGS += -DHB_GT_LIB=$(patsubst gt%,%,$(HB_GT_LIB))
CFLAGS += -DHB_GT_LIB=$(HB_GT_LIB:gt%=%)
endif
