#
# $Id: Makefile,v 1.2 2003/05/16 19:52:09 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtdos.c \
	mousedos.c \

LIBNAME=gtdos

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
