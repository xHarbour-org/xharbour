#
# $Id: Makefile,v 1.2 2003/05/16 19:52:09 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
    gtcrs.c \
    chrmap.c \

LIBNAME=gtcrs

include $(TOP)$(ROOT)config/lib.cf

# a workaround of a problem with different include directories 
# for slang curses files between different unix distributions
CFLAGS += -I/usr/include/ncurses

ifeq ($(HB_GPM_MOUSE),yes)
CFLAGS := $(CFLAGS) -DHAVE_GPM_H
endif

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
