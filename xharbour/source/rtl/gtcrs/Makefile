#
# $Id: Makefile,v 1.3 2003/05/17 15:55:18 druzus Exp $
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
ifeq ($(HB_GT_CRS_BCEHACK),yes)
CFLAGS += -DHB_GT_CRS_BCEHACK
endif

ifeq ($(HB_GPM_MOUSE),yes)
CFLAGS += -DHAVE_GPM_H
endif

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
