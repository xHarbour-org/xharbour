#
# $Id: Makefile,v 1.2 2003/05/16 19:52:09 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
        gtos2.c \
        mouseos2.c \

LIBNAME=gtos2

include $(TOP)$(ROOT)config/lib.cf

# uncomment the lines below when this gt will be ready for MULTI_GT
ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
