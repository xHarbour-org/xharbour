#
# $Id: Makefile,v 1.3 2004/11/01 04:30:15 druzus Exp $
#

vpath %.c ../

ROOT = ../../../

C_SOURCES=\
	hvm.c \
	fm.c \
	classes.c \

ifeq ($(HB_MT),MT)
   MT_LIBNAME=optguimt
endif

LIBNAME=optgui

include $(TOP)$(ROOT)config/lib.cf

CFLAGS += -DHB_OPT_GUI
