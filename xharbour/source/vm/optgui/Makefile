#
# $Id: Makefile,v 1.1 2004/08/02 21:05:50 lf_sfnet Exp $
#

ROOT = ../../../

C_SOURCES=\
	../hvm.c \
	../fm.c \

LIBNAME=optgui

include $(TOP)$(ROOT)config/lib.cf

CFLAGS := $(CFLAGS) -DHB_NO_PROFILE -DHB_NO_TRACE -DHB_FM_STATISTICS_OFF -DHB_GUI
