#
# $Id: Makefile,v 1.2 2005/01/22 18:49:52 fsgiudice Exp $
#

ROOT = ../../

C_SOURCES=\
	wvtcore.c \
	wvtutils.c

PRG_SOURCES=\
	wvtclass.prg \
	wvtpaint.prg

LIBNAME=wvtgui

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
