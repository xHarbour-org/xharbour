#
# $Id: Makefile,v 1.2 2004/05/18 12:15:25 lf_sfnet Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtwvt.c \
	wvtcore.c \
	wvtutils.c

PRG_SOURCES=\
	wvtclass.prg \
	wvtpaint.prg

LIBNAME=gtwvt

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
