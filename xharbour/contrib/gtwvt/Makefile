#
# $Id: Makefile,v 1.1 2005/01/22 15:21:11 lf_sfnet Exp $
#

ROOT = ../../

C_SOURCES=\
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
