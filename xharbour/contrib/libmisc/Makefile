#
# $Id: Makefile,v 1.2 2003/06/06 00:56:55 lculik Exp $
#

ROOT = ../../

C_SOURCES=\
	dates2.c \
	hb_f.c \
	mathx.c \
	strfmt.c \
	stringsx.c \

PRG_SOURCES=\
	fileread.prg \
	nconvert.prg \
	numtxthu.prg \
	numtxten.prg \
	stringp.prg \
	twirler.prg \

LIBNAME=misc

ifneq ($(HB_ARCHITECTURE),dos)
MT_LIBNAME=miscmt
endif

DIRS=tests

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
