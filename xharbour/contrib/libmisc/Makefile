#
# $Id: Makefile,v 1.1 2003/04/14 16:09:11 lculik Exp $
#

ROOT = ../../

C_SOURCES=\
	dates2.c \
	dbftools.c \
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
