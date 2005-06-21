#
# $Id: Makefile,v 1.3 2003/08/10 21:52:01 druzus Exp $
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

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
