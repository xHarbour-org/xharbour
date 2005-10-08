#
# $Id: Makefile,v 1.1 2005/10/05 20:52:18 lf_sfnet Exp $
#

ROOT = ../../

C_SOURCES=\
	dates2.c \
	hb_f.c \
	mathx.c \
	strfmt.c \
	stringsx.c \
	date.c \
	dbf.c \
	environ.c \
	gauge.c \
	num.c \
	stack.c \
	status.c \
	time.c
	
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
