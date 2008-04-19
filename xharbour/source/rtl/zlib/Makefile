#
# $Id: Makefile,v 1.76 2008/03/18 11:05:37 likewolf Exp $
#

ROOT = ../../../

C_SOURCES=\
	adler32.c \
	compress.c \
	crc32.c \
	deflate.c \
	gzio.c \
	infback.c \
	inffast.c \
	inflate.c \
	inftrees.c \
	trees.c \
	uncompr.c \
	zutil.c \

LIBNAME=zlib

# The list of all valid GT drivers is defined in config/<arch>/global.cf.

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf

ifneq ($(HB_GT_DEFAULT),)
CFLAGS += -DHB_GT_DEFAULT=$(HB_GT_DEFAULT:gt%=%)
endif
ifneq ($(HB_GT_LIB),)
CFLAGS += -DHB_GT_LIB=$(HB_GT_LIB:gt%=%)
endif
CFLAGS += -I../..
