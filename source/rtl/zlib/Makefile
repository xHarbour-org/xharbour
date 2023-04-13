#
# $Id$
#

ROOT = ../../../

C_SOURCES=\
	adler32.c \
	compress.c \
	crc32.c \
	deflate.c \
   gzlib.c \
   gzwrite.c \
   gzread.c \
   gzclose.c \
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
