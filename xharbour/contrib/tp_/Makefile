#
# $Id: Makefile,v 1.3 2004/08/25 08:49:06 mauriliolongo Exp $
#

ROOT = ../../

ifneq ($(HB_ARCHITECTURE),dos)

ifeq ($(HB_ARCHITECTURE),linux)
C_SOURCES=\
	linux.c
else
C_SOURCES=\
	os2.c
endif

PRG_SOURCES=\
        telepath.prg

LIBNAME=telepath

include $(TOP)$(ROOT)config/lib.cf

endif

