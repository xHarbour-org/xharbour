#
# $Id: Makefile,v 1.1 2002/07/11 02:57:52 lculik Exp $
#

ROOT = ../../

C_SOURCES=    \
   ads1.c     \
   adsfunc.c  \
   adsmgmnt.c \

PRG_HEADERS=\
	ads.ch \

PRG_SOURCES=\
	ads0.prg \

LIBNAME=rddads
ifneq ($(HB_ARCHITECTURE),dos)
MT_LIBNAME=rddadsmt
endif

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/header.cf