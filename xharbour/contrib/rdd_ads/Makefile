#
# $Id: Makefile,v 1.2 2003/04/09 17:47:14 lculik Exp $
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

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

