#
# $Id: Makefile,v 1.3 2003/09/17 04:30:39 lculik Exp $
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

ifeq ($(HB_ARCHITECTURE),w32)
  C_USR += -DWIN32
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

