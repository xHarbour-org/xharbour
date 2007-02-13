#
# $Id: Makefile,v 1.6 2006/02/21 19:37:06 druzus Exp $
#

ROOT = ../../

C_SOURCES=    \
   ads1.c     \
   adsfunc.c  \
   adsmgmnt.c \

PRG_HEADERS=\
	ads.ch \

LIBNAME=rddads

ifeq ($(HB_ARCHITECTURE),w32)
  C_USR += -DWIN32
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
