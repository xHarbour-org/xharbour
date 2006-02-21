#
# $Id: Makefile,v 1.5 2005/10/04 02:05:32 druzus Exp $
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

