#
# $Id: Makefile 9279 2011-02-14 18:06:32Z druzus $
#

ROOT = ../../

C_SOURCES = \
	hbcurl.c \

LIBNAME=hbcurl

PRG_HEADERS=\
	hbcurl.ch   \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
CFLAGS += -I../..