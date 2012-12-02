#
# $Id: Makefile,v 1.3 2003/09/17 04:30:39 lculik Exp $
#

ROOT = ../../

C_SOURCES=    \
		hbmxml.c \
		mxml_att.c \
		mxml_ent.c \
		mxml_fil.c \
		mxml_get.c \
		mxml_ind.c \
		mxml_nod.c \
		mxml_pri.c \
		mxml_sea.c \
		mxml_set.c \
		mxml_str.c \


LIBNAME=hbmxml

PRG_HEADERS=\
	hbmxml.ch   \


include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

