#
# $Id: Makefile 9279 2011-02-14 18:06:32Z druzus $
#

ROOT = ../../

C_SOURCES =\
	core.c \
	minilzo.c 

PRG_HEADERS=\
	hbmlzo.ch

LIBNAME=hbmlzo

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
