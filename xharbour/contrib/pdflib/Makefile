#
# $Id: Makefile,v 1.19 2004/03/04 00:27:39 likewolf Exp $
#

ROOT = ../../

PRG_HEADERS = \
    pdf.ch \
    pdfhbdoc.ch \


PRG_SOURCES= \
    pdf1.prg  \
    pdfhbdocs.prg 


LIBNAME=ct
ifeq ($(HB_MT),MT)
  MT_LIBNAME=ctmt
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
