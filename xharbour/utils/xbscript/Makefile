#
# $Id: Makefile,v 1.1 2004/05/20 21:09:22 ronpinkas Exp $
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	xbscript.prg \

PRG_MAIN=xbscript.prg

PRG_HEADERS = \
	rp_dot.src \
	rp_run.src \

LIBS=\
	debug \
	vm \
	rtl \
	lang \
	rdd \
	macro \
	pp \
	common \

ifeq ($(HB_ARCHITECTURE),OS2)
LIBS += socket 
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)

include $(TOP)$(ROOT)config/bin.cf

install::
	$(INSTALL_RULE_HEADERS)
