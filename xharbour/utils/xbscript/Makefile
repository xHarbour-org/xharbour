#
# $Id: Makefile,v 1.0 2004/05/20 11:47:08 ronpinkas Exp $
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	xbscript.prg \

PRG_MAIN=hbrun.prg

ifeq ($(HB_ARCHITECTURE),OS2)
SOCK = socket 
else
SOCK = 
endif

LIBS=\
	debug \
	vm \
	rtl \
	lang \
	rdd \
	rtl \
	vm \
	macro \
	pp \
	common \
	$(SOCK) \


include $(TOP)$(ROOT)config/bin.cf
