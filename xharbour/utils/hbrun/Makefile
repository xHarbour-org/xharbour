#
# $Id: Makefile,v 1.4 2007/11/07 23:34:24 likewolf Exp $
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbrun.prg \
	external.prg \

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
	ct \
	lang \
	codepage \
	rdd \
	rtl \
	vm \
	macro \
	pp \
	common \
	zlib \
	$(SOCK) \


include $(TOP)$(ROOT)config/bin.cf
