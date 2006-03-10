#
# $Id: Makefile,v 1.2 2004/05/10 10:38:08 mauriliolongo Exp $
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
	rdd \
	rtl \
	vm \
	macro \
	pp \
	common \
	$(SOCK) \


include $(TOP)$(ROOT)config/bin.cf
