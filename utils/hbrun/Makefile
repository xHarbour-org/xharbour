#
# $Id$
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
	$(SOCK) \

ifeq ($(HB_ARCHITECTURE)/$(HB_COMPILER),w32/mingw32)
LIBS += mainstd
endif

include $(TOP)$(ROOT)config/bin.cf
