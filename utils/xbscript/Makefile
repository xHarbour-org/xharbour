#
# $Id$
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
	codepage \
	ct \

ifeq ($(HB_ARCHITECTURE),OS2)
LIBS += socket 
endif

ifeq ($(HB_ARCHITECTURE)/$(HB_COMPILER),w32/mingw32)
LIBS += mainstd
endif

ifeq ($(findstring _USE_APPMAIN_,$(PRG_USR)),)
PRG_USR+= -D_USE_APPMAIN_
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)

include $(TOP)$(ROOT)config/bin.cf

install::
	$(INSTALL_RULE_HEADERS)
