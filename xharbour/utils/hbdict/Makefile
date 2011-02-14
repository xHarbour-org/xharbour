#
# $Id$
#

ifeq ($(HB_MAIN),)
    HB_MAIN = cgi
endif

ROOT = ../../

PRG_SOURCES=\
	hbdict.prg 

ifeq ($(HB_ARCHITECTURE),dos)
    PRG_USR := -DDJGPP     
endif

PRG_MAIN=hbdict.prg

LIBS=\
	debug \
	vm \
	rtl \
	rdd \
	rtl \
	vm \
	macro \
	pp \
	lang \
	common \

include $(TOP)$(ROOT)config/bin.cf

INSTALL_OBJS += i18n/*.hit
