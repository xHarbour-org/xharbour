#
# $Id: Makefile,v 1.6 2003/11/16 00:13:24 likewolf Exp $
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
