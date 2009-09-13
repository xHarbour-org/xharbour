##################################
# $Id: Makefile,v 1.6 2004/08/21 17:27:14 lf_sfnet Exp $
#
# Makefile for XWT Test program
#

ROOT = ../../../

PRG_SOURCES=\
	xwt_test.prg \

PRG_MAIN = xwt_test

LIBS=\
	vm \
	rtl \
	lang \
	rdd \
	macro \
	common \

CONTRIBS=\
	xwt \
	xwt_gtk \

include $(TOP)$(ROOT)config/bin.cf

GTK_LIBS := $(shell pkg-config --libs gtk+-2.0)
HB_INC_COMPILE += -I$(TOP)../include
LINKPATHS += -L$(TOP)../src/$(HB_ARCH) -L$(TOP)../src/xwt_gtk/$(HB_ARCH) $(GTK_LIBS)
