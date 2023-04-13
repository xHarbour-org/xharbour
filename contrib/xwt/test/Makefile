##################################
# $Id$
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

ifeq ($(HB_ARCHITECTURE),w32)
XWT_BACKEND=xwt_win
else
XWT_BACKEND=xwt_gtk
GTK_LIBS := $(shell pkg-config --libs gtk+-2.0)
endif

CONTRIBS=\
	xwt \
	$(XWT_BACKEND) \

include $(TOP)$(ROOT)config/bin.cf

HB_INC_COMPILE += -I$(TOP)../include
LINKPATHS += -L$(TOP)../src/$(HB_ARCH) -L$(TOP)../src/$(XWT_BACKEND)/$(HB_ARCH) $(GTK_LIBS)
