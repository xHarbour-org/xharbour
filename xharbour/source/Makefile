#
# $Id: Makefile,v 1.4 2003/11/30 10:02:46 druzus Exp $
#

ROOT = ../

ifeq ($(HB_COMPILER),bcc16)

# Don't build the Harbour compiler for 16-bit DOS,
# because it frequently runs out of memory, so it
# is necessary to use the DJGPP Harbour compiler.

DIRS=\
	common \
	pp \
	rtl \
	vm \
	macro \
	codepage \
	lang \
	rdd \
	debug \

else

DIRS=\
	common \
	pp \
	compiler \
	rtl \
	vm \
	macro \
	codepage \
	lang \
	rdd \
	debug \
	clipsamp \
	odbc \

ifneq ($(HB_ARCHITECTURE),dos)

DIRS+=\
	tip \

endif

endif


include $(ROOT)config/dir.cf
