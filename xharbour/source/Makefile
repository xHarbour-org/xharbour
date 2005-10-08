#
# $Id: Makefile,v 1.8 2005/10/05 21:39:34 lf_sfnet Exp $
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
	odbc \
	ct \
	misc \
	cgi

ifneq ($(HB_ARCHITECTURE),dos)

DIRS+=\
	tip 

endif

endif

include $(ROOT)config/dir.cf
