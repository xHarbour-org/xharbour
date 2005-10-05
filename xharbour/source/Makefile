#
# $Id: Makefile,v 1.7 2005/10/05 20:25:43 lf_sfnet Exp $
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
	ct \
	misc 

ifneq ($(HB_ARCHITECTURE),dos)

DIRS+=\
	tip \
	cgi

endif

endif

include $(ROOT)config/dir.cf
