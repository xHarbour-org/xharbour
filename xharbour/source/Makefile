#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:40:33 ronpinkas Exp $
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
	lang \
	rdd \
	debug \
   clipsamp \
   odbc \

endif

include $(ROOT)config/dir.cf
