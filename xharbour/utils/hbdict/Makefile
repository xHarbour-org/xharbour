#
# $Id: Makefile,v 1.5 2003/11/03 05:16:58 jonnymind Exp $
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

INSTALL_RULE=\
	cp -f $(HB_ARCHITECTURE)/$(HB_COMPILER)/hbdict$(EXE_EXT) $(HB_BIN_INSTALL);\
	for file in $$(ls i18n/*.hit); do\
		cp -f $$file $(HB_BIN_INSTALL)/hbdict_$${file\#\#i18n/};\
	done;\
		
	
