#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:45:03 ronpinkas Exp $
#

ifeq ($(HB_MAIN),)
    HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
    hbmake.prg \
    hbmutils.prg \
    fclass1.prg \
    ffile1.prg \
    ft_funcs.prg \
    radios.prg \
    checks.prg  \
    pickarry.prg \
    pickfile.prg \
    prb_stak.prg \

C_SOURCES=\
   hbmlang.c        \


PRG_MAIN=hbmake.prg

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

