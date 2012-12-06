#
# $Id$
#

ifeq ($(HB_MAIN),)
 HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbtest.prg   \
	rt_array.prg \
	rt_date.prg  \
	rt_file.prg  \
	rt_hvm.prg   \
	rt_hvma.prg  \
	rt_math.prg  \
	rt_misc.prg  \
	rt_str.prg   \
	rt_stra.prg  \
	rt_trans.prg \
	rt_ccall.prg \

PRG_MAIN=hbtest.prg

LIBS=\
	debug \
	vm \
	rtl \
	lang \
	rdd \
	rtl \
	vm \
	macro \
	common \

ifeq ($(HB_ARCHITECTURE)/$(HB_COMPILER),w32/mingw32)
LIBS += mainstd
endif

include $(TOP)$(ROOT)config/bin.cf
