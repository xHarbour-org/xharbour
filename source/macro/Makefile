#
# $Id$
#

HB_LEX := SIMPLEX

ROOT = ../../

YACC_FLAGS = -d

YACC_SOURCE=macro.y

YACC_HEADERS=\
	hbmacro.h \
	hbsetup.h \
	hbpcode.h \
	hbdefs.h \

ifeq ($(HB_LEX),SIMPLEX)

C_EXTRA=macroslx.c

else

#NOTE: You can pass additional parameters that control the speed/size
# ratio of generated flex scanner. These parameters are:
# -Cf   - fastest/biggest
# -CF
# -C    - in between
# -Cm
# -Ce
# -Cem  - slowest/smallest
# see Flex documentation for full set of switches
LEX_FLAGS = -Phb_comp -C

LEX_SOURCE=macro.l

LEX_HEADERS=\
	hbsetup.h \
	hberrors.h \
	hbdefs.h \
	hbmacro.h \

endif

C_SOURCES=\
	$(C_EXTRA) \
	macroa.c \
	macrob.c \
	macroc.c \

ifeq ($(HB_MT),MT)
   MT_LIBNAME=macromt
endif

LIBNAME=macro

include $(TOP)$(ROOT)config/lib.cf
