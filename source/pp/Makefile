#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	pptable.c \
	ppcore.c  \
	pplib.c   \
	pplib2.c  \
	pplib3.c  \

LIBNAME=pp

LIBS=\
	common \

ifeq ($(HB_PPGEN_PATH),)
   HB_PPGEN_PATH=.
endif

include $(TOP)$(ROOT)config/lib.cf

pptable.c : ppgen$(EXE_EXT)
	$(HB_PPGEN_PATH)/ppgen$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(TOP)$(ROOT)ChangeLog -v$(TOP)$(ROOT)include/hbverbld.h -x$(TOP)$(ROOT)source/compiler/ppword.c

# ppcore.c includes hbverbld.h in xHarbour so it must have a dependency on ppgen run:
ppcore$(OBJ_EXT) : | pptable.c

clean ::
	-$(RM) $(TOP)$(ROOT)include/hbverbld.h
