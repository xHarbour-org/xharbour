#
# $Id: Makefile,v 1.5 2007/05/27 14:07:27 likewolf Exp $
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

HB_PPGEN_PATH ?= .

include $(TOP)$(ROOT)config/lib.cf

pptable.c : ppgen$(EXE_EXT)
	$(HB_PPGEN_PATH)/ppgen$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q
