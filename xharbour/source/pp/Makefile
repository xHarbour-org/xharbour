#
# $Id: Makefile,v 1.8 2006/11/17 12:29:24 druzus Exp $
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

include $(TOP)$(ROOT)config/lib.cf

pptable.c : ppgen$(EXE_EXT)
	./ppgen$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q
