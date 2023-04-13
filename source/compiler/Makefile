#
# $Id$
#

HB_LEX := SIMPLEX

ROOT = ../../

ifeq ($(HB_LEX),SIMPLEX)

YACC_SOURCE=harbour.sly

else

YACC_SOURCE=harbour.y

endif

YACC_HEADERS=\
	hbcomp.h \
	hbsetup.h \
	hbpcode.h \
	hbdefs.h \
	hberrors.h \
	hbpp.h

ifeq ($(HB_LEX),SIMPLEX)

C_EXTRA=hbslex.c

else

LEX_SOURCE=harbour.l

LEX_HEADERS=\
	hbsetup.h \
	hberrors.h \
	hbdefs.h
endif

C_SOURCES=\
	harbour.c \
        hbmain.c \
	cmdcheck.c \
	comptool.c \
	genc.c \
	gencc.c \
	gencc1.c \
	gencobj.c \
	genhrb.c \
	hbfunchk.c \
	hbgenerr.c \
	hbpcode.c \
	hbusage.c \
	hbdead.c \
	hblbl.c \
	hbfix.c \
	hbident.c \
	ppcomp.c   \
	ppword.c   \
	hbstripl.c \
	hbdbginf.c \
	hbpcstat.c \
	hbstrong.c \
	expropta.c \
	exproptb.c \
	exproptc.c \
	$(C_EXTRA)

C_MAIN=harbour.c

LIBS=\
	pp \
	common \

include $(TOP)$(ROOT)config/bin.cf
