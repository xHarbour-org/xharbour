#
# $Id: Makefile,v 1.4 2005/10/18 12:14:32 druzus Exp $
#

ROOT = ../../../

ifeq ($(HB_COMPILER),xcc)
C_USR:=$(subst -Ox,,$(subst -Ot,,$(C_USR)))
endif

C_SOURCES=\
	dbffpt1.c \

LIBNAME=dbffpt

include $(TOP)$(ROOT)config/lib.cf
