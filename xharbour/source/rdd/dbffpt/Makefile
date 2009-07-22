#
# $Id: Makefile,v 1.5 2007/01/11 01:00:35 druzus Exp $
#

ROOT = ../../../

ifeq ($(HB_COMPILER),xcc)
C_USR:=$(subst -Ox,,$(subst -Ot,,$(C_USR)))
endif

C_SOURCES=\
	dbffpt1.c \
	redbffpt1.c \

LIBNAME=dbffpt

include $(TOP)$(ROOT)config/lib.cf
