#
# $Id: Makefile,v 1.4 2005/02/24 15:32:30 lf_sfnet Exp $
#

vpath %.c ../

ROOT = ../../../

C_SOURCES=\
	fm.c 

LIBNAME=fmstat

ifeq ($(HB_MT),MT)
   MT_LIBNAME=fmstatmt
endif

include $(TOP)$(ROOT)config/lib.cf

C_USR += -DHB_FM_STATISTICS

