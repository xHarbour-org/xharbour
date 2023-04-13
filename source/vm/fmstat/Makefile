#
# $Id$
#

vpath %.c ../

ROOT = ../../../

C_SOURCES=\
	fm.c 

LIBNAME=fmstat

ifeq ($(HB_MT),MT)
   MT_LIBNAME=$(LIBNAME)mt
endif

include $(TOP)$(ROOT)config/lib.cf

C_USR += -DHB_FM_STATISTICS
