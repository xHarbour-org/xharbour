#
# $Id: Makefile 7630 2007-08-02 15:45:52Z druzus $
#

ROOT = ../../../

C_SOURCES=\
	gttrm.c \

LIBNAME=gttrm

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_GPM_MOUSE),yes)
  CFLAGS += -DHAVE_GPM_H
ifeq ($(HB_GPM_NOICE_DISABLE),yes)
  CFLAGS += -DHB_GPM_NOICE_DISABLE
endif
endif
