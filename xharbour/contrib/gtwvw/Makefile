#
# $Id$
#

ROOT = ../../

C_SOURCES=\
    gtwvw.c \
    wvwcheck.c \
    wvwdraw.c \
    wvwedit.c \
    wvwfuncs.c \
    wvwmenu.c \
    wvwpush.c \
    wvwstbar.c \
    wvwtbar.c \


LIBNAME=gtwvw

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
