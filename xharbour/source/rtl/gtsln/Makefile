#
# $Id: Makefile,v 1.4 2003/05/16 19:52:11 druzus Exp $
#

ROOT = ../../../

C_SOURCES=\
	gtsln.c \
	kbsln.c \
	termtype.c \
	mousesln.c \

LIBNAME=gtsln

include $(TOP)$(ROOT)config/lib.cf

# a workaround of a problem with different include directories 
# for slang header files between different unix distributions
#TEMPOR := $(CFLAGS)
CFLAGS := -I/usr/include/slang -I/usr/local/include \
	  -I/usr/local/include/slang -I/sw/include $(CFLAGS)

#TEMPOR  := $(LDFLAGS)
#LDFLAGS := -L/usr/lib/slang -L/usr/local/lib -L/usr/local/lib/slang $(TEMPOR)

ifeq ($(HB_GPM_MOUSE),yes)
CFLAGS := $(CFLAGS) -DHAVE_GPM_H
endif

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
