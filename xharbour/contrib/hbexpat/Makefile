#
# $Id: Makefile 9279 2011-02-14 18:06:32Z druzus $
#

ROOT = ../../

C_SOURCES = \
	core.c \
	xmlparse.c \
	xmltok_n.c \
	xmlrole.c \
	xmltok.c \
	xmltok_i.c \
	internal.c \

PRG_SOURCES= \
	unitable.prg \

C_HEADERS=\

PRG_HEADERS=\
	hbexpat.ch

LIBNAME=hbexpat

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/header.cf

CFLAGS := $(CFLAGS) -I../..  -DHARBOUR_CONF