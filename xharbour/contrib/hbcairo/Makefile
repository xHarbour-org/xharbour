#
# $Id: Makefile 9279 2011-02-14 18:06:32Z druzus $
#

ROOT = ../../

C_SOURCES = \
	core.c \
	image.c \
	paths.c \
	pdf.c \
	png.c \
	surface.c \
	text.c \
	transfor.c \
	ps.c \
	util.c \
	context.c


LIBNAME=hbcairo

PRG_HEADERS=\
	hbcairo.ch \

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/header.cf
CFLAGS := `pkg-config --cflags cairo `