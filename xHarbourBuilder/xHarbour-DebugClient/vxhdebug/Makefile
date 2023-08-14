# $Id$
#
# This is a GNU Makefile for the Linux version of the debugger.

OBJDIR = obj
SRCDIR = source

SOURCES = \
	gtkdapp.prg \
	gtkdarea.prg \
	gtkdcall.prg \
	gtkdcon.prg \
	gtkdedit.prg \
	gtkdgui.prg \
	gtkdmon.prg \
	gtkdset.prg \
	gtkdtime.prg \
	gtkdwatch.prg \
	vxhdebug.prg

OBJECTS = $(patsubst %.prg,${OBJDIR}/%.o,${SOURCES})

all:	${OBJDIR}/xdebugl
# gtkdappd

clean:
	rm -f ${OBJECTS} $(patsubst %,${OBJDIR}/%,xdebugl gtkdappd)

${OBJDIR}/xdebugl:	${OBJECTS}
	xhblnk -o$@ -gtcrs -xbgtk $^

.SUFFIXES: .prg

${OBJDIR}/%.o:	${SRCDIR}/%.prg
	xhbcmp -n -o$@ -b -xbgtk $<

gtkdappd:	${OBJECTS} ${OBJDIR}/dbgserve.o
	xhblnk -o$@ -xbgtk $^

${OBJDIR}/dbgserve.o:	../../xHarbour-DebugServer/server/source/dbgserve.prg
	xhbcmp -q0 -n -o$@ $<
