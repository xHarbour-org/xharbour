#
# $Id: Makefile,v 1.1.1.1 2001/12/21 10:42:40 ronpinkas Exp $
#

ROOT = ../../../

CPP_SOURCES=\
	gtQTc.cpp \
	qtconsole.cpp \
	qtconsoleview.cpp \
	qtconsoledoc.cpp \
	qtconsole.moc.cpp \
	qtconsoleview.moc.cpp \
	qtconsoledoc.moc.cpp 

C_SOURCES=\
	mouseQTc.c

LIBNAME=gtQTc

C_USR=-I/usr/lib/qt3/include

%.moc.cpp: %.h
	moc -o $@ $<

include $(TOP)$(ROOT)config/lib.cf

#.PHONY: clean

#clean::
#	rm -f *.o
#	rm -f *.a
#	rm -f *.moc.cpp
#
