##################################
# $Id: Makefile,v 1.2 2003/10/09 23:57:23 jonnymind Exp $
#
# Makefile for XWT Test program
#


PRGFLAGS=-I../include
LIBS=`pkg-config --libs gtk+-2.0` -L../lib -lxwt -lxwt_gtk -lxwt
include ../Rules.make

t_browse: t_browse.o
	$(CC) -o t_browse t_browse.o $(LIBDIR_) $(LIBS_)

xwt_test: TARGET = xwt_test
xwt_test: SOURCES= xwt_test.prg
