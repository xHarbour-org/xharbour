##################################
# $Id: Makefile,v 1.5 2004/08/08 11:24:05 lf_sfnet Exp $
#
# Makefile for XWT Test program
#

TARGET = xwt_test
SOURCES = xwt_test.prg

#TARGET = t_browse
#SOURCES = t_browse.prg

PRGFLAGS=-I../include
LIBS=-L../lib -lxwt -lxwt_gtk -lxwt `pkg-config --libs gtk+-2.0` 

include ../Rules.make
