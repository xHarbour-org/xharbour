##################################
# $Id: Makefile,v 1.1 2003/04/02 00:56:38 jonnymind Exp $
#
# Toplevel XWT makefile

all:
	make -C src
	make -C src/xwt_gtk
	make -C test

clean:
	make -C src clean
	make -C src/xwt_gtk clean
	make -C test clean
	rm -f lib/*.a


install:
	make -C src install
	make -C src/xwt_gtk install
	make -C test clean

