##################################
# $Id: Makefile,v 1.1 2003/04/02 00:56:38 jonnymind Exp $
#
# Toplevel XWT makefile

all:
	make -C src
	make -C xwt_gtk

clean:
	make -C src clean
	make -C xwt_gtk clean
	make -C tests clean


install: all
	make -C src install
	make -C xwt_gtk install

.PHONY: clean install

