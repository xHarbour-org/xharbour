##################################
# $Id: Makefile,v 1.2 2003/03/27 23:19:10 gian Exp $
#
# Toplevel XWT makefile

all:
	make -C src
	make -C src/xwt_gtk
	#make -C test

clean:
	make -C src clean
	make -C src/xwt_gtk clean
	make -C test clean
	rm -f lib/*.a


install:
	make -C src install
	make -C src/xwt_gtk install

