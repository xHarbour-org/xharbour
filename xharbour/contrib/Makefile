#
# $Id: Makefile,v 1.1 2003/02/23 22:17:57 lculik Exp $
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	btree \
	htmllib \
	hbzlib \
	libct \
	libgt \
	libmisc \
	libnf \
	rdd_ads \
	samples \


else
ifeq ($(HB_COMPILER),icc)

DIRS=\
	hgf/os2pm \
	htmllib \
	hbzlib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \

else
ifeq ($(HB_ARCHITECTURE),os2)

DIRS=\
	btree \
	hgf/os2pm \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \

else

DIRS=\
	htmllib \
	hbzlib \
	libct \
	libgt \
	libmisc \
	samples \

endif
endif
endif

include $(ROOT)config/dir.cf
