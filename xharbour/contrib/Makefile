#
# $Id: Makefile,v 1.2 2003/11/10 00:59:30 fsgiudice Exp $
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	btree \
	htmllib \
	hbzlib \
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
	libgt \
	libmisc \
	libnf \
	samples \

else

DIRS=\
	htmllib \
	hbzlib \
	libgt \
	libmisc \
	samples \

endif
endif
endif

include $(ROOT)config/dir.cf
