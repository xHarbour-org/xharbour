#
# $Id: Makefile,v 1.3 2004/08/27 05:42:17 paultucker Exp $
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	htmllib \
	hbzlib \
	libmisc \
	libnf \
	rdd_ads \


else
ifeq ($(HB_COMPILER),icc)

DIRS=\
	htmllib \
	hbzlib \
	libmisc \
	libnf \

else
ifeq ($(HB_ARCHITECTURE),os2)

DIRS=\
	htmllib \
	libmisc \
	libnf \

else

DIRS=\
	htmllib \
	hbzlib \
	libmisc \
	libnf \

endif
endif
endif

include $(ROOT)config/dir.cf
