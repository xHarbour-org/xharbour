#
# $Id: Makefile,v 1.4 2004/11/01 05:38:08 likewolf Exp $
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
	libmisc \
	libnf \

endif
endif
endif

include $(ROOT)config/dir.cf
