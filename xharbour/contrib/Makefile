#
# $Id: Makefile,v 1.7 2009/09/13 01:41:23 likewolf Exp $
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	gtwvg \
	gtwvw \
	hbzlib \
	libnf \
	rdd_ads \


else
ifeq ($(HB_COMPILER),icc)

DIRS=\
	hbzlib \
	libnf \

else
ifeq ($(HB_ARCHITECTURE),os2)

DIRS=\
	libnf \

else

DIRS=\
	hbzlib \
	libnf \
	xwt \

endif
endif
endif

include $(ROOT)config/dir.cf
