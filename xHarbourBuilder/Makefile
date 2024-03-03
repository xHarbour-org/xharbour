#
# $Id$
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	gtwvg \
	gtwvw \
	hbzlib \
	libnf \
	rdd_ads \
	filemem \
	xHarbour-SQLRDD \
	xHarbour-xBuild \



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
	xHarbour-SQLRDD \
	xHarbour-xBuild \
	

endif
endif
endif

include $(ROOT)config/dir.cf
