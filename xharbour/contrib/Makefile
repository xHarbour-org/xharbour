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
	hbexpat \
	hbzebra \
	hbcurl \
	hblzo \
	hbbz2 \
	hbcurl \
	tipssl \
	hblzf \
	hbhpdf \
	hbmxml \
	
	

endif
endif
endif

include $(ROOT)config/dir.cf
