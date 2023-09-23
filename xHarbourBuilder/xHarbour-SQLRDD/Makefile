#
# $Id$
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	include \
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
	Include \
	source \
	

endif
endif
endif

include $(ROOT)config/dir.cf
