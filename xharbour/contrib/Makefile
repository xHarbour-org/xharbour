#
# $Id: Makefile,v 1.5 2005/06/21 20:19:49 druzus Exp $
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
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
	libnf \

endif
endif
endif

include $(ROOT)config/dir.cf
