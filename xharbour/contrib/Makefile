#
# $Id: Makefile,v 1.6 2005/10/05 21:39:34 lf_sfnet Exp $
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
	libnf \

endif
endif
endif

include $(ROOT)config/dir.cf
