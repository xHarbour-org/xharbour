#
# $Id: Makefile,v 1.3 2003/01/10 16:12:59 jonnymind Exp $
#

ROOT = ./

DIRS=\
	include \
	source \
	utils \
#	tests \
#	samples \

ifneq ($(SHLVL),)
HB_POSTINST = $(TOP)$(ROOT)bin/postinst.sh
else
ifeq ($(HB_ARCHITECTURE),dos)
HB_POSTINST = $(TOP)$(ROOT)bin/postinst.bat
else
ifeq ($(HB_ARCHITECTURE),w32)
HB_POSTINST = $(TOP)$(ROOT)bin/postinst.bat
endif
endif
endif

include $(ROOT)config/dir.cf
include $(ROOT)config/install.cf
