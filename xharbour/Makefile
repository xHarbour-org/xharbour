#
# $Id: Makefile,v 1.4 2003/12/07 19:36:17 druzus Exp $
#

ROOT = ./

DIRS=\
	include \
	source \
	utils \
#	tests \
#	samples \

ifeq ($(HB_POSTINST),)

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

endif

include $(ROOT)config/dir.cf
