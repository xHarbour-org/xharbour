#
# $Id: Makefile,v 1.6 2004/09/03 16:40:56 lf_sfnet Exp $
#

ROOT = ./

DIRS=\
	include \
	source \
	utils \
#	tests \
#	samples \

ifeq ($(HB_POSTINST),)

ifneq ($(HB_ROOTPOSTINST),)
HB_POSTINST = $(HB_ROOTPOSTINST) $(HB_POSTINSTPARAM)
else
ifneq ($(SHLVL),)
HB_POSTINST = $(TOP)$(ROOT)bin/postinst.sh $(HB_POSTINSTPARAM)
else
ifeq ($(HB_ARCHITECTURE),dos)
HB_POSTINST = $(subst /,\,$(TOP)$(ROOT)bin/postinst.bat) $(HB_POSTINSTPARAM)
else
ifeq ($(HB_ARCHITECTURE),w32)
HB_POSTINST = $(subst /,\,$(TOP)$(ROOT)bin/postinst.bat) $(HB_POSTINSTPARAM)
endif
endif
endif
endif

endif

include $(ROOT)config/dir.cf
