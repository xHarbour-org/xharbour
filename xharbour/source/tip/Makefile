#
# $Id: Makefile,v 1.5 2004/04/05 13:32:47 lf_sfnet Exp $
#

ROOT = ../../

C_SOURCES = \
	base64x.c \
	utils.c \

PRG_SOURCES= \
	httpcln.prg \
	popcln.prg \
	credent.prg \
	encoder.prg \
	encb64.prg \
	encqp.prg \
	encurl.prg \
	mail.prg \
	url.prg \
	client.prg \
	ftpcln.prg \
	smtpcln.prg	\
	
LIBNAME=hbtip
ifeq ($(HB_MT),MT)
  MT_LIBNAME=hbtipmt
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
