#
# $Id: Makefile,v 1.19 2004/03/04 00:27:39 likewolf Exp $
#

ROOT = ../../

PRG_HEADERS = \
	include/tip.ch \

C_SOURCES = \
	base64x.c \
	tiputils.c \

PRG_SOURCES= \
	tipclienthttp.prg \
	tipclientpop.prg \
	tipcredentials.prg \
	tipencoder.prg \
	tipencoderbase64.prg \
	tipencoderqp.prg \
	tipencoderurl.prg \
	tipmail.prg \
	tipurl.prg \
	tipclient.prg \
	tipclientftp.prg \
	tipclientsmtp.prg	\
	
LIBNAME=hbtip
ifeq ($(HB_MT),MT)
  MT_LIBNAME=hbtipmt
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
