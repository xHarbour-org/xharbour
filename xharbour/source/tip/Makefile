#
# $Id: Makefile,v 1.2 2004/08/29 00:26:25 druzus Exp $
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
	
LIBNAME=tip
ifeq ($(HB_MT),MT)
  MT_LIBNAME=tipmt
endif

include $(TOP)$(ROOT)config/lib.cf
