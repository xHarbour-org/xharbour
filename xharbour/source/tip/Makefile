#
# $Id: Makefile,v 1.1 2004/08/05 12:21:16 lf_sfnet Exp $
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

include $(TOP)$(ROOT)config/lib.cf
