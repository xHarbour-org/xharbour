#
# $Id: Makefile,v 1.2 2003/08/25 19:50:06 lculik Exp $
#

ROOT = ../../

PRG_HEADERS = \
	ct.ch \
	cterror.ch \
	ctwin.ch \

C_SOURCES = \
	zip.c \


CPP_SOURCES = \
	stdafx.cpp \
	ziparchive.cpp \
	zipautobuffer.cpp \
	zipcentraldir.cpp \
	zipcollections.cpp \
	zipcomp.cpp \
	zipcompatibility.cpp \
	zipexception.cpp \
	zipfile.cpp  \
	zipfileheader.cpp \
	zipinternalinfo.cpp \
	zipmemfile.cpp \
	zipnew.cpp \
	Linux/ZipPathComponent.cpp \
	Linux/ZipPlatform.cpp \
	zipplatformcomm.cpp \
	zipstorage.cpp \
	ZipString.cpp \



PRG_SOURCES= \

LIBNAME=ziparchive
ifeq ($(HB_MT),MT)
  MT_LIBNAME=ziparchivemt
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf


CFLAGS := $(CFLAGS) -I../../Linux -I../..



install::
	$(INSTALL_RULE_HEADERS)

