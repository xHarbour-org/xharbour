#
# $Id: Makefile,v 1.5 2003/06/17 00:54:33 druzus Exp $
#

ROOT = ../../

PRG_HEADERS = \
	ct.ch \
	cterror.ch \
	ctwin.ch \

C_SOURCES = \
	strcomp.c \
	zip.c \
	zutil.c \


CPP_SOURCES = \
	stdafx.cpp \
	ziparchive.cpp \
	zipautobuffer.cpp \
	zipcentraldir.cpp \
	zipcollections.cpp \
	zipcompatibility.cpp \
	zipcomp.cpp \
	zipexception.cpp \
	zipfile.cpp  \
	zipfileheader.cpp \
	zipinternalinfo.cpp \
	zipmemfile.cpp \
	zipnew.cpp \
	../../Linux/ZipPathComponent.cpp \
	zipplatformcomm.cpp \
	../../Linux/ZipPlatform.cpp \
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

