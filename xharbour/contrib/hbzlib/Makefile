#
# $Id: Makefile,v 1.4 2003/09/23 04:52:34 paultucker Exp $
#

ROOT = ../../

PRG_HEADERS = \


C_SOURCES = \
	zip.c \


CPP_SOURCES = \
	stdafx.cpp \
	ziparchive.cpp \
	zipautobuffer.cpp \
	zipcentraldir.cpp \
	zipcomp.cpp \
	zipcompatibility.cpp \
	zipexception.cpp \
	zipfile.cpp  \
	zipfileheader.cpp \
	zipmemfile.cpp \
	zipnew.cpp \
	../../Linux/ZipPathComponent.cpp \
	../../Linux/ZipPlatform.cpp \
	zipplatformcomm.cpp \
	zipstorage.cpp \
	ZipString.cpp



PRG_SOURCES= \

LIBNAME=ziparchive
ifeq ($(HB_MT),MT)
  MT_LIBNAME=ziparchivemt
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf


CFLAGS := $(CFLAGS) -I../../Linux -I../.. -I/usr/include



install::
	$(INSTALL_RULE_HEADERS)

