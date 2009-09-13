#
# $Id: Makefile,v 1.5 2004/02/14 13:23:15 lculik Exp $
#

ROOT = ../../

PRG_HEADERS = \


C_SOURCES = \
	zip.c \


CPP_SOURCES := \
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
	zipplatformcomm.cpp \
	zipstorage.cpp \
	ZipString.cpp

ifeq ($(HB_ARCHITECTURE),linux)
CPP_SOURCES +=
	../../Linux/ZipPathComponent.cpp \
	../../Linux/ZipPlatform.cpp \

endif


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

