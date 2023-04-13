#
# $Id$
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
	zipstring.cpp

ifeq ($(HB_ARCHITECTURE),linux)
CPP_SOURCES +=\
	../../Linux/ZipPathComponent.cpp \
	../../Linux/ZipPlatform.cpp \

endif

ifeq ($(HB_ARCHITECTURE),w32)
CPP_SOURCES +=\
   zippathcomponent.cpp \
   zipplatform.cpp \

endif


PRG_SOURCES= \

LIBNAME=ziparchive
ifeq ($(HB_MT),MT)
  MT_LIBNAME=ziparchivemt
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf


CFLAGS := $(CFLAGS) -I../../linux -I../..



install::
	$(INSTALL_RULE_HEADERS)

