##################################
# $Id: Rules.make,v 1.3 2003/07/23 15:58:02 lculik Exp $
#
# Rules for making simwin
#
# USAGE:
# Include from subdirectories defining a TARGET
# that can be a binary file, a library file or
# a shared file.
# HARBOUR flags can be defined using variable PRGFLAGS
# C flags are CFLAGS ;-)
# Use LIBDIR and LIBS to define extra libs.
#
# If variable SOURCES has been defined, the list of files
# is taken from here, else all the files are taken from
# *.prg.
#

#Generic make options
LINKER = ar
CC = gcc
CFLAGS += -Wall -I.
LIBRARIAN = ranlib
GT_LIBS=-lgtcgi

#libraries for binary building
ifeq ($(HB_MT),MT)
LIBFILES_=-ldebug -lgpm -lvmmt -lrtlmt $(GT_LIBS) -lrddmt -lrtlmt -lvmmt -lmacro -lppmt -ldbfntxmt -ldbfcdxmt -ldbfdbt -lcommon -lm -lpthread
else
LIBFILES_=-ldebug -lgpm -lvm -lrtl $(GT_LIBS)  -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbfdbt -lcommon -lm
endif

LIBDIR_ = $(LIBDIR) -L$(HB_LIB_INSTALL)
LIBS_=  $(LIBS) $(LIBFILES_)

ifeq ($(strip $(SOURCE_TYPE)),)
SOURCE_TYPE=prg
endif

#Sources / object determination rule
#subidr might override this file by providing a makefile.sources
ifeq ($(strip $(SOURCES)),)
SOURCES=$(wildcard *.$(SOURCE_TYPE))
endif

ifeq ($(strip $(OBJECTS)),)
OBJECTS=$(patsubst %.$(SOURCE_TYPE),%.o,$(SOURCES))
ifneq ($(strip $(CSOURCES)),)
OBJECTS+=$(patsubst %.c,%.o,$(CSOURCES))
endif
endif


#COMMANDS
all:$(TARGET)

.PHONY: clean install

%.o: %.c
	$(CC) -c -o$@ $(CFLAGS) -I$(HB_INC_INSTALL) $<

%.c: %.prg
	$(HB_BIN_INSTALL)/harbour -w2 -p -n $(PRGFLAGS) -I$(HB_INC_INSTALL)  -o$@ $<

$(TARGET): $(OBJECTS)
ifeq ( lib , $(patsubst %.a, lib, $(TARGET)))
	$(LINKER) -r $(TARGET) $(OBJECTS)
	$(LIBRARIAN) $(TARGET)
else
	$(CC) -o $(TARGET) $(OBJECTS) $(LIBDIR_) $(LIBS_)
endif

clean:
	rm -f *.o
	rm -f *~
	rm -f $(TARGET)

install: all
	cp -f *.a $(XWT_INSTALL)
