##################################
# $Id: Rules.make,v 1.4 2006/03/20 23:39:30 fsgiudice Exp $
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

# set environment variables
# export HB_ARCHITECTURE="linux"
# export HB_COMPILER="gcc"
ifeq ("${HB_BIN_INSTALL}","")
  export HB_BIN_INSTALL="/usr/bin"
endif
ifeq ("${HB_INC_INSTALL}","")
  export HB_INC_INSTALL="/usr/include/xharbour"
endif
ifeq ("${HB_LIB_INSTALL}","")
  export HB_LIB_INSTALL="/usr/lib/xharbour"
endif  

ifeq (${HB_COMPILER},gpp)
   HB_CC="g++"
else
   HB_CC="gcc"
endif

#Generic make options
LINKER = ar
ifeq ($(HB_COMPILER),mingw32)
   CFLAGS += -Wall -mno-cygwin -mms-bitfields -mwindows -I.
else
   CFLAGS += -Wall -I.
endif
LIBRARIAN = ranlib
GT_LIBS=-lgtcgi

#libraries for binary building
ifeq ($(HB_MT),MT)
LIBFILES_ = -ldebug -lvmmt -lrtlmt $(GT_LIBS) -lrddmt -lrtlmt -lvmmt -lmacro -lppmt -ldbfntxmt -ldbfcdx -ldbffpt -lhbsix -lcommon -lm -lpthread
else
LIBFILES_ = -ldebug -lvm -lrtl $(GT_LIBS) -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -lhbsix -lcommon -lm
endif

ifeq ($(HB_COMPILER),mingw32)
   LIBFILES_ += -luser32 -lgdi32 -lcomdlg32 -lwinspool
   EXETYPE=.exe
else
   LIBFILES_ += -lgpm
   EXETYPE=
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
all:$(TARGET) $(TARGETS)

.PHONY: clean install

%$(EXETYPE):%.o
	$(HB_CC) -o$@ $< $(LIBDIR_) $(LIBS_)

%.o: %.c
	$(HB_CC) -c -o$@ $(CFLAGS) -I$(HB_INC_INSTALL) $<

%.c: %.prg
	$(HB_BIN_INSTALL)/harbour -q0 -gc0 -w2 -n $(PRGFLAGS) -I$(HB_INC_INSTALL)  -o$@ $<

$(TARGET): $(OBJECTS)
ifeq ( lib , $(patsubst %.a, lib, $(TARGET)))
	$(LINKER) -r $(TARGET) $(OBJECTS)
	$(LIBRARIAN) $(TARGET)
else
	$(HB_CC) -o $(TARGET) $(OBJECTS) $(LIBDIR_) $(LIBS_)
endif

clean:
	rm -f *.o
	rm -f *~
	rm -f *.ppo
	rm -f $(TARGET)
	rm -f $(TARGET).exe
	rm -f $(TARGETS)

install: all
	cp -f *.a $(HB_LIB_INSTALL)
	cp -f ../include/gd.ch $(HB_INC_INSTALL)
