#
# Rules for making a generic xharbour library or program
# $Id: Rules.make,v 1.4 2003/11/14 12:01:41 jonnymind Exp $
#
# (C) Giancarlo Niccolai 2003
#


#Generic make options
LINKER = ar
CC = gcc
LIBRARIAN = ranlib
ifeq ($(HB_INCLUDE),)
   HB_INCLUDE=$(HB_INC_INSTALL)
endif
CFLAGS += -Wall -I.:$(HB_INCLUDE)

#libraries for binary building

ifeq ($(HB_MT),MT)
MTLIBS= -lvmmt -lrtlmt -lrddmt -lrtlmt -lvmmt -ldbfntxmt -ldbfcdx -ldbfdbt
ifeq ($(HB_ARCHITECTURE),linux)
MTLIBS+=-lpthread
endif
else
  MTLIBS= -lvm -lrtl -lrdd -lrtl -lvm  -ldbfntx -ldbfcdx -ldbfdbt
endif

ifeq ($(HB_ARCHITECTURE),linux)
   ifeq ($(HB_GT_LIB),)
      GT_LIBS=-lgtcrs -lncurses -lgpm
   else
      ifeq ($(HB_GT_LIB),gtcgi)
         GT_LIBS=-lgtcgi
      endif
   endif
EXE_EXT=
else
  ifeq ($(HB_ARCHITECTURE),darwin)
    ifeq ($(HB_GT_LIB),gtcrs)
      GT_LIBS=-lgtcrs -lncurses
    else
      ifeq ($(HB_GT_LIB),)
        HB_GT_LIB=gtstd
      endif
      GT_LIBS=-l$(HB_GT_LIB)
    endif
    EXE_EXT=
  else
    GT_LIBS=-lgtwin -lwsock32 -lwinspool
    EXE_EXT=.exe
  endif
endif

#Sources / object determination rule
#subidr might override this file by providing a makefile.sources
ifeq ($(strip $(SOURCES)),)
SOURCES=$(wildcard *.prg)
endif

ifeq ($(strip $(OBJECTS)),)
OBJECTS=$(patsubst %.prg,%.o,$(SOURCES))
endif


#COMMANDS

all:$(TARGET)

.PHONY: clean

%.c: %.prg
	$(HB_BIN_INSTALL)/harbour -I$(HB_INC_INSTALL) $(INCLUDE) $(PRG_USR) -n -w2 $<

%.o: %.c
	$(CC) -c -I$(HB_INC_INSTALL) $(INCLUDE) $(C_USR) $<

$(TARGET): $(OBJECTS)
ifeq ( lib , $(patsubst %.a, lib, $(TARGET)))
	$(LINKER) -r $(TARGET) $(OBJECTS)
	$(LIBRARIAN) $(TARGET)
else
	$(CC) -o $(TARGET) $(OBJECTS) -L$(HB_LIB_INSTALL) $(LIBDIR) $(LIBS) $(LIBFILES) \
		-ldebug $(MTLIBS) -lmacro  -lpp  -llang  -lcommon\
		$(GT_LIBS) -lm 
endif

clean:
	rm -f *.o
	rm -f *.c
	rm -f *~
	rm -f $(TARGET)

install: $(TARGET)
ifeq ( lib , $(patsubst %.a, lib, $(TARGET)))
	cp -f $(TARGET) $(HB_LIB_INSTALL)
else
	cp -f $(TARGET) $(HB_BIN_INSTALL)
endif

