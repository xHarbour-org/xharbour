# xHarbour project
# Demo makefile for GNU MAKE in tests/ subdir
#
# $Id: Makefile,v 1.8 2003/03/14 11:22:32 jonnymind Exp $
#
# Copyright 2003 Giancarlo Niccolai
#
# To create a binary file from a single .prg, just
# launch 
#    make <target_file>
#
# where <target_file> is the name of the binary file
# to be generated. Examples:
# 
#    make test  --->to compile and link test from test.prg
#    make test.exe ---> under windows
#
# Remember to set HB_MT=MT if you want to compile
# programs using Multithreading aware libs
#

ifeq ($(HB_MT),MT)
MTLIBS= -lvmmt -lrtlmt -lrddmt -lrtlmt -lvmmt -ldbfntxmt -ldbfcdxmt
ifeq ($(HB_ARCHITECTURE),linux)
MTLIBS+=-lpthread
endif
else
  MTLIBS= -lvm -lrtl -lrdd -lrtl -lvm -ldbfntx -ldbfcdx
endif

ifeq ($(HB_ARCHITECTURE),linux)
   ifeq ($(HB_GT_LIB),gtQTc)
      GT_LIBS=-lgtQTc -lqt-mt -L/usr/lib/qt3/lib
   else
     ifeq ($(HB_GT_LIB),gtcrs)
       GT_LIBS=-lgtcrs -lncurses
     else
       GT_LIBS=-lgtsln -lslang -lgpm
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

%.c: %.prg
	$(HB_BIN_INSTALL)/harbour -I$(HB_INC_INSTALL) $(PRG_USR) -n $<
%.o: %.c
		gcc -I$(HB_INC_INSTALL) -c -o $@ $<


%$(EXE_EXT): %.o
	gcc -o $@ $< $(C_USR) -I$(HB_INC_INSTALL) -L$(HB_LIB_INSTALL) \
		-ldebug $(MTLIBS) -lmacro  -lpp  -llang  -lcommon\
		$(GT_LIBS) -lm\

