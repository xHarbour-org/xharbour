# xHarbour project
# Demo makefile for GNU MAKE in tests/ subdir
#
# $Id: Makefile,v 1.17 2003/11/15 16:55:18 jonnymind Exp $
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
# and HB_MULTI_GT=yes if you use multi GT libs
#

ifeq ($(HB_MT),MT)
   MTLIBS= -lvmmt -lrtlmt -lrddmt -lrtlmt -lvmmt -ldbfntx -ldbfcdx -ldbfdbt -lmacromt
   ifeq ($(HB_ARCHITECTURE),linux)
      MTLIBS+=-lpthread
   endif
else
   MTLIBS= -lvm -lrtl -lrdd -lrtl -lvm -ldbfntx -ldbfcdx -ldbfdbt -lmacro
endif

ifeq ($(HB_ARCHITECTURE),linux)
   ifeq ($(HB_GT_LIB),gtQTc)
      GT_LIBS=-lgtQTc -lqt-mt -L/usr/lib/qt3/lib
   else
     ifeq ($(HB_GT_LIB),gtsln)
       GT_LIBS=-lgtsln -lslang -lgpm
     else
       ifeq ($(HB_GT_LIB),gtcgi)
          GT_LIBS=-lgtcgi
       else
          GT_LIBS=-lgtcrs -lncurses -lgpm
       endif
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
ifeq ($(HB_MULTI_GT),yes)
   GT_LIBS += -lgtnul
endif

ALL_LIBS = -ldebug $(MTLIBS) -lpp -llang -lcommon $(GT_LIBS) -lm
ifeq ($(HB_ARCHITECTURE),darwin)
   LD_LIBS = $(ALL_LIBS) $(ALL_LIBS)
else
   LD_LIBS = -Wl,--start-group $(ALL_LIBS) -Wl,--end-group
endif

%.c: %.prg
	$(HB_BIN_INSTALL)/harbour -I$(HB_INC_INSTALL) $(PRG_USR) -n -w2 -es2 $<
%.o: %.c
	gcc -I$(HB_INC_INSTALL) -c -o $@ $<


%$(EXE_EXT): %.o
	gcc -o $@ $< $(C_USR) -I$(HB_INC_INSTALL) -L$(HB_LIB_INSTALL) \
		$(LD_LIBS)

