# xHarbour project
# Makefile for hbdict
#
# $Id: Makefile,v 1.1 2003/06/21 06:59:23 jonnymind Exp $
#
# Copyright 2003 Giancarlo Niccolai
#
#

ifeq ($(HB_MT),MT)
MTLIBS= -lvmmt -lrtlmt -lrddmt -lrtlmt -lvmmt -ldbfntxmt -ldbfcdxmt -lmacromt
ifeq ($(HB_ARCHITECTURE),linux)
MTLIBS+=-lpthread
endif
else
  MTLIBS= -lvm -lrtl -lrdd -lrtl -lvm -ldbfntx -ldbfcdx -lmacro
endif

ifeq ($(HB_ARCHITECTURE),linux)
   ifeq ($(HB_GT_LIB),gtQTc)
      GT_LIBS=-lgtQTc -lqt-mt -L/usr/lib/qt3/lib
   else
     ifeq ($(HB_GT_LIB),gtcrs)
       GT_LIBS=-lgtcrs -lncurses -lgpm
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
ifeq ($(HB_MULTI_GT),yes)
GT_LIBS += -lgtnul
endif

all: hbdict

%.c: %.prg
	$(HB_BIN_INSTALL)/harbour -I$(HB_INC_INSTALL) $(PRG_USR) -n -j $<
%.o: %.c
	gcc -I$(HB_INC_INSTALL) -c -o $@ $<


%$(EXE_EXT): %.o
	gcc -o $@ $< $(C_USR) -I$(HB_INC_INSTALL) -L$(HB_LIB_INSTALL) \
		-Wl,--start-group \
			-ldebug $(MTLIBS) -lpp -llang -lcommon \
			$(GT_LIBS) -lm \
		-Wl,--end-group

clean:
	rm -f hbdict *~ *.hil *.o *.log

.PHONY: clean
