
ifeq ($(HB_MT),MT)
  MTLIBS= -lvmmt -lrtlmt -lrddmt -lrtlmt -lvmmt -ldbfntxmt -ldbfcdxmt 
else
  MTLIBS= -lvm -lrtl -lrdd -lrtl -lvm -ldbfntx -ldbfcdx
endif

ifeq ($(HB_ARCHITECTURE),linux)
  GT_LIBS=-lgtsln -lslang -lgpm -lpthread
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
	harbour -I$(HB_INC_INSTALL) $(PRG_USR) -n $<
%.o: %.c
		gcc -I$(HB_INC_INSTALL) -c -o $@ $< 
	

%$(EXE_EXT): %.o
	gcc -o $@ $< $(C_USR) -I$(HB_INC_INSTALL) -L$(HB_LIB_INSTALL) \
		-ldebug $(MTLIBS) -lmacro  -lpp  -llang  -lcommon\
		$(GT_LIBS) -lm\
   
