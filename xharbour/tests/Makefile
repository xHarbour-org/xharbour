
ifeq ($(HB_MT),MT)
MTLIBS= -lvmmt -lrtlmt -lrddmt -lrtlmt -lvmmt -ldbfntxmt -ldbfcdxmt 
else
MTLIBS= -lvm -lrtl -lrdd -lrtl -lvm -ldbfntx -ldbfcdx
endif

ifeq ($(HB_ARCHITECTURE),linux)
GT_LIBS=-lgtsln -lslang -lgpm -lpthread
EXE_EXT=
else
GT_LIBS=-lgtwin -lwsock32 -lwinspool
EXE_EXT=.exe
endif

%.c: %.prg
	harbour -I$(HB_INC_INSTALL) -n $<
%.o: %.c
		gcc -I$(HB_INC_INSTALL) -c -o $@ $< 
	

%$(EXE_EXT): %.o
	gcc -o $@ $< -I$(HB_INC_INSTALL) -L$(HB_LIB_INSTALL) \
		-ldebug $(MTLIBS) -lmacro  -lpp  -llang  -lcommon\
		$(GT_LIBS) -lm\
   
