
ifeq ($(HB_MT),MT)
MTLIBS= -lvmmt -lrtlmt -lrddmt -lrtlmt -lvmmt -ldbfntxmt -ldbfcdxmt -lpthread
else
MTLIBS= -lvm -lrtl -lrdd -lrtl -lvm -ldbfntx -ldbfcdx
endif

%.o: %.prg
	harbour -n -go $<

%: %.o
	gcc -o $@ $< -I$(HB_INC_INSTALL) -L$(HB_LIB_INSTALL) \
		-ldebug $(MTLIBS) -lmacro  -lpp  -llang  -lcommon\
		-lgtsln -lslang -lgpm -lm\
   
