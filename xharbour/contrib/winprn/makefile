

ALL_LIBS = \
    common.lib \
    vm.lib \
    rtl.lib \
    gtwvt.lib \
    lang.lib \
    rdd.lib \
    macro.lib \
    pp.lib \
    dbfcdx.lib \
    dbffpt.lib \
    dbfntx.lib \
    dbfdbt.lib \
    wininet.lib \
    import32.lib \
    cw32.lib


testprn.exe : testprn.obj tprint.obj makefile
  ilink32 -ap -Gn -Tpe testprn.obj c0w32.obj tprint.obj, testprn,, $(ALL_LIBS)

testprn.obj : testprn.c
   bcc32 -D__WIN32__ -O2 -d -c -Ic:\xharbour\include testprn.c

tprint.obj : tprint.c
   bcc32 -D__WIN32__ -O2 -d -c -Ic:\xharbour\include tprint.c

testprn.c : testprn.prg
   harbour -D__WIN32__ -n -q -w2 /ic:\xharbour\include  testprn.prg

tprint.c : tprint.prg
   harbour -D__WIN32__ -n -q -w2 /ic:\xharbour\include tprint.prg
