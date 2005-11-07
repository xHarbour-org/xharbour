OBJDIR       = .
HB_DIR       = ..\..\..
LIB_DIR      = ..
APPEXE       = $(PRGFILE).exe
COBJFLAGS    = -DHB_FM_STATISTICS_OFF -O2 -OS -6 -c -tW -I$(LIB_DIR)\INCLUDE;$(HB_DIR)\INCLUDE -d -w3
HARBOURFLAGS = -n -i$(LIB_DIR)\include;$(HB_DIR)\INCLUDE -w1
CC           = bcc32.exe
ILINK_EXE    = ilink32.exe
COMPILER     = $(HB_DIR)\bin\harbour.exe
HARBOURLIB   = $(HB_DIR)\lib
MAINWIN      = $(HB_DIR)\SOURCE\VM\MAINWIN.C

$(APPEXE) : $(OBJDIR)\$(PRGFILE).obj \
            $(OBJDIR)\mainwin.obj \
            $(HARBOURLIB)\harbour.lib

   echo $(OBJDIR)\$(PRGFILE).obj + > b32.bc
   echo $(OBJDIR)\mainwin.obj + >> b32.bc
   echo c0w32.obj, + >> b32.bc
   echo $(APPEXE),, + >> b32.bc
   echo $(HARBOURLIB)\harbour.lib + >> b32.bc
   echo $(LIB_DIR)\lib\fi_lib.lib + >> b32.bc
   echo $(LIB_DIR)\lib\libfi.lib + >> b32.bc
   echo ws2_32.lib + >> b32.bc
   echo cw32.lib + >> b32.bc
   echo import32.lib, >> b32.bc
   @$(ILINK_EXE) -ap -Tpe -Gn @b32.bc

$(OBJDIR)\$(PRGFILE).c : $(PRGFILE).prg
   $(COMPILER) $(HARBOURFLAGS) $** -o$@

$(OBJDIR)\$(PRGFILE).obj : $(OBJDIR)\$(PRGFILE).c
   $(CC) $(COBJFLAGS) -o$@ $**

$(OBJDIR)\mainwin.obj : $(MAINWIN)
   $(CC) $(COBJFLAGS) -o$@ $**
