OBJDIR       = .
APPEXE       = $(PRGFILE).exe
COBJFLAGS    = -DHB_FM_STATISTICS_OFF -O2 -OS -6 -c -tW -I..\INCLUDE -d -w3
HARBOURFLAGS = -n -i..\include -w1
INCLUDEDIR   = ..\include
ifeq ($(CC),)
CC           = bcc32.exe
endif
ILINK_EXE    = ilink32.exe
COMPILER     = ..\bin\%SUB_DIR%\harbour.exe
HARBOURLIB   = ..\lib\%SUB_DIR%
MAINWIN      = ..\SOURCE\VM\MAINWIN.C

$(APPEXE) : $(OBJDIR)\$(PRGFILE).obj \
            $(OBJDIR)\mainwin.obj \
            $(HARBOURLIB)\harbour.lib

   echo $(OBJDIR)\$(PRGFILE).obj + > b32.bc
   echo $(OBJDIR)\mainwin.obj + >> b32.bc
   echo c0w32.obj, + >> b32.bc
   echo $(APPEXE),, + >> b32.bc
   echo $(HARBOURLIB)\harbour.lib + >> b32.bc
   echo ws2_32.lib + >> b32.bc
   echo cw32.lib + >> b32.bc
   echo import32.lib, >> b32.bc
   @$(ILINK_EXE) -ap -Tpe -Gn -L$(BCC_LIB) @b32.bc

$(OBJDIR)\$(PRGFILE).c : $(PRGFILE).prg
   $(COMPILER) $(HARBOURFLAGS) $** -o$@

$(OBJDIR)\$(PRGFILE).obj : $(OBJDIR)\$(PRGFILE).c
   $(CC) $(COBJFLAGS) -o$@ $**

$(OBJDIR)\mainwin.obj : $(MAINWIN)
   $(CC) $(COBJFLAGS) -o$@ $**
