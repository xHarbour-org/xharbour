OBJDIR       = .
APPEXE       = $(PRGFILE).exe
COBJFLAGS    = -DHB_FM_STATISTICS_OFF -O2 -OS -6 -c -tW -I$(HRB_DIR)\INCLUDE -d -w3
HARBOURFLAGS = -n -i$(HRB_DIR)\include;c:\borland\bcc55\include;c:\curl\include\curl -w1
INCLUDEDIR   = $(HRB_DIR)\include
CC           = $(COMPILERDIR)\bin\bcc32.exe
ILINK_EXE    = $(COMPILERDIR)\bin\ilink32.exe
COMPILER     = $(HRB_DIR)\bin\55\harbour.exe
HARBOURLIB   = $(HRB_DIR)\lib\55
MAINWIN      = $(HRB_DIR)\SOURCE\VM\MAINWIN.C

$(APPEXE) : $(OBJDIR)\$(PRGFILE).obj \
            $(OBJDIR)\mainwin.obj \
            $(HARBOURLIB)\harbour.lib

   echo $(OBJDIR)\$(PRGFILE).obj + > b32.bc
   echo $(OBJDIR)\mainwin.obj + >> b32.bc
   echo $(COMPILERDIR)\lib\c0w32.obj, + >> b32.bc
   echo $(APPEXE),, + >> b32.bc
   echo $(HARBOURLIB)\harbour.lib + >> b32.bc
   echo $(COMPILERDIR)\lib\ws2_32.lib + >> b32.bc
   echo $(COMPILERDIR)\lib\cw32.lib + >> b32.bc
   echo $(COMPILERDIR)\lib\cw32.lib + >> b32.bc
   echo $(COMPILERDIR)\lib\import32.lib, >> b32.bc
   @$(ILINK_EXE) -ap -Tpe -Gn @b32.bc
   @delbat

$(OBJDIR)\$(PRGFILE).c : hnews.ch $(PRGFILE).prg
   $(COMPILER) $(HARBOURFLAGS) $** -o$@

$(OBJDIR)\$(PRGFILE).obj : $(OBJDIR)\$(PRGFILE).c
   $(CC) $(COBJFLAGS) -o$@ $**

$(OBJDIR)\mainwin.obj : $(MAINWIN)
   $(CC) $(COBJFLAGS) -o$@ $**
