OBJDIR       = .
CAEXE        = $(PRGFILE).exe
COBJFLAGS    = -DHB_FM_STATISTICS_OFF -DHB_NO_DEFAULT_API_MACROS -a8 -O2 -OS -5 -6 -c -tWM -I$(HRB_DIR)\INCLUDE -L$(BCC_DIR)\LIB -d -w3
HARBOURFLAGS = -n -i$(HRB_DIR)\include -dHARBOUR -w1 -d__GLOBAL__
CC           = $(BCC_DIR)\bin\bcc32.exe
BRC_EXE      = $(BCC_DIR)\bin\brc32.exe
ILINK_EXE    = $(BCC_DIR)\bin\ilink32.exe
COMPILER     = $(HRB_DIR)\bin\harbour.exe
HARBOURLIB   = $(HRB_DIR)\lib\

$(CAEXE) : $(OBJDIR)\$(PRGFILE).obj \
           $(OBJDIR)\mainwin.obj \
           $(HARBOURLIB)\harbour.lib

   echo $(OBJDIR)\$(PRGFILE).obj + > b32.bc
   echo $(OBJDIR)\mainwin.obj + >> b32.bc
   echo $(BCC_DIR)\lib\c0w32.obj, + >> b32.bc
   echo $(CAEXE),, + >> b32.bc
   echo $(HARBOURLIB)\harbour.lib + >> b32.bc
   echo $(BCC_DIR)\lib\cw32.lib + >> b32.bc
   echo $(BCC_DIR)\lib\import32.lib, >> b32.bc
   @$(ILINK_EXE) -ap -Tpe -Gn @b32.bc
   
$(OBJDIR)\$(PRGFILE).c : $(PRGFILE).prg
   $(COMPILER) $(HARBOURFLAGS) $** -o$@

$(OBJDIR)\$(PRGFILE).obj : $(OBJDIR)\$(PRGFILE).c
   $(CC) $(COBJFLAGS) -o$@ $**

$(OBJDIR)\mainwin.obj : $(HRB_DIR)\source\vm\mainwin.c
   $(CC) $(COBJFLAGS) -o$@ $**
