@echo off
rem
rem $Id: make_b32.bat,v 1.28 2006/11/06 10:20:14 marchuet Exp $
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist obj md obj
if not exist obj\b32 md obj\b32
if not exist obj\b32\mt md obj\b32\mt
if not exist obj\b32\fmstat md obj\b32\fmstat
if not exist obj\b32\mt\fmstat md obj\b32\mt\fmstat

if not exist lib md lib
if not exist lib\b32 md lib\b32

if not exist bin md bin
if not exist bin\b32 md bin\b32

:BUILD
   make -l EXE_OBJ_DIR=obj\b32\bin OBJ_DIR=obj\b32 -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR
   make -l OBJ_DIR=obj\b32\mt -DHB_THREAD_SUPPORT -DHB_MT=mt -fmakefile.bc %2 %3 >> make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\b32\*.exe bin\*.* > nul
   copy lib\b32\*.lib lib\*.* > nul
   if exist lib\b32\*.bak del lib\b32\*.bak
   if exist bin\b32\*.tds del bin\b32\*.tds
   if exist bin\b32\*.map del bin\b32\*.map
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist bin\xharbour.exe   del bin\xharbour.exe
   if exist bin\xharbour.dll   del bin\xharbour.dll
   if exist bin\harbour.exe    del bin\harbour.exe
   if exist bin\hbdoc.exe      del bin\hbdoc.exe
   if exist bin\hbmake.exe     del bin\hbmake.exe
   if exist bin\hbpp.exe       del bin\hbpp.exe
   if exist bin\hbrun.exe      del bin\hbrun.exe
   if exist bin\hbrunmt.exe    del bin\hbrunmt.exe
   if exist bin\hbtest.exe     del bin\hbtest.exe
   if exist bin\hbtestmt.exe   del bin\hbtestmt.exe
   if exist bin\xbscript.exe   del bin\xbscript.exe

   if exist lib\xharbour.lib   del lib\xharbour.lib
   if exist lib\codepage.lib   del lib\codepage.lib
   if exist lib\common.lib     del lib\common.lib
   if exist lib\ct.lib         del lib\ct.lib
   if exist lib\ctmt.lib       del lib\ctmt.lib
   if exist lib\dbfcdx.lib     del lib\dbfcdx.lib
   if exist lib\dbfcdxmt.lib   del lib\dbfcdxmt.lib
   if exist lib\bmdbfcdx.lib   del lib\bmdbfcdx.lib
   if exist lib\bmdbfcdxmt.lib del lib\bmdbfcdxmt.lib
   if exist lib\dbfdbt.lib     del lib\dbfdbt.lib
   if exist lib\dbfdbtmt.lib   del lib\dbfdbtmt.lib
   if exist lib\dbffpt.lib     del lib\dbffpt.lib
   if exist lib\dbffptmt.lib   del lib\dbffptmt.lib
   if exist lib\dbfntx.lib     del lib\dbfntx.lib
   if exist lib\dbfntxmt.lib   del lib\dbfntxmt.lib
   if exist lib\debug.lib      del lib\debug.lib
   if exist lib\dllmain.lib    del lib\dllmain.lib
   if exist lib\fmstat.lib     del lib\fmstat.lib
   if exist lib\fmstatmt.lib   del lib\fmstatmt.lib
   if exist lib\gtcgi.lib      del lib\gtcgi.lib
   if exist lib\gtgui.lib      del lib\gtgui.lib
   if exist lib\gtnul.lib      del lib\gtnul.lib
   if exist lib\gtpca.lib      del lib\gtpca.lib
   if exist lib\gtstd.lib      del lib\gtstd.lib
   if exist lib\gtwin.lib      del lib\gtwin.lib
   if exist lib\gtwvt.lib      del lib\gtwvt.lib
   if exist lib\hbodbc.lib     del lib\hbodbc.lib
   if exist lib\hbodbcmt.lib   del lib\hbodbcmt.lib
   if exist lib\hbsix.lib      del lib\hbsix.lib
   if exist lib\hbsixmt.lib    del lib\hbsixmt.lib
   if exist lib\hsx.lib        del lib\hsx.lib
   if exist lib\hsxmt.lib      del lib\hsxmt.lib
   if exist lib\lang.lib       del lib\lang.lib
   if exist lib\libmisc.lib    del lib\libmisc.lib
   if exist lib\macro.lib      del lib\macro.lib
   if exist lib\macromt.lib    del lib\macromt.lib
   if exist lib\nulsys.lib     del lib\nulsys.lib
   if exist lib\optcon.lib     del lib\optcon.lib
   if exist lib\optconmt.lib   del lib\optconmt.lib
   if exist lib\optgui.lib     del lib\optgui.lib
   if exist lib\optguimt.lib   del lib\optguimt.lib
   if exist lib\pcrepos.lib    del lib\pcrepos.lib
   if exist lib\pp.lib         del lib\pp.lib
   if exist lib\ppmt.lib       del lib\ppmt.lib
   if exist lib\rdd.lib        del lib\rdd.lib
   if exist lib\rddmt.lib      del lib\rddmt.lib
   if exist lib\rdds.lib       del lib\rdds.lib
   if exist lib\rddsmt.lib     del lib\rddsmt.lib
   if exist lib\rtl.lib        del lib\rtl.lib
   if exist lib\rtlmt.lib      del lib\rtlmt.lib
   if exist lib\samples.lib    del lib\samples.lib
   if exist lib\samplesmt.lib  del lib\samplesmt.lib
   if exist lib\sixcdx.lib     del lib\sixcdx.lib
   if exist lib\sixcdxmt.lib   del lib\sixcdxmt.lib
   if exist lib\tip.lib        del lib\tip.lib
   if exist lib\tipmt.lib      del lib\tipmt.lib
   if exist lib\usrrdd.lib     del lib\usrrdd.lib
   if exist lib\usrrddmt.lib   del lib\usrrddmt.lib
   if exist lib\vm.lib         del lib\vm.lib
   if exist lib\vmmt.lib       del lib\vmmt.lib

   if exist lib\*.dll          del lib\*.dll

   if exist lib\*.bak          del lib\*.bak
   if exist lib\*.obj          del lib\*.obj

   if exist make_b32.log       del make_b32.log

   REM *** CLEAN ALL TEMP FOLDERS\FILES ***

   if exist bin\*.tds del bin\*.tds
   if exist bin\*.map del bin\*.map

   if exist bin\b32\*.exe del bin\b32\*.exe
   if exist bin\b32\*.tds del bin\b32\*.tds
   if exist bin\b32\*.map del bin\b32\*.map
   if exist bin\b32\*.dll del bin\b32\*.dll
   if exist bin\b32\*.lib del bin\b32\*.lib

   if exist lib\b32\*.bak    del lib\b32\*.bak
   if exist lib\b32\*.obj    del lib\b32\*.obj
   if exist lib\b32\*.map    del lib\b32\*.map
   if exist lib\b32\*.lib    del lib\b32\*.lib

   if exist obj\b32\*.bak    del obj\b32\*.bak
   if exist obj\b32\*.obj    del obj\b32\*.obj
   if exist obj\b32\*.output del obj\b32\*.output
   if exist obj\b32\*.c      del obj\b32\*.c
   if exist obj\b32\*.h      del obj\b32\*.h

   if exist obj\b32\bin\*.bak    del obj\b32\bin\*.bak
   if exist obj\b32\bin\*.obj    del obj\b32\bin\*.obj
   if exist obj\b32\bin\*.output del obj\b32\bin\*.output
   if exist obj\b32\bin\*.c      del obj\b32\bin\*.c
   if exist obj\b32\bin\*.h      del obj\b32\bin\*.h

   if exist obj\b32\mt\*.obj     del obj\b32\mt\*.obj
   if exist obj\b32\mt\*.output  del obj\b32\mt\*.output
   if exist obj\b32\mt\*.c       del obj\b32\mt\*.c
   if exist obj\b32\mt\*.h       del obj\b32\mt\*.h

   if exist obj\b32\fmstat\*.obj    del obj\b32\fmstat\*.obj
   if exist obj\b32\mt\fmstat\*.obj del obj\b32\mt\fmstat\*.obj

   rem remove obsolete optgui and optcon directories
   if exist obj\b32\opt\console\*.obj    del obj\b32\opt\console\*.obj
   if exist obj\b32\opt\gui\*.obj        del obj\b32\opt\gui\*.obj
   if exist obj\b32\mt\opt\console\*.obj del obj\b32\mt\opt\console\*.obj
   if exist obj\b32\mt\opt\gui\*.obj     del obj\b32\mt\opt\gui\*.obj
   if exist obj\b32\opt\console\nul      rd obj\b32\opt\console
   if exist obj\b32\opt\gui\nul          rd obj\b32\opt\gui
   if exist obj\b32\opt\nul              rd obj\b32\opt
   if exist obj\b32\mt\opt\console\nul   rd obj\b32\mt\opt\console
   if exist obj\b32\mt\opt\gui\nul       rd obj\b32\mt\opt\gui
   if exist obj\b32\mt\opt\nul           rd obj\b32\mt\opt

:EXIT
