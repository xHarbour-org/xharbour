@echo off
rem
rem $Id: make_w32.bat,v 1.15 2004/03/05 08:27:07 mlombardo Exp $
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

SET BISON_SIMPLE=c:\windows\bison.simple
SET _LIB=%LIB%
SET _PATH=%PATH%
SET _INCLUDE=%INCLUDE%
rem Please set up WatCom Directives accordingly
SET PATH=C:\watcom\BINNT;C:\watcom\BINW;%_PATH%
SET LIB=C:\watcom\LIB386;C:\watcom\LIB386\NT;%_LIB%
SET WATCOM=C:\watcom
SET EDPATH=C:\watcom\EDDAT
SET INCLUDE=C:\watcom\H;C:\watcom\H\NT

if not exist obj md obj
if not exist obj\w32 md obj\w32
if not exist obj\w32\mt md obj\w32\mt

rem added optimize subdir for optimized library
rem start in build 81
if not exist obj\w32\opt md obj\w32\opt
if not exist obj\w32\opt\console md obj\w32\opt\console
if not exist obj\w32\opt\gui md obj\w32\opt\gui
if not exist obj\w32\mt\opt md obj\w32\mt\opt
if not exist obj\w32\mt\opt\console md obj\w32\mt\opt\console
if not exist obj\w32\mt\opt\gui md obj\w32\mt\opt\gui

if not exist lib md lib
if not exist lib\w32 md lib\w32

if not exist bin md bin
if not exist bin\w32 md bin\w32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   SET HB_MT=
   wmake -h -ms -f makefile.wc %1 %2 %3 > make_w32.log
   wmake -h -ms HB_THREAD_SUPPORT=1 -f makefile.wc %2 %3 >> make_w32.log

   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\w32\*.exe bin\*.* > nul
   copy lib\w32\*.lib lib\*.* > nul
   if exist lib\w32\*.bak del lib\w32\*.bak
   goto EXIT

:BUILD_ERR

   notepad make_w32.log
   goto EXIT

:CLEAN

   if exist bin\w32\*.exe del bin\w32\*.exe
   if exist bin\w32\*.tds del bin\w32\*.tds
   if exist bin\w32\*.map del bin\w32\*.map

rem   if exist lib\*.lib     del lib\*.lib

   if exist lib\codepage.lib   del lib\codepage.lib
   if exist lib\common.lib     del lib\common.lib
   if exist lib\dbfcdx.lib     del lib\dbfcdx.lib
   if exist lib\dbfcdxmt.lib   del lib\dbfcdxmt.lib
   if exist lib\dbfdbt.lib     del lib\dbfdbt.lib
   if exist lib\dbfdbtmt.lib   del lib\dbfdbtmt.lib
   if exist lib\dbffpt.lib     del lib\dbffpt.lib
   if exist lib\dbffptmt.lib   del lib\dbffptmt.lib
   if exist lib\dbfntx.lib     del lib\dbfntx.lib
   if exist lib\dbfntxmt.lib   del lib\dbfntxmt.lib
   if exist lib\debug.lib      del lib\debug.lib
   if exist lib\gtcgi.lib      del lib\gtcgi.lib
   if exist lib\gtnul.lib      del lib\gtnul.lib
   if exist lib\gtpca.lib      del lib\gtpca.lib
   if exist lib\gtstd.lib      del lib\gtstd.lib
   if exist lib\gtwin.lib      del lib\gtwin.lib
   if exist lib\gtwvt.lib      del lib\gtwvt.lib
   if exist lib\hbodbc.lib     del lib\hbodbc.lib
   if exist lib\hbodbcmt.lib   del lib\hbodbcmt.lib
   if exist lib\lang.lib       del lib\lang.lib
   if exist lib\macro.lib      del lib\macro.lib
   if exist lib\macromt.lib    del lib\macromt.lib
   if exist lib\nulsys.lib     del lib\nulsys.lib
   if exist lib\optcon.lib     del lib\optcon.lib
   if exist lib\optconmt.lib   del lib\optconmt.lib
   if exist lib\optgui.lib     del lib\optgui.lib
   if exist lib\optguimt.lib   del lib\optguimt.lib
   if exist lib\pp.lib         del lib\pp.lib
   if exist lib\ppmt.lib       del lib\ppmt.lib
   if exist lib\rdd.lib        del lib\rdd.lib
   if exist lib\rddmt.lib      del lib\rddmt.lib
   if exist lib\rtl.lib        del lib\rtl.lib
   if exist lib\rtlmt.lib      del lib\rtlmt.lib
   if exist lib\samples.lib    del lib\samples.lib
   if exist lib\samplesmt.lib  del lib\samplesmt.lib
   if exist lib\vm.lib         del lib\vm.lib
   if exist lib\vmmt.lib       del lib\vmmt.lib

   if exist lib\*.dll     del lib\*.dll

   if exist lib\*.bak     del lib\*.bak
   if exist lib\*.obj     del lib\*.obj
   if exist lib\w32\bcc640.lib copy lib\w32\bcc640.lib lib >nul
   if exist lib\w32\*.lib del lib\w32\*.lib
   if exist lib\bcc640.lib copy lib\bcc640.lib lib\w32 >nul
   if exist lib\w32\*.bak del lib\w32\*.bak
   if exist lib\w32\*.obj del lib\w32\*.obj

   if exist obj\w32\*.obj del obj\w32\*.obj
   if exist obj\w32\*.c   del obj\w32\*.c
   if exist obj\w32\*.h   del obj\w32\*.h

   if exist obj\w32\mt\*.obj del obj\w32\mt\*.obj
   if exist obj\w32\mt\*.c   del obj\w32\mt\*.c

   if exist obj\w32\opt\console\*.obj del obj\w32\opt\console\*.obj
   if exist obj\w32\opt\gui\*.obj del obj\w32\opt\gui\*.obj
   if exist obj\w32\mt\opt\console\*.obj del obj\w32\mt\opt\console\*.obj
   if exist obj\w32\mt\opt\gui\*.obj del obj\w32\mt\opt\gui\*.obj

   if exist make_w32.log  del make_w32.log

:EXIT
SET LIB=%_LIB%
SET PATH=%_PATH%
SET INCLUDE=%_INCLUDE%
SET _LIB=
SET _PATH=
SET _INCLUDE=
