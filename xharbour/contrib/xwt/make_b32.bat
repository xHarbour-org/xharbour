@echo off
rem
rem $Id: make_b32.bat,v 1.1 2003/09/23 21:23:56 paultucker Exp $
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

if not exist obj md obj
if not exist obj\b32 md obj\b32

:BUILD

   SET HB_MT=
   echo.
   echo NOTE: Witout GTK, this lib does not serve any purpose at this time!
   echo.
   make -l -fmakefile.bc %1 %2 %3 > make_b32.log
   if "%1" == "clean" goto CLEAN
   if "%1" == "CLEAN" goto CLEAN
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\xwt.lib ..\..\lib > nul
   if exist lib\b32\*.bak del lib\b32\*.bak
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist make_b32.log  del make_b32.log

:EXIT

