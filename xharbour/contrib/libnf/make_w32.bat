@echo off
rem
rem $Id: make_w32.bat,v 1.1 2004/12/26 17:42:43 ptsarenko Exp $
rem

if not exist obj md obj
if not exist obj\w32 md obj\w32

:BUILD

   wmake -h -ms -f makefile.wc %1 %2 %3 > make_w32.log
   if "%1" == "clean" goto CLEAN
   if "%1" == "CLEAN" goto CLEAN
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\w32\libnf.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_w32.log
   goto EXIT

:CLEAN
   if exist make_w32.log del make_w32.log > nul

:EXIT

