@echo off
rem
rem $Id: make_vc.bat,v 1.1 2003/08/20 15:03:51 lculik Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake -fziparchive.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy release\ziparchive.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN

   del release\*.obj
   goto EXIT

:EXIT

