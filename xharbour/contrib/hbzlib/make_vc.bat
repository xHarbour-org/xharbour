@echo off
rem
rem $Id: make_b32.bat,v 1.1 2003/02/14 02:29:10 lculik Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake -fziparchive.vc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy ziparchive.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   goto EXIT

:EXIT

