@echo off
rem
rem $Id: make_b32.bat,v 1.2 2001/11/12 23:24:38 lculik Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fziparchive.mak %1 %2 %3 > make_b32.log
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

