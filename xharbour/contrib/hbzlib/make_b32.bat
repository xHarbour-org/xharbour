@echo off
rem
rem $Id: make_b32.bat,v 1.1 2003/02/14 02:29:10 lculik Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if not exist obj md obj
if not exist obj\b32 md obj\b32
if not exist lib md lib
if not exist lib\b32 md lib\b32

:BUILD

   make -fziparchive.mak %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy lib\b32\ziparchive.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   goto EXIT

:EXIT

