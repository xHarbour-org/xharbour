@echo off
rem
rem $Id: make_b32.bat,v 1.4 2003/09/12 21:04:44 paultucker Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if not exist obj md obj
if not exist obj\b32 md obj\b32

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy ..\..\lib\b32\hbzip.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   goto EXIT

:EXIT

