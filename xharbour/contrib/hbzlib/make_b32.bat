@echo off
rem
rem $Id: make_b32.bat,v 1.3 2003/09/12 20:17:25 paultucker Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if not exist obj md obj
if not exist obj\b32 md obj\b32
if not exist lib md lib
if not exist lib\b32 md lib\b32

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy lib\b32\hbzip.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   goto EXIT

:EXIT

