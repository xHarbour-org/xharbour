@echo off
rem
rem $Id: make_b32.bat,v 1.5 2003/09/23 04:52:34 paultucker Exp $
rem

if not exist obj md obj
if not exist obj\b32 md obj\b32

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR
   if "%1" == "clean" goto exit
   if "%1" == "CLEAN" goto exit

:BUILD_OK
   copy ..\..\lib\b32\hbzip.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:EXIT

