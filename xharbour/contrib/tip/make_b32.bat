@echo off
rem
rem $Id: make_b32.bat,v 1.3 2003/09/23 21:23:56 paultucker Exp $
rem

if not exist obj\ md obj 
if not exist obj\b32\ md obj\b32

:BUILD
   make -fmakefile.bc %1 %2 %3 %4 %5 > make_b32.log
   if "%1" == "clean" goto CLEAN
   if "%1" == "CLEAN" goto CLEAN
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy ..\..\lib\b32\hbtip.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist make_b32.log del make_b32.log

:EXIT
