@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 %4 %5 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy hbtip.lib ..\..\lib\*.* > nul
   copy include\*.* ..\..\include\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   del ..\..\lib\hbtip.lib
   goto EXIT

:EXIT

