@echo off
rem 
rem $Id: make_b32.bat,v 1.1 2003/04/14 16:09:13 lculik Exp $
rem 

if not exist obj md obj
if not exist obj\b32 md obj\b32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\libmisc.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   goto EXIT

:EXIT

