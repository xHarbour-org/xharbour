@echo off
rem
rem $Id: make_b32.bat,v 1.1 2007/04/27 08:44:37 fmancera Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\adordd.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\adordd.bak del ..\..\lib\b32\adordd.bak
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\adordd.lib   del ..\..\lib\b32\adordd.lib
   if exist ..\..\lib\b32\adordd.bak  del ..\..\lib\b32\adordd.bak

   goto EXIT

:EXIT