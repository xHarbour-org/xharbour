@echo off
rem
rem $Id: make_b32.bat,v 1.2 2004/01/17 22:01:02 andijahja Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\..\lib\b32\gtalleg.lib ..\..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\gtalleg.lib   del ..\..\lib\b32\gtalleg.lib
   if exist ..\..\lib\b32\gtalleg.bak   del ..\..\lib\b32\gtalleg.bak
   if exist ..\..\obj\b32\gtalleg.obj   del ..\..\obj\b32\gtalleg.obj
   if exist ..\..\obj\b32\ssf.obj       del ..\..\obj\b32\ssf.obj

   goto EXIT

:EXIT












