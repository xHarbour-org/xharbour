@echo off
rem
rem $Id: make_b32.bat,v 1.2 2002/08/15 04:47:06 ronpinkas Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\gtwvt.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\gtwvts.lib   del ..\..\lib\b32\gtwvt.lib
   if exist ..\..\lib\b32\gtwvt.bak   del ..\..\lib\b32\gtwvt.bak
   if exist ..\..\obj\b32\wvtcore.obj    del ..\..\obj\b32\wvtcore.obj
   if exist ..\..\obj\b32\wvtpaint.obj    del ..\..\obj\b32\wvtpaint.obj
   if exist ..\..\obj\b32\wvtclass.obj    del ..\..\obj\b32\wvtclass.obj
   if exist ..\..\obj\b32\wvtutils.obj    del ..\..\obj\b32\wvtutils.obj

   goto EXIT

:EXIT

