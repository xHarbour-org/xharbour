@echo off
rem
rem $Id: make_b32.bat,v 1.1 2005/01/22 15:21:11 lf_sfnet Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\wvtgui.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\wvtgui.lib   del ..\..\lib\b32\wvtgui.lib
   if exist ..\..\lib\b32\wvtgui.bak   del ..\..\lib\b32\wvtgui.bak
   if exist ..\..\obj\b32\wvtcore.obj    del ..\..\obj\b32\wvtcore.obj
   if exist ..\..\obj\b32\wvtpaint.obj    del ..\..\obj\b32\wvtpaint.obj
   if exist ..\..\obj\b32\wvtclass.obj    del ..\..\obj\b32\wvtclass.obj
   if exist ..\..\obj\b32\wvtutils.obj    del ..\..\obj\b32\wvtutils.obj

   goto EXIT

:EXIT

