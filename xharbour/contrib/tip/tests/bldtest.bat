@echo off
rem
rem $Id: bldtest.bat,v 1.1 2003/02/22 21:26:52 jonnymind Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD
   if not exist obj mkdir obj
   if exist %1.exe del %1.exe
   if exist obj\%1.* del obj\%1.*
   make -fmakefile.bc -DTARGET=%1 > bldtest.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   if exist *.tds del *.tds
   if exist *.map del *.map
   goto EXIT

:BUILD_ERR
   notepad bldtest.log
   goto EXIT

:CLEAN

   if exist *.exe del *.exe
   if exist *.tds del *.tds
   if exist *.obj del *.obj
   goto EXIT

:EXIT

