@echo off
rem
rem $Id: bldtest.bat,v 1.1 2003/02/22 21:26:52 jonnymind Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD
   mkdir obj
   make -fmakefile.bc %1 %2 %3 %4 %5 > bldtest.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   rem del *.tds
   goto EXIT

:BUILD_ERR
   notepad bldtest.log
   goto EXIT

:CLEAN

   del *.exe
   rem del *.tds
   del *.obj
   goto EXIT

:EXIT

