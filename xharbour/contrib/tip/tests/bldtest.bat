@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD
   mkdir obj
   make -fmakefile.bc %1 %2 %3 %4 %5 > bldtest.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   del *.tds
   goto EXIT

:BUILD_ERR
   notepad bldtest.log
   goto EXIT

:CLEAN

   del *.exe
   del *.tds
   del *.obj
   goto EXIT

:EXIT

