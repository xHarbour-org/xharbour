rem @echo off
rem
rem $Id: make_b32.bat,v 1.1 2003/02/22 16:44:46 jonnymind Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD
   mkdir obj
   make -fmakefile.bc %1 %2 %3 %4 %5 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy hbtip.lib ..\..\lib\*.* > nul
   copy include\*.* ..\..\include\*.* > nul
   del *.tds
   del *.map
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   del ..\..\lib\hbtip.lib
   del *.tds
   del *.map
   del *.lib
   del *.obj
   del *.bak
   del obj\*.*
   goto EXIT

:EXIT

