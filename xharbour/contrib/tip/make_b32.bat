@echo off
rem
rem $Id: make_b32.bat,v 1.2 2003/02/22 21:26:49 jonnymind Exp $
rem

if not exist obj\ md obj 
if not exist obj\b32\ md obj\b32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD
   make -fmakefile.bc %1 %2 %3 %4 %5 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy ..\..\lib\b32\hbtip.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist ..\..\lib\hbtip.lib del ..\..\lib\hbtip.lib
   if exist ..\..\lib\b32\hbtip.lib del ..\..\lib\b32\hbtip.lib
   if exist obj\b32\*.obj del obj\b32\*.obj
   if exist obj\b32\*.c del obj\b32\*.c

:EXIT
