@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\telepath.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist ..\..\lib\b32\telepath.bak	del ..\..\lib\b32\telepath.bak
   if exist ..\..\lib\b32\telepath.lib  del ..\..\lib\b32\telepath.lib
   if exist ..\..\obj\b32\telepath.obj	del ..\..\obj\b32\telepath.obj
   if exist ..\..\obj\b32\telepath.c  	del ..\..\obj\b32\telepath.c
   if exist ..\..\obj\b32\win32.obj    	del ..\..\obj\b32\win32.obj
   goto EXIT

:EXIT

