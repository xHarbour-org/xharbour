@echo off
rem
rem $Id:  $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\gtwvw.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist ..\..\lib\b32\gtwvw.bak	del ..\..\lib\b32\gtwvw.bak
   if exist ..\..\lib\b32\gtwvw.lib  del ..\..\lib\b32\gtwvw.lib
   if exist ..\..\obj\b32\gtwvw.obj	del ..\..\obj\b32\gtwvw.obj
   if exist ..\..\obj\b32\gtwvw.c  	del ..\..\obj\b32\gtwvw.c
   goto EXIT

:EXIT

