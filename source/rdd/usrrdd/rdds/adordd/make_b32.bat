@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\%SUB_DIR%\adordd.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\%SUB_DIR%\adordd.bak del ..\..\lib\%SUB_DIR%\adordd.bak
   goto EXIT

:BUILD_ERR

   notepad make_%SUB_DIR%.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\%SUB_DIR%\adordd.lib   del ..\..\lib\%SUB_DIR%\adordd.lib
   if exist ..\..\lib\%SUB_DIR%\adordd.bak  del ..\..\lib\%SUB_DIR%\adordd.bak

   goto EXIT

:EXIT