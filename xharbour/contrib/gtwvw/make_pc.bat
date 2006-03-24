@echo off
rem $Id: make_pc.bat,v 1.1 2005/11/02 18:00:21 modalsist Exp $
rem
rem make file for Pelles C/C++ compiler.
rem
rem

SET HB_DIR=C:\xHarbour
SET POCC_DIR=C:\PellesC

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET TARGET_LIB_DIR=%HB_DIR%\lib\pocc
SET TARGET_OBJ_DIR=obj\pocc

SET LIB=%POCC_DIR%\LIB;%POCC_DIR%\LIB\WIN;%HB_DIR\LIB;%LIB%
SET INCLUDE=%POCC_DIR%\INCLUDE;%POCC_DIR%\INCLUDE\WIN;%HB_DIR%\INCLUDE;%INCLUDE%
SET PATH=%POCC_DIR%\BIN;%HB_DIR%\BIN;%PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   if not exist %TARGET_OBJ_DIR% md %TARGET_OBJ_DIR%
   if not exist %TARGET_LIB_DIR% md %TARGET_LIB_DIR%

   echo Compiling gtwvw.lib

   pomake /f makefile.pc > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy %TARGET_LIB_DIR%\gtwvw.lib  %HB_DIR%\lib\gtwvw.lib > nul
   del  %TARGET_LIB_DIR%\gtwvw.lib > null
   del make_pc.log > null
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   echo Cleaning gtwvw.lib

   if exist %HB_DIR%\lib\gtwvw.lib      del %HB_DIR%\lib\gtwvw.lib

   if exist %TARGET_LIB_DIR%\gtwvw.lib  del %TARGET_LIB_DIR%\gtwvw.lib
   if exist %TARGET_LIB_DIR%\gtwvw.bak  del %TARGET_LIB_DIR%\gtwvw.bak

   if exist %TARGET_OBJ_DIR%\gtwvw.obj  del %TARGET_OBJ_DIR%\gtwvw.obj
   if exist %TARGET_OBJ_DIR%\gtwvw.bak  del %TARGET_OBJ_DIR%\gtwvw.bak
   if exist %TARGET_OBJ_DIR%\gtwvw.c    del %TARGET_OBJ_DIR%\gtwvw.c

   goto EXIT

:EXIT

SET HB_DIR=
SET POCC_DIR=

SET TARGET_LIB_DIR=
SET TARGET_OBJ_DIR=

SET PATH=%_PATH%
SET LIB=%_LIB%
SET INCLUDE=%_INCLUDE%

SET _PATH=
SET _LIB=
SET _INCLUDE=
