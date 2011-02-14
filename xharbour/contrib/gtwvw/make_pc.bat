@echo off
rem $Id$
rem
rem make file for Pelles C/C++ compiler.
rem
rem

REM ***********************************************
REM ** Change only the two sets below, if need. ***
REM ***********************************************

SET HB_DIR=c:\xharbour
SET CC_DIR=c:\pellesc

REM ***********************************
REM ** Don't change the sets below. ***
REM ***********************************

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB_DIR=%HB_DIR%\lib\pocc
SET OBJ_DIR=obj\pocc

SET LIB=%CC_DIR%\lib;%CC_DIR%\lib\win;%HB_DIR%\lib;%LIB%
SET INCLUDE=%CC_DIR%\include;%CC_DIR%\include\win;%HB_DIR%\include;%INCLUDE%
SET PATH=%CC_DIR%\bin;%HB_DIR%\bin;%PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   if not exist %OBJ_DIR% md %OBJ_DIR%
   if not exist %LIB_DIR% md %LIB_DIR%

   echo Compiling gtwvw.lib

   pomake /f makefile.pc > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy %LIB_DIR%\gtwvw.lib  %HB_DIR%\lib\gtwvw.lib > nul
   del  %LIB_DIR%\gtwvw.lib > null
   del make_pc.log > null
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   echo Cleaning gtwvw.lib

   if exist %HB_DIR%\lib\gtwvw.lib  del %HB_DIR%\lib\gtwvw.lib
   if exist %LIB_DIR%\gtwvw.lib     del %LIB_DIR%\gtwvw.lib
   if exist %OBJ_DIR%\gtwvw.obj     del %OBJ_DIR%\gtwvw.obj

   goto EXIT

:EXIT

SET HB_DIR=
SET CC_DIR=

SET LIB_DIR=
SET OBJ_DIR=

SET PATH=%_PATH%
SET LIB=%_LIB%
SET INCLUDE=%_INCLUDE%

SET _PATH=
SET _LIB=
SET _INCLUDE=
