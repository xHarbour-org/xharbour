@echo off
rem $Id: make_pc.bat,v 1.0 0000/00/00 00:00:00 modalsist Exp $
rem
rem make file for Pelles C compiler.
rem
rem

REM ***********************************************
REM ** Change only the two sets below, if need. ***
REM ***********************************************

IF "%CC_DIR%" == "" SET CC_DIR=C:\PellesC
IF "%HB_DIR%" == "" SET HB_DIR=..\..

REM ***********************************
REM ** Don't change the sets below. ***
REM ***********************************

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB_DIR=%HB_DIR%\lib
SET OBJ_DIR=obj\pc

SET LIB=%CC_DIR%\lib;%CC_DIR%\lib\win;%HB_DIR%\lib;%LIB%
SET INCLUDE=%CC_DIR%\include;%CC_DIR%\include\win;%HB_DIR%\include;%INCLUDE%
SET PATH=%CC_DIR%\bin;%HB_DIR%\bin;%PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   if not exist %OBJ_DIR% md %OBJ_DIR%

   pomake /f makefile.pc > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   IF EXIST make_pc.log DEL make_pc.log > null
   goto EXIT

:BUILD_ERR

   notepad make_pc.log
   goto EXIT

:CLEAN

   if exist %OBJ_DIR%\*.obj  del %OBJ_DIR%\*.obj
   if exist %OBJ_DIR%\*.c    del %OBJ_DIR%\*.c
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
