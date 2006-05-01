@echo off
REM $Id: make_pc.bat,v 1.0 0000/00/00 00:00:00 modalsist Exp $
REM
REM Batch file for libnf on Pelles C compiler.
REM

REM ***********************************************
REM *** Change only the two sets below, if need. **
REM ***********************************************

SET HB_DIR=c:\xharbour
SET CC_DIR=c:\pellesc

REM ***********************************
REM *** Don't change the sets below ***
REM ***********************************

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB_DIR=%HB_DIR%\lib\pocc
SET OBJ_DIR=obj\pocc

SET LIB=%CC_DIR%\LIB;%CC_DIR%\LIB\WIN;%HB_DIR%\LIB;%LIB%
SET INCLUDE=%CC_DIR%\INCLUDE;%CC_DIR%\INCLUDE\WIN;%HB_DIR%\INCLUDE;%INCLUDE%
SET PATH=%CC_DIR%\BIN;%HB_DIR%\BIN;%PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   if not exist %OBJ_DIR% md %OBJ_DIR%
   if not exist %LIB_DIR% md %LIB_DIR%

   echo Compiling libnf.lib

   pomake /f makefile.pc > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy %LIB_DIR%\libnf.lib  %HB_DIR%\lib\libnf.lib > nul
   del  %LIB_DIR%\libnf.lib > null
   del make_pc.log > null
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   echo Cleaning libnf.lib

   if exist %HB_DIR%\lib\libnf.lib  del %HB_DIR%\lib\libnf.lib
   if exist %LIB_DIR%\libnf.lib     del %LIB_DIR%\libnf.lib

   if exist %OBJ_DIR%\*.c           del %OBJ_DIR%\*.c
   if exist %OBJ_DIR%\*.obj         del %OBJ_DIR%\*.obj

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
