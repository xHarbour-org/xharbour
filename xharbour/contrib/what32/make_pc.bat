@echo off
rem $Id$
rem
rem Make file for Pelles C compiler.
rem
rem

REM ***********************************************
REM *** Change only the two sets below, if need ***
REM ***********************************************

SET HB_DIR=c:\xharbour
SET POCC_DIR=c:\pellesc

REM ************************************
REM *** Don't change the sets below. ***
REM ************************************

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB_DIR=%HB_DIR%\lib\pocc
SET OBJ_DIR=obj\pocc

SET LIB=%POCC_DIR%\LIB;%POCC_DIR%\LIB\WIN;%HB_DIR%\LIB;%LIB%
SET INCLUDE=%POCC_DIR%\INCLUDE;%POCC_DIR%\INCLUDE\WIN;%HB_DIR%\INCLUDE;%INCLUDE%
SET PATH=%POCC_DIR%\BIN;%HB_DIR%\BIN;%PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   if not exist %OBJ_DIR% md %OBJ_DIR%
   if not exist %LIB_DIR% md %LIB_DIR%

   echo Compiling what32.lib

   pomake /f makefile.pc > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   COPY %LIB_DIR%\what32.lib  %HB_DIR%\lib\what32.lib > nul
   if exist %LIB_DIR%\what32.lib DEL  %LIB_DIR%\what32.lib > null
   del make_pc.log > null
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   echo Cleaning what32.lib

   if exist %HB_DIR%\lib\what32.lib      del %HB_DIR%\lib\what32.lib

   if exist %LIB_DIR%\what32.lib  del %LIB_DIR%\what32.lib
   if exist %LIB_DIR%\what32.bak  del %LIB_DIR%\what32.bak

   if exist %OBJ_DIR%\*.obj  del %OBJ_DIR%\*.obj
   if exist %OBJ_DIR%\*.c    del %OBJ_DIR%\*.c

   goto EXIT

:EXIT

SET HB_DIR=
SET POCC_DIR=

SET LIB_DIR=
SET OBJ_DIR=

SET PATH=%_PATH%
SET LIB=%_LIB%
SET INCLUDE=%_INCLUDE%

SET _PATH=
SET _LIB=
SET _INCLUDE=
