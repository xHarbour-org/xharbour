@echo off
rem $Id: make_pc.bat,v 1.0 2006/03/19 00:00:00 modalsist Exp $
rem
rem make for Pelles C/C++ compiler.
rem
rem

SET HB_DIR=c:\xharbcvs
SET POCC_DIR=c:\pellesc

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB_DIR=%HB_DIR%\lib\pocc
SET OBJ_DIR=obj\pocc

SET LIB=%POCC_DIR%\LIB;%POCC_DIR%\LIB\WIN;%HB_DIR\LIB;%LIB%
SET INCLUDE=%POCC_DIR%\INCLUDE;%POCC_DIR%\INCLUDE\WIN;%HB_DIR%\INCLUDE;%INCLUDE%
SET PATH=%POCC_DIR%\BIN;%HB_DIR%\BIN;%PATH%


   if "%1" == "clean" goto CLEAN
   if "%1" == "CLEAN" goto CLEAN

   if not exist %OBJ_DIR% md %OBJ_DIR%
   if not exist %LIB_DIR% md %LIB_DIR%

   echo Compiling libmisc.lib

   pomake /f makefile.pc > make_pc.log
   if errorlevel 1 goto BUILD_ERR


:BUILD_OK

   copy %LIB_DIR%\libmisc.lib  %HB_DIR%\lib\libmisc.lib > nul
   if exist %LIB_DIR%\libmisc.lib del  %LIB_DIR%\libmisc.lib > null
   del make_pc.log > null
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   echo Cleaning libmisc.lib

   if exist %HB_DIR%\lib\libmisc.lib del %HB_DIR%\lib\libmisc.lib
   if exist %LIB_DIR%\libmisc.lib  del %LIB_DIR%\libmisc.lib
   if exist %OBJ_DIR%\libmisc.obj  del %OBJ_DIR%\libmisc.obj

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
