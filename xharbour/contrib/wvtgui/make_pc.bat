@echo off
rem $Id: make_pc.bat,v 1.0 2005/11/02 00:00:00 modalsist Exp $
rem
rem make file for build under Pelles C compiler.
rem
rem

SET HB_DIR=C:\xMyCVS
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

   echo Compiling wvtgui.lib

   pomake /f makefile.pc > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy %TARGET_LIB_DIR%\wvtgui.lib  %HB_DIR%\lib\wvtgui.lib > nul
   del  %TARGET_LIB_DIR%\wvtgui.lib > null
   del make_pc.log > null
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   echo Cleaning wvtgui.lib

   if exist %HB_DIR%\lib\wvtgui.lib          del %HB_DIR%\lib\wvtgui.lib

   if exist %TARGET_LIB_DIR%\wvtgui.lib      del %TARGET_LIB_DIR%\wvtgui.lib
   if exist %TARGET_LIB_DIR%\wvtgui.bak      del %TARGET_LIB_DIR%\wvtgui.bak

   if exist %TARGET_OBJ_DIR%\wvtcore.obj     del %TARGET_OBJ_DIR%\wvtcore.obj
   if exist %TARGET_OBJ_DIR%\wvtpaint.obj    del %TARGET_OBJ_DIR%\wvtpaint.obj
   if exist %TARGET_OBJ_DIR%\wvtclass.obj    del %TARGET_OBJ_DIR%\wvtclass.obj
   if exist %TARGET_OBJ_DIR%\wvtutils.obj    del %TARGET_OBJ_DIR%\wvtutils.obj

   if exist %TARGET_OBJ_DIR%\wvtpaint.c      del %TARGET_OBJ_DIR%\wvtpaint.c
   if exist %TARGET_OBJ_DIR%\wvtclass.c      del %TARGET_OBJ_DIR%\wvtclass.c

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
