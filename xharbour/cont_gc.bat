@echo off
rem ============================================================================
rem
rem $Id: make_gc.bat,v 1.11 2008/04/29 22:14:09 andijahja Exp $
rem
rem FILE: make_gc.bat
rem BATCH FILE FOR MINGW32 (CONTRIBS)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:/MinGW
SET SUB_DIR=gc
SET BISON_DIR=C:/MSYS/1.0/bin

SET _PATH=%PATH%
SET PATH=%CC_DIR%\bin;%BISON_DIR%;%PATH%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET DIR_SEP=/
SET OBJEXT=.o
SET LIBEXT=.a
SET LIBPREFIX=lib

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   @CALL mdir.bat

:BUILD
   SET HB_THREAD_SUPPORT=0
   SET HB_MT=
   SET HB_MT_DIR=
   mingw32-make.exe -fcontrib.gc %1 %2 %3 1>cont_gc0.log 2>cont_gc.log
   if errorlevel 1 goto BUILD_ERR

   REM SET HB_THREAD_SUPPORT=1
   REM SET HB_MT=mt
   REM SET HB_MT_DIR=/mt
   REM mingw32-make.exe -f contrib.gc %1 %2 %3 1>>cont_gc0.log 2>>cont_gc.log
   REM if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:BUILD_ERR
   if exist cont_gc.log notepad cont_gc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat cleancontrib
   IF EXIST cont_gc.log DEL cont_gc.log
   IF EXIST cont_gc0.log DEL cont_gc0.log

:EXIT
   SET CC_DIR=
   SET SUB_DIR=
   SET BISON_DIR=
   SET PATH=%_PATH%
   SET _PATH=
   SET HB_THREAD_SUPPORT=
   SET DIR_SEP=
   SET OBJEXT=
   SET LIBEXT=
   SET LIBPREFIX=
   SET OBJEXT=
   SET LIBEXT=
   SET HB_MT=
   SET HB_MT_DIR=
