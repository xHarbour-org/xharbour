@echo off
rem ============================================================================
rem
rem $Id: makefile.bc,v 1.218 2008/04/14 06:06:20 andijahja Exp $
rem
rem FILE: make_gc.bat
rem BATCH FILE FOR PELLESC
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:/MinGW
SET SUB_DIR=gc
SET HB_GT_LIB=$(GTWIN_LIB)
SET BISON_DIR=C:/MSYS/1.0/bin

SET _PATH=%PATH%
SET PATH=%CC_DIR%\bin;%BISON_DIR%

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

   @CALL MDIR.BAT

:BUILD
   SET HB_THREAD_SUPPORT=0
   SET HB_MT=
   SET HB_MT_DIR=
   mingw32-make.exe -fmakefile.gc %1 %2 %3 1>make_gc0.log 2>make_gc.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_THREAD_SUPPORT=1
   SET HB_MT=mt
   SET HB_MT_DIR=/mt
   mingw32-make.exe -f makefile.gc %1 %2 %3 1>>make_gc0.log 2>>make_gc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   goto EXIT

:BUILD_ERR
   notepad make_gc.log
   goto EXIT

:CLEAN
   @CALL MDIR.BAT CLEAN
   IF EXIST make_gc.log DEL make_gc.log

:EXIT
   SET CC_DIR=
   SET SUB_DIR=
   SET HB_GT_LIB=
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
