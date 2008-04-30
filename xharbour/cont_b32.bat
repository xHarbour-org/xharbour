@echo off
rem ============================================================================
rem
rem $Id: make_b32.bat,v 1.36 2008/04/29 22:14:09 andijahja Exp $
rem
rem FILE: cont_b32.bat
rem BATCH FILE FOR BORLAND C++ (CONTRIBS)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

if "%BCCDIR%" == "" SET BCCDIR=C:\BORLAND\BCC58

SET CC_DIR=C:\BORLAND\BCC58
SET SUB_DIR=b32

SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%BISON_DIR%;%PATH%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET LIBEXT=.lib
SET OBJEXT=.obj
SET DIR_SEP=\
REM SET LIBPREFIX=
rem ============================================================================

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   @CALL MDIR.BAT

:BUILD
   REM SET HB_MT=
   REM SET HB_MT_DIR=
   make -l -fcontrib.bc %1 %2 %3 >cont_b32.log
   if errorlevel 1 goto BUILD_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=\mt
   REM make -l -DHB_THREAD_SUPPORT -fcontrib.bc %1 %2 %3 >>cont_b32.log
   REM if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:BUILD_ERR
   IF EXIST cont_b32.log notepad cont_b32.log
   goto EXIT

:CLEAN
   @CALL mdir.bat cleancontrib
   IF EXIST cont_b32.log DEL cont_b32.log

:EXIT
   SET CC_DIR=
   SET SUB_DIR=
   SET PATH=%_PATH%
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   REM SET LIBPREFIX=
   IF NOT "%HB_MT%"=="" SET HB_MT=
   IF NOT "%HB_MT_DIR%"=="" SET HB_MT_DIR=
