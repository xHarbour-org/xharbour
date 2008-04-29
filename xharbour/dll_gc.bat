@echo off
rem ============================================================================
rem
rem $Id: dll_gc.bat,v 1.4 2008/04/27 14:00:38 andijahja Exp $
rem
rem FILE: dll_gc.bat
rem BATCH FILE FOR MINGW (DLL)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:/MinGW
SET BISON_DIR=C:/MSYS/1.0/bin
SET SUB_DIR=gc
SET HB_GT_LIB=$(GTWIN_LIB)

SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%BISON_DIR%;%PATH%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET LIBEXT=.a
SET OBJEXT=.o
SET DIR_SEP=/
SET LIBPREFIX=lib
rem ============================================================================

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if "%1" == "/clean" goto CLEAN
if "%1" == "/CLEAN" goto CLEAN

   @CALL mdir.bat dllcreate

:BUILD
   rem==========================================================================
   rem We use HB_MT_DIR envar for DLL object folder here
   rem==========================================================================
   SET HB_MT_DIR=/dll
   mingw32-make.exe -fhrbdll.gc %1 %2 %3 1>dll_gc0.log 2>dll_gc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat dllcopy
   goto EXIT

:BUILD_ERR
   if exist dll_gc.log notepad dll_gc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat dllclean
   if exist dll_gc.log del dll_gc.log
   if exist dll_gc0.log del dll_gc0.log

:EXIT
   SET CC_DIR=
   SET BISON_DIR=
   SET SUB_DIR=
   SET HB_GT_LIB=
   SET PATH=%_PATH%
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   SET LIBPREFIX=
   SET HB_MT_DIR=
