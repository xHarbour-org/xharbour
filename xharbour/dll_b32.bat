@echo off
rem ============================================================================
rem
rem $Id: dll_b32.bat,v 1.15 2008/04/27 14:00:38 andijahja Exp $
rem
rem FILE: dll_b32.bat
rem BATCH FILE FOR BORLAND C++ (DLL)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\BORLAND\BCC58
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=b32
SET HB_GT_LIB=$(GTWIN_LIB)

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
if "%1" == "/clean" goto CLEAN
if "%1" == "/CLEAN" goto CLEAN

   @CALL mdir.bat dllcreate

:BUILD
rem
rem We use HB_MT_DIR envar for DLL object folder here
rem
   SET HB_MT_DIR=\dll
   make -fhrbdll.bc %1 %2 %3 >dll_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat dllcopy
   goto EXIT

:BUILD_ERR
   notepad dll_b32.log
   goto EXIT

:CLEAN
   @CALL mdir.bat dllclean
   if exist dll_b32.log del dll_b32.log

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
   REM SET LIBPREFIX=
   IF NOT "%HB_MT_DIR%"=="" SET HB_MT_DIR=
