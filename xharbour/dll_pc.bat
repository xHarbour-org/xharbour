@echo off
rem ============================================================================
rem
rem $Id: makefile.bc,v 1.218 2008/04/14 06:06:20 andijahja Exp $
rem
rem FILE: dll_pc.bat
rem BATCH FILE FOR PELLESC (DLL)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\PELLESC
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=pc
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
SET LIBPREFIX=
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
   SET HB_MT_DIR=\dll
   pomake /F hrbdll.pc %1 %2 %3 >dll_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat dllcopy
   goto EXIT

:BUILD_ERR
   notepad dll_pc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat dllclean
   if exist dll_pc.log del dll_pc.log

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
