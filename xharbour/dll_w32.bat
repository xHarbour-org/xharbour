@echo off
rem ============================================================================
rem
rem $Id: dll_w32.bat,v 1.5 2008/04/27 14:00:38 andijahja Exp $
rem
rem FILE: dll_w32.bat
rem BATCH FILE FOR OPENWATCOM (DLL)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\WATCOM
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=wc
SET HB_GT_LIB=$(GTWIN_LIB)

SET _LIB=%LIB%
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BINNT;%CC_DIR%\BINW;%BISON_DIR%;%_PATH%
SET WATCOM=%CC_DIR%
SET EDPATH=%CC_DIR%\EDDAT
SET LIB=%CC_DIR%\lib386;%CC_DIR%\lib386\nt;%_LIB%

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
   rem==========================================================================
   rem We use HB_MT_DIR envar for DLL object folder here
   rem==========================================================================
   SET HB_MT_DIR=\dll
   wmake -h -ms -f hrbdll.wc %1 %2 %3 >dll_w32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat dllcopy
   if exist *.err del *.err > nul
   goto EXIT

:BUILD_ERR
   if exist dll_w32.log notepad dll_w32.log
   goto EXIT

:CLEAN
   @CALL mdir.bat dllclean
   if exist dll_w32.log del dll_w32.log

:EXIT
   SET CC_DIR=
   SET BISON_DIR=
   SET SUB_DIR=
   SET HB_GT_LIB=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   REM SET LIBPREFIX=
   SET HB_MT_DIR=
   SET LIB=%_LIB%
   SET PATH=%_PATH%
   SET _PATH=
   SET _LIB=
   SET WATCOM=
   SET EDPATH=
   SET LIB=
