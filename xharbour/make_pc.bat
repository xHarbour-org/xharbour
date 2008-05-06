@echo off
rem ============================================================================
rem
rem $Id: make_pc.bat,v 1.25 2008/04/29 22:14:09 andijahja Exp $
rem
rem FILE: make_pc.bat
rem BATCH FILE FOR PELLESC
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
REM SET LIBPREFIX=
rem ============================================================================

if "%1" == ""        goto SYNTAX
if "%1" == "clean"   goto CLEAN
if "%1" == "CLEAN"   goto CLEAN
if "%1" == "CORE"    goto BUILD
if "%1" == "core"    goto BUILD
if "%1" == "DLL"     goto DLL
if "%1" == "dll"     goto DLL
if "%1" == "CONTRIB" goto CONTRIBS
if "%1" == "contrib" goto CONTRIBS
if "%1" == "ALL"     goto BUILD_ALL
if "%1" == "all"     goto BUILD_ALL
goto SYNTAX

rem=============================================================================
:BUILD
rem=============================================================================
   @CALL MDIR.BAT
   SET HB_MT=
   SET HB_MT_DIR=
   SET __MT__=
   SET HB_MT_FLAGS=
   SET PROJECT=$(ST_PROJECT)
   POMAKE /F makefile.pc %2 %3 >make_pc.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_MT=mt
   SET HB_MT_DIR=\mt
   SET __MT__=-MT -DHB_THREAD_SUPPORT
   SET HB_MT_FLAGS=-dHB_THREAD_SUPPORT
   SET PROJECT=$(MT_PROJECT)
   POMAKE /F makefile.pc %2 %3 >>make_pc.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK

:BUILD_OK
   @CALL mdir.bat copytobin
   if "MAKEALL" == "" goto EXIT
   if "%1" == "CORE" goto EXIT
   if "%1" == "core" goto EXIT
   goto DLL

:BUILD_ERR
   IF EXIST make_pc.log notepad make_pc.log
   goto EXIT

rem=============================================================================
:DLL
rem=============================================================================
rem
rem We use HB_MT_DIR envar for DLL object folder here
rem
   @CALL mdir.bat dllcreate
   SET HB_MT=
   SET __MT__=
   SET HB_MT_FLAGS=
   SET HB_MT_DIR=\dll
   pomake /F hrbdll.pc %2 %3 >dll_pc.log
   if errorlevel 1 goto BUILD_ERR
   if errorlevel 1 goto DLL_ERR
   goto DLL_OK

:DLL_OK
   @CALL mdir.bat dllcopy
   IF "MAKEALL" == ""   goto EXIT
   IF "%1" == "DLL" goto EXIT
   IF "%1" == "dll" goto EXIT
   goto CONTRIBS

:DLL_ERR
   if exist dll_pc.log notepad dll_pc.log
   goto EXIT

rem=============================================================================
:CONTRIBS
rem=============================================================================
   @CALL MDIR.BAT
   SET HB_MT=
   SET __MT__=
   SET HB_MT_FLAGS=
   SET HB_MT_DIR=
   POMAKE /F contrib.pc %2 %3 >cont_pc.log
   if errorlevel 1 goto BUILD_ERR

:CONTRIBS_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:CONTRIBS_ERR
   IF EXIST cont_pc.log notepad cont_pc.log
   goto EXIT

rem=============================================================================
:BUILD_ALL
rem=============================================================================
   SET MAKEALL=yes
   goto BUILD

rem=============================================================================
:SYNTAX
rem=============================================================================
   ECHO.Syntax:
   ECHO. make_pc core    : Build xHarbour CORE files
   ECHO. make_pc dll     : Build xHarbour DLL
   ECHO. make_pc contrib : Build CONTRIB Libraries
   ECHO. make_pc all     : Build CORE, DLL and CONTRIB
   ECHO. make_pc clean   : Erase all files once built
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL mdir.bat clean
   IF EXIST make_pc.log DEL make_pc.log
   @CALL mdir.bat dllclean
   if exist dll_pc.log del dll_pc.log
   @CALL mdir.bat cleancontrib
   IF EXIST cont_pc.log DEL cont_pc.log

rem=============================================================================
:EXIT
rem=============================================================================
   SET HB_MT_FLAGS=
   SET PROJECT=
   SET HB_MT=
   SET __MT__=
   @CALL mdir.bat resetenvar
