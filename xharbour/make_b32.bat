@echo off
rem ============================================================================
rem
rem $Id: make_b32.bat,v 1.37 2008/05/06 05:47:52 andijahja Exp $
rem
rem FILE: make_b32.bat
rem BATCH FILE FOR BORLAND C++
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

if "%BCCDIR%" == "" SET BCCDIR=C:\BORLAND\BCC58

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
   SET __BLD__=CORE_BLD
   SET HB_MT=
   SET HB_MT_DIR=
   make -s -l -fmakefile.bc %2 %3 >make_b32.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_MT=mt
   SET HB_MT_DIR=\mt
   make -s -l -DHB_THREAD_SUPPORT -fmakefile.bc %2 %3 >>make_b32.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK

:BUILD_OK
   @CALL mdir.bat copytobin
   if "MAKEALL" == ""    goto EXIT
   if "%1" == "CORE" goto EXIT
   if "%1" == "core" goto EXIT
   goto DLL

:BUILD_ERR
   IF EXIST make_b32.log notepad make_b32.log
   goto EXIT

rem=============================================================================
:DLL
rem=============================================================================
rem
rem We use HB_MT_DIR envar for DLL object folder here
rem
   @CALL mdir.bat dllcreate
   SET __BLD__=DLL_BLD
   SET HB_MT=
   SET HB_MT_DIR=\dll
   make -fmakefile.bc %2 %3 >dll_b32.log
   if errorlevel 1 goto DLL_ERR
   goto DLL_OK

:DLL_OK
   @CALL mdir.bat dllcopy
   IF "MAKEALL" == ""   goto EXIT
   IF "%1" == "DLL" goto EXIT
   IF "%1" == "dll" goto EXIT
   goto CONTRIBS

:DLL_ERR
   if exist dll_b32.log notepad dll_b32.log
   goto EXIT

rem=============================================================================
:CONTRIBS
rem=============================================================================
   @CALL MDIR.BAT
   SET __BLD__=CONTRIB_BLD
   SET HB_MT=
   SET HB_MT_DIR=
   make -s -l -fmakefile.bc %2 %3 >cont_b32.log
   if errorlevel 1 goto CONTRIBS_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=\mt
   REM make -s -l -DHB_THREAD_SUPPORT -fmakefile.bc %2 %3 >>cont_b32.log
   REM if errorlevel 1 goto CONTRIBS_ERR

:CONTRIBS_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:CONTRIBS_ERR
   IF EXIST cont_b32.log notepad cont_b32.log
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
   ECHO. make_b32 core    : Build xHarbour CORE files
   ECHO. make_b32 dll     : Build xHarbour DLL
   ECHO. make_b32 contrib : Build CONTRIB Libraries
   ECHO. make_b32 all     : Build CORE, DLL and CONTRIB
   ECHO. make_b32 clean   : Erase all files once built
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL mdir.bat clean
   IF EXIST make_b32.log DEL make_b32.log
   @CALL mdir.bat dllclean
   if exist dll_b32.log del dll_b32.log
   @CALL mdir.bat cleancontrib
   IF EXIST cont_b32.log DEL cont_b32.log

rem=============================================================================
:EXIT
rem=============================================================================
   @CALL mdir.bat resetenvar
