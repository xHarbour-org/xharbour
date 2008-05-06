@echo off
rem ============================================================================
rem
rem $Id: make_gc.bat,v 1.11 2008/04/29 22:14:09 andijahja Exp $
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
SET PATH=%CC_DIR%\bin;%BISON_DIR%;%PATH%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET DIR_SEP=/
SET OBJEXT=.o
SET LIBEXT=.a
SET LIBPREFIX=lib

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
   SET HB_THREAD_SUPPORT=
   SET HB_MT=
   SET HB_MT_DIR=
   mingw32-make.exe -fmakefile.gc %2 %3 1>make_gc0.log 2>make_gc.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_THREAD_SUPPORT=1
   SET HB_MT=mt
   SET HB_MT_DIR=/mt
   mingw32-make.exe -f makefile.gc %2 %3 1>>make_gc0.log 2>>make_gc.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK

:BUILD_OK
   @CALL mdir.bat copytobin
   if "MAKEALL" == ""    goto EXIT
   if "%1" == "CORE" goto EXIT
   if "%1" == "core" goto EXIT
   goto DLL

:BUILD_ERR
   IF EXIST make_gc.log notepad make_gc.log
   goto EXIT

rem=============================================================================
:DLL
rem=============================================================================
   rem==========================================================================
   rem We use HB_MT_DIR envar for DLL object folder here
   rem==========================================================================
   @CALL mdir.bat dllcreate
   SET __BLD__=DLL_BLD
   SET HB_THREAD_SUPPORT=
   SET HB_MT=
   SET HB_MT_DIR=/dll
   mingw32-make.exe -fmakefile.gc %2 %3 1>dll_gc0.log 2>dll_gc.log
   if errorlevel 1 goto BUILD_ERR
   goto DLL_OK

:DLL_OK
   @CALL mdir.bat dllcopy
   IF "MAKEALL" == ""   goto EXIT
   IF "%1" == "DLL" goto EXIT
   IF "%1" == "dll" goto EXIT
   goto CONTRIBS

:DLL_ERR
   if exist dll_gc.log notepad dll_gc.log
   goto EXIT

rem=============================================================================
:CONTRIBS
rem=============================================================================
   @CALL MDIR.BAT
   SET __BLD__=CONTRIB_BLD
   SET HB_THREAD_SUPPORT=
   SET HB_MT=
   SET HB_MT_DIR=
   mingw32-make.exe -fmakefile.gc %2 %3 1>cont_gc0.log 2>cont_gc.log
   if errorlevel 1 goto BUILD_ERR

   REM SET HB_THREAD_SUPPORT=1
   REM SET HB_MT=mt
   REM SET HB_MT_DIR=/mt
   REM mingw32-make.exe -f makefile.gc %2 %3 1>>cont_gc0.log 2>>cont_gc.log
   REM if errorlevel 1 goto BUILD_ERR

:CONTRIBS_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:CONTRIBS_ERR
   IF EXIST cont_gc.log notepad cont_gc.log
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
   ECHO. make_gc core    : Build xHarbour CORE files
   ECHO. make_gc dll     : Build xHarbour DLL
   ECHO. make_gc contrib : Build CONTRIB Libraries
   ECHO. make_gc all     : Build CORE, DLL and CONTRIB
   ECHO. make_gc clean   : Erase all files once built
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL mdir.bat clean
   IF EXIST make_gc.log DEL make_gc.log
   @CALL mdir.bat dllclean
   if exist dll_gc.log del dll_gc.log
   @CALL mdir.bat cleancontrib
   IF EXIST cont_gc.log DEL cont_gc.log

rem=============================================================================
:EXIT
rem=============================================================================
   @CALL mdir.bat resetenvar
