@echo off
rem ============================================================================
rem
rem $Id: make_vc.bat,v 1.21 2008/05/05 20:10:01 ronpinkas Exp $
rem
rem FILE: nake_vc.bat
rem BATCH FILE FOR MSVC
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\VC
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=vc
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
   nmake  /NOLOGO -fmakefile.vc %2 %3 >make_vc.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_MT=mt
   SET HB_MT_DIR=\mt
   nmake /NOLOGO HB_THREAD_SUPPORT=1 -fmakefile.vc %2 %3 >>make_vc.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK

:BUILD_OK
   @CALL mdir.bat copytobin
   if "MAKEALL" == ""    goto EXIT
   if "%1" == "CORE" goto EXIT
   if "%1" == "core" goto EXIT
   goto DLL

:BUILD_ERR
   IF EXIST make_vc.log notepad make_vc.log
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
   nmake /NOLOGO -f makefile.vc %2 %3 >dll_vc.log
   if errorlevel 1 goto DLL_ERR
   goto DLL_OK

:DLL_OK
   @CALL mdir.bat dllcopy
   IF "MAKEALL" == ""   goto EXIT
   IF "%1" == "DLL" goto EXIT
   IF "%1" == "dll" goto EXIT
   goto CONTRIBS

:DLL_ERR
   if exist dll_vc.log notepad dll_vc.log
   goto EXIT

rem=============================================================================
:CONTRIBS
rem=============================================================================
   @CALL MDIR.BAT
   SET __BLD__=CONTRIB_BLD
   SET HB_MT=
   SET HB_MT_DIR=
   REM SET HB_MT=
   REM SET HB_MT_DIR=
   nmake /NOLOGO -f makefile.vc %2 %3 >cont_vc.log
   if errorlevel 1 goto CONTRIBS_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=\mt
   REM nmake  /NOLOGO HB_THREAD_SUPPORT=1 -fmakefile.vc %2 %3 >>cont_vc.log
   REM if errorlevel 1 goto CONTRIBS_ERR

:CONTRIBS_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:CONTRIBS_ERR
   IF EXIST cont_vc.log notepad cont_vc.log
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
   ECHO. make_vc core    : Build xHarbour CORE files
   ECHO. make_vc dll     : Build xHarbour DLL
   ECHO. make_vc contrib : Build CONTRIB Libraries
   ECHO. make_vc all     : Build CORE, DLL and CONTRIB
   ECHO. make_vc clean   : Erase all files once built
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL mdir.bat clean
   IF EXIST make_vc.log DEL make_vc.log
   @CALL mdir.bat dllclean
   if exist dll_vc.log del dll_vc.log
   @CALL mdir.bat cleancontrib
   IF EXIST cont_vc.log DEL cont_vc.log

rem=============================================================================
:EXIT
rem=============================================================================
   @CALL mdir.bat resetenvar
