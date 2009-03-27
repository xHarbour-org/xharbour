@echo off
rem ============================================================================
rem
rem $Id: make_pc.bat,v 1.32 2008/11/24 12:56:46 modalsist Exp $
rem
rem FILE: make_pc.bat
rem BATCH FILE FOR PELLESC
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

IF "%SUB_DIR%"=="" SET SUB_DIR=pc
IF "%HB_GT_LIB%"=="" SET HB_GT_LIB=$(GTWIN_LIB)

IF NOT "%CC_DIR%"=="" GOTO FIND_BISON
 
:FIND_POCC
   IF EXIST "%ProgramFiles%\PellesC" GOTO SET_POCC
   IF EXIST \PellesC                 GOTO SET_POCC2
   GOTO FIND_BISON
 
:SET_POCC
   SET CC_DIR=%ProgramFiles%\PellesC
   GOTO FIND_BISON

:SET_POCC2
   SET CC_DIR=\PellesC
   GOTO FIND_BISON

:FIND_BISON
   IF NOT "%BISON_DIR%"=="" GOTO READY
   IF EXIST "%ProgramFiles%\GnuWin32\Bin" GOTO SET_BISON1
   IF EXIST \GnuWin32\Bin                 GOTO SET_BISON2 
   GOTO READY
 
:SET_BISON1
   SET BISON_DIR=%ProgramFiles%\GnuWin32\Bin
   GOTO READY

:SET_BISON2
   SET BISON_DIR=\GnuWin32\Bin
   GOTO READY 

:READY   
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

if "%1"=="/?"      goto SYNTAX
if "%1"=="-?"      goto SYNTAX
if "%1"=="?"       goto SYNTAX
if "%1"==""        goto BUILD
if "%1"=="NOMT"    goto BUILD
if "%1"=="nomt"    goto BUILD
if "%1"=="clean"   goto CLEAN
if "%1"=="CLEAN"   goto CLEAN
if "%1"=="CORE"    goto BUILD
if "%1"=="core"    goto BUILD
if "%1"=="DLL"     goto DLL
if "%1"=="dll"     goto DLL
if "%1"=="CONTRIB" goto CONTRIBS
if "%1"=="contrib" goto CONTRIBS
if "%1"=="ALL"     goto BUILD_ALL
if "%1"=="all"     goto BUILD_ALL
goto SYNTAX

rem=============================================================================
:BUILD
rem=============================================================================
   SET __BLD__=CORE_BLD
   SET HB_MT=
   SET HB_MT_DIR=
   SET __MT__=
   SET HB_MT_FLAGS=
   SET PROJECT=$(ST_PROJECT)
   @CALL MDIR.BAT
   POMAKE /F makefile.pc >make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   if "%1"=="NOMT" goto BUILD_OK
   if "%1"=="nomt" goto BUILD_OK
   if "%2"=="NOMT" goto BUILD_OK
   if "%2"=="nomt" goto BUILD_OK

   SET HB_MT=mt
   SET HB_MT_DIR=\mt
   SET __MT__=-MT -DHB_THREAD_SUPPORT
   SET HB_MT_FLAGS=-dHB_THREAD_SUPPORT
   SET PROJECT=$(MT_PROJECT)
   @CALL MDIR.BAT
   POMAKE /F makefile.pc >>make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK

rem=============================================================================
:BUILD_OK
rem=============================================================================
   @CALL mdir.bat copytobin
   if "%MAKEALL%"=="" @ECHO ****** End of Job *****
   if "%MAKEALL%"=="" goto EXIT
   if "%1"=="CORE" @ECHO ****** End of Job *****
   if "%1"=="core" @ECHO ****** End of Job *****
   if "%1"=="CORE" goto EXIT
   if "%1"=="core" goto EXIT
   goto DLL

rem=============================================================================
:BUILD_ERR
rem=============================================================================
   IF EXIST make_%SUB_DIR%.log notepad make_%SUB_DIR%.log
   goto EXIT

rem=============================================================================
:DLL
rem=============================================================================
   rem
   rem We use HB_MT_DIR envar for DLL object folder here
   rem
   SET __BLD__=DLL_BLD
   SET HB_MT=
   SET __MT__=
   SET HB_MT_FLAGS=
   SET HB_MT_DIR=\dll
   @CALL mdir.bat dllcreate
   pomake /F hrbdll.pc  >dll_%SUB_DIR%.log
   if errorlevel 1 goto DLL_ERR
   goto DLL_OK

rem=============================================================================
:DLL_OK
rem=============================================================================
   @CALL mdir.bat dllcopy
   if "%MAKEALL%"=="" @ECHO ****** End of Job *****
   IF "%MAKEALL%"=="" goto EXIT
   IF "%1"=="DLL" @ECHO ****** End of Job *****
   IF "%1"=="dll" @ECHO ****** End of Job *****
   IF "%1"=="DLL" goto EXIT
   IF "%1"=="dll" goto EXIT
   goto CONTRIBS

rem=============================================================================
:DLL_ERR
rem=============================================================================
   if exist dll_%SUB_DIR%.log notepad dll_%SUB_DIR%.log
   goto EXIT

rem=============================================================================
:CONTRIBS
rem=============================================================================
   SET __BLD__=CONTRIB_BLD
   SET HB_MT=
   SET __MT__=
   SET HB_MT_FLAGS=
   SET HB_MT_DIR=
   @CALL MDIR.BAT
   POMAKE /F contrib.pc  >cont_%SUB_DIR%.log
   if errorlevel 1 goto CONTRIBS_ERR
   goto CONTRIBS_OK

rem=============================================================================
:CONTRIBS_OK
rem=============================================================================
   @CALL mdir.bat copycontrib
   @ECHO ****** End of Job *****
   goto EXIT

rem=============================================================================
:CONTRIBS_ERR
rem=============================================================================
   IF EXIST cont_%SUB_DIR%.log notepad cont_%SUB_DIR%.log
   goto EXIT

rem=============================================================================
:BUILD_ALL
rem=============================================================================
   SET MAKEALL=yes
   goto BUILD

rem=============================================================================
:SYNTAX
rem=============================================================================
   ECHO.
   ECHO. ------------------------
   ECHO. Make Utility for PellesC
   ECHO. ------------------------
   @CALL mdir.bat howto
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL mdir.bat clean
   IF "%2"=="BUILD" goto BUILD_ALL
   IF "%2"=="build" goto BUILD_ALL
   @ECHO ****** End of Job *****
   goto EXIT

rem=============================================================================
:EXIT
rem=============================================================================
   SET HB_MT_FLAGS=
   SET PROJECT=
   SET HB_MT=
   SET __MT__=
   @CALL mdir.bat resetenvar
