@echo off
rem ============================================================================
rem
rem $Id$
rem
rem FILE: make_vc.bat
rem BATCH FILE FOR Microsoft C/C++
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

REM SET HB_ARCH=w64
REM SET HB_WARNING_FLAGS=
REM SET HB_DEBUG=d
REM SET HB_GUI=1
REM SET HB_NO_BACKGROUND=1

REM SET HB_DIR_POSTGRESQL=
REM SET HB_DIR_OCILIB=
REM SET HB_DIR_MYSQL=
REM SET HB_DIR_FIREBIRD=
REM SET HB_DIR_CAIRO=
REM SET HB_DIR_CURL=
REM SET HB_DIR_OPENSSL=
REM SET HB_DIR_MAGIC=
REM SET HB_DIR_ADS=

IF "%__MAKE__%"=="" SET __MAKE__=nmake

REM Make that the current directory is the same as the batch file (root of xHarbour)
CD %~dp0

SET "scriptName=%~n0"
ECHO *** START [%~f0](%*) > winmake\functions.log

:SET_CC
   CALL winmake\find_vc.bat
   IF ERRORLEVEL 99 GOTO ERROR_99
   IF ERRORLEVEL  2 GOTO ABORTED
   IF ERRORLEVEL  1 GOTO NOT_READY

   GOTO FIND_BISON

:ERROR_99
   ECHO.
   ECHO. ---------------------------------------
   ECHO. Make Utility for %C_LONG_NAME%
   ECHO. ---------------------------------------
   ECHO.
   ECHO. Unexpected error!
   ECHO.
   GOTO EXIT

:ABORTED
   ECHO.
   ECHO. ---------------------------------------
   ECHO. Make Utility for %C_LONG_NAME%
   ECHO. ---------------------------------------
   ECHO.
   ECHO. Aborted by user.
   ECHO.
   GOTO EXIT

:NOT_READY
   ECHO.
   ECHO. ---------------------------------------
   ECHO. Make Utility for %C_LONG_NAME%
   ECHO. ---------------------------------------
   ECHO.
   ECHO. %C_LONG_NAME% not found.
   ECHO. Please install and try again.
   ECHO.
   GOTO EXIT

:FIND_BISON
   IF NOT "%BISON_DIR%"=="" GOTO READY
   CALL winmake\find_bison.bat
   IF "%CC%"=="" GOTO NOT_READY
   GOTO READY

:READY
   REM NOT an error using $() synttax because it will be LATE expanded by make!
   IF "%HB_GT_LIB%" == "" SET "HB_GT_LIB=$(GTWIN_LIB)"

   REM echo "%CC_DIR%"

:SAVE_PATH
   REM Save the original path before further modifications   
   SET _PATH=%PATH%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET LIBEXT=%HB_DEBUG%.lib
SET OBJEXT=%HB_DEBUG%.obj
SET DIR_SEP=\
SET LIBPREFIX=
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
   SET HB_THREAD_SUPPORT=
   SET HB_MT=
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   %__MAKE__% /NOLOGO -f winmake\makefile.%C_SHORT_NAME% >make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   if "%1"=="NOMT" goto BUILD_OK
   if "%1"=="nomt" goto BUILD_OK
   if "%2"=="NOMT" goto BUILD_OK
   if "%2"=="nomt" goto BUILD_OK

   SET HB_THREAD_SUPPORT=1
   SET HB_MT=mt
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   %__MAKE__% /NOLOGO -f winmake\makefile.%C_SHORT_NAME% >>make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK

rem=============================================================================
:BUILD_OK
rem=============================================================================
   @CALL winmake\mdir.bat copytobin
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
   SET HB_THREAD_SUPPORT=
   SET HB_MT=
   SET HB_MT_DIR=\dll
   REM SET HB_NO_VM_ALL=1
   @CALL winmake\mdir.bat dllcreate
   %__MAKE__% /NOLOGO -f winmake\makefile.%C_SHORT_NAME% >dll_%SUB_DIR%.log
   if errorlevel 1 goto DLL_ERR
   goto DLL_OK

rem=============================================================================
:DLL_OK
rem=============================================================================
   @CALL winmake\mdir.bat dllcopy
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
   SET HB_THREAD_SUPPORT=
   SET HB_MT=
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   %__MAKE__% /NOLOGO -f winmake\makefile.%C_SHORT_NAME% >cont_%SUB_DIR%.log
   if errorlevel 1 goto CONTRIBS_ERR


rem=============================================================================
:CONTRIBS_OK
rem=============================================================================
   @CALL winmake\mdir.bat copycontrib
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
   ECHO. ---------------------------------------
   ECHO. Make Utility for %C_LONG_NAME%
   ECHO. ---------------------------------------
   @CALL winmake\mdir.bat howto
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL winmake\mdir.bat clean
   IF "%2"=="BUILD" goto BUILD_ALL
   IF "%2"=="build" goto BUILD_ALL
   @ECHO ****** End of Job *****
   goto EXIT

rem=============================================================================
:EXIT
rem=============================================================================
   @CALL winmake\mdir.bat resetenvar
   set "scriptName="
   ECHO *** END[%ERRORLEVEL%] [%~f0] >> winmake\functions.log
