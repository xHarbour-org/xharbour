@echo off
rem ============================================================================
rem
rem $Id$
rem
rem FILE: make_b32.bat
rem BATCH FILE FOR BORLAND C++
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

REM SET HB_ARCH=w64
REM SET HB_OPTIMFLAGS=-gc3
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

REM Make that the current directory is the same as the batch file (root of xHarbour)
CD %~dp0

IF NOT "%CC%"=="" (
   IF NOT "%CC%"=="bcc32" IF NOT "%CC%"=="bcc32c" GOTO SWITCH_CC
)

GOTO SET_BCC

:SWITCH_CC
   ECHO Your Environment is set to use %CC% as the compiler.
   ECHO This batch file is intended to be used with BCC only.
   SET /P RESPONSE="Do you want to switch to BCC (Y/N)? "
   IF /I "%RESPONSE%"=="Y" (
      SET CC=
      SET CC_DIR=
      SET RC_DIR=
      SET BCC_LIB=
      SET LIBEXT=
      GOTO SET_BCC
   )
   EXIT /B 1

:SET_BCC
   CALL bin\find_bc.bat
   IF ERRORLEVEL 1 GOTO NOT_READY
   GOTO FIND_BISON

:NOT_READY
   ECHO.
   ECHO. ------------------------------
   ECHO. Make Utility for Borland C/C++
   ECHO. ------------------------------
   ECHO.
   ECHO. Borland C/C++ not found.
   ECHO. Please install Borland C/C++ and try again.
   ECHO.
   GOTO EXIT

:FIND_BISON
   IF NOT "%BISON_DIR%"=="" GOTO READY
   CALL bin\find_bison.bat
   IF "%CC%"=="" GOTO NOT_READY
   GOTO READY

:READY
   IF "%SUB_DIR%"=="" SET SUB_DIR=bc32
   IF "%HB_ARCH%"=="64" SET SUB_DIR=bc64

   SET HB_GT_LIB=$(GTWIN_LIB)

   REM echo "%CC_DIR%"
   REM echo "%RC_DIR%"

:SAVE_PATH
   REM Save the original path before further modifications   
   SET _PATH=%PATH%

IF "%HB_USE_BISON%"=="1" SET PATH=%BISON_DIR%;%PATH%
REM echo %path%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET LIBEXT=%HB_DEBUG%.lib
SET OBJEXT=%HB_DEBUG%.obj
SET DIR_SEP=\
REM IF "%HB_ARCH%"=="64" SET LIBEXT=%HB_ARCH%%HB_DEBUG%.a
IF "%HB_ARCH%"=="64" SET LIBEXT=%HB_DEBUG%.a
IF "%HB_ARCH%"=="64" SET OBJEXT=%HB_DEBUG%.o
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
   @CALL winmake\mdir.bat
   make -l -fwinmake\makefile.bc >make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   if "%1"=="NOMT" goto BUILD_OK
   if "%1"=="nomt" goto BUILD_OK
   if "%2"=="NOMT" goto BUILD_OK
   if "%2"=="nomt" goto BUILD_OK

   SET HB_MT=mt
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   make -l -DHB_THREAD_SUPPORT -fwinmake\makefile.bc >>make_%SUB_DIR%.log
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
   IF "%HB_DEBUG%"=="d" goto EXIT
   rem
   rem We use HB_MT_DIR envar for DLL object folder here
   rem
   SET __BLD__=DLL_BLD
   SET HB_MT=
   SET HB_MT_DIR=\dll
   @CALL winmake\mdir.bat dllcreate
   make -l -fwinmake\makefile.bc >dll_%SUB_DIR%.log
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
   SET HB_MT=
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   make -l -fwinmake\makefile.bc >cont_%SUB_DIR%.log
   if errorlevel 1 goto CONTRIBS_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=
   REM make -s -l -DHB_THREAD_SUPPORT -fwinmake\makefile.bc >>cont_%SUB_DIR%.log
   REM if errorlevel 1 goto CONTRIBS_ERR

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
   ECHO. ------------------------------
   ECHO. Make Utility for Borland C/C++
   ECHO. ------------------------------
   @CALL winmake\mdir.bat howto
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL winmake\mdir.bat clean
   REM IF EXIST "%HB_DIR_ADS%\ace32.dll" implib -c lib\b32\ace32.lib "%HB_DIR_ADS%\ace32.dll"
   REM IF EXIST lib\b32\ace32.lib copy lib\b32\ace32.lib lib
   IF "%2"=="BUILD" goto BUILD_ALL
   IF "%2"=="build" goto BUILD_ALL
   @ECHO ****** End of Job *****
   goto EXIT

rem=============================================================================
:EXIT
rem=============================================================================
   @CALL winmake\mdir.bat resetenvar
