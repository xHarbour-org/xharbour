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

REM SET HB_ARCH=64
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

IF "%SUB_DIR%"=="" SET SUB_DIR=b32
IF "%HB_ARCH%"=="64" SET SUB_DIR=b64
IF "%HB_GT_LIB%"=="" SET HB_GT_LIB=$(GTWIN_LIB)
IF NOT "%CC_DIR%"=="" GOTO FIND_BISON

:FIND_BCC
   IF EXIST "%ProgramFiles%\Borland\BDS\4.0" GOTO SET_BDS_40
   IF EXIST \Borland\bcc58                   GOTO SET_BORLAND_58
   IF EXIST \bcc58                           GOTO SET_BCC_58
   IF EXIST \Borland\bcc55                   GOTO SET_BORLAND_55
   IF EXIST \bcc55                           GOTO SET_BCC_55
   GOTO FIND_BISON

:SET_BDS_40
   SET CC_DIR=%ProgramFiles%\Borland\BDS\4.0
   GOTO FIND_BISON

:SET_BORLAND_58
   SET CC_DIR=\Borland\bcc58
   GOTO FIND_BISON

:SET_BCC_58
   SET CC_DIR=\bcc58
   GOTO FIND_BISON

:SET_BORLAND_55
   SET CC_DIR=\Borland\bcc55
   GOTO FIND_BISON

:SET_BCC_55
   SET CC_DIR=\bcc55
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
IF "%HB_DEBUG%"=="d" SET LIBEXT=%HB_DEBUG%.lib
IF "%HB_DEBUG%"=="d" SET OBJEXT=%HB_DEBUG%.obj
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
   make -s -l -fwinmake\makefile.bc >make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   if "%1"=="NOMT" goto BUILD_OK
   if "%1"=="nomt" goto BUILD_OK
   if "%2"=="NOMT" goto BUILD_OK
   if "%2"=="nomt" goto BUILD_OK

   SET HB_MT=mt
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   make -s -l -DHB_THREAD_SUPPORT -fwinmake\makefile.bc >>make_%SUB_DIR%.log
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
   make -s -l -fwinmake\makefile.bc >dll_%SUB_DIR%.log
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
   make -s -l -fwinmake\makefile.bc >cont_%SUB_DIR%.log
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
