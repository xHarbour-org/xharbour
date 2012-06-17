@echo off
rem ============================================================================
rem
rem $Id$
rem
rem FILE: make_gc.bat
rem BATCH FILE FOR MINGW32
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

IF "%CC_DIR%"=="" SET CC_DIR=C:/MinGW
IF "%SUB_DIR%"=="" SET SUB_DIR=gc
IF "%HB_GT_LIB%"=="" SET HB_GT_LIB=$(GTWIN_LIB)
IF "%BISON_DIR%"=="" SET BISON_DIR=C:/MSYS/1.0/bin

SET _PATH=%PATH%
SET PATH=%CC_DIR%\bin;%BISON_DIR%;%PATH%

rem Added for MinGW 4.70 -mno-cygwin is no longer valid
rem SET NO_CYGWIN=1

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET DIR_SEP=/
SET OBJEXT=.o
SET LIBEXT=.a
SET LIBPREFIX=lib

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
   mingw32-make.exe -f winmake\makefile.gc 1>make0_%SUB_DIR%.log 2>make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   if "%1"=="NOMT" goto BUILD_OK
   if "%1"=="nomt" goto BUILD_OK
   if "%2"=="NOMT" goto BUILD_OK
   if "%2"=="nomt" goto BUILD_OK

   SET HB_THREAD_SUPPORT=1
   SET HB_MT=mt
   SET HB_MT_DIR=/mt
   @CALL winmake\mdir.bat
   mingw32-make.exe -f winmake\ makefile.gc 1>>make0_%SUB_DIR%.log 2>>make_%SUB_DIR%.log
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
   rem==========================================================================
   rem We use HB_MT_DIR envar for DLL object folder here
   rem==========================================================================
   SET __BLD__=DLL_BLD
   SET HB_THREAD_SUPPORT=
   SET HB_MT=
   SET HB_MT_DIR=/dll
   @CALL winmake\mdir.bat dllcreate
   mingw32-make.exe -f winmake\makefile.gc  1>dll0_%SUB_DIR%.log 2>dll_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
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
   mingw32-make.exe -f winmake\makefile.gc  1>cont0_%SUB_DIR%.log 2>cont_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR

   REM SET HB_THREAD_SUPPORT=1
   REM SET HB_MT=mt
   REM SET HB_MT_DIR=/mt
   REM mingw32-make.exe -f winmake\ makefile.gc  1>>cont0_%SUB_DIR%.log 2>>cont_%SUB_DIR%.log
   REM if errorlevel 1 goto BUILD_ERR

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
   ECHO. ------------------------
   ECHO. Make Utility for MinGW32
   ECHO. ------------------------
   @CALL winmake\mdir.bat howto
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL winmake\mdir.bat clean
   IF EXIST make0_%SUB_DIR%.log	DEL make0_%SUB_DIR%.log
   IF EXIST cont0_%SUB_DIR%.log	DEL cont0_%SUB_DIR%.log
   IF EXIST dll0_%SUB_DIR%.log	DEL dll0_%SUB_DIR%.log
   IF "%2"=="BUILD" goto BUILD_ALL
   IF "%2"=="build" goto BUILD_ALL
   @ECHO ****** End of Job *****
   goto EXIT

rem=============================================================================
:EXIT
rem=============================================================================
   @CALL winmake\mdir.bat resetenvar
