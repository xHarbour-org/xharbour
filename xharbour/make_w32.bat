@echo off
rem ============================================================================
rem
rem $Id$
rem
rem FILE: make_w32.bat
rem BATCH FILE FOR OPENWATCOM
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\WATCOM
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=w32
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
   wmake -h -ms -f winmake\makefile.wc >make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   if "%1"=="NOMT" goto BUILD_OK
   if "%1"=="nomt" goto BUILD_OK
   if "%2"=="NOMT" goto BUILD_OK
   if "%2"=="nomt" goto BUILD_OK

   SET HB_MT=mt
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   wmake -h -ms HB_THREAD_SUPPORT=1 -f winmake\makefile.wc >>make_%SUB_DIR%.log
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
   SET HB_MT=
   SET HB_MT_DIR=\dll
   @CALL winmake\mdir.bat dllcreate
   wmake -h -ms -f winmake\makefile.wc >dll_%SUB_DIR%.log
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
   wmake -h -ms -f winmake\makefile.wc >cont_%SUB_DIR%.log
   if errorlevel 1 goto CONTRIBS_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=
   REM wmake -h -ms HB_THREAD_SUPPORT=1 -f winmake\makefile.wc >>cont_%SUB_DIR%.log
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
   ECHO. ---------------------------------
   ECHO. Make Utility for OpenWatcom C/C++
   ECHO. ---------------------------------
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
   SET LIB=%_LIB%
   SET _LIB=
   SET WATCOM=
   SET EDPATH=
   @CALL winmake\mdir.bat resetenvar
