@echo off
rem ============================================================================
rem
rem $Id$
rem
rem FILE: make_vc.bat
rem BATCH FILE FOR MSVC
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

REM SET HB_ARCH=64
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

IF NOT "%CC_DIR%"=="" GOTO FIND_BISON

:FIND_VC
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 12.0\VC"      GOTO SET_VC2013
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc" GOTO SET_VC2012X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 11.0\vc"      GOTO SET_VC2012
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc" GOTO SET_VC2010X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 10.0\vc"      GOTO SET_VC2010
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 9.0\vc"       GOTO SET_VC2008
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 8\vc"         GOTO SET_VC2005
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 2003\vc"      GOTO SET_VC2003
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\vc8"          GOTO SET_VC6
   GOTO FIND_BISON

:SET_VC2013
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 12.0\vc
   IF "%VS120COMNTOOLS%"=="" SET VS120COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 12.0\Common7\Tools\
   IF NOT "%VS120COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS120COMNTOOLS%
   IF EXIST "%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET RC_DIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\
   GOTO FIND_BISON

:SET_VC2012X86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc
   IF "%VS110COMNTOOLS%"=="" SET VS110COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\Common7\Tools\
   IF NOT "%VS110COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS110COMNTOOLS%
   IF EXIST "%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET RC_DIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\
   GOTO FIND_BISON

:SET_VC2012
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 11.0\vc
   IF "%VS110COMNTOOLS%"=="" SET VS110COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 11.0\Common7\Tools\
   IF NOT "%VS110COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS110COMNTOOLS%
   IF EXIST "%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET RC_DIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\
   GOTO FIND_BISON

:SET_VC2010X86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc
   IF "%VS100COMNTOOLS%"=="" SET VS100COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\Common7\Tools\
   IF NOT "%VS100COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS100COMNTOOLS%
   GOTO FIND_BISON

:SET_VC2010
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 10.0\vc
   IF "%VS100COMNTOOLS%"=="" SET VS100COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 10.0\Common7\Tools\
   IF NOT "%VS100COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS100COMNTOOLS%
   GOTO FIND_BISON

:SET_VC2008
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 9.0\vc
   IF "%VS90COMNTOOLS%"=="" SET VS90COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 9.0\Common7\Tools\
   IF NOT "%VS90COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS90COMNTOOLS%
   GOTO FIND_BISON

:SET_VC2005
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 8\vc
   GOTO FIND_BISON

:SET_VC2003
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio .NET 2003\VC7
   GOTO FIND_BISON

:SET_VC6
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio\VC98
   GOTO FIND_BISON

:FIND_BISON
   IF NOT "%BISON_DIR%"=="" GOTO READY
   IF EXIST "%ProgramFiles(x86)%\GnuWin32\Bin" GOTO SET_BISONX86
   IF EXIST "%ProgramFiles%\GnuWin32\Bin"      GOTO SET_BISON1
   IF EXIST \GnuWin32\Bin                      GOTO SET_BISON2
   GOTO READY

:SET_BISONX86
   SET BISON_DIR=%ProgramFiles(x86)%\GnuWin32\Bin
   GOTO READY

:SET_BISON1
   SET BISON_DIR=%ProgramFiles%\GnuWin32\Bin
   GOTO READY

:SET_BISON2
   SET BISON_DIR=\GnuWin32\Bin
   GOTO READY 

:READY
SET SUB_DIR=vc
SET HB_GT_LIB=$(GTWIN_LIB)

SET _PATH=%PATH%
IF EXIST "%CC_DIR%"\vcvarsall.bat CALL "%CC_DIR%"\vcvarsall.bat
PATH "%CC_DIR%\bin";%VSCOMMONTOOLS%;"%RC_DIR%";"%BISON_DIR%";%~dp0bin;%PATH%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET LIBEXT=%HB_ARCH%%HB_DEBUG%.lib
SET OBJEXT=%HB_ARCH%%HB_DEBUG%.obj
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
   nmake /NOLOGO -f winmake\makefile.vc >make_%SUB_DIR%.log
   if errorlevel 1 goto BUILD_ERR
   if "%1"=="NOMT" goto BUILD_OK
   if "%1"=="nomt" goto BUILD_OK
   if "%2"=="NOMT" goto BUILD_OK
   if "%2"=="nomt" goto BUILD_OK

   SET HB_MT=mt
   SET HB_MT_DIR=
   @CALL winmake\mdir.bat
   nmake /NOLOGO HB_THREAD_SUPPORT=1 -f winmake\makefile.vc >>make_%SUB_DIR%.log
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
   nmake /NOLOGO -f winmake\makefile.vc >dll_%SUB_DIR%.log
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
   nmake /NOLOGO -f winmake\makefile.vc >cont_%SUB_DIR%.log
   if errorlevel 1 goto CONTRIBS_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=
   REM nmake  /NOLOGO HB_THREAD_SUPPORT=1 -f winmake\makefile.vc >>cont_%SUB_DIR%.log
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
   ECHO. ---------------------------------------
   ECHO. Make Utility for Miscosoft Visual C/C++
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
