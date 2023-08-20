rem @echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

set HB_ARCHITECTURE=w32
set HB_COMPILER=msvc

SET _PATH=%PATH%
SET _INCLUDE=%INCLUDE%
SET _LIB=%LIB%

:FIND_VC
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools"   GOTO SET_VC2017EX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools"        GOTO SET_VC2017E
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Common7\Tools" GOTO SET_VC2017PX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Professional\Common7\Tools"      GOTO SET_VC2017P
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Common7\Tools"    GOTO SET_VC2017CX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Community\Common7\Tools"         GOTO SET_VC2017C
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2022\Community\Common7\Tools"         GOTO SET_VC2022E   
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC" GOTO SET_VC2015X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 14.0\Vc"      GOTO SET_VC2015
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\vc" GOTO SET_VC2013X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 12.0\VC"      GOTO SET_VC2013
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc" GOTO SET_VC2012X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 11.0\vc"      GOTO SET_VC2012
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc" GOTO SET_VC2010X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 10.0\vc"      GOTO SET_VC2010
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 9.0\vc"       GOTO SET_VC2008
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 8\vc"         GOTO SET_VC2005
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 2003\vc"      GOTO SET_VC2003
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\vc8"          GOTO SET_VC6

:SET_VC2017EX86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Vc
   GOTO SET_PATH

:SET_VC2017E
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\Vc
   GOTO SET_PATH

:SET_VC2017PX86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Vc
   GOTO SET_PATH

:SET_VC2017P
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2017\Professional\Vc
   GOTO SET_PATH

:SET_VC2017CX86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\
   SET _ARCH=x64
   GOTO SET_PATH

:SET_VC2017C
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\
   SET _ARCH=x86
   GOTO SET_PATH

:SET_VC2015X86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\vc
   GOTO SET_PATH

:SET_VC2015
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 14.0\vc
   GOTO SET_PATH

:SET_VC2013X86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\vc
   GOTO SET_PATH

:SET_VC2013
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 12.0\vc
   GOTO SET_PATH

:SET_VC2012X86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc
   GOTO SET_PATH

:SET_VC2012
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 11.0\vc
   GOTO SET_PATH

:SET_VC2010X86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc
   GOTO SET_PATH

:SET_VC2010
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 10.0\vc
   GOTO SET_PATH

:SET_VC2008
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 9.0\vc
   GOTO SET_PATH

:SET_VC2005
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 8\vc
   GOTO SET_PATH

:SET_VC2003
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio .NET 2003\VC7
   GOTO SET_PATH

:SET_VC6
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio\VC98
   GOTO SET_PATH

:SET_VC2022E
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2022\Community\vc\Auxiliary\Build
   GOTO SET_PATH

:SET_PATH
IF EXIST "%CC_DIR%"\vcvars32.bat CALL "%CC_DIR%"\vcvars32.bat

SET PATH="%CC_DIR%\bin";%~dp0;%PATH%

call bld.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

SET PATH=%_PATH%
SET INCLUDE=%_INCLUDE%
SET LIB=%_LIB%

SET _PATH=
SET _INCLUDE=
SET _LIB=
SET CC_DIR=


