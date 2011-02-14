@echo off
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
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 9.0\vc"  GOTO SET_VC2008
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 8\vc"    GOTO SET_VC2005
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 2003\vc" GOTO SET_VC2003
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\vc8"     GOTO SET_VC6

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

:SET_PATH
IF EXIST "%CC_DIR%"\vcvarsall.bat CALL "%CC_DIR%"\vcvarsall.bat

SET PATH="%CC_DIR%\bin";%~dp0;%PATH%

call bld.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

SET PATH=%_PATH%
SET INCLUDE=%_INCLUDE%
SET LIB=%_LIB%

SET _PATH=
SET _INCLUDE=
SET _LIB=
SET CC_DIR=
