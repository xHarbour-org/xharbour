@ECHO OFF

SET _PRESET_PATH=%PATH%
SET _PRESET_INCLUDE=%INCLUDE%
SET _PRESET_LIB=%LIB%
SET _PRESET_CFLAGS=%CFLAGS%
SET _PRESET_LFLAGS=%LFLAGS%

IF NOT "%MSVCDIR%"=="" GOTO READY 
:FIND_VC
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 12.0\VC"      GOTO SET_VC2013
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc" GOTO SET_VC2012X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 11.0\vc"      GOTO SET_VC2012
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc" GOTO SET_VC2010X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 10.0\vc"      GOTO SET_VC2010

   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 9.0\vc"  GOTO SET_VC2008
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 8\vc"    GOTO SET_VC2005
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 2003\vc" GOTO SET_VC2003
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\vc8"     GOTO SET_VC6
   GOTO NONE

:SET_VC2013
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 12.0\vc
   SET PSDKDIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2012X86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2012
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 11.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2010X86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2010
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 10.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2008
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 9.0\vc
   SET PSDKDIR=%ProgramFiles%\Microsoft SDKs\Windows\v6.0A
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2005
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 8\vc
   SET PSDKDIR=%ProgramFiles%\Microsoft Visual Studio 8\Common7\Tools
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2003
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual .NET 2003\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC6
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio\vc98
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:NONE   

:READY
SET PELLESCDIR=%ProgramFiles%\PellesC
SET PATH=%PATH%;"%PELLESCDIR%\bin"

SET CFLAGS=/Od /EHsc /RTC1 /MTd /Gs /GS /Gy /GR /Zi /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE
SET LFLAGS=-DEBUG -DEBUGTYPE:CV

IF NOT EXIST MSObj MD MSObj
IF NOT EXIST Obj MD Obj

NMAKE -f DLL.MAK > Log
@IF ERRORLEVEL 1 GOTO Q

NMAKE -f EXE.MAK >> Log
@IF ERRORLEVEL 1 GOTO Q

:Q
IF ERRORLEVEL 0 xcopy xrc.exe \xhb\bin /d /r /y
IF ERRORLEVEL 0 xcopy xrc.dll \xhb\bin /d /r /y

SET PATH=%_PRESET_PATH%
SET INCLUDE=%_PRESET_INCLUDE% 
SET LIB=%_PRESET_LIB% 
SET CFLAGS=%_PRESET_CFLAGS% 
SET LFLAGS=%_PRESET_LFLAGS% 

SET _PRESET_PATH=
SET _PRESET_INCLUDE=
SET _PRESET_LIB=
SET _PRESET_CFLAGS=
SET _PRESET_LFLAGS=
