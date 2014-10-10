ECHO ON

SET _PRESET_PATH=%PATH%
SET _PRESET_INCLUDE=%INCLUDE%
SET _PRESET_LIB=%LIB%
SET _PRESET_CFLAGS=%CFLAGS%
SET _PRESET_LFLAGS=%LFLAGS%
 
IF NOT "%MSVCDIR%"=="" GOTO READY
:FIND_VC
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\VC" GOTO SET_VC2013X86
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

:SET_VC2013X86
   SET CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\Vc
   IF "%VS120COMNTOOLS%"=="" SET VS120COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\Common7\Tools\
   IF NOT "%VS120COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS120COMNTOOLS%
   IF EXIST "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET RC_DIR=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\
   GOTO FIND_BISON

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
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***""
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC6
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio\vc98
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***""
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:NONE   
   ECHO MSVC not found!!!
   GOTO RESTORE
   
:READY

ECHO ON

SET PELLESCDIR=%ProgramFiles%\PellesC
SET XCCDIR=\xhb\bin
SET PATH=%PATH%;"%XCCDIR%"

REM SET CFLAGS=/Od /EHsc /RTC1 /MTd /Gs /GS /Gy /GR /Zi /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE
REM SET LFLAGS=-DEBUG -DEBUGTYPE:CV

IF NOT EXIST Objs MD Objs
IF NOT EXIST mtObjs MD mtObjs

echo single-thread > log
nmake >> log
if errorlevel 1 goto q

echo multi-thread >>log
nmake __MT__=1 >> log
if errorlevel 1 goto q

REM echo wince
REM ..\utils\narmw -o wince\xdiv.obj -f win32 wince\xdiv.asm
REM @if errorlevel 1 goto Q
REM xlib.exe -out:crtce.lib wince\xdiv.obj

:q
IF ERRORLEVEL 0 xcopy crt.lib \xhb\c_lib /d /r /y
IF ERRORLEVEL 0 xcopy crtmt.lib \xhb\c_lib /d /r /y
xcopy "%PELLESCDIR%\lib\win" \xhb\c_lib\win /d /y 

:RESTORE
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
