@ECHO OFF

:SAVE
   SET _PRESET_PATH=%PATH%
   SET _PRESET_INCLUDE=%INCLUDE%
   SET _PRESET_LIB=%LIB%
   SET _PRESET_CFLAGS=%CFLAGS%
   SET _PRESET_LFLAGS=%LFLAGS%
   SET _PRESET_MSVCDIR=%MSVCDIR%
   SET _PRESET_PSDKDIR=%PSDKDIR%
   SET _PRESET_PELLESCDIR=%PELLESCDIR%

:FIND_VC
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools"   GOTO SET_VC2017EX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools"        GOTO SET_VC2017E
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Common7\Tools" GOTO SET_VC2017PX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Professional\Common7\Tools"      GOTO SET_VC2017P
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Common7\Tools"    GOTO SET_VC2017CX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Community\Common7\Tools"         GOTO SET_VC2017C
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC" GOTO SET_VC2015X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 14.0\VC"      GOTO SET_VC2015
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

:SET_VC2017EX86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Vc
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\..\Common7\Tools\vsdevcmd.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY

:SET_VC2017E
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\Vc
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\..\Common7\Tools\vsdevcmd.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY

:SET_VC2017PX86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Vc
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\..\Common7\Tools\vsdevcmd.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY

:SET_VC2017P
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio\2017\Professional\Vc
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\..\Common7\Tools\vsdevcmd.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY

:SET_VC2017CX86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Vc
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\..\Common7\Tools\vsdevcmd.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY

:SET_VC2017C
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio\2017\Community\Vc
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\..\Common7\Tools\vsdevcmd.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY

:SET_VC2015X86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\Vc
   SET PELLESCDIR=%ProgramFiles(x86)%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY

:SET_VC2015
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 14.0\vc
   SET PSDKDIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2013X86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\Vc
   SET PSDKDIR=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A
   SET PELLESCDIR=%ProgramW6432%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2013
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 12.0\vc
   SET PSDKDIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2012X86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   SET PELLESCDIR=%ProgramW6432%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2012
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 11.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2010X86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   SET PELLESCDIR=%ProgramW6432%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2010
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 10.0\vc
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2008
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 9.0\vc
   SET PSDKDIR=%ProgramFiles%\Microsoft SDKs\Windows\v6.0A
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2005
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio 8\vc
   SET PSDKDIR=%ProgramFiles%\Microsoft Visual Studio 8\Common7\Tools
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC2003
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio .NET 2003\VC7
   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\vcvarsall.bat"
   GOTO READY

:SET_VC6

   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio\VC98

   SET PSDKDIR="*** PLEASE SET PSDKDIR ***"

   SET PELLESCDIR=%ProgramFiles%\PellesC

   CALL "%MSVCDIR%\vcvarsall.bat"

   GOTO READY



:NONE
   ECHO Could not locate any MSVC installation!
   GOTO RESTORE

:READY
   SET PATH=%PATH%;"%PELLESCDIR%\bin"

   SET CFLAGS=/Od /EHsc /RTC1 /MTd /Gs /GS /Gy /GR /Zi /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE
   SET LFLAGS=-DEBUG -DEBUGTYPE:CV

   IF NOT EXIST MSObj MD MSObj
   IF NOT EXIST Obj MD Obj

   NMAKE.exe -S > Log

   IF ERRORLEVEL 0 GOTO SUCCESS

:FAILURE
   TYPE Log.
   PAUSE
   GOTO RESTORE

:SUCCESS
   xcopy xlib.exe \xhb\bin /d /r /y

:RESTORE
   SET PATH=%_PRESET_PATH%
   SET INCLUDE=%_PRESET_INCLUDE%
   SET LIB=%_PRESET_LIB%
   SET CFLAGS=%_PRESET_CFLAGS%
   SET LFLAGS=%_PRESET_LFLAGS%
   SET MSVCDIR=%_PRESET_MSVCDIR%
   SET PSDKDIR=%_PRESET_PSDKDIR%
   SET PELLESCDIR=%_PRESET_PELLESCDIR%

:CLEANUP
   SET _PRESET_PATH=
   SET _PRESET_INCLUDE=
   SET _PRESET_LIB=
   SET _PRESET_CFLAGS=
   SET _PRESET_LFLAGS=
   SET _PRESET_MSVCDIR=
   SET _PRESET_PSDKDIR=
   SET _PRESET_PELLESCDIR=
