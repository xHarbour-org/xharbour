@ECHO OFF

IF "%1"=="-ALL"  GOTO CLEAN
IF "%1"=="-All"  GOTO CLEAN
IF "%1"=="-all"  GOTO CLEAN
IF "%1"=="CLEAN" GOTO CLEAN
GOTO SAVE

:CLEAN
IF EXIST \xHarbour.com\xHarbour-XBScript\XBScript    RD \xHarbour.com\xHarbour-XBScript\XBScript    /S/Q
IF EXIST \xHarbour.com\xHarbour-XBScript\XBScriptPro RD \xHarbour.com\xHarbour-XBScript\XBScriptPro /S/Q
IF EXIST \xHarbour.com\xHarbour-XBScript\XBScriptEnt RD \xHarbour.com\xHarbour-XBScript\XBScriptEnt /S/Q
IF "%1"=="CLEAN" GOTO CLEANUP

:SAVE
   SET _PRESET_PATH=%PATH%
   SET _PRESET_INCLUDE=%INCLUDE%
   SET _PRESET_LIB=%LIB%
   SET _PRESET_CFLAGS=%CFLAGS%
   SET _PRESET_LFLAGS=%LFLAGS%
   SET _PRESET_MSVCDIR=%MSVCDIR%
   SET _PRESET_PELLESCDIR=%PELLESCDIR%

:FIND_VC
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Common7\Tools"    GOTO SET_VC2017CX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Community\Common7\Tools"         GOTO SET_VC2017C
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2022\Community\Common7\Tools"         GOTO SET_VC2022E   
   GOTO NONE

:SET_VC2017EX86
   SET MSVCDIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Vc
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

:SET_VC2022E
   SET MSVCDIR=%ProgramFiles%\Microsoft Visual Studio\2022\Community\Vc
   SET PELLESCDIR=%ProgramFiles%\PellesC
   CALL "%MSVCDIR%\..\Common7\Tools\vsdevcmd.bat"
   SET PSDKDIR=%WindowsSdkBinPath%..
   GOTO READY


:NONE
   ECHO Could not locate any MSVC installation!
   GOTO RESTORE

:READY
   SET PATH=%PATH%;"%PELLESCDIR%\bin"

   SET CFLAGS=/Od /EHsc /RTC1 /MTd /Gs /GS /Gy /GR /Zi /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE
   SET LFLAGS=-DEBUG -DEBUGTYPE:CV
   SET XHB_VC8=\xharbour
   
   @ECHO ON
   
   \xHB\Bin\xBuild.exe \xharbour\xHarbourBuilder\xHarbour-HBZlib\vc8\xhbzip.lib.xbp %1 > xhbzip.log
   IF NOT ERRORLEVEL 0 GOTO FAILURE

   msbuild XBScript.sln > XBScript.log 2>CON
   IF NOT ERRORLEVEL 0 GOTO FAILURE
   
   msbuild XBScriptPro.sln > XBScriptPro.log
   IF NOT ERRORLEVEL 0 GOTO FAILURE
   
   msbuild XBScriptEnt.sln > XBScriptEnt.log
   IF NOT ERRORLEVEL 0 GOTO FAILURE

   GOTO SUCCESS

:FAILURE
   PAUSE "Error building!"
   GOTO RESTORE

:SUCCESS
   @ECHO Success
   
:RESTORE
   SET PATH=%_PRESET_PATH%
   SET INCLUDE=%_PRESET_INCLUDE%
   SET LIB=%_PRESET_LIB%
   SET CFLAGS=%_PRESET_CFLAGS%
   SET LFLAGS=%_PRESET_LFLAGS%
   SET MSVCDIR=%_PRESET_MSVCDIR%
   SET PELLESCDIR=%_PRESET_PELLESCDIR%

:CLEANUP
   SET _PRESET_PATH=
   SET _PRESET_INCLUDE=
   SET _PRESET_LIB=
   SET _PRESET_CFLAGS=
   SET _PRESET_LFLAGS=
   SET _PRESET_MSVCDIR=
   SET _PRESET_PELLESCDIR=
