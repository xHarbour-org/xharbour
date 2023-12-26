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

IF NOT "%CC%"=="" (
   IF NOT "%CC%"=="cl" GOTO SWITCH_CC
)
GOTO FOUND

:SWITCH_CC
   ECHO Your Environment is set to use %CC% as the compiler.
   ECHO This batch file is intended to be used with MSVC only.
   SET /P RESPONSE="Do you want to switch to MSVC (Y/N)? "
   IF /I "%RESPONSE%"=="Y" (
      SET CC=
      SET CC_DIR=
      SET RC_DIR=
      SET BCC_LIB=
      SET LIBEXT=
      SET HB_LIB_INSTALL=
      
      GOTO SET_VC
   )
   EXIT /B 1

:SET_VC
   IF NOT "%CC%"=="" IF NOT "%CC_DIR%"=="" GOTO FOUND
   CALL %~dp0find_vc.bat

IF "%CC%"=="" GOTO NOT_FOUND

:FOUND
REM Make sure that xHarbour's bin and MSVC's bin are in the path even after we restore the original path! 
harbour -credit > nul 2>&1 || ECHO For your convenience xHarbour's bin directory was added to your PATH && set PATH=%~dp0;%PATH%
IF "%VSINSTALLDIR%"=="" IF EXIST "%CC_DIR%"\vcvarsall.bat cl 2>&1 || ECHO For your convenience vcvarsall.bat will be called to setup your MSVC... && CALL "%CC_DIR%"\vcvarsall.bat && GOTO SAVE_PATH
IF "%VSINSTALLDIR%"=="" IF "%VCINSTALLDIR%"=="" cl > nul 2>&1 || ECHO For your convenience MSVC's bin folders were added to PATH && set PATH="%CC_DIR%\bin";%VSCOMMONTOOLS%;"%RC_DIR%"

:SAVE_PATH
   REM Save the original path before further modifications   
   SET _PATH=%PATH%

CALL %~dp0bld.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO END

:NOT_FOUND
    rem Let's return an error code to the caller
    exit /b 1

:END
SET PATH=%_PATH%
SET _PATH=


