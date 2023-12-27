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
   IF NOT "%CC%"=="bcc32" IF NOT "%CC%"=="bcc32c" GOTO SWITCH_CC
)

GOTO SET_BCC

:SWITCH_CC
   ECHO Your Environment is set to use %CC% as the compiler.
   ECHO This batch file is intended to be used with BCC only.
   SET /P RESPONSE="Do you want to switch to BCC (Y/N)? "
   IF /I "%RESPONSE%"=="Y" (
      SET CC=
      SET CC_DIR=
      SET RC_DIR=
      SET BCC_LIB=
      SET LIBEXT=
      SET HB_LIB_INSTALL=
      
      GOTO SET_BCC
   )
   EXIT /B 1

:SET_BCC
   call %~dp0find_bc.bat

IF "%CC%"=="" GOTO NOT_FOUND

:FOUND
   REM Make sure that xHarbour's bin and BCC's bin are in the path even after we restore the original path! 
   harbour -credit > nul 2>&1 || ECHO For your convenience xHarbour's bin directory was added to your PATH && set PATH=%~dp0;%PATH%

:SAVE_PATH
   SET _PATH=%PATH%

call %~dp0bld.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:NOT_FOUND
    rem Let's return an error code to the caller
    exit /b 1

:END
SET PATH=%_PATH%
SET _PATH=

