:: Copyright (c) 2023 Ron Pinkas (ron@ronpinkas.com)
:: 
:: Permission is granted, free of charge, to any person obtaining a copy 
:: of this software and associated documentation files, to deal in the 
:: Software without restriction, including rights to use, copy, modify, 
:: merge, publish, distribute, sublicense, and/or sell copies of the Software, 
:: and to permit persons to whom the Software is furnished to do so, 
:: subject to the terms of the MIT License.
@ECHO OFF

IF "%scriptName%" == "" SET "scriptName=%~n0" && ECHO *** START [%~f0] > %~dp0functions.log

REM First check if already set.
IF "%CC%" NEQ "" GOTO CHECK_CC

:SET_CC
   REM Quick check if CAN be quickly set (CC_DIR and PATH might be set).
   CALL %~dp0found_cc.bat pocc
   IF ERRORLEVEL 2 EXIT /B 2
   IF ERRORLEVEL 1 GOTO SET_C_COMPILER
   IF ERRORLEVEL 0 GOTO FOUND

   ECHO "[%~f0] - (%ERRORLEVEL%) Unexpected error!"  
   EXIT /B 99

:CHECK_CC
   REM Will force a SWITCH_CC if CC is NOT pocc
   IF "%CC%" NEQ "pocc" GOTO SET_CC

   REM Quick check if the compiler is already set.
   CALL %~dp0found_cc.bat %CC%
   IF ERRORLEVEL 2 EXIT /B 2
   IF ERRORLEVEL 1 GOTO SET_C_COMPILER
   IF ERRORLEVEL 0 GOTO FOUND

   ECHO "[%~f0] - (%ERRORLEVEL%) Unexpected error!" 
   EXIT /B 99
   
REM The Entry point for FIRST run.

:SET_C_COMPILER
   ver > nul REM Reset ERRORLEVEL

   REM Check if CC_DIR is set by user
   REM Will continue as appropriate or jump to NOT_FOUND
   IF "%CC_DIR%" NEQ "" (
      CALL :CHECK_CC_DIR
      IF ERRORLEVEL 1      GOTO NOT_FOUND
      IF ERRORLEVEL 0      GOTO DIR_SET
   )

   REM Fall through to FIND_C_COMPILER

:FIND_C_COMPILER
   ECHO Searching for Pelles C...
   SET "CC=pocc"

   REM Check if the compiler is in the path
   CALL %~dp0functions.bat findInPath CC CC_DIR
   IF "%CC_DIR%" NEQ ""                         GOTO DIR_SET

   SET LD=

   REM Check if the compiler is in the known locations file

   REM *** Make sure to set the correct KNOWN file for your compiler ***
   CALL %~dp0functions.bat findKnown %~dp0known.pc CC CC_DIR
   
   IF "%CC_DIR%" NEQ ""                         GOTO DIR_SET

   GOTO NOT_FOUND

:DIR_SET
   IF "%CC%" == "" SET "CC=pocc"
   
   REM Remove the trailing backslash
   IF "%CC_DIR:~-1%" == "\" CALL %~dp0functions.bat Left CC_DIR -1 CC_DIR

   WHERE %CC%.exe >nul 2>&1 &&                  GOTO PATH_OK
   IF EXIST "%CC_DIR%\bin\%CC%.exe"             GOTO PATH_SET

   GOTO NOT_FOUND

:PATH_SET
   REM ECHO PATH_SET '%CC_DIR%' for '%CC%'
   REM We arrived here ONLY if %CC%.exe exists in %CC_DIR%\bin and not in PATH.
   ECHO For your convenience %CC%'s bin directory was added to your PATH && SET "PATH=%CC_DIR%\bin;%PATH%"

:PATH_OK
   WHERE %CC%.exe >nul 2>&1 || GOTO NOT_FOUND

   GOTO FOUND

:NOT_FOUND
   rem Let's return an error code to the caller
   echo "ERROR: Pelles C not found!"
   exit /b 1
   
:FOUND
   IF "%CC%" == "" SET "CC=pocc"
   SET "LD=%CC%"

   IF "%HB_ARCH%" == "" SET "HB_ARCH=w32"

   IF "%HB_ARCH%" == "w32" SET "SUB_DIR=pc32"
   IF "%HB_ARCH%" == "w64" SET "SUB_DIR=pc64"

   exit /b 0

REM this is called as a FUNCTION from find_bc.bat (do not use GOTOs!!!)
REM Returns ERRORLEVEL 2 [Abort] if CC_DIR is not set and user does not want to search for known locations.
REM Returns ERRORLEVEL 1 [Continue] if CC_DIR is not set and user wants to search for known locations.
REM Returns ERRORLEVEL 0 [Ready] if CC_DIR is set.
:CHECK_CC_DIR 
   REM Check if CC is set by user

   IF "%CC%" == "" SET "CC=pocc"

   REM IF %CC% is in bin sub folder then we can SKIP the FIND_C_COMPILER section and go directly to DIR_SET
   IF EXIST "%CC_DIR%\bin\%CC%.exe"                      EXIT /B 0

   REM If we are here then compiler was not found in the user specified CC_DIR!

   ECHO Could not find Pelles C at '%CC_DIR%'!

   REM Ask the user if they want to search for known locations.
   CALL %~dp0functions.bat continue_Y_N "Search known locations (Y/N)? "
   IF "%ERRORLEVEL%" == "0" SET "CC_DIR=" && EXIT /B 1 
   EXIT /B %ERRORLEVEL%
