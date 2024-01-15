:: Copyright (c) 2023 Ron Pinkas (ron@ronpinkas.com)
:: 
:: Permission is granted, free of charge, to any person obtaining a copy 
:: of this software and associated documentation files, to deal in the 
:: Software without restriction, including rights to use, copy, modify, 
:: merge, publish, distribute, sublicense, and/or sell copies of the Software, 
:: and to permit persons to whom the Software is furnished to do so, 
:: subject to the terms of the MIT License.
@ECHO OFF

IF "%scriptName%" NEQ "" ((SET "scriptName=%~n0") & ECHO    *** started [%~f0] >> %~dp0\functions.log)
IF "%scriptName%" == ""  ((SET "scriptName=%~n0") & ECHO *** START [%~f0] > %~dp0\functions.log)

SET "C_NAME=pocc"
SET "C_LONG_NAME=Pelles C"
SET "C_SHORT_NAME=pc"

REM First check if already set.
IF "%CC%" NEQ "" GOTO CHECK_CC

:SET_CC
   :TRY_C_NAME
      REM Quick check if CAN be quickly set (CC_DIR and PATH might be set).
      CALL %~dp0found_cc.bat %C_NAME% %C_LONG_NAME%
      IF ERRORLEVEL 99 GOTO FIND_EXIT_99
      IF ERRORLEVEL  2 GOTO FIND_EXIT_2
      IF ERRORLEVEL  1 GOTO SET_C_COMPILER
      IF ERRORLEVEL  0 GOTO FOUND

      ECHO "[%~f0](30) - (%ERRORLEVEL%) Unexpected error!"  
      GOTO FIND_EXIT_99

:CHECK_CC
   REM Will force a SWITCH_CC if CC is NOT C_NAME
   IF "%CC%" NEQ "%C_NAME%" GOTO SET_CC

:CHECK_FOUND
   REM Quick check if the compiler is already set.
   CALL %~dp0found_cc.bat %CC%
      IF ERRORLEVEL 99 GOTO FIND_EXIT_99
      IF ERRORLEVEL  2 GOTO FIND_EXIT_2
      IF ERRORLEVEL  1 GOTO SET_C_COMPILER
      IF ERRORLEVEL  0 GOTO FOUND
      ECHO "[%~f0](44) - (%ERRORLEVEL%) Unexpected error!" 
      GOTO FIND_EXIT_99
   
REM The Entry point for FIRST run.
:SET_C_COMPILER
   ver > nul REM Reset ERRORLEVEL
   SET "CC=%C_NAME%"

   REM Check if CC_DIR is set by user and continue to DIR_SET | FIND_C_COMPILER | NOT_FOUND
   IF "%CC_DIR%" NEQ "" GOTO CHECK_CC_DIR

   REM Fall through to FIND_C_COMPILER

:FIND_C_COMPILER
   ECHO Searching for %C_LONG_NAME%...
   SET "CC_DIR="

   REM Check if the compiler is in the path
   CALL %~dp0functions.bat rootOfAppInPath CC CC_DIR
   IF "%CC_DIR%" NEQ ""                         GOTO DIR_SET

   REM Check if the compiler is in the known locations file
   
   REM *** Make sure to set the correct KNOWN file for your compiler ***
   CALL %~dp0functions.bat findKnown %~dp0known.%C_SHORT_NAME% CC CC_DIR

   IF "%CC_DIR%" NEQ ""                         GOTO DIR_SET

   GOTO NOT_FOUND

ECHO [%~f0](74) - (%ERRORLEVEL%) Unexpected error!
GOTO FIND_EXIT_99

:DIR_SET
   REM Remove the trailing backslash if any
   IF "%CC_DIR:~-1%" == "\" CALL %~dp0functions.bat Left CC_DIR -1 CC_DIR

   :CHECK_IN_PATH
      SET CC_BIN=%CC_DIR%\bin
      ECHO Checking if '%CC_BIN%' is in '%PATH%'... >> %~dp0\functions.log
      CALL %~dp0functions.bat isPathInPath CC_BIN _inPath
      IF "%_inPath%" == "true" ((SET "_inPath=") & GOTO PATH_OK)

   REM If we are here then %CC_DIR%\bin is NOT in PATH so let's add it
   IF EXIST "%CC_DIR%\bin\%CC%.exe" GOTO PATH_SET

   GOTO NOT_FOUND

:PATH_SET
   ECHO Adding '%CC_DIR%\bin' to PATH... >> %~dp0\functions.log
   REM We arrived here ONLY if %CC%.exe exists in %CC_DIR%\bin and not in PATH.
   IF NOT EXIST "%CC_DIR%\bin\%CC%.exe" GOTO EXE_NOT_FOUND
   SET "PATH=%CC_DIR%\bin;%PATH%"
   ECHO For your convenience %CC%'s bin directory was added to your PATH
   GOTO PATH_OK

:EXE_NOT_FOUND
   ECHO [%~f0](101) - Unexpected error! %CC_DIR%\bin\%CC%.exe does not exist!
   GOTO FIND_EXIT_99

:PATH_OK
   ECHO PATH OK for: CC='%CC%' CC_DIR='%CC_DIR%' HB_ARCH='%HB_ARCH%' >> %~dp0\functions.log
   GOTO FOUND

:NOT_FOUND
   REM Let's return an error code to the caller
   ECHO "ERROR: %C_LONG_NAME% not found!"
   GOTO FIND_EXIT_1
   
:FOUND
   REM CC Must have been set above
   SET "LD=%CC%"

   IF "%HB_ARCH%" == "" (
      ECHO Defaulting HB_ARCH to w32.
      SET "HB_ARCH=w32"
   )

   SET "SUB_DIR=%C_SHORT_NAME%%HB_ARCH%"

   GOTO FIND_EXIT_0

:CHECK_CC_DIR
   ECHO :CHECK_CC_DIR Checking if %C_LONG_NAME% is in '%CC_DIR%'... >> %~dp0\functions.log

   REM IF %CC% is in bin sub folder then we can SKIP the FIND_C_COMPILER section and go directly to DIR_SET
   IF EXIST "%CC_DIR%\bin\%CC%.exe"                      GOTO DIR_SET

:CHECK_FAILED
   REM If we are here then compiler was not found in the user specified CC_DIR!

   SET "CC="

   ECHO Could not find %C_LONG_NAME% at '%CC_DIR%'!

   REM Ask the user if they want to search for known locations.
   CALL %~dp0functions.bat continue_Y_N "Search known locations (Y/N)? "
      REM User does not want to search for known locations - Abort!
      IF ERRORLEVEL 1                                      GOTO FIND_EXIT_2
      
      REM User wants to search for known locations - Continue.
      GOTO FIND_C_COMPILER
 
ECHO [%~f0](147) - (%ERRORLEVEL%) Unexpected error!
GOTO FIND_EXIT_99

 :FIND_EXIT_0
   SET "scriptName="

   echo EXIT CC='%CC%' CC_DIR='%CC_DIR%' SUB_DIR='%SUB_DIR%' >> %~dp0\functions.log
   ECHO    *** finished[0] [%~f0] >> %~dp0\functions.log
   exit /b 0

:FIND_EXIT_1
   SET "scriptName="
   ECHO    *** finished[1] [%~f0] >> %~dp0\functions.log
   exit /b 1

:FIND_EXIT_2
   SET "scriptName="
   ECHO    *** finished[2] [%~f0] >> %~dp0\functions.log
   exit /b 2

:FIND_EXIT_99
   SET "scriptName="
   ECHO    *** finished[99] [%~f0] >> %~dp0\functions.log
   exit /b 99
