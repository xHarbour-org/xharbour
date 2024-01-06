:: Copyright (c) 2023 Ron Pinkas (ron@ronpinkas.com)
:: 
:: Permission is granted, free of charge, to any person obtaining a copy 
:: of this software and associated documentation files, to deal in the 
:: Software without restriction, including rights to use, copy, modify, 
:: merge, publish, distribute, sublicense, and/or sell copies of the Software, 
:: and to permit persons to whom the Software is furnished to do so, 
:: subject to the terms of the MIT License.
@ECHO OFF

IF "%scriptName%" NEQ "" SET "scriptName=%~n0" && ECHO    *** started [%~f0] >> %~dp0\functions.log
IF "%scriptName%" == ""  SET "scriptName=%~n0" && ECHO *** START [%~f0] > %~dp0\functions.log

REM First check if already set.
IF "%CC%" NEQ "" GOTO CHECK_CC

:SET_CC
   IF "%HB_ARCH%" == "w64" SET "CC=bcc64" && GOTO TRY_BCC64

   :TRY_BCC32C
      REM Quick check if CAN be quickly set (CC_DIR and PATH might be set).
      CALL %~dp0found_cc.bat bcc32c
         IF ERRORLEVEL 99 GOTO FIND_EXIT_99
         IF ERRORLEVEL  2 GOTO FIND_EXIT_2
         IF ERRORLEVEL  1 SET "CC=" && GOTO TRY_BCC32
         IF ERRORLEVEL  0 GOTO FOUND
         ECHO [%~f0] - (%ERRORLEVEL%) Unexpected error!
         GOTO FIND_EXIT_99

   :TRY_BCC32
      CALL %~dp0found_cc.bat bcc32
         IF ERRORLEVEL 99 GOTO FIND_EXIT_99
         IF ERRORLEVEL  2 GOTO FIND_EXIT_2
         IF ERRORLEVEL  1 SET "CC=" && GOTO TRY_BCC64
         IF ERRORLEVEL  0 GOTO FOUND
         ECHO [%~f0] - (%ERRORLEVEL%) Unexpected error!
         GOTO FIND_EXIT_99

   :TRY_BCC64
      CALL %~dp0found_cc.bat bcc64
         IF ERRORLEVEL 99 GOTO FIND_EXIT_99
         IF ERRORLEVEL  2 GOTO FIND_EXIT_2
         IF ERRORLEVEL  1 SET "CC=" && GOTO SET_C_COMPILER
         IF ERRORLEVEL  0 GOTO FOUND
         ECHO [%~f0] - (%ERRORLEVEL%) Unexpected error!
      GOTO FIND_EXIT_99

:CHECK_CC
   REM Will force a SWITCH_CC if CC is NOT bcc32c or bcc32 or bcc64
   IF "%CC%" NEQ "bcc32c" IF "%CC%" NEQ "bcc32" IF "%CC%" NEQ "bcc64" GOTO SET_CC

   REM Quick check if the compiler is already set.
   CALL %~dp0found_cc.bat %CC%
      IF ERRORLEVEL 99 GOTO FIND_EXIT_99
      IF ERRORLEVEL  2 GOTO FIND_EXIT_2
      IF ERRORLEVEL  1 GOTO SET_C_COMPILER
      IF ERRORLEVEL  0 GOTO FOUND
      ECHO "[%~f0] - (%ERRORLEVEL%) Unexpected error!" 
      GOTO FIND_EXIT_99
   
REM The Entry point for FIRST run.
:SET_C_COMPILER
   ver > nul REM Reset ERRORLEVEL

   IF "%CC%" == "" (
      IF "%HB_ARCH%" == "w64" (
         SET "CC=bcc64"
      ELSE (
         SET "CC=bcc32c"
      )
   )

   IF "%CC_DIR%" NEQ "" GOTO CHECK_CC_DIR

   REM Fall through to FIND_C_COMPILER

:FIND_C_COMPILER
   ECHO Searching for Borland C++...
   SET "CC_DIR="

   REM Check if the compiler is in the path

   IF "%CC%" == "" GOTO FIND_BCC_VARIANT

   :FIND_USER_CC
      CALL %~dp0functions.bat findInPath CC CC_DIR
      IF "%CC_DIR%" NEQ ""    GOTO DIR_SET

   :FIND_BCC_VARIANT 
      IF "%HB_ARCH%" == "w64" GOTO FIND_BCC64

      :FIND_BCC32C
         REM BCC specific because it has three possible compilers bcc32c.exe and bcc32.exe
         SET "CC=bcc32c"
         CALL %~dp0functions.bat findInPath CC CC_DIR
         IF "%CC_DIR%" NEQ "" GOTO DIR_SET

      REM Fall through to FIND_BCC32

      :FIND_BCC32
         REM BCC specific because it has three possible compilers bcc32c.exe and bcc32.exe
         SET "CC=bcc32"
         CALL %~dp0functions.bat findInPath CC CC_DIR
         IF "%CC_DIR%" NEQ "" GOTO DIR_SET

      REM Fall through to FIND_BCC64

      :FIND_BCC64
         REM BCC specific because it has three possible compilers bcc32c.exe and bcc32.exe
         SET "CC=bcc64"
         CALL %~dp0functions.bat findInPath CC CC_DIR
         IF "%CC_DIR%" NEQ "" SET "HB_ARCH=w64" && GOTO DIR_SET
   
   REM IF we are here then the compiler was not found in the path so let's try to find it in the known locations file
   REM SET BCC_LIB=
   SET LD=

   REM Check if the compiler is in the known locations file
   
   REM *** Make sure to set the correct KNOWN file for your compiler ***

   IF "%HB_ARCH%" == "w64" GOTO KNOWN_BCC64
   REM Fall through to KNOWN_BCC32C

   :KNOWN_BCC32C
      SET "CC=bcc32c"
      CALL %~dp0functions.bat findKnown %~dp0known.bc CC CC_DIR
      IF "%CC_DIR%" NEQ "" GOTO DIR_SET
      REM Fall through to KNOWN_BCC32

   :KNOWN_BCC32
      REM BCC specific because it has two possible compilers bcc32c.exe and bcc32.exe
      SET "CC=bcc32"
      CALL %~dp0functions.bat findKnown %~dp0known.bc CC CC_DIR
      IF "%CC_DIR%" NEQ "" GOTO DIR_SET
      REM Fall through to KNOWN_BCC64

   :KNOWN_BCC64
      REM BCC specific because it has two possible compilers bcc32c.exe and bcc32.exe
      SET "CC=bcc64"
      CALL %~dp0functions.bat findKnown %~dp0known.bc CC CC_DIR
      IF "%CC_DIR%" NEQ "" SET "HB_ARCH=w64" && GOTO DIR_SET
      GOTO NOT_FOUND

ECHO [%~f0] - (%ERRORLEVEL%) Unexpected error!
GOTO FIND_EXIT_99

:DIR_SET
   REM Remove the trailing backslash if any
   IF "%CC_DIR:~-1%" == "\" CALL %~dp0functions.bat Left CC_DIR -1 CC_DIR

   REM BCC specific because it has three possible compilers bcc32c.exe and bcc32.exe
   IF "%HB_ARCH%" == "w64" GOTO DIR_SET64
   REM Fall through to DIR_SET32C

   :DIR_SET32
      IF "%CC%" == ""    IF EXIST "%CC_DIR%\bin\bcc32c.exe" SET "CC=bcc32c"
      IF "%CC%" == ""    IF EXIST "%CC_DIR%\bin\bcc32.exe"  SET "CC=bcc32"
      REM Fall through to DIR_SET64

   :DIR_SET64
      IF "%CC%" == ""    IF EXIST "%CC_DIR%\bin\bcc64.exe"  SET "CC=bcc64"

   WHERE %CC%.exe >nul 2>&1 &&      GOTO PATH_OK
   IF EXIST "%CC_DIR%\bin\%CC%.exe" GOTO PATH_SET

   GOTO NOT_FOUND

:PATH_SET
   REM We arrived here ONLY if %CC%.exe exists in %CC_DIR%\bin and not in PATH.
   ECHO For your convenience %CC%'s bin directory was added to your PATH && SET "PATH=%CC_DIR%\bin;%PATH%"

:PATH_OK
   WHERE %CC%.exe >nul 2>&1 || GOTO NOT_FOUND

   IF "%BCC_LIB%" NEQ "" GOTO FOUND

   REM Let's borrow the LIB paths from the config file 
   SETLOCAL EnableDelayedExpansion
      SET LIB_PATHS=

      for /f "tokens=*" %%a in ('type %CC_DIR%\bin\%CC%.cfg ^| findstr /r "^-L"') do (
         SET "line=%%a"
         SET "path=!line:~2!"
         SET "path=!path:@\..=%CC_DIR%!"
         IF NOT "!LIB_PATHS!" == "" SET "LIB_PATHS=!LIB_PATHS!;"
         SET "LIB_PATHS=!LIB_PATHS!!path!"
         echo Added LIB path: !path! >> %~dp0\functions.log
      )
   ENDLOCAL & SET "BCC_LIB=%LIB_PATHS%" & SET "LIB_PATHS="
   echo BCC_LIB: %BCC_LIB% >> %~dp0\functions.log

   REM If the config file does not have any LIB paths then let's try to guess them
   IF "%BCC_LIB%" == "" (
      IF EXIST "%CC_DIR%\lib\win32c\debug" (
         IF EXIST "%CC_DIR%\lib\win32c\release" (
            IF EXIST "%CC_DIR%\lib\win32c\release\psdk" (
               SET "BCC_LIB=%CC_DIR%\lib\win32c\debug;%CC_DIR%\lib\win32c\release;%CC_DIR%\lib\win32c\release\psdk"
            )
         )
      ) ELSE (
         IF EXIST "%CC_DIR%\lib" (
            IF EXIST "%CC_DIR%\lib\psdk" (
               SET "BCC_LIB=%CC_DIR%\lib;%CC_DIR%\lib\psdk"
            )
         )
      )
   )
   echo before found BCC_LIB: %BCC_LIB% >> %~dp0\functions.log
   GOTO FOUND

:NOT_FOUND
   REM Let's return an error code to the caller
   ECHO "ERROR: Borland C++ not found!"
   GOTO FIND_EXIT_1
   
:FOUND
   REM Must be bcc32.exe or it would have been set to bcc32c.exe above
   SET "LD=%CC%"

   REM BCC specific because it has three possible compilers bcc32c.exe and bcc32.exe
   IF "%CC%" == "bcc32c" SET "SUB_DIR=b32c" && SET "HB_ARCH=w32"
   IF "%CC%" == "bcc32"  SET "SUB_DIR=b32"  && SET "HB_ARCH=w32"
   IF "%CC%" == "bcc64"  SET "SUB_DIR=b64"  && SET "HB_ARCH=w64"

   GOTO FIND_EXIT_0

:CHECK_CC_DIR
   REM IF %CC% is in bin sub folder then we can SKIP the FIND_C_COMPILER section and go directly to DIR_SET
   IF EXIST "%CC_DIR%\bin\%CC%.exe"                      GOTO DIR_SET

   REM BCC Specific because it has three possible compilers bcc32c.exe and bcc32.exe
   IF "%HB_ARCH%" == "w64" GOTO CHK_CC_DIR64
   :CHK_CC_DIR32
      SET "CC=bcc32c"
      IF EXIST "%CC_DIR%\bin\%CC%.exe"                      GOTO DIR_SET

      SET "CC=bcc32"
      IF EXIST "%CC_DIR%\bin\%CC%.exe"                      GOTO DIR_SET

   :CHK_CC_DIR64
      SET "CC=bcc64"
      IF EXIST "%CC_DIR%\bin\%CC%.exe" SET "HB_ARCH=w64" && GOTO DIR_SET
      REM Fall through...

:CHECK_FAILED
   REM If we are here then compiler was not found in the user specified CC_DIR!

   REM BCC Specific because it has two possible compilers bcc32c.exe and bcc32.exe
   SET "CC="

   ECHO Could not find Borland C++ at '%CC_DIR%'!

   REM Ask the user if they want to search for known locations.
   CALL %~dp0functions.bat continue_Y_N "Search known locations (Y/N)? "
      REM User does not want to search for known locations - Abort!
      IF ERRORLEVEL 1                                      GOTO FIND_EXIT_2
      
      REM User wants to search for known locations - Continue.
      GOTO FIND_C_COMPILER
 
ECHO [%~f0] - (%ERRORLEVEL%) Unexpected error!
GOTO FIND_EXIT_99

 :FIND_EXIT_0
   SET "scriptName="
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