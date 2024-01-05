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
IF "%scriptName%" == "" SET "scriptName=%~n0" && ECHO *** START [%~f0] > %~dp0\functions.log

REM First check if already set.
IF "%CC%" NEQ "" GOTO CHECK_CC

:SET_CC
   IF "%HB_ARCH%" == "w64" SET "CC=bcc64" && GOTO TRY_BCC64

   :TRY_BCC32C
      REM Quick check if CAN be quickly set (CC_DIR and PATH might be set).
      CALL %~dp0found_cc.bat bcc32c
      IF ERRORLEVEL 2 EXIT /B 2
      IF ERRORLEVEL 1 SET "CC=" && GOTO TRY_BCC32
      IF ERRORLEVEL 0 GOTO FOUND

   :TRY_BCC32
      CALL %~dp0found_cc.bat bcc32
      IF ERRORLEVEL 2 EXIT /B 2
      IF ERRORLEVEL 1 SET "CC=" && GOTO TRY_BCC64
      IF ERRORLEVEL 0 GOTO FOUND

   :TRY_BCC64
      CALL %~dp0found_cc.bat bcc64
      IF ERRORLEVEL 2 EXIT /B 2
      IF ERRORLEVEL 1 SET "CC=" && GOTO SET_C_COMPILER
      IF ERRORLEVEL 0 GOTO FOUND

   ECHO "[%~f0] - (%ERRORLEVEL%) Unexpected error!"  
   EXIT /B 99

:CHECK_CC
   REM Will force a SWITCH_CC if CC is NOT set to bcc32c or bcc32 or bcc64
   IF "%CC%" NEQ "bcc32c" IF "%CC%" NEQ "bcc32" IF "%CC%" NEQ "bcc64" GOTO SET_CC

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

   IF "%CC%" == "" (IF "%HB_ARCH%" == "w64" SET "CC=bcc64" ELSE SET "CC=bcc32c")

   REM Check if CC_DIR is set by user and conttinue to DIR_SET | FIND_C_COMPILER | NOT_FOUND
   IF "%CC_DIR%" NEQ "" GOTO CHECK_CC_DIR

   REM CC_DIR not set so fall through to FIND_C_COMPILER

:FIND_C_COMPILER
   ECHO Searching for Borland C++...
   SET "CC_DIR="

   REM Check if the compiler is in the path
   CALL %~dp0functions.bat findInPath CC CC_DIR
   IF "%CC_DIR%" NEQ ""    GOTO DIR_SET

   IF "%HB_ARCH%" == "w32" GOTO FIND_BCC32C
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
   
   REM SET BCC_LIB=
   SET LD=

   REM Check if the compiler is in the known locations file
   
   IF "%HB_ARCH%" == "w32" GOTO KNOWN_BCC32C
   IF "%HB_ARCH%" == "w64" GOTO KNOWN_BCC64

   REM *** Make sure to set the correct KNOWN file for your compiler ***

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

:DIR_SET
   REM Remove the trailing backslash
   IF "%CC_DIR:~-1%" == "\" CALL %~dp0functions.bat Left CC_DIR -1 CC_DIR

   REM BCC specific because it has three possible compilers bcc32c.exe and bcc32.exe
   IF "%CC%" == ""    IF EXIST "%CC_DIR%\bin\bcc32c.exe" SET "CC=bcc32c"
   IF "%CC%" == ""    IF EXIST "%CC_DIR%\bin\bcc32.exe"  SET "CC=bcc32"
   IF "%CC%" == ""    IF EXIST "%CC_DIR%\bin\bcc64.exe"  SET "CC=bcc64"

   WHERE %CC%.exe >nul 2>&1 &&      GOTO PATH_OK
   IF EXIST "%CC_DIR%\bin\%CC%.exe" GOTO PATH_SET

   GOTO NOT_FOUND

:PATH_SET
   REM ECHO PATH_SET '%CC_DIR%' for '%CC%'
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
      )
      ECHO !LIB_PATHS! > %TEMP%\bcc_lib_paths.txt
   ENDLOCAL
   SET /P BCC_LIB=<%TEMP%\bcc_lib_paths.txt

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
   GOTO FOUND

:NOT_FOUND
   rem Let's return an error code to the caller
   echo "ERROR: Borland C++ not found!"
   exit /b 1
   
:FOUND
   REM Must be bcc32.exe or it would have been set to bcc32c.exe above
   SET "LD=%CC%"

   IF "%HB_ARCH%" == "" SET "HB_ARCH=w32"

   REM BCC specific because it has three possible compilers bcc32c.exe and bcc32.exe
   IF "%CC%" == "bcc32c" SET "SUB_DIR=b32c" && SET "HB_ARCH=w32"
   IF "%CC%" == "bcc32"  SET "SUB_DIR=b32"  && SET "HB_ARCH=w32"
   IF "%CC%" == "bcc64"  SET "SUB_DIR=b64"  && SET "HB_ARCH=w64"

   exit /b 0

:CHECK_CC_DIR 

   REM IF %CC% is in bin sub folder then we can SKIP the FIND_C_COMPILER section and go directly to DIR_SET
   IF EXIST "%CC_DIR%\bin\%CC%.exe"                      GOTO DIR_SET

   REM BCC Specific because it has three possible compilers bcc32c.exe and bcc32.exe
   SET "CC=bcc32"
   IF EXIST "%CC_DIR%\bin\%CC%.exe"                      GOTO DIR_SET

   REM BCC Specific because it has three possible compilers bcc32c.exe and bcc32.exe
   SET "CC=bcc64"
   IF EXIST "%CC_DIR%\bin\%CC%.exe" SET "HB_ARCH=w64" && GOTO DIR_SET
   
   REM If we are here then compiler was not found in the user specified CC_DIR!

   REM BCC Specific because it has two possible compilers bcc32c.exe and bcc32.exe
   SET "CC="

   ECHO Could not find Borland C++ at '%CC_DIR%'!

   REM Ask the user if they want to search for known locations.
   CALL %~dp0functions.bat continue_Y_N "Search known locations (Y/N)? "
   REM User does not want to search for known locations - Abort.
   IF ERRORLEVEL 1                                      GOTO NOT_FOUND
   REM User wants to search for known locations - Continue.
   GOTO FIND_C_COMPILER
 
