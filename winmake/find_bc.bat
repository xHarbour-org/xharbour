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

SET "C_NAME=bcc32c"
SET "C_NAME2=bcc32"
SET "C_NAME64=bcc64"
SET "C_LONG_NAME=Borland C/C++"
SET "C_SHORT_NAME=bc"

REM First check if already set.
IF "%CC%" NEQ "" GOTO CHECK_CC

:SET_CC
   IF "%HB_ARCH%" == "w64" (SET "CC=%C_NAME64%") & GOTO TRY_C_NAME64

   :TRY_C_NAME
      REM Quick check if CAN be quickly set (CC_DIR and PATH might be set).
      CALL %~dp0found_cc.bat %C_NAME% %C_LONG_NAME%
         IF ERRORLEVEL 99 GOTO FIND_EXIT_99
         IF ERRORLEVEL  2 GOTO FIND_EXIT_2
         IF ERRORLEVEL  1 SET "CC=" && GOTO TRY_C_NAME2
         IF ERRORLEVEL  0 GOTO FOUND
         ECHO [%~f0](33) - (%ERRORLEVEL%) Unexpected error!
         GOTO FIND_EXIT_99

   :TRY_C_NAME2
      CALL %~dp0found_cc.bat %C_NAME2% %C_LONG_NAME%
         IF ERRORLEVEL 99 GOTO FIND_EXIT_99
         IF ERRORLEVEL  2 GOTO FIND_EXIT_2
         IF ERRORLEVEL  1 SET "CC=" && GOTO TRY_C_NAME64
         IF ERRORLEVEL  0 GOTO FOUND
         ECHO [%~f0] - (%ERRORLEVEL%) Unexpected error!
         GOTO FIND_EXIT_99

   :TRY_C_NAME64
      CALL %~dp0found_cc.bat %C_NAME64% %C_LONG_NAME%
         IF ERRORLEVEL 99 GOTO FIND_EXIT_99
         IF ERRORLEVEL  2 GOTO FIND_EXIT_2
         IF ERRORLEVEL  1 SET "CC=" && GOTO SET_C_COMPILER
         IF ERRORLEVEL  0 GOTO FOUND
         ECHO [%~f0](51) - (%ERRORLEVEL%) Unexpected error!
         GOTO FIND_EXIT_99

:CHECK_CC
   REM Will force a SWITCH_CC if CC is NOT C_NAME or C_NAM2 or C_NAME64
   IF "%CC%" == "%C_NAME%"   GOTO CHECK_FOUND
   IF "%CC%" == "%C_NAME2%"  GOTO CHECK_FOUND
   IF "%CC%" == "%C_NAME64%" GOTO CHECK_FOUND 
   
   REM None of the above so let's try to set it
   GOTO SET_CC

:CHECK_FOUND
   REM Quick check if the compiler is already set.
   CALL %~dp0found_cc.bat %CC%
      IF ERRORLEVEL 99 GOTO FIND_EXIT_99
      IF ERRORLEVEL  2 GOTO FIND_EXIT_2
      IF ERRORLEVEL  1 GOTO SET_C_COMPILER
      IF ERRORLEVEL  0 GOTO FOUND
      ECHO "[%~f0](70) - (%ERRORLEVEL%) Unexpected error!" 
      GOTO FIND_EXIT_99
   
REM The Entry point for FIRST run.
:SET_C_COMPILER
   ver > nul REM Reset ERRORLEVEL

   REM BCC Specific!
   IF "%CC%" == "" GOTO CHECK_ARCH
   GOTO AFTER_ARCH
   
   :CHECK_ARCH
      IF "%HB_ARCH%" == "w64" (
         SET "CC=%C_NAME64%"
      ) ELSE (
         SET "CC=%C_NAME%"
      )

   :AFTER_ARCH
   REM Check if CC_DIR is set by user and conttinue to DIR_SET | FIND_C_COMPILER | NOT_FOUND
   IF "%CC_DIR%" NEQ "" GOTO CHECK_CC_DIR

   REM Fall through to FIND_C_COMPILER

:FIND_C_COMPILER
   ECHO Searching for %C_LONG_NAME%...
   SET "CC_DIR="

   IF "%CC%" == "" GOTO FIND_VARIANT

   REM Check if the compiler is in the path
   :FIND_USER_CC
      CALL %~dp0functions.bat rootOfAppInPath CC CC_DIR
      IF "%CC_DIR%" NEQ ""    GOTO DIR_SET

   :FIND_VARIANT 
      IF "%HB_ARCH%" == "w64" GOTO FIND_C_NAME64

      :FIND_C_NAME
         REM Already tried %C_NAME% so let's try %C_NAME2%
         IF "%CC%" == "%C_NAME%" GOTO FIND_C_NAME2
         SET "CC=%C_NAME%"
         CALL %~dp0functions.bat rootOfAppInPath CC CC_DIR
         IF "%CC_DIR%" NEQ "" (SET "HB_ARCH=w32") & GOTO DIR_SET

      REM Fall through to FIND_C_NAME2

      :FIND_C_NAME2
         IF "%CC%" == "%C_NAME2%" GOTO FIND_C_NAME64
         SET "CC=%C_NAME2%"
         CALL %~dp0functions.bat rootOfAppInPath CC CC_DIR
         IF "%CC_DIR%" NEQ "" (SET "HB_ARCH=w32") & GOTO DIR_SET

      REM Fall through to FIND_C_NAME64

      :FIND_C_NAME64
         IF "%CC%" == "%C_NAME64%" GOTO AFTER_FIND
         SET "CC=%C_NAME64%"
         CALL %~dp0functions.bat rootOfAppInPath CC CC_DIR
         IF "%CC_DIR%" NEQ "" (SET "HB_ARCH=w64") & GOTO DIR_SET
   
   :AFTER_FIND
   REM IF we are here then the compiler was not found in the path so let's try to find it in the known locations file
   SET LD=

   REM Check if the compiler is in the known locations file
   
   REM *** Make sure to set the correct KNOWN file for your compiler ***

   IF "%HB_ARCH%" == "w64" GOTO KNOWN_C_NAME64
   REM Fall through to KNOWN_%C_NAME%

   :KNOWN_C_NAME
      SET "CC=%C_NAME%"
      CALL %~dp0functions.bat findKnown %~dp0known.%C_SHORT_NAME% CC CC_DIR
      IF "%CC_DIR%" NEQ "" (SET "HB_ARCH=w32") & GOTO DIR_SET
      REM Fall through

   :KNOWN_C_NAME2
      SET "CC=%C_NAME2%"
      CALL %~dp0functions.bat findKnown %~dp0known.%C_SHORT_NAME% CC CC_DIR
      IF "%CC_DIR%" NEQ "" (SET "HB_ARCH=w32") & GOTO DIR_SET
      REM Fall through

   :KNOWN_C_NAME64
      SET "CC=%C_NAME64%"
      CALL %~dp0functions.bat findKnown %~dp0known.%C_SHORT_NAME% CC CC_DIR
      IF "%CC_DIR%" NEQ "" (SET "HB_ARCH=w64") & GOTO DIR_SET

   GOTO NOT_FOUND

ECHO [%~f0](161) - (%ERRORLEVEL%) Unexpected error!
GOTO FIND_EXIT_99

:DIR_SET
   REM Remove the trailing backslash if any
   IF "%CC_DIR:~-1%" == "\" CALL %~dp0functions.bat Left CC_DIR -1 CC_DIR

   IF "%HB_ARCH%" == "w64" GOTO DIR_SET64

   REM Fall through to DIR_SET32

   :DIR_SET32
      IF "%CC%" NEQ "" (IF EXIST "%CC_DIR%\bin\%CC%.exe"       ((SET "HB_ARCH=w32") & GOTO CHECK_IN_PATH))
      IF "%CC%" == ""  (IF EXIST "%CC_DIR%\bin\%C_NAME%.exe"   ((SET "HB_ARCH=w32") & (SET "CC=%C_NAME%") & GOTO CHECK_IN_PATH))
      IF "%CC%" == ""  (IF EXIST "%CC_DIR%\bin\%C_NAME2%.exe"  ((SET "HB_ARCH=w32") & (SET "CC=%C_NAME2%") & GOTO CHECK_IN_PATH))
      REM Fall through to DIR_SET64

   :DIR_SET64
      IF "%CC%" == "" (IF EXIST "%CC_DIR%\bin\%C_NAME64%.exe" ((SET "HB_ARCH=w64") & SET "CC=%C_NAME64%" & GOTO CHECK_IN_PATH))
   
   ECHO [%~f0](183) - (%ERRORLEVEL%) Unexpected error!
   GOTO FIND_EXIT_99

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
   SET "PATH=%CC_DIR%\bin;%PATH%"
   ECHO For your convenience %CC%'s bin directory was added to your PATH

REM Fall through to PATH_OK
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

   SET "SUB_DIR=%CC%"

   REM BCC specific.
   IF "%BCC_LIB%" NEQ "" GOTO FIND_EXIT_0
   REM Let's borrow the LIB paths from the config file
   ECHO Start BCC_LIB: %BCC_LIB% >> %~dp0\functions.log
   SETLOCAL EnableDelayedExpansion
      SET LIB_PATHS=

      for /f "tokens=*" %%a in ('type %CC_DIR%\bin\%CC%.cfg ^| findstr /r "^-L"') do (
         SET "line=%%a"
         SET "path=!line:~2!"
         SET "path=!path:@\..=%CC_DIR%!"
         IF NOT "!LIB_PATHS!" == "" SET "LIB_PATHS=!LIB_PATHS!;"
         SET "LIB_PATHS=!LIB_PATHS!!~path!"
         echo Added LIB path: !path! (!LIB_PATHS!) >> %~dp0\functions.log
      )
   ENDLOCAL & SET "BCC_LIB=%LIB_PATHS%" & SET "LIB_PATHS="
   echo After .cfg BCC_LIB: %BCC_LIB% >> %~dp0\functions.log

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
   echo End BCC_LIB: %BCC_LIB% >> %~dp0\functions.log

   GOTO FIND_EXIT_0

:CHECK_CC_DIR
   ECHO :CHECK_CC_DIR Checking if %C_LONG_NAME% is in '%CC_DIR%'... >> %~dp0\functions.log

   REM IF %CC% is in bin sub folder then we can SKIP the FIND_C_COMPILER section and go directly to DIR_SET
   IF "%CC%" NEQ "" (IF EXIST "%CC_DIR%\bin\%CC%.exe"         GOTO DIR_SET)

   IF "%HB_ARCH%" == "w64" GOTO CHK_CC_DIR64
   :CHK_CC_DIR32
      SET "CC=%C_NAME%"
      IF EXIST "%CC_DIR%\bin\%CC%.exe" ((SET "HB_ARCH=w32") & GOTO DIR_SET)

      SET "CC=%C_NAME2%"
      IF EXIST "%CC_DIR%\bin\%CC%.exe" ((SET "HB_ARCH=w32") & GOTO DIR_SET)

   :CHK_CC_DIR64
      SET "CC=%C_NAME64%"
      IF EXIST "%CC_DIR%\bin\%CC%.exe" ((SET "HB_ARCH=w64") & GOTO DIR_SET)
      REM Fall through...

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
 
ECHO [%~f0](289) - (%ERRORLEVEL%) Unexpected error!
GOTO FIND_EXIT_99

 :FIND_EXIT_0
   SET "scriptName="

   REM BCC Specific!
   echo EXIT CC='%CC%' CC_DIR='%CC_DIR%' SUB_DIR='%SUB_DIR%' BCC_LIB='%BCC_LIB%' >> %~dp0\functions.log
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
