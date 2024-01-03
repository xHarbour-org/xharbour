:: Copyright (c) 2023 Ron Pinkas (ron@ronpinkas.com)
:: 
:: Permission is granted, free of charge, to any person obtaining a copy 
:: of this software and associated documentation files, to deal in the 
:: Software without restriction, including rights to use, copy, modify, 
:: merge, publish, distribute, sublicense, and/or sell copies of the Software, 
:: and to permit persons to whom the Software is furnished to do so, 
:: subject to the terms of the MIT License.
rem @ECHO OFF

REM This batch file is used by find_xx.bat to check if the compiler is already set.
REM It checks if BOTH CC_DIR is set, and CC (or desired) and CC_DIR are in the path.
REM It supports argument to set CC, and if so it will check if desired CC is different
REM than the current CC, and if so will prompt the user to switch to the desired CC.

IF "%scriptName%" NEQ "" SET "scriptName=%~n0" && ECHO       *** started [%~f0] >> %~dp0functions.log
IF "%scriptName%" == ""  SET "scriptName=%~n0" && ECHO *** START [%~f0] > %~dp0functions.log

REM Set path if not already set
IF "%HB_INSTALL%" == ""  CALL %~dp0functions.bat toAbsPath %~dp0.. HB_INSTALL
IF "%HB_INSTALL%" == ""  SET "HB_INSTALL=%~dp0.." && ECHO "Warning: HB_INSTALL was not set by :toAbsPath so reverting to %~dp0.."
IF "%HB_INSTALL%" NEQ "" (WHERE harbour.exe > nul 2>&1 || ( ECHO For your convenience xHarbour's bin directory was added to your PATH. && SET "PATH=%HB_INSTALL%\bin;%PATH%"))

REM Clearly NOT FOUND if CC_DIR is not set, or BOTH CC and _WantsCC are not set.

IF "%CC_DIR%" == ""                  EXIT /B 1
IF "%CC%"     == "" IF "%~1" == ""   EXIT /B 1

IF "%~1" NEQ "" SET "_WantsCC=%~1"
IF "%CC%" == "" SET "CC=%_WantsCC%"

IF "%~1" NEQ "" IF "%CC%" NEQ "%_WantsCC%" (
   REM Compilers mismatch so prompt user to switch

   REM Build the compiler name for the prompt
   IF "%CC%" == "bcc32c" SET "CC_NAME=Borland C++"               && GOTO WANTS_NAME
   IF "%CC%" == "bcc32"  SET "CC_NAME=Borland C++"               && GOTO WANTS_NAME
   IF "%CC%" == "cl"     SET "CC_NAME=Microsoft C++"             && GOTO WANTS_NAME
   IF "%CC%" == "pocc"   SET "CC_NAME=Pelles C"                  && GOTO WANTS_NAME
   ECHO '%CC%' is not supported by this batch file.
   EXIT /B 2

   :WANTS_NAME
   IF "%_WantsCC%" == "bcc32c" SET "_WantsCC_NAME=Borland C++"   && GOTO SWITCH_CC
   IF "%_WantsCC%" == "bcc32"  SET "_WantsCC_NAME=Borland C++"   && GOTO SWITCH_CC
   IF "%_WantsCC%" == "cl"     SET "_WantsCC_NAME=Microsoft C++" && GOTO SWITCH_CC 
   IF "%_WantsCC%" == "pocc"   SET "_WantsCC_NAME=Pelles C"      && GOTO SWITCH_CC 
   ECHO '%_WantsCC%' is not supported by this batch file.

   SET "_WantsCC="
   SET "_WantsCC_NAME="
   EXIT /B 2
) ELSE (
   SET "_WantsCC="
   SET "_WantsCC_NAME="
)

REM not called just a label.
:CHECK_PATH
   REM Check if CC and CC_DIR are in the path
   FOR /F "tokens=*" %%G IN ('where %CC%.exe 2^>nul') DO SET "_WherePath=%%G"
   IF "%_WherePath%" == "" EXIT /B 1

   CALL %~dp0functions.bat strLen CC_DIR _CC_DIR_LEN
   REM The trailing \ is important to avoid false positives 
   SET /a "_CC_DIR_LEN+=1"

   SETLOCAL EnableDelayedExpansion
      SET "_WhereResult=!_WherePath:~0,%_CC_DIR_LEN%!"
   ENDLOCAL & SET "_WhereResult=%_WhereResult%" & SET "CC_DIR=%CC_DIR%"
   
   IF "%_WhereResult%" == "%CC_DIR%\" (
      REM %CC_DIR% was found so we are ready to go.
      SET '_CC_DIR_LEN='
      SET "_WherePath="
      SET "_WhereResult="
      EXIT /B 0
   )

   REM Not found so set ERRORLEVEL to 1 and exit
   SET '_CC_DIR_LEN='
   SET "_WherePath="
   SET "_WhereResult="
   EXIT /B 1

:SWITCH_CC
   ECHO Your Environment is set to use '%CC_NAME%(%CC%)' as the compiler.
   ECHO This batch file is intended to be used with '%_WantsCC_NAME%(%_WantsCC%)'.

   CALL %~dp0functions.bat continue_Y_N "Do you want to switch to %_WantsCC% (Y/N)? "
   IF %ERRORLEVEL%==0 (
      SET "CC_DIR="
      SET "CC="
      SET "RC="
      SET "CC_DIR="
      SET "LIBEXT="

      SET "_WantsCC="
      SET "_WantsCC_NAME="
      EXIT /B 1
   )

   REM User does not want to switch so set ERRORLEVEL to 2 to tell caller to exit
   SET "_WantsCC="
   SET "_WantsCC_NAME="
   EXIT /B 2