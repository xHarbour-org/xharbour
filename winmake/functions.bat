:: Copyright (c) 2023 Ron Pinkas (ron@ronpinkas.com)
:: 
:: Permission is granted, free of charge, to any person obtaining a copy 
:: of this software and associated documentation files, to deal in the 
:: Software without restriction, including rights to use, copy, modify, 
:: merge, publish, distribute, sublicense, and/or sell copies of the Software, 
:: and to permit persons to whom the Software is furnished to do so, 
:: subject to the terms of the MIT License.

REM Windows batch files are extremly fragile and limited. The slightest change
REM in the syntax EVEN in REM or :: comments as wellll as ECHO can cause failures.
REM While I made every effort to make this batch file as robust as possible,
REM I have encountered cases where even value of assignments may cause odd
REM behavior, and maybe dificult to debug. I have included a detailed log
REM of the execution of this batch file in the file functions.log in the same
REM directory as this batch file. If you encounter any problems, please review
REM the log file. Also worth mentioning is that IF statements in batch files
REM when used with parenthesis to create BLOCKs of logic are a special case where
REM assignment of variables within the block is not visible until the block is
REM exited. Same is true in tthe case of any otther BLOCKs of code sorrounded by
REM parenthesis - like FOR loops. To solve this odd limitation Microsft introduced
REM the SETLOCAL ENABLEDELAYEDEXPANSION command. The name of course makes it even
REM more confusing. The ENABLEDELAYEDEXPANSION is not a delay in the execution of
REM the SET command, but rather what allows for immediate expansion of variables
REM by means of the !var! syntax. The problem is that the SETLOCAL creates a new
REM environment, and assignments to variables in the new environment are not
REM visible in the parent environment. To solve this problem, the ENDLOCAL command
REM may be tricked into passing the value of a variable back to the parent
REM environment by means of the following syntax: ENDLOCAL & SET "var=%var%". This
REM is the reason why you will see this syntax in many places in this batch file.
REM In summary CAUTION is advised when making changes to this batch file. If you
REM do make changes, please test them thoroughly. If you encounter any problems,
REM please review the log file functions.log and consider that tthe DATA used by
REM the failed run may very well be the explanation for the failure. 

@echo off
echo  *** Raw Arguments: (%*) >> %~dp0functions.log
echo  *** Raw Arguments: [%1] [%2] [%3] [%4] [%5] >> %~dp0functions.log

SET funcName=%1
IF "%funcName%" == "" (
   ECHO "Syntax: %~0 funcName [arg1 [arg2 [arg3 [arg4]]]]"
   EXIT /b 1
) ELSE (
   IF "%6" NEQ "" (
      ECHO %*
      ECHO "Too many arguments passed to: '%~0'!"
      ECHO "Syntax: %~0 funcName [arg1 [arg2 [arg3 [arg4]]]]"
      EXIT /b 1
   )
)

SET /a argC=0
SET "parsed= :%funcName%("

SETLOCAL EnableDelayedExpansion
   IF "%~2" NEQ "" (SET /a argC+=1) && SET "arg1=%2" && SET "parsed=%parsed% %2=!%2! "
   IF "%~3" NEQ "" (SET /a argC+=1) && SET "arg2=%3" && SET "parsed=%parsed% %3=!%3! "
   IF "%~4" NEQ "" (SET /a argC+=1) && SET "arg3=%4" && SET "parsed=%parsed% %4=!%4! "
   IF "%~5" NEQ "" (SET /a argC+=1) && SET "arg4=%5" && SET "parsed=%parsed% %5=!%5! "   
ENDLOCAL & SET "parsed=%parsed%)" & SET "argC=%argC%" & SET "arg1=%arg1%" & SET "arg2=%arg2%" & SET "arg3=%arg3%" & SET "arg4=%arg4%"

rem echo  *** Parsed [%argC%] %parsed%
echo  *** Parsed [%argC%] %parsed% >> %~dp0functions.log

GOTO :call%argC% 

:call0
   echo  +++ Entry point: %funcName%()(%argC%) >> %~dp0functions.log
   CALL :%funcName%
   SET "errorResult=%ERRORLEVEL%"
   echo  --- Exit point: %funcName% >> %~dp0functions.log
   echo   *** Ret: %errorResult% %ERRORLEVEL% >> %~dp0functions.log
   GOTO scriptEnd

:call1
   echo  +++ Entry point: %funcName%('%arg1%')(%argC%) >> %~dp0functions.log
   CALL :%funcName% %arg1%
   SET "errorResult=%ERRORLEVEL%"
   echo  --- Exit point: %funcName% %arg1% >> %~dp0functions.log
   echo   *** Ret: %errorResult% %ERRORLEVEL% >> %~dp0functions.log
   GOTO scriptEnd

:call2
   echo  +++ Entry point: %funcName%('%arg1%' '%arg2%')(%argC%) >> %~dp0functions.log
   CALL :%funcName% %arg1% %arg2%
   SET "errorResult=%ERRORLEVEL%"
   echo  --- Exit point: %funcName% %arg1% %arg2% >> %~dp0functions.log
   echo   *** Ret: %errorResult% %ERRORLEVEL% >> %~dp0functions.log
   GOTO scriptEnd

:call3
   echo  +++ Entry point: %funcName%('%arg1%' '%arg2%' '%arg3%')(%argC%) >> %~dp0functions.log
   CALL :%funcName% %arg1% %arg2% %arg3%
   SET "errorResult=%ERRORLEVEL%"
   echo  --- Exit point: %funcName% %arg1% %arg2% %arg3% >> %~dp0functions.log
   echo   *** Ret: %errorResult% %ERRORLEVEL% >> %~dp0functions.log
   GOTO scriptEnd

:call4
   echo  +++ Entry point: %funcName%('%arg1%' '%arg2%' '%arg3%' '%arg4%'(%argC%)) >> %~dp0functions.log
   CALL :%funcName% %arg1% %arg2% %arg3% %arg4%
   SET "errorResult=%ERRORLEVEL%"
   echo  --- Exit point: %funcName% %arg1% %arg2% %arg3% %arg4% >> %~dp0functions.log
   echo   *** Ret: %errorResult% %ERRORLEVEL% >> %~dp0functions.log
   GOTO scriptEnd

:scriptEnd
SET "parsed="
SET "argC="
SET "arg1="
SET "arg2="
SET "arg3="
SET "arg4="
EXIT /b

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: varOrValue varResult
:: Returns passed value or value of variable if var exists
:value
echo    :value(%*) >> %~dp0functions.log

   IF "%3" NEQ "" (
      ECHO "Too many arguments passed to: ':value'! " >> %~dp0functions.log
      ECHO "Too many arguments passed to: ':value'! "
      EXIT /b 1
   )

   CALL :isValidVarName %2
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid result variable name: '%2' passed to: ':value'! " >> %~dp0functions.log
         ECHO "Invalid result variable name: '%2' passed to: ':value'! "
         EXIT /b 1
      )

   SETLOCAL EnableDelayedExpansion
      IF DEFINED %1 (
         SET "value=!%1!"
      ) ELSE (
         SET "value=%~1"
      )
   ENDLOCAL & set "%~2=%value%"
   
   EXIT /b 0

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: literalValue varResult
:: Returns: the expanded (by redirection)) value of a passed value
:expandInner
echo     :expandInner(%*) >> %~dp0functions.log

   IF "%3" NEQ "" (
      ECHO "Too many arguments passed to: ':expandInner'!"
      EXIT /b 1
   )

   CALL :isValidVarName %~2
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid result variable name: '%~2' passed to: ':expandInner'!"
         EXIT /b 1
      )

   SET "%~2=%~1"
   
   EXIT /b 0

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: varName
:: Returns errorlvl 0 if varName is a valid variable name, 1 otherwise
:isValidVarName
echo    :isValidVarName(%*) >> %~dp0functions.log

   IF "%2" NEQ "" (
      REM ECHO "Too many arguments passed to: ':isValidVarName'!" >> %~dp0functions.log
      REM ECHO "Too many arguments passed to: ':isValidVarName'!"
      EXIT /b 1
   )

   SETLOCAL EnableDelayedExpansion
      SET /a "%1=0" 2>nul
   ENDLOCAL & SET "_Result=%ERRORLEVEL%"
   
   IF %_Result% NEQ 0 (
      ECHO ":isValidVarName(%1) is NOT a valid variable name!" >> %~dp0functions.log
      REM Cleanup...
      SET "_Result="
      EXIT /b 1
   )
   REM Cleanup...
   SET "_Result="
   EXIT /b 0

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: string varResult
:: Returns the length of a string
:strLen 
echo    :strLen(%*) >> %~dp0functions.log

   IF "%3" NEQ "" (
      ECHO "Too many arguments passed to: ':strLen'!"
      EXIT /b 1
   )

   CALL :isValidVarName %~2
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid result variable name: '%~2' passed to: ':strLen'!"
         EXIT /b 1
      )

   SETLOCAL EnableDelayedExpansion
      CALL :value %1 str

      SET "len=0"
      FOR /l %%A in (12,-1,0) DO (
         SET /a "len|=1<<%%A"
         FOR %%B in (!len!) DO IF "!str:~%%B,1!"=="" set /a "len&=~1<<%%A"
      )
   ENDLOCAL & set /a "%~2=1+%len%"
   EXIT /b 0

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- 
:: Args: relativePath varResult
:: Returns the absolute path
:toAbsPath
echo    :toAbsPath(%*) >> %~dp0functions.log

   IF "%3" NEQ "" (
      ECHO "Too many arguments passed to: ':toAbsPath'!"
      EXIT /b 1
   )

   CALL :isValidVarName %2
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid ressult variable name: '%~2' passed to: ':toAbsPath'!"
         EXIT /b 1
      )

   call :value %1 _varOrRelPath

   call :innerToAbsPath %_varOrRelPath% _absPath
   set "%~2=%_absPath%"
   set "_varOrRelPath="
   set "_absPath="

   EXIT /b 0

      :innerToAbsPath - relPath resAbsPath 
         SET "%~2=%~f1"
         EXIT /b 0

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: string varResult
:: Returns the lowercase version of string to resultVar
:toLower
echo    :toLower(%*) >> %~dp0functions.log

   IF "%3" NEQ "" (
      ECHO "Too many arguments passed to: ':toLower'!"
      EXIT /b 1
   )

   CALL :isValidVarName %2
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid result variable name: '%~2' passed to: ':toLower'!"
         EXIT /b 1
      )

   SETLOCAL EnableDelayedExpansion
      CALL :value %1 str

      FOR %%A in (
         "A=a" "B=b" "C=c" "D=d" "E=e" "F=f" "G=g" "H=h" "I=i"
         "J=j" "K=k" "L=l" "M=m" "N=n" "O=o" "P=p" "Q=q" "R=r"
         "S=s" "T=t" "U=u" "V=v" "W=w" "X=x" "Y=y" "Z=z" "Ä=ä"
         "Ö=ö" "Ü=ü"
      ) DO SET str=!str:%%~A!
   ENDLOCAL & set "%~2=%str%"
   EXIT /b 0

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: string cutLen varResult
:: Returns the left cutLen (or length - cutLen if cutLen is negative) characters of string
:Left
echo    :Left(%*) >> %~dp0functions.log

   IF "%4" NEQ "" (
      ECHO "Too many arguments passed to: ':Left'!"
      EXIT /b 1
   )

   CALL :isValidVarName %3
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid result variable name: '%~3' passed to: ':Left'!"
         EXIT /b 1
      )

   SET /a "len=%~2" 2>nul
   IF %ERRORLEVEL% NEQ 0 (
      ECHO "Invalid argument len:'!len!' passed to :Left!"
      EXIT /b 1
   )

   SETLOCAL EnableDelayedExpansion
      CALL :value %1 str

      SET "left=!str:~0,%len%!"
   ENDLOCAL & set "%~3=%left%"
   EXIT /B

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: fileName compiler varResult specificFile
:: Returns the path to the compiler if found in the known locations file
:findKnown
echo    :findKnown(%*) >> %~dp0functions.log

   IF "%5" NEQ "" (
      ECHO "Too many arguments passed to: ':findKnown'!"
      EXIT /b 1
   )

   IF NOT EXIST "%~1" (
      ECHO "File '%~1' passed to: ':findKnown' does not exist!"
      EXIT /b 1
   )

   CALL :isValidVarName %3
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid result variable name: '%~3' passed to: ':findKnown'!"
         EXIT /b 1
      )

   SETLOCAL EnableDelayedExpansion

      CALL :value %2 app

      IF "%~4" == "" (SET "specificFile=\bin\!app!.exe") ELSE (SET "specificFile=%~4")

      FOR /F "tokens=*" %%G IN (%~1) DO (
         IF "!knownPathFound!" == "" (
            SET "knownPath=%%G"

            CALL :expandInner "!knownPath!" localExpanded
            SET "knownPath=!localExpanded!"

            echo   search: '!knownPath!!specificFile!' >> %~dp0functions.log
            IF EXIST "!knownPath!!specificFile!" (
               set "knownPathFound=!knownPath!"
               echo  found: '!knownPathFound!' >> %~dp0functions.log
            ) ELSE (
               echo  not found: '!knownPath!!specificFile!' >> %~dp0functions.log
            )
         )
      )
   ENDLOCAL & set "_Result=%knownPathFound%"

   IF "%_Result%" NEQ "" (
      SET "%~3=%_Result%"
      REM ECHO ":findKnown(%1 %2 %3 %4) found: '%_Result%'"
      SET "_Result="
      EXIT /b 0 
   ) ELSE (
      SET "%~3="
      EXIT /B 1
   )

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: exeName varResult
:: Returns the root path to \bin\<exeName>.exe if found in the path
:findInPath 
echo    :findInPath(%*) >> %~dp0functions.log

   IF "%~3" NEQ "" (
      ECHO "Too many arguments passed to: ':findInPath'!"
      EXIT /b 1
   )

   CALL :isValidVarName %2
      IF %ERRORLEVEL% NEQ 0 (
         ECHO "Invalid result variable name: '%~2' passed to: ':findInPath'!"
         exit /b 1
      )

   SETLOCAL EnableDelayedExpansion
      call :value %1 app

      FOR /f "delims=" %%a IN ('where !app!.exe 2^>nul') DO set "exeDir=%%a"

      REM Search for the root directory of the executable assuming it is in \bin
      IF "!exeDir!" NEQ "" (
         SET "binAndExeSuffix=\bin\!app!.exe"
         
         CALL :strLen !binAndExeSuffix! len
         SET /a "negativeLen=-!len!"
         CALL :Left exeDir !negativeLen! rootDir
         
         IF EXIST "!rootDir!!binAndExeSuffix!" (
            set "pathFound=!rootDir!"
         )
      )
   ENDLOCAL & SET "_Result=%pathFound%"

   IF "%_Result%" NEQ "" (
      SET "%~2=%_Result%"
      REM ECHO ":findInPath(%~1) found: '%_Result%'"
      SET "_Result="
      EXIT /b 0 
   ) ELSE (
       SET "%~2="
      EXIT /B 1
   )

::-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
:: Args: questionPrompt
:: Returns errorlevel 0 if user answered Y, 1 otherwise
:continue_Y_N 
echo    :Continue_Y_N(%*) >> %~dp0functions.log

   IF "%2" NEQ "" (
      ECHO "Too many arguments passed to: ':Continue'!"
      EXIT /b 1
   )

   IF "%~1" == "" (
      ECHO "No question prompt passed to: 'continue_Y_N'!"
      EXIT /b 1
   )

   SET /P _Result="%~1"
   IF /I "%_Result%" == "Y" (
      SET _Result=
      EXIT /b 0
   )
   SET _Result=
   EXIT /B 1
