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

:SET_BCC
   call %~dp0..\winmake\find_bc.bat

IF "%CC%"=="" GOTO NOT_FOUND

:SAVE_PATH
   SET _PATH=%PATH%

call %~dp0bld.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:NOT_FOUND
    rem Let's return an error code to the caller
    exit /b 1

:END
SET "PATH=%_PATH%"
SET "_PATH="

