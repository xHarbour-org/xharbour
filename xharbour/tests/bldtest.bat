@echo off
rem ***********************************************************
rem * bldtest.bat
rem *
rem * $Id: bldtest.bat,v 1.1 2003/01/16 23:38:19 fsgiudice Exp $
rem *
rem * Batch file to build test programs in ST or MT environment
rem *
rem * (C) 2003 Francesco Saverio Giudice <info@fsgiudice.com>
rem *
rem ***********************************************************
rem *
rem * This is a generic batch file, if it doesn't fit your own needs
rem * please DON'T MODIFY IT.
rem *
rem * Instead, make a local copy and modify that one, or make a call to
rem * this batch file from your customized one.
rem *
rem ***********************************************************

rem Saving current HB_MT state
set OLDENVMT=%HB_MT%

set HB_ARCHITECTURE=w32
set HB_COMPILER=bcc32
set HB_INSTALL=..
SET HB_BIN_INSTALL=%HB_INSTALL%\bin
set HB_INC_INSTALL=%HB_INSTALL%\include
set HB_LIB_INSTALL=%HB_INSTALL%\lib

rem Check help request
IF %1.==/?. GOTO SHOWHELP
IF %1.==/H. GOTO SHOWHELP
IF %1.==/h. GOTO SHOWHELP

echo.
echo.BldTest.bat - /? or /h to display options
echo.

rem Check MT build request
IF %1.==/MT. GOTO SETMT
IF %1.==/mt. GOTO SETMT
GOTO SETST

:SETMT
echo.Setting MultiThread (MT) mode
echo.
SET HB_MT=mt
SHIFT
GOTO CALLBLD

:SETST
echo.Setting SingleThread (ST) mode
echo.
SET HB_MT=
GOTO CALLBLD

:CALLBLD
echo.Running %HB_BIN_INSTALL%\bld.bat %1 %2 %3 %4 %5
echo.
echo.please wait ...
echo.
call %HB_BIN_INSTALL%\bld.bat %1 %2 %3 %4 %5 > bldtest.log
IF ERRORLEVEL 1 GOTO SHOWERROR

if exist *.c   del *.c
if exist *.obj del *.obj
if exist *.tds del *.tds

GOTO COMPILEOK

:SHOWERROR
echo.
echo.Error on compiling ...
echo.
echo.Running notepad, please close to end this batch file ...
echo.
notepad bldtest.log
echo.
echo.Notepad closed, exiting ...
echo.
GOTO ENDSET

:COMPILEOK
echo.
echo.Compiled successfully
echo.
if exist bldtest.log del bldtest.log
GOTO ENDSET

:SHOWHELP
echo.
echo."bldtest [/MT|/mt|/?|/H|/h] prgname"
echo.
echo. /MT       = Set MT envinronment to build test program in MultiThread mode
echo.             otherwise program will be compiled in SingleThread mode
echo. /? or /H  = Show this help
echo. prgname   = Name of prg file to compile without extension [.prg]
echo.

:ENDSET
rem Restore Old MT Setting
set HB_MT=%OLDENVMT%
set OLDENVMT=
