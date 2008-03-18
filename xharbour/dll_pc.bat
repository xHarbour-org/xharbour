@echo off
rem
rem $Id: dll_pc.bat,v 1.10 2007/11/10 04:38:32 guerra000 Exp $
rem
rem Batch file to build harbour.dll for Pelles C
rem
rem


REM *************************************
REM *** CHANGE ONLY THE 3 SETS BELLOW ***
REM *************************************

IF %HB_DIR%.==. SET HB_DIR=.\
IF %CC_DIR%.==. SET CC_DIR=C:\PellesC
SET BISON_DIR=C:\Bison

REM ****************************************
REM *** DON'T CHANGE THE VARIABLES BELOW ***
REM ****************************************

IF "%_PATH%" == ""    SET _PATH=%PATH%
IF "%_LIB%" == ""     SET _LIB=%LIB%
IF "%_INCLUDE%" == "" SET _INCLUDE=%INCLUDE%

SET LIB=%CC_DIR%\lib;%CC_DIR%\lib\win;%HB_DIR%\lib;%LIB%
SET INCLUDE=%CC_DIR%\include;%CC_DIR%\include\win;%HB_DIR%\include;%INCLUDE%

SET PATH=%CC_DIR%\bin;%BISON_DIR%\bin;%HB_DIR%\bin;%PATH%

SET OBJ_DIR=%HB_DIR%\obj\pc\dll
SET LIB_DIR=%HB_DIR%\lib\pc
SET BIN_DIR=%HB_DIR%\bin\pc

IF %BISON_SIMPLE%.==. SET BISON_SIMPLE=%BISON_DIR%\share\bison\bison.simple

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if "%1" == "/clean" goto CLEAN
if "%1" == "/CLEAN" goto CLEAN

:BUILD

   echo Compiling harbour.dll

   if not exist lib md lib
   if not exist obj md obj
   if not exist %LIB_DIR%  md %LIB_DIR%
   if not exist %OBJ_DIR%  md %OBJ_DIR%

   pomake /f hrbdll.pc > dll_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy %LIB_DIR%\harbour.dll %HB_DIR%\bin > null
   copy %LIB_DIR%\harbour.lib %HB_DIR%\lib > null
   del  %LIB_DIR%\harbour.dll > null
   del  %LIB_DIR%\harbour.lib > null
   del  %LIB_DIR%\*.exp  > null

   copy %BIN_DIR%\hbdocdll.exe %HB_DIR%\bin > null
   copy %BIN_DIR%\hbmakedll.exe %HB_DIR%\bin > null
   copy %BIN_DIR%\hbrundll.exe %HB_DIR%\bin > null
   copy %BIN_DIR%\hbtestdll.exe %HB_DIR%\bin > null
   copy %BIN_DIR%\xbscriptdll.exe %HB_DIR%\bin > null
   del  %BIN_DIR%\hbdocdll.exe > null
   del  %BIN_DIR%\hbmakedll.exe > null
   del  %BIN_DIR%\hbrundll.exe > null
   del  %BIN_DIR%\hbtestdll.exe > null
   del  %BIN_DIR%\xbscriptdll.exe > null
   del  %BIN_DIR%\*.exp  > null

   del dll_pc.log >null
   goto EXIT

:BUILD_ERR

   if exist dll_pc.log edit dll_pc.log
   goto EXIT

:CLEAN

   echo Cleaning harbour.dll

   if exist %HB_DIR%\bin\harbour.exp            del %HB_DIR%\bin\harbour.exp
   if exist %HB_DIR%\bin\harbour.lib            del %HB_DIR%\bin\harbour.lib
   if exist %HB_DIR%\bin\harbour.def            del %HB_DIR%\bin\harbour.def
   if exist %HB_DIR%\bin\harbour.dll            del %HB_DIR%\bin\harbour.dll

   if exist %HB_DIR%\lib\harbour.lib            del %HB_DIR%\lib\harbour.lib
   if exist %HB_DIR%\lib\harbour.dll            del %HB_DIR%\lib\harbour.dll

   if exist %HB_DIR%\bin\hbdocdll.exe           del %HB_DIR%\bin\hbdocdll.exe
   if exist %HB_DIR%\bin\hbmakedll.exe          del %HB_DIR%\bin\hbmakedll.exe
   if exist %HB_DIR%\bin\hbrundll.exe           del %HB_DIR%\bin\hbrundll.exe
   if exist %HB_DIR%\bin\hbtestdll.exe          del %HB_DIR%\bin\hbtestdll.exe
   if exist %HB_DIR%\bin\xharbourdll.exe        del %HB_DIR%\bin\xharbourdll.exe
   if exist %HB_DIR%\bin\xbscriptdll.exe        del %HB_DIR%\bin\xbscriptdll.exe
   if exist %HB_DIR%\bin\harbour.dll            del %HB_DIR%\bin\harbour.dll

   if exist %LIB_DIR%\harbour.exp        del %LIB_DIR%\harbour.exp
   if exist %LIB_DIR%\harbour.lib        del %LIB_DIR%\harbour.lib
   if exist %LIB_DIR%\harbour.dll        del %LIB_DIR%\harbour.dll
   if exist %LIB_DIR%\harbour.def        del %LIB_DIR%\harbour.def

   if exist %BIN_DIR%\hbdocdll.lib       del %BIN_DIR%\hbdocdll.lib
   if exist %BIN_DIR%\hbmakedll.lib      del %BIN_DIR%\hbmakedll.lib
   if exist %BIN_DIR%\hbrundll.lib       del %BIN_DIR%\hbrundll.lib
   if exist %BIN_DIR%\hbtestdll.lib      del %BIN_DIR%\hbtestdll.lib
   if exist %BIN_DIR%\xbscriptdll.lib    del %BIN_DIR%\xbscriptdll.lib
   if exist %BIN_DIR%\xharbourdll.lib    del %BIN_DIR%\xharbourdll.lib

   if exist %BIN_DIR%\hbdocdll.exe       del %BIN_DIR%\hbdocdll.exe
   if exist %BIN_DIR%\hbmakedll.exe      del %BIN_DIR%\hbmakedll.exe
   if exist %BIN_DIR%\hbrundll.exe       del %BIN_DIR%\hbrundll.exe
   if exist %BIN_DIR%\hbtestdll.exe      del %BIN_DIR%\hbtestdll.exe
   if exist %BIN_DIR%\xharbourdll.exe    del %BIN_DIR%\xharbourdll.exe
   if exist %BIN_DIR%\xbscriptdll.exe    del %BIN_DIR%\xbscriptdll.exe

   if exist %BIN_DIR%\hbdocdll.exp       del %BIN_DIR%\hbdocdll.exp
   if exist %BIN_DIR%\hbmakedll.exp      del %BIN_DIR%\hbmakedll.exp
   if exist %BIN_DIR%\hbrundll.exp       del %BIN_DIR%\hbrundll.exp
   if exist %BIN_DIR%\hbtestdll.exp      del %BIN_DIR%\hbtestdll.exp
   if exist %BIN_DIR%\xharbourdll.exp    del %BIN_DIR%\xharbourdll.exp
   if exist %BIN_DIR%\xbscriptdll.exp    del %BIN_DIR%\xbscriptdll.exp

   if exist %OBJ_DIR%\*.c                del %OBJ_DIR%\*.c
   if exist %OBJ_DIR%\*.obj              del %OBJ_DIR%\*.obj
   if exist %OBJ_DIR%\*.h                del %OBJ_DIR%\*.h
   if exist %OBJ_DIR%\*.output           del %OBJ_DIR%\*.output

   if exist tests\harbour.dll            del tests\harbour.dll

   goto EXIT


:EXIT

rem SET CC_DIR=
rem SET HB_DIR=
SET BISON_DIR=
rem SET BISON_SIMPLE=

SET OBJ_DIR=
SET LIB_DIR=
SET BIN_DIR=

SET PATH=%_PATH%
SET LIB=%_LIB%
SET INCLUDE=%_INCLUDE%

SET _PATH=
SET _LIB=
SET _INCLUDE=

