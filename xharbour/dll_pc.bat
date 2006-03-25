@echo off
rem
rem $Id: dll_pc.bat,v 1.6 2006/03/24 17:45:38 modalsist Exp $
rem
rem Batch file to build harbour.dll for Pelles C
rem
rem


REM *************************************
REM *** CHANGE ONLY THE 3 SETS BELLOW ***
REM *************************************

SET HB_DIR=.\
SET POCC_DIR=c:\pellesc
SET BISON_DIR=c:\bison

REM *****************************************
REM *** DON'T CHANGE THE VARIABLES BELLOW ***
REM *****************************************

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB=%POCC_DIR\lib;%POCC_DIR\lib\win;%HB_DIR\lib;%LIB%
SET INCLUDE=%POCC_DIR\include;%POCC_DIR\include\win;%HB_DIR%\include;%INCLUDE%

SET PATH=%POCC_DIR%\bin;%BISON_DIR%\bin;%HB_DIR%\bin;%PATH%

SET TARGET_OBJ_DIR=%HB_DIR%\obj\pocc\dll
SET TARGET_LIB_DIR=%HB_DIR%\lib\pocc
SET TARGET_BIN_DIR=%HB_DIR%\bin\pocc

SET BISON_SIMPLE=%BISON_DIR%\share\bison\bison.simple

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   echo Compiling harbour.dll

   if not exist lib md lib
   if not exist obj md obj
   if not exist %TARGET_LIB_DIR%  md %TARGET_LIB_DIR%
   if not exist %TARGET_OBJ_DIR%  md %TARGET_OBJ_DIR%

   pomake /f hrbdll.pc > dll_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy %TARGET_LIB_DIR%\harbour.dll %HB_DIR%\bin > null
   copy %TARGET_LIB_DIR%\harbour.lib %HB_DIR%\lib > null
   del  %TARGET_LIB_DIR%\harbour.dll > null
   del  %TARGET_LIB_DIR%\harbour.lib > null
   del  %TARGET_LIB_DIR%\*.exp  > null

   copy %TARGET_BIN_DIR%\hbdocdll.exe %HB_DIR%\bin > null
   copy %TARGET_BIN_DIR%\hbmakedll.exe %HB_DIR%\bin > null
   copy %TARGET_BIN_DIR%\hbrundll.exe %HB_DIR%\bin > null
   copy %TARGET_BIN_DIR%\hbtestdll.exe %HB_DIR%\bin > null
   copy %TARGET_BIN_DIR%\xbscriptdll.exe %HB_DIR%\bin > null
   del  %TARGET_BIN_DIR%\hbdocdll.exe > null
   del  %TARGET_BIN_DIR%\hbmakedll.exe > null
   del  %TARGET_BIN_DIR%\hbrundll.exe > null
   del  %TARGET_BIN_DIR%\hbtestdll.exe > null
   del  %TARGET_BIN_DIR%\xbscriptdll.exe > null
   del  %TARGET_BIN_DIR%\*.exp  > null

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

   if exist %TARGET_LIB_DIR%\harbour.exp        del %TARGET_LIB_DIR%\harbour.exp
   if exist %TARGET_LIB_DIR%\harbour.lib        del %TARGET_LIB_DIR%\harbour.lib
   if exist %TARGET_LIB_DIR%\harbour.dll        del %TARGET_LIB_DIR%\harbour.dll
   if exist %TARGET_LIB_DIR%\harbour.def        del %TARGET_LIB_DIR%\harbour.def

   if exist %TARGET_BIN_DIR%\hbdocdll.lib       del %TARGET_BIN_DIR%\hbdocdll.lib
   if exist %TARGET_BIN_DIR%\hbmakedll.lib      del %TARGET_BIN_DIR%\hbmakedll.lib
   if exist %TARGET_BIN_DIR%\hbrundll.lib       del %TARGET_BIN_DIR%\hbrundll.lib
   if exist %TARGET_BIN_DIR%\hbtestdll.lib      del %TARGET_BIN_DIR%\hbtestdll.lib
   if exist %TARGET_BIN_DIR%\xbscriptdll.lib    del %TARGET_BIN_DIR%\xbscriptdll.lib
   if exist %TARGET_BIN_DIR%\xharbourdll.lib    del %TARGET_BIN_DIR%\xharbourdll.lib

   if exist %TARGET_BIN_DIR%\hbdocdll.exe       del %TARGET_BIN_DIR%\hbdocdll.exe
   if exist %TARGET_BIN_DIR%\hbmakedll.exe      del %TARGET_BIN_DIR%\hbmakedll.exe
   if exist %TARGET_BIN_DIR%\hbrundll.exe       del %TARGET_BIN_DIR%\hbrundll.exe
   if exist %TARGET_BIN_DIR%\hbtestdll.exe      del %TARGET_BIN_DIR%\hbtestdll.exe
   if exist %TARGET_BIN_DIR%\xharbourdll.exe    del %TARGET_BIN_DIR%\xharbourdll.exe
   if exist %TARGET_BIN_DIR%\xbscriptdll.exe    del %TARGET_BIN_DIR%\xbscriptdll.exe

   if exist %TARGET_BIN_DIR%\hbdocdll.exp       del %TARGET_BIN_DIR%\hbdocdll.exp
   if exist %TARGET_BIN_DIR%\hbmakedll.exp      del %TARGET_BIN_DIR%\hbmakedll.exp
   if exist %TARGET_BIN_DIR%\hbrundll.exp       del %TARGET_BIN_DIR%\hbrundll.exp
   if exist %TARGET_BIN_DIR%\hbtestdll.exp      del %TARGET_BIN_DIR%\hbtestdll.exp
   if exist %TARGET_BIN_DIR%\xharbourdll.exp    del %TARGET_BIN_DIR%\xharbourdll.exp
   if exist %TARGET_BIN_DIR%\xbscriptdll.exp    del %TARGET_BIN_DIR%\xbscriptdll.exp

   if exist %TARGET_OBJ_DIR%\*.c                del %TARGET_OBJ_DIR%\*.c
   if exist %TARGET_OBJ_DIR%\*.obj              del %TARGET_OBJ_DIR%\*.obj
   if exist %TARGET_OBJ_DIR%\*.h                del %TARGET_OBJ_DIR%\*.h
   if exist %TARGET_OBJ_DIR%\*.output           del %TARGET_OBJ_DIR%\*.output

   if exist tests\harbour.dll                   del tests\harbour.dll
   goto EXIT


:EXIT

SET POCC_DIR=
SET HB_DIR=
SET BISON_DIR=
SET BISON_SIMPLE=

SET TARGET_TARGET_OBJ_DIR=
SET TARGET_TARGET_LIB_DIR=
SET TARGET_TARGET_BIN_DIR=
SET HB_MT=

SET PATH=%_PATH%
SET LIB=%_LIB%
SET INCLUDE=%_INCLUDE%

SET _PATH=
SET _LIB=
SET _INCLUDE=



