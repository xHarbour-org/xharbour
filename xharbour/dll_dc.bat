@echo off
rem
rem $Id: dll_dc.bat,v 1.2 2005/03/03 14:16:44 andijahja Exp $
rem
rem Batch File For Building xHarbour DLL with DigitalMars C/C++ under Windows
rem

REM Creating DEF file which is needed
@if not exist dmcdll.def goto CREATE_DEF
GOTO BEGIN_CFG

:CREATE_DEF

ECHO LIBRARY "harbour.dll" > dmcdll.def
ECHO EXETYPE NT >> dmcdll.def
ECHO SUBSYSTEM CONSOLE >> dmcdll.def
ECHO CODE SHARED EXECUTE >> dmcdll.def
ECHO DATA WRITE >> dmcdll.def

:BEGIN_CFG
rem Our DigitalMars Root Directory
@SET DMCMAIN=F:\DM

@SET MAKEEXE=%DMCMAIN%\bin\make.exe

rem Our BISON BIN Directory
@set BISONDIR=F:\Bison\bin

rem SET xHarbour Working Root Directory Here
SET XHARBOUR_ROOT=C:\XHARBOUR

rem Set up our environment for output files here
rem Let them be like that

rem Set up our BIN paths
@set _PATH=%PATH%
@set PATH=%DMCMAIN%\bin;%BISONDIR%

@set BIN_DIR=bin\dmc
@set LIB_DIR=lib\dmc
@set OBJ_DIR=obj\dmc\dll

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

if not exist lib        md lib
if not exist obj        md obj
if not exist %LIB_DIR%  md %LIB_DIR%
if not exist %OBJ_DIR%  md %OBJ_DIR%

   %MAKEEXE% -fhrbdll.dc
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   goto EXIT

:CLEAN

if exist %LIB_DIR%\harbour.lib        del %LIB_DIR%\harbour.lib
if exist %BIN_DIR%\harbour.dll        del %BIN_DIR%\harbour.dll
if exist %BIN_DIR%\hbmakedll.exe      del %BIN_DIR%\hbmakedll.exe
if exist %BIN_DIR%\xharbourdll.exe    del %BIN_DIR%\xharbourdll.exe
if exist %BIN_DIR%\hbdocdll.exe       del %BIN_DIR%\hbdocdll.exe
if exist %BIN_DIR%\hbrundll.exe       del %BIN_DIR%\hbrundll.exe
if exist %BIN_DIR%\hbtestdll.exe      del %BIN_DIR%\hbtestdll.exe
if exist %BIN_DIR%\xbscriptdll.exe    del %BIN_DIR%\xbscriptdll.exe
if exist %LIB_DIR%\harbour.def        del %LIB_DIR%\harbour.def
if exist %OBJ_DIR%\*.c                del %OBJ_DIR%\*.c
if exist %OBJ_DIR%\*.obj              del %OBJ_DIR%\*.obj
if exist %OBJ_DIR%\*.h                del %OBJ_DIR%\*.h
if exist %OBJ_DIR%\*.output           del %OBJ_DIR%\*.output
if exist dmcdll.def                   del dmcdll.def
   goto EXIT

:EXIT
rem Clean up and restore environment
@set PATH=%_PATH%
@set _PATH=
@set BIN_DIR=
@set OBJ_DIR=
@set LIB_DIR=
@set DMCMAIN=
@set BISONDIR=
@set XHARBOUR_ROOT=
