@echo off
rem
rem $Id: dll_pc.bat,v 1.1 2005/02/19 11:22:29 andijahja Exp $
rem
rem Batch File For Building xHarbour DLL with PellesC under Windows
rem
rem  *************** WARNING WARNING WARNING WARNING WARNING *******************
rem  PellesC POMAKE.EXE is buggy. It will crash if used.
rem  ERROR: POMAKE: error: Internal error (Stack overflow).
rem  This is true until version: Pelles Make Utility, Version 3.00.4
rem  Please use other's compiler make utility
rem  This batch file is tested using Borland CPP make.exe
rem  *************** WARNING WARNING WARNING WARNING WARNING *******************
rem

@SET MAKEEXE=f:\borland\bcc55\bin\make.exe

rem Our MinGW32 Root Directory
@SET POCCMAIN=f:\pellesc

rem Our BISON BIN Directory
@set BISONDIR=F:\Bison\bin

rem SET xHarbour Working Root Directory Here
SET XHARBOUR_ROOT=d:\xharbour

rem Set up our environment for output files here
rem Let them be like that

rem Set up our BIN paths
@set _PATH=%PATH%
@set PATH=%POCCMAIN%\bin;%BISONDIR%

@set BIN_DIR=bin\pocc
@set LIB_DIR=lib\pocc
@set OBJ_DIR=obj\pocc\dll

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

if not exist lib        md lib
if not exist obj        md obj
if not exist %LIB_DIR%  md %LIB_DIR%
if not exist %OBJ_DIR%  md %OBJ_DIR%

   %MAKEEXE% -f "hrbdll.pc"
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   goto EXIT

:CLEAN

if exist %LIB_DIR%\harbour.exp        del %LIB_DIR%\harbour.exp
if exist %LIB_DIR%\harbour.lib        del %LIB_DIR%\harbour.lib
if exist %BIN_DIR%\harbour.dll        del %BIN_DIR%\harbour.dll
if exist %LIB_DIR%\harbour.def        del %LIB_DIR%\harbour.def

if exist %BIN_DIR%\hbdocdll.lib       del %BIN_DIR%\hbdocdll.lib
if exist %BIN_DIR%\hbmakedll.lib      del %BIN_DIR%\hbmakedll.lib
if exist %BIN_DIR%\hbrundll.lib       del %BIN_DIR%\hbrundll.lib
if exist %BIN_DIR%\hbtestdll.lib      del %BIN_DIR%\hbtestdll.lib
if exist %BIN_DIR%\xbscriptdll.lib    del %BIN_DIR%\xbscriptdll.lib

if exist %BIN_DIR%\hbdocdll.exe       del %BIN_DIR%\hbdocdll.exe
if exist %BIN_DIR%\hbmakedll.exe      del %BIN_DIR%\hbmakedll.exe
if exist %BIN_DIR%\hbrundll.exe       del %BIN_DIR%\hbrundll.exe
if exist %BIN_DIR%\hbtestdll.exe      del %BIN_DIR%\hbtestdll.exe
if exist %BIN_DIR%\xbscriptdll.exe    del %BIN_DIR%\xbscriptdll.exe

if exist %BIN_DIR%\hbdocdll.exp       del %BIN_DIR%\hbdocdll.exp
if exist %BIN_DIR%\hbmakedll.exp      del %BIN_DIR%\hbmakedll.exp
if exist %BIN_DIR%\hbrundll.exp       del %BIN_DIR%\hbrundll.exp
if exist %BIN_DIR%\hbtestdll.exp      del %BIN_DIR%\hbtestdll.exp
if exist %BIN_DIR%\xbscriptdll.exp    del %BIN_DIR%\xbscriptdll.exp

if exist %OBJ_DIR%\*.c                del %OBJ_DIR%\*.c
if exist %OBJ_DIR%\*.obj              del %OBJ_DIR%\*.obj
if exist %OBJ_DIR%\*.h                del %OBJ_DIR%\*.h
if exist %OBJ_DIR%\*.output           del %OBJ_DIR%\*.output
   goto EXIT

:EXIT
rem Clean up and restore environment
@set PATH=%_PATH%
@set _PATH=
@set BIN_DIR=
@set OBJ_DIR=
@set LIB_DIR=
@set POCCMAIN=
@set BISONDIR=
@set XHARBOUR_ROOT=
