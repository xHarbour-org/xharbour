@echo off
rem
rem $Id: dll_gc.bat,v 1.1 2005/02/19 11:22:29 andijahja Exp $
rem
rem Batch File For Building xHarbour DLL with MinGW32 under Windows
rem
rem What we have to do is to set the following 3 envars, ONE, TWO and THREE

rem ENVAR ONE
rem Our MinGW32 Root Directory
@set MINGWDIR=F:/MinGW

rem ENVAR TWO
rem Our BISON BIN Directory
@set BISONDIR=F:/Bison/bin

rem ENVAR THREE
rem SET xHarbour Working Root Directory Here
@set HARBOUR_DIR=d:/xharbour

rem Set up our environment for output files here
rem Let them be like that

rem Set up our BIN paths
@set _PATH=%PATH%
@set PATH=%MINGWDIR%\bin;%BISONDIR%

@set BIN_DIR=bin\gcc
@set LIB_DIR=lib\gcc
@set OBJ_DIR=obj\gcc\dll

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

if not exist lib                       md lib
if not exist obj                       md obj
if not exist %LIB_DIR%                 md %LIB_DIR%
if not exist %OBJ_DIR%                 md %OBJ_DIR%

   mingw32-make.exe -f hrbdll.gc
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   goto EXIT

:CLEAN

if exist %LIB_DIR%\libharbour.a       del %LIB_DIR%\libharbour.a
if exist %LIB_DIR%\harbour.dll        del %LIB_DIR%\harbour.dll
if exist %BIN_DIR%\harbour.dll        del %BIN_DIR%\harbour.dll
if exist %LIB_DIR%\harbour.def        del %LIB_DIR%\harbour.def
if exist %BIN_DIR%\hbdocdll.exe       del %BIN_DIR%\hbdocdll.exe
if exist %BIN_DIR%\hbmakedll.exe      del %BIN_DIR%\hbmakedll.exe
if exist %BIN_DIR%\hbrundll.exe       del %BIN_DIR%\hbrundll.exe
if exist %BIN_DIR%\hbtestdll.exe      del %BIN_DIR%\hbtestdll.exe
if exist %BIN_DIR%\xbscriptdll.exe    del %BIN_DIR%\xbscriptdll.exe
if exist %OBJ_DIR%\*.c                del %OBJ_DIR%\*.c
if exist %OBJ_DIR%\*.o                del %OBJ_DIR%\*.o
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
@set MINGWDIR=
@set BISONDIR=
@set HARBOUR_DIR=
