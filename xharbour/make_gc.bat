@echo off
rem
rem $Id: make_gc.bat,v 1.2 2005/02/17 12:22:26 andijahja Exp $
rem
rem Batch File For Building xHarbour with MinGW32 under Windows
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
@set BIN_DIR=bin\gcc
@set OBJ_DIR=lib\gcc
@set LIB_DIR=obj\gcc

rem Set up our BIN paths
@set _PATH=%PATH%
@set PATH=%MINGWDIR%\bin;%BISONDIR%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist %OBJ_DIR%              md %OBJ_DIR%
if not exist %LIB_DIR%              md %LIB_DIR%
if not exist %BIN_DIR%              md %BIN_DIR%
if not exist %OBJ_DIR%\opt          md %OBJ_DIR%\opt
if not exist %OBJ_DIR%\unicode      md %OBJ_DIR%\unicode
if not exist %OBJ_DIR%\libmisc      md %OBJ_DIR%\libmisc
if not exist %OBJ_DIR%\nanfor       md %OBJ_DIR%\nanfor
if not exist %OBJ_DIR%\libct        md %OBJ_DIR%\libct
if not exist %OBJ_DIR%\html         md %OBJ_DIR%\html
if not exist %OBJ_DIR%\design       md %OBJ_DIR%\design
if not exist %OBJ_DIR%\rddads       md %OBJ_DIR%\rddads
if not exist %OBJ_DIR%\opt\console  md %OBJ_DIR%\opt\console
if not exist %OBJ_DIR%\opt\gui      md %OBJ_DIR%\opt\gui

:BUILD

   mingw32-make.exe -f makefile.gc
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   goto EXIT

:CLEAN

if exist %LIB_DIR%\*.a              del %LIB_DIR%\*.a
if exist %BIN_DIR%\*.exe            del %BIN_DIR%\*.exe
if exist %OBJ_DIR%\*.o              del %OBJ_DIR%\*.o
if exist %OBJ_DIR%\*.c              del %OBJ_DIR%\*.c
if exist %OBJ_DIR%\*.h              del %OBJ_DIR%\*.h
if exist %OBJ_DIR%\*.output         del %OBJ_DIR%\*.output
if exist %OBJ_DIR%\opt\*.o          del %OBJ_DIR%\opt\*.o
if exist %OBJ_DIR%\unicode\*.o      del %OBJ_DIR%\unicode\*.o
if exist %OBJ_DIR%\libmisc\*.o      del %OBJ_DIR%\libmisc\*.o
if exist %OBJ_DIR%\nanfor\*.o       del %OBJ_DIR%\nanfor\*.o
if exist %OBJ_DIR%\libct\*.o        del %OBJ_DIR%\libct\*.o
if exist %OBJ_DIR%\design\*.o       del %OBJ_DIR%\design\*.o
if exist %OBJ_DIR%\html\*.o         del %OBJ_DIR%\html\*.o
if exist %OBJ_DIR%\rddads\*.o       del %OBJ_DIR%\rddads\*.o
if exist %OBJ_DIR%\opt\console\*.o  del %OBJ_DIR%\opt\console\*.o
if exist %OBJ_DIR%\opt\gui\*.o      del %OBJ_DIR%\opt\gui\*.o
if exist %OBJ_DIR%\opt\*.c          del %OBJ_DIR%\opt\*.c
if exist %OBJ_DIR%\unicode\*.c      del %OBJ_DIR%\unicode\*.c
if exist %OBJ_DIR%\libmisc\*.c      del %OBJ_DIR%\libmisc\*.c
if exist %OBJ_DIR%\nanfor\*.c       del %OBJ_DIR%\nanfor\*.c
if exist %OBJ_DIR%\libct\*.c        del %OBJ_DIR%\libct\*.c
if exist %OBJ_DIR%\rddads\*.c       del %OBJ_DIR%\rddads\*.c
if exist %OBJ_DIR%\opt\console\*.c  del %OBJ_DIR%\opt\console\*.c
if exist %OBJ_DIR%\opt\gui\*.c      del %OBJ_DIR%\opt\gui\*.c
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
