@echo off
rem
rem $Id: make_gcc.bat,v 1.0 2005/02/16 08:37:21 andijahja Exp $
rem
rem Batch File For Building xHarbour with MinGW32 under Windows
rem
rem What we have to do is to set the following 2 envars, ONE and TWO

rem ENVAR ONE
rem Our MinGW32 Root Directory
@set MINGWDIR=F:\MinGW

rem ENVAR TWO
rem Our BISON BIN Directory
@set BISONDIR=F:\Bison\bin

rem Set up our environment for output files here
rem Let them be like that
@set GCCBIN=bin\gcc
@set GCCLIB=lib\gcc
@set GCCOBJ=obj\gcc

rem Set up our BIN paths
@set _PATH=%PATH%
@set PATH=%MINGWDIR%\bin;%BISONDIR%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist %GCCOBJ%              md %GCCOBJ%
if not exist %GCCLIB%              md %GCCLIB%
if not exist %GCCBIN%              md %GCCBIN%
if not exist %GCCOBJ%\opt          md %GCCOBJ%\opt
if not exist %GCCOBJ%\unicode      md %GCCOBJ%\unicode
if not exist %GCCOBJ%\libmisc      md %GCCOBJ%\libmisc
if not exist %GCCOBJ%\nanfor       md %GCCOBJ%\nanfor
if not exist %GCCOBJ%\libct        md %GCCOBJ%\libct
if not exist %GCCOBJ%\html         md %GCCOBJ%\html
if not exist %GCCOBJ%\design       md %GCCOBJ%\design
if not exist %GCCOBJ%\rddads       md %GCCOBJ%\rddads
if not exist %GCCOBJ%\opt\console  md %GCCOBJ%\opt\console
if not exist %GCCOBJ%\opt\gui      md %GCCOBJ%\opt\gui

:BUILD

   mingw32-make.exe -f makefile.gc
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   goto EXIT

:CLEAN

if exist %GCCLIB%\*.a              del %GCCLIB%\*.a
if exist %GCCBIN%\*.exe            del %GCCBIN%\*.exe
if exist %GCCOBJ%\*.o              del %GCCOBJ%\*.o
if exist %GCCOBJ%\*.c              del %GCCOBJ%\*.c
if exist %GCCOBJ%\*.h              del %GCCOBJ%\*.h
if exist %GCCOBJ%\*.output         del %GCCOBJ%\*.output
if exist %GCCOBJ%\opt\*.o          del %GCCOBJ%\opt\*.o
if exist %GCCOBJ%\unicode\*.o      del %GCCOBJ%\unicode\*.o
if exist %GCCOBJ%\libmisc\*.o      del %GCCOBJ%\libmisc\*.o
if exist %GCCOBJ%\nanfor\*.o       del %GCCOBJ%\nanfor\*.o
if exist %GCCOBJ%\libct\*.o        del %GCCOBJ%\libct\*.o
if exist %GCCOBJ%\design\*.o       del %GCCOBJ%\design\*.o
if exist %GCCOBJ%\html\*.o         del %GCCOBJ%\html\*.o
if exist %GCCOBJ%\rddads\*.o       del %GCCOBJ%\rddads\*.o
if exist %GCCOBJ%\opt\console\*.o  del %GCCOBJ%\opt\console\*.o
if exist %GCCOBJ%\opt\gui\*.o      del %GCCOBJ%\opt\gui\*.o
if exist %GCCOBJ%\opt\*.c          del %GCCOBJ%\opt\*.c
if exist %GCCOBJ%\unicode\*.c      del %GCCOBJ%\unicode\*.c
if exist %GCCOBJ%\libmisc\*.c      del %GCCOBJ%\libmisc\*.c
if exist %GCCOBJ%\nanfor\*.c       del %GCCOBJ%\nanfor\*.c
if exist %GCCOBJ%\libct\*.c        del %GCCOBJ%\libct\*.c
if exist %GCCOBJ%\rddads\*.c       del %GCCOBJ%\rddads\*.c
if exist %GCCOBJ%\opt\console\*.c  del %GCCOBJ%\opt\console\*.c
if exist %GCCOBJ%\opt\gui\*.c      del %GCCOBJ%\opt\gui\*.c
   goto EXIT

:EXIT
rem Clean up and restore environment
@set PATH=%_PATH%
@set _PATH=
@set GCCLIB=
@set GCCOBJ=
@set MINGWDIR=
@set BISONDIR=
