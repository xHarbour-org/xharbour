@echo off
rem
rem $Id$
rem

if not "%HB_DIR_ADS%" == "" goto DIR_OK

echo ---------------------------------------------------------------
echo IMPORTANT: You'll need Advantage Client Engine (5.0 or upper)
echo            and this envvar to be set to successfully build this library:
echo            set HB_DIR_ADS=C:\ads\acesdk
echo ---------------------------------------------------------------
goto EXIT

:DIR_OK

set _CFLAGS=%CFLAGS%
set CFLAGS=-I"%HB_DIR_ADS%" %_CFLAGS%
set HB_DLL_NAME=ace32
if exist "%HB_DIR_ADS%\Redistribute\%HB_DLL_NAME%.dll" set HB_DLL_DIR=%HB_DIR_ADS%\Redistribute
if exist "%HB_DIR_ADS%\%HB_DLL_NAME%.dll"              set HB_DLL_DIR=%HB_DIR_ADS%
if exist "%HB_DIR_ADS%\32bit\%HB_DLL_NAME%.dll"        set HB_DLL_DIR=%HB_DIR_ADS%\32bit

echo Using this .dll: "%HB_DLL_DIR%\%HB_DLL_NAME%.dll"

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\rddads.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\rddads.bak del ..\..\lib\b32\rddads.bak
   if exist ..\..\lib\%HB_DLL_NAME%.lib del ..\..\lib\%HB_DLL_NAME%.lib
   if "%HB_ADS_IMPLIB%" == "no" goto EXIT
   implib ..\..\lib\%HB_DLL_NAME%.lib "%HB_DLL_DIR%\%HB_DLL_NAME%.dll" >> make_b32.log
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\rddads.lib   del ..\..\lib\b32\rddads.lib
   if exist ..\..\lib\b32\rddads.bak   del ..\..\lib\b32\rddads.bak
   if exist ..\..\obj\b32\ace32.obj    del ..\..\obj\b32\ace32.obj
   if exist ..\..\obj\b32\ads1.obj     del ..\..\obj\b32\ads1.obj
   if exist ..\..\obj\b32\adsfunc.obj  del ..\..\obj\b32\adsfunc.obj
   if exist ..\..\obj\b32\adsmgmnt.obj del ..\..\obj\b32\adsmgmnt.obj

   goto EXIT

:EXIT

set CFLAGS=%_CFLAGS%
set _CFLAGS=
set HB_DLL_NAME=
set HB_DLL_DIR=
