@echo off

rem Create environment vars and folders

rem Check if already exists a FireBird env var
IF %FBDIR%. == . GOTO ERROR
rem echo.FBDIR=%FBDIR%
echo.

rem Create, if not exist, local directories
IF NOT EXIST lib     md lib
IF NOT EXIST lib\b32 md lib\b32
IF NOT EXIST obj     md obj
IF NOT EXIST obj\b32 md obj\b32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   rem Check if not exist the import fbclient lib
   if not exist lib\b32\fbclient.lib implib lib\b32\fbclient.lib %FBDIR%\bin\fbclient.dll

   rem Copy the lib in xHarbour lib dir
   copy lib\b32\fbclient.lib ..\..\lib\*.* > nul

   rem Now copy final lib in xHarbour lib dir
   copy lib\b32\firebird.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist lib\b32\firebird.lib   del lib\b32\firebird.lib
   if exist lib\b32\firebird.bak   del lib\b32\firebird.bak
   if exist obj\b32\firebird.obj   del obj\b32\firebird.obj
   if exist obj\b32\TFirebird.c    del obj\b32\TFirebird.c
   if exist obj\b32\TFirebird.obj  del obj\b32\TFirebird.obj
   goto EXIT

:ERROR
   echo.
   echo.Error! FBDIR not defined.
   echo.
   echo.Please define the FBDIR environment var that point to your
   echo.Firebird install dir.
   echo.
   echo.i.e.: set FBDIR=c:\Firebird\Firebird_1_5
   echo.
   goto EXIT

:EXIT

