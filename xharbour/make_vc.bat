@echo off
rem 
rem $Id: make_vc.bat,v 1.6 2003/04/04 17:21:14 paultucker Exp $
rem 

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs 
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to 
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

set rem=rem
if "%1"=="/?" set rem=echo.
%rem% ---------------------------------------------------------------
%rem% Usage: make_vc [/y] [/a or CLEAN or other specific target]
%rem% Call with nothing, /Y, /A, or CLEAN
%rem% nothing - compiles what needs it.  If hb_mt is set, then multi-threaded
%rem% versions of the harbour programs will be placed into the bin directory.
%rem% CLEAN, delete targets.
%rem% /A clean, then compile all - In addition, this will compile standard
%rem%    libs as well as Multi-Threaded libs.  If you set HB_MT=MT
%rem%    prior to executing this batch file, then the Multi-Threaded versions
%rem%    of the programs will be active in the bin dir - otherwise, it will
%rem%    be the standard versions.
%rem% /Y non batch mode (forces makefile.vc)
%rem% ---------------------------------------------------------------
set rem=
if "%1"=="/?" goto exit

if not exist obj md obj
if not exist obj\vc md obj\vc
if not exist obj\vcmt md obj\vcmt
if not exist lib md lib
if not exist lib\vc md lib\vc
if not exist bin md bin
if not exist bin\vc md bin\vc
if not exist bin\vcmt md bin\vcmt

set MK_FILE=makefile.vc
if "%OS%" == "Windows_NT" set MK_FILE=makefile.nt
if "%1" == "/Y" set MK_FILE=makefile.vc
if "%1" == "/y" set MK_FILE=makefile.vc
if "%2" == "/Y" set MK_FILE=makefile.vc
if "%2" == "/y" set MK_FILE=makefile.vc
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f %MK_FILE% %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

if "%1"=="/A" goto check3

:CHECK2

if not "%1"=="/a" goto build_ok

:CHECK3

if "%hb_mt%"=="" set hb_mt=xyzzy
   nmake /f %MK_FILE% %2 %3 >> make_vc.log
if "%hb_mt%"=="xyzzy" set hb_mt=
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\vc%HB_MT%\*.exe bin > nul
   copy lib\vc\*.lib lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN

   nmake /f %MK_FILE% %1
   rem in this case, the makefile handles most cleanup. Add what you need here
   if exist make_vc.log del make_vc.log
   rem etc.

:EXIT
SET MK_FILE=
