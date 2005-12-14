@echo off
rem 
rem $Id: make_vc.bat,v 1.12 2003/06/24 08:37:21 andijahja Exp $
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
%rem% /Y non batch mode (for Win_98 build env for instance)
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

rem added subdir for optimized library
rem start in build 81

if not exist obj\vc\opt md obj\vc\opt
if not exist obj\vc\opt\console md obj\vc\opt\console
if not exist obj\vc\opt\gui md obj\vc\opt\gui

if not exist obj\vcmt\opt md obj\vcmt\opt
if not exist obj\vcmt\opt\console md obj\vcmt\opt\console
if not exist obj\vcmt\opt\gui md obj\vcmt\opt\gui

SET _HBMT=%HB_MT%
SET HB_FL=
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if "%1" == "/y" set HB_FL=/y
if "%1" == "/Y" set HB_FL=/y
if "%HB_FL%" == "/y" shift
:BUILD

   SET HB_MT=
echo.
   nmake /f makefile.vc %HB_FL% %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR
   SET HB_MT=MT
rem do not pass /a (if used) the second time!
   nmake /f makefile.vc %HB_FL% %2 %3 >> make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\vc\*.exe bin > nul
   copy lib\vc\*.lib lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN

   SET HB_MT=
   nmake /f makefile.vc %1
   SET HB_MT=MT
   nmake /f makefile.vc %1
   rem in this case, the makefile handles most cleanup. Add what you need here
   if exist make_vc.log del make_vc.log
   rem etc.

:EXIT
SET HB_FL=
SET HB_MT=%_HBMT%
SET _HBMT=
