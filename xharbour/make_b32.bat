@echo off
rem
rem $Id: make_b32.bat,v 1.33 2006/07/05 18:31:07 vszakats Exp $
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem
rem Set any of the below settings to customize your build process:
rem    set HB_BUILD_MODE=P
rem    set HB_BUILD_DLL=yes
rem    set HB_BUILD_DEBUG=yes
rem    set HB_BUILD_VERBOSE=yes
rem    set HB_MAKE_PROGRAM=
rem    set HB_MAKE_FLAGS=
rem ---------------------------------------------------------------

if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=make.exe

rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

rem ---------------------------------------------------------------

:BUILD

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS%            -r -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR
   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -DHB_MT=mt -r -fmakefile.bc %1 %2 %3 > mkmt_b32.log
   if errorlevel 1 goto BUILD_ERR_MT

rem ---------------------------------------------------------------

:BUILD_OK

   rem copy bin\b32\*.exe bin\*.* > nul
   rem copy lib\b32\*.lib lib\*.* > nul
   goto EXIT

rem ---------------------------------------------------------------

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

rem ---------------------------------------------------------------

:BUILD_ERR_MT

   notepad mkmt_b32.log
   goto EXIT

rem ---------------------------------------------------------------

:CLEAN

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS%            -f makefile.bc CLEAN > make_b32.log
   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -DHB_MT=mt -f makefile.bc CLEAN > make_b32.log

   rem In this case, the makefile handles most cleanup.

   if exist make_b32.log del make_b32.log
   if exist mkmt_b32.log del mkmt_b32.log
   goto EXIT

rem ---------------------------------------------------------------

:EXIT
