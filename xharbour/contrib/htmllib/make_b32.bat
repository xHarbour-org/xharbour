@echo off
rem
rem $Id: make_b32.bat,v 1.13 2003/07/26 08:01:36 brianhays Exp $
rem

if not exist .\obj md .\obj
if not exist .\obj\b32 md .\obj\b32

make -fmakefile.bc %1 %2 %3 >make_b32.log
if errorlevel 1 goto error
if "%1" == "clean" goto end
if "%1" == "CLEAN" goto end


:copy_files
   copy ..\..\lib\b32\html.lib ..\..\lib >nul
   goto End

:error
   notepad make_b32.log

:end
