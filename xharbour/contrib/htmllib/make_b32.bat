@echo off
rem
rem $Id: make_b32.bat,v 1.1 2003/09/23 21:28:26 paultucker Exp $
rem

if not exist .\obj md .\obj
if not exist .\obj\b32 md .\obj\b32

   make -fmakefile.bc %1 %2 %3 >make_b32.log
   if "%1" == "clean" goto clean
   if "%1" == "CLEAN" goto clean
   if errorlevel 1 goto error

:copy_files
   copy ..\..\lib\b32\html.lib ..\..\lib >nul
   goto End

:error
   notepad make_b32.log
   goto End

:clean
   if exist make_b32.log del make_b32.log

:end
