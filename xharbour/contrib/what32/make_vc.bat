@echo off
rem $Id: make_vc.bat,v 1.3 2002/10/30 02:32:48 paultucker Exp $
if not exist .\lib md .\lib
if not exist .\obj md .\obj

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
set MK_FILE=makefile.vc
if "%OS%" == "Windows_NT" set MK_FILE=makefile.nt
if "%1" == "/Y" set MK_FILE=makefile.vc
if "%1" == "/y" set MK_FILE=makefile.vc

nmake -f%MK_FILE% %1 %2 %3
if errorlevel 1 goto error
goto copy_files

:clean

   if exist lib\*.lib del .\lib\*.lib
   if exist obj\*.obj del .\obj\*.obj
   if exist obj\*.c   del .\obj\*.c
   goto End

:error
echo there is an error on make files
goto end

:copy_files
copy lib\*.lib ..\..\lib

:end
