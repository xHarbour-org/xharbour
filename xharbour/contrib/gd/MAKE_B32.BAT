@echo off
set OLDENVMT=%HB_MT%
set OLDGTLIB=%HB_GT_LIB%
set PRGNAME=gdlib

set HB_MT=
set HB_GT_LIB=gtcgi
rem if exist %PRGNAME%.exe del %PRGNAME%.exe

if not exist bgd.dll goto NODLL

if not exist .\lib md .\lib
if not exist .\obj md .\obj

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

make -fmakefile.bc  %1 %2 %3 >make_b32.log
if errorlevel 1 goto error
goto copy_files

:error
echo there is an error on make files
notepad make_b32.log
goto end

:clean

   if exist lib\*.lib del .\lib\*.lib
   if exist obj\*.obj del .\obj\*.obj
   if exist obj\*.c   del .\obj\*.c
   goto End

:copy_files
impdef -a lib\libbgd.def bgd.dll
implib -a lib\libbgd.lib lib\libbgd.def
copy lib\*.lib ..\..\lib

goto end

:NODLL

echo.
echo.Missing bgd.dll, please download it from:
echo.http://www.boutell.com/gd/http/gdwin32.zip
echo.
echo.Make aborted.
echo.

:end
if exist *.tds del *.tds
if exist *.map del *.map
rem Restore Old Settings
set HB_MT=%OLDENVMT%
set OLDENVMT=
set HB_GT_LIB=%OLDGTLIB%
set OLDGTLIB=

