@echo off
if not exist .\obj md .\obj
if not exist .\obj\b32 md .\obj\b32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

make -fmakefile.bc %1 %2 %3 >make_b32.log
if errorlevel 1 goto error

:copy_files
copy ..\..\lib\b32\what32.lib ..\..\lib >nul
goto end

:error
notepad make_b32.log
goto end

:clean

   if exist ..\..\lib\b32\what32.lib del ..\..\lib\b32\what32.lib
   if exist obj\b32\*.obj del .\obj\b32\*.obj
   if exist obj\b32\*.c   del .\obj\b32\*.c
   if exist make_b32.log del make_b32.log

:end
