@echo off
if not exist .\obj md .\obj
if not exist .\obj\b32 md .\obj\b32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

make -fmakefile.bc %1 %2 %3 >make_b32.log
if errorlevel 1 goto error

:copy_files
   copy ..\..\lib\b32\whoo.lib ..\..\lib >nul
   goto End

:error
   echo there is an error on make files
   notepad make_b32.log
   goto end

:clean

   if exist ..\..\lib\b32\whoo.lib del ..\..\lib\b32\whoo.lib
   if exist obj\b32\*.obj del obj\b32\*.obj
   if exist obj\b32\*.c   del obj\b32\*.c

:end
