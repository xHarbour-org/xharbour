@echo off
if not exist .\lib md .\lib
if not exist .\obj md .\obj
if not exist .\lib\b32 md .\lib\b32
if not exist .\obj\b32 md .\obj\b32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

make -fmakefile.bc
if errorlevel 1 goto error
goto copy_files

:clean

   if exist lib\b32\*.lib del .\lib\b32\*.lib
   if exist obj\b32\*.obj del .\obj\b32\*.obj
   if exist obj\b32\*.c   del .\obj\b32\*.c
   goto End

:error
echo there is an error on make files
goto end

:copy_files
copy lib\b32\*.lib lib
copy lib\*.lib ..\..\lib

:end
