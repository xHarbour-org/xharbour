@echo off
if not exist .\lib md .\lib
if not exist .\obj md .\obj

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

make -fmakefile.bc
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
