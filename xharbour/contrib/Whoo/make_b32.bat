@echo off
if not exist .\lib md .\lib
if not exist .\obj md .\obj
make -fmakefile.bc
if errorlevel 1 goto error
goto copy_files
:error
echo there is an error on make files
goto end
:copy_files
copy lib\*.lib ..\..\lib
:end
