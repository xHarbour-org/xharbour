@echo off
if not exist .\lib md .\lib
if not exist .\obj md .\obj
if not exist .\lib\b32 md .\lib\b32
if not exist .\obj\b32 md .\obj\b32
make -fmakefile.bc
if errorlevel 1 goto error
goto copy_files
:error
echo there is an error on make files
goto end
:copy_files
copy lib\b32\*.lib lib
copy lib\b32\*.lib ..\..\lib
:end
