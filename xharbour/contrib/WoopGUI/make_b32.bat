@echo off

if not exist .\obj md .\obj
if not exist .\obj\b32 md .\obj\b32

make -fmakefile.bc %1 %2 %3 >make_b32.log
if errorlevel 1 goto error

copy ..\..\lib\b32\woopgui.lib ..\..\lib >nul
goto end

:error
echo there is an error on make files
notepad make_b32.log

:end
