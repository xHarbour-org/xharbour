@echo off

if not exist .\obj md .\obj
if not exist .\obj\b32 md .\obj\b32

if "%1" == "clean" goto clean
if "%1" == "CLEAN" goto clean

make -fmakefile.bc %1 %2 %3 >make_b32.log
if errorlevel 1 goto error

copy ..\..\lib\b32\woopgui.lib ..\..\lib >nul
goto end

:error
echo there is an error on make files
notepad make_b32.log
goto end

:clean
   if exist ..\..\lib\b32\woopgui.lib del ..\..\lib\b32\woopgui.lib
   if exist obj\b32\*.obj del .\obj\b32\*.obj
   if exist obj\b32\*.c   del .\obj\b32\*.c
   if exist make_b32.log del make_b32.log

:end
