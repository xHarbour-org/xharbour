@echo off
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist obj md obj
if not exist obj\b32 md obj\b32
if not exist lib md lib
if not exist lib\b32 md lib\b32

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy lib\b32\*.lib ..\..\lib
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist lib\b32\*.lib del lib\b32\*.lib
   if exist lib\b32\*.bak del lib\b32\*.bak
   if exist obj\b32\*.obj del obj\b32\*.obj
   if exist obj\b32\*.c del obj\b32\*.c
   if exist make_vc.log del make_vc.log

:EXIT

