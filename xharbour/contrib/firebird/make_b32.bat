:@echo off

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\firebird.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist ..\..\lib\b32\firebird.lib   del ..\..\lib\b32\firebird.lib
   if exist ..\..\lib\b32\firebird.bak   del ..\..\lib\b32\firebird.bak
   if exist ..\..\obj\b32\firebird.obj   del ..\..\obj\b32\firebird.obj
   if exist ..\..\obj\b32\TFirebird.c    del ..\..\obj\b32\TFirebird.c
   if exist ..\..\obj\b32\TFirebird.obj  del ..\..\obj\b32\TFirebird.obj
   goto EXIT

:EXIT

