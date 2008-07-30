@echo off
rem
rem $Id: make_b32.bat 8596 2008-06-04 02:03:48Z druzus $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist lib md lib
if not exist obj md obj
if not exist obj\b32 md obj\b32
if not exist obj\b32\mt md obj\b32\mt
:BUILD

make -l EXE_OBJ_DIR=obj\b32\bin OBJ_DIR=obj\b32 -fmakefile.bc %1 %2 %3 > make_b32.log
if errorlevel 1 goto BUILD_ERR
make -l OBJ_DIR=obj\b32\mt -DHB_THREAD_SUPPORT -DHB_MT=mt -fmakefile.bc %2 %3 >> make_b32.log
if errorlevel 1 goto BUILD_ERR


:BUILD_OK

   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   del lib\*.lib
   del lib\*.bak
   del obj\b32\*.obj
   del obj\b32\*.c
   del obj\b32\mt\*.obj
   del obj\b32\mt\*.c

   del make_b32.log

   goto EXIT

:EXIT
