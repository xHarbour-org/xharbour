@echo off
rem
rem $Id: make_b32.bat,v 1.7 2003/02/19 16:50:50 iananderson Exp $
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs 
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to 
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

if not exist obj md obj
if not exist obj\b32 md obj\b32
if not exist obj\b32\mt md obj\b32\mt

if not exist lib md lib
if not exist lib\b32 md lib\b32

if not exist bin md bin
if not exist bin\b32 md bin\b32

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -l -fmakefile.bc %1 %2 %3 > make_b32.log
   make -l -DHB_THREAD_SUPPORT -fmakefile.bc %2 %3 > make_b32.log

   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\b32\*.exe bin\*.* > nul
   copy lib\b32\*.lib lib\*.* > nul
   copy obj\b32\hvm*.obj lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist bin\b32\*.exe del bin\b32\*.exe
   if exist bin\b32\*.tds del bin\b32\*.tds
   if exist bin\b32\*.map del bin\b32\*.map

   if exist lib\b32\*.lib del lib\b32\*.lib
   if exist lib\b32\*.bak del lib\b32\*.bak

   if exist obj\b32\*.obj del obj\b32\*.obj
   if exist obj\b32\*.c   del obj\b32\*.c
   if exist obj\b32\*.h   del obj\b32\*.h

   if exist obj\b32\mt\*.obj del obj\b32\mt\*.obj
   if exist obj\b32\mt\*.c   del obj\b32\mt\*.c

   if exist make_b32.log  del make_b32.log
   goto EXIT

:EXIT

