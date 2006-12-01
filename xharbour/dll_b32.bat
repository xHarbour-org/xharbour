@echo off
REM
REM $Id: dll_b32.bat,v 1.11 2006/07/27 16:47:37 map Exp $
REM
REM ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
REM ³ This is a batch file to create harbour.dll ³Û
REM ³ Please adjust envars accordingly           ³Û
REM ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
REM  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist obj md obj
if not exist obj\dll md obj\dll
if not exist obj\dll\b32 md obj\dll\b32
if not exist lib\b32 md lib\b32

:BUILD

   make -fhrbdll.bc %1 %2 %3 > dll_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

if exist hdll.tmp del hdll.tmp
if exist bin\b32\harbour.lib copy bin\b32\harbour.lib lib > nul
if exist bin\b32\harbour.dll copy bin\b32\harbour.dll bin > nul
REM if exist bin\b32\harbour.dll copy bin\b32\harbour.dll tests > nul
REM if exist bin\b32\harbour.dll copy bin\b32\harbour.lib lib\b32 > nul

goto EXIT

:BUILD_ERR

notepad dll_b32.log
goto EXIT

:CLEAN
  if exist dll_b32.log del dll_b32.log

  if exist bin\b32\harbour.lib del bin\b32\harbour.lib
  if exist bin\b32\harbour.dll del bin\b32\harbour.dll
  if exist bin\harbour.dll del bin\harbour.dll
  if exist bin\harbour.lib del bin\harbour.lib

  if exist lib\harbour.lib del lib\harbour.lib
  if exist lib\harbour.dll del lib\harbour.dll
  if exist lib\b32\harbour.lib del lib\b32\harbour.lib
  if exist lib\b32\harbour.dll del lib\b32\harbour.dll

  if exist tests\harbour.dll del tests\harbour.dll

  if exist obj\dll\b32\*.obj del obj\dll\b32\*.obj
  if exist obj\dll\b32\*.c   del obj\dll\b32\*.c
  if exist obj\dll\b32\*.h   del obj\dll\b32\*.h


:EXIT
