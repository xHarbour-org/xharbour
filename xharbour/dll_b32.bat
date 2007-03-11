@echo off
REM
REM $Id: dll_b32.bat,v 1.13 2007/02/28 22:58:51 modalsist Exp $
REM
REM ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
REM ³ This is a batch file to create harbour.dll ³Û
REM ³ Please adjust envars accordingly           ³Û
REM ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
REM  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if "%1" == "/clean" goto CLEAN
if "%1" == "/CLEAN" goto CLEAN

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
if exist bin\b32\hbdocdll.exe copy bin\b32\hbdocdll.exe bin > nul
if exist bin\b32\hbmakedll.exe copy bin\b32\hbmakedll.exe bin > nul
if exist bin\b32\hbrundll.exe copy bin\b32\hbrundll.exe bin > nul
if exist bin\b32\hbtestdll.exe copy bin\b32\hbtestdll.exe bin > nul
if exist bin\b32\xbscriptdll.exe copy bin\b32\xbscriptdll.exe bin > nul

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
  if exist bin\harbour.dll     del bin\harbour.dll
  if exist bin\harbour.lib     del bin\harbour.lib

  if exist bin\b32\hbdocdll.exe    del bin\b32\hbdocdll.exe
  if exist bin\b32\hbmakedll.exe   del bin\b32\hbmakedll.exe
  if exist bin\b32\hbrundll.exe    del bin\b32\hbrundll.exe
  if exist bin\b32\hbtestdll.exe   del bin\b32\hbtestdll.exe
  if exist bin\b32\xbscriptdll.exe del bin\b32\xbscriptdll.exe

  if exist bin\hbdocdll.exe    del bin\hbdocdll.exe
  if exist bin\hbmakedll.exe   del bin\hbmakedll.exe
  if exist bin\hbrundll.exe    del bin\hbrundll.exe
  if exist bin\hbtestdll.exe   del bin\hbtestdll.exe
  if exist bin\xbscriptdll.exe del bin\xbscriptdll.exe

  if exist lib\harbour.lib     del lib\harbour.lib
  if exist lib\harbour.dll     del lib\harbour.dll
  if exist lib\b32\harbour.lib del lib\b32\harbour.lib
  if exist lib\b32\harbour.dll del lib\b32\harbour.dll

  if exist tests\harbour.dll  del tests\harbour.dll

  if exist obj\dll\b32\*.obj del obj\dll\b32\*.obj
  if exist obj\dll\b32\*.c   del obj\dll\b32\*.c
  if exist obj\dll\b32\*.h   del obj\dll\b32\*.h


:EXIT
