@ECHO OFF

REM
REM $Id: vcdll.bat,v 1.1 2002/01/06 03:45:43 andijahja Exp $
REM
REM  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
REM  ³ This is a template for building application with MsVC   ³Û
REM  ³ and harbour.dll which is built with MSVC                ³Û
REM  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
REM   ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

if "%1" == "" GOTO :SINTAX
if NOT EXIST %1.prg GOTO :NOEXIST

rem set envar for import library
rem please adjust accordingly
SET _HARBOURLIB_=c:\xhrb\lib\vc\harbour.lib

rem set envar for Microsoft C++ Directory here
rem please adjust accordingly
SET __MSVCDIR__=C:\COMPILER\VC

rem set envar for HARBOUR Directory here
rem please adjust accordingly
SET __HARBOURDIR__=C:\XHRB

rem envar for Microsoft C++
SET _LIB=%LIB%
SET _PATH=%PATH%
SET _INCLUDE=%INCLUDE%
SET LIB=%__MSVCDIR__%\LIB;%__HARBOURDIR__%\LIB\VC;%PATH%
SET PATH=%__MSVCDIR__%\BIN;%__HARBOURDIR__%\BIN\VC;%PATH%
SET INCLUDE=%__HARBOURDIR__%\INCLUDE;%__MSVCDIR__%\INCLUDE;%_INCLUDE%

ECHO Compiling...

harbour %1 /n %2 %3

IF ERRORLEVEL 1 PAUSE
IF ERRORLEVEL 1 GOTO EXIT

rem using MT flag !!
cl /Ox /G6 /MT /TP /W3 /c /GA %1.c
cl /Ox /G6 /MT /TP /W3 /c /GA %__HARBOURDIR__%\source\vm\mainstd.c

echo %1.obj  > msvc.tmp
echo mainstd.obj  >> msvc.tmp
echo %_HARBOURLIB_% >> msvc.tmp
echo winspool.lib >> msvc.tmp
echo user32.lib >> msvc.tmp
link @msvc.tmp /nodefaultlib:libc.lib /out:%1.exe /nologo /subsystem:console

rem delete temporary files
@del %1.c
@del msvc.tmp

SET LIB=%_LIB%
SET PATH=%_PATH%
SET INCLUDE=%_INCLUDE%
SET _LIB=
SET _PATH=
SET _INCLUDE=
SET __HARBOURDIR__=
SET __MSVCDIR__=
SET _HARBOURLIB_=

IF ERRORLEVEL 1 GOTO LINKERROR
IF EXIST %1.exe dir %1.exe
echo.
ECHO %1.exe successfully built
echo.
GOTO EXIT
ECHO

:LINKERROR
GOTO EXIT

:SINTAX
ECHO Syntax: VCDLL [Program] -- Don't specify .PRG extension
GOTO EXIT

:NOEXIST
ECHO The specified PRG %1 does not exist

:EXIT
