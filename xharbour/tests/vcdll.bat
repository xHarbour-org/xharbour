@ECHO OFF

REM
REM $Id: vcdll.bat,v 1.1 2002/01/07 04:08:11 andijahja Exp $
REM
REM  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
REM  ³ This is a template for building application with MsVC   ³Û
REM  ³ and harbour.dll which is built with MSVC                ³Û
REM  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
REM   ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

if "%1" == "" GOTO :SINTAX
if NOT EXIST %1.prg GOTO :NOEXIST

ECHO Compiling...

harbour %1 -n -I..\include %2 %3

IF ERRORLEVEL 1 PAUSE
IF ERRORLEVEL 1 GOTO EXIT

rem using MT flag !!
cl -I..\include /Ox /G6 /MT /TP /W3 /c /GA %1.c
cl -I..\include /Ox /G6 /MT /TP /W3 /c /GA ..\source\vm\mainstd.c

echo %1.obj  > msvc.tmp
echo mainstd.obj  >> msvc.tmp
echo ..\lib\vc\harbour.lib >> msvc.tmp
echo winspool.lib >> msvc.tmp
echo user32.lib >> msvc.tmp
link @msvc.tmp /nodefaultlib:libc.lib /out:%1.exe /nologo /subsystem:console

rem delete temporary files
@del %1.c
@del msvc.tmp

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
