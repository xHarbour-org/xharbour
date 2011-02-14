@echo off
rem
rem $Id$
rem
rem ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
rem ³ * this is a batch file for creating pCode DLL ³Û
rem ³ * environment should be adjusted accordingly  ³Û
rem ³ * USAGE: PDLL55 <progam>                      ³Û
rem ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
rem  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

SET CLPCOMMAND=%CLIPPERCMD%
SET CLIPPERCMD=
SET HARBOURDIR=C:\DEV
SET HARBOURLIB=%HARBOURDIR%\lib\55
SET HARBOUREXE=%HARBOURDIR%\bin\55\harbour.exe
SET _PATH=%PATH%
SET PATH=C:\BORLAND\BCC55\BIN;%PATH%

if "%1" == "" goto syntax
if not exist %1.prg goto noexist
if not exist %HARBOUREXE% goto noharbour
if exist %1.dll del %1.dll
if exist %1.c del %1.c

%HARBOUREXE% %1 /d__EXPORT__/n /i%HARBOURDIR%\include /gc2 /es2 /w /vd >NUL
if not exist %1.c goto end
@BCC32 -D__EXPORT__ -I%HARBOURDIR%\include -5 -6 -a8 -d -OS -O2 -c -o%1.obj %1.c >NUL
@BCC32 -I%HARBOURDIR%\include -5 -6 -a8 -d -OS -O2 -c -omaildllp.obj ..\source\vm\maindllp.c >NUL
if not exist %1.obj goto end
@ILINK32 -ap -Tpd -x -Gn -L%HARBOURLIB% %1.obj maildllp.obj c0d32.obj,%1.dll,,import32.lib uuid.lib cw32.lib >NUL

if exist %1.dll @IMPDEF %1.def %1.dll >NUL
if exist %1.dll @IMPLIB %1.lib %1.dll >NUL
if exist %1.dll echo.
if exist %1.dll echo.%1.dll succesfully built
if exist %1.def echo.%1.def (function list) generated
if exist %1.lib echo.%1.lib (import lib) generated
if exist %1.dll echo.
if exist %1.dll dir %1.dll
goto end

:syntax
echo.
echo Syntax: PDLL55 program [ do not specify prg extension ]
echo.
goto end

:noexist
echo.
echo Cannot find %1.prg file
echo.
goto end

:noharbour
echo.
echo Cannot find Harbour.exe
echo.
goto end

:end
set PATH=%_PATH%
set _PATH=
set HARBOUREXE=
set HARBOURLIB=
set HARBOURDIR=
set CLIPPERCMD=%CLPCOMMAND%
set CLPCOMMAND=
