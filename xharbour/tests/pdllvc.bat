@echo off
rem
rem $Id$
rem
rem ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
rem ³ * this is a batch file for creating pCode DLL ³Û
REM ³ * ENVIRONMENT SHOULD BE ADJUSTED ACCORDINGLY  ³Û
rem ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
rem  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
SET SUB_DIR=VC6
SET CC_DIR=..\..\%SUB_DIR%
SET _MYLIB=codepage.lib rddads.lib zlib.lib common.lib dbffpt.lib dbfcdx.lib dbfntx.lib hbsix.lib debug.lib gtwin.lib macro.lib pp.lib rdd.lib rtl.lib vm.lib lang.lib pcrepos.lib ct.lib
SET HARBOURDIR=..
SET HARBOURLIB=%HARBOURDIR%\lib\%SUB_DIR%
SET HARBOUREXE=%HARBOURDIR%\bin\%SUB_DIR%\harbour.exe
SET ____CFL=/c /TP /I%CC_DIR%\INCLUDE /I%HARBOURDIR%\INCLUDE /Ot2yb1 /Gs /GA /W3 /nologo
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%PATH%
SET DEFFILE=%HARBOURDIR%\source\vm\export.def

%HARBOUREXE% pexec.prg  /n0 /i%HARBOURDIR%\include /gc2 /es2 /w
%HARBOUREXE% pcode.prg  /n1 /i%HARBOURDIR%\include /gc2 /es2 /w
%HARBOUREXE% pcode1.prg /n1 /i%HARBOURDIR%\include /gc2 /es2 /w
CL %____CFL% pexec.c
CL -D__EXPORT__ %____CFL% pcode.c
CL -D__EXPORT__ %____CFL% pcode1.c
CL %____CFL% %HARBOURDIR%\source\vm\maindllp.c
IF ERRORLEVEL 1 GOTO end
LINK /DEF:%DEFFILE% /NOLOGO /LIBPATH:%HARBOURLIB% /LIBPATH:%CC_DIR%\LIB /OUT:pexec.exe /FORCE:MULTIPLE /INCLUDE:__matherr /SUBSYSTEM:CONSOLE pexec.obj %_MYLIB%
LINK /DLL /NOLOGO /LIBPATH:%HARBOURLIB% /LIBPATH:%CC_DIR%\LIB /OUT:pcode.dll  /FORCE:MULTIPLE /INCLUDE:__matherr /SUBSYSTEM:WINDOWS pcode.obj  maindllp.obj
LINK /DLL /NOLOGO /LIBPATH:%HARBOURLIB% /LIBPATH:%CC_DIR%\LIB /OUT:pcode1.dll /FORCE:MULTIPLE /INCLUDE:__matherr /SUBSYSTEM:WINDOWS pcode1.obj maindllp.obj

:end
SET PATH=%_PATH%
SET _PATH=
SET HARBOUREXE=
SET HARBOURLIB=
SET HARBOURDIR=
SET _MYLIB=
SET CC_DIR=
SET ____CFL=
SET DEFFILE=
