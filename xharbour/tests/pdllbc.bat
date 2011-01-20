@echo off
rem
rem $Id: pdll55.bat,v 1.1 2008/01/03 05:43:01 andijahja Exp $
rem
rem ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
rem ³ * this is a batch file for creating pCode DLL ³Û
REM ³ * ENVIRONMENT SHOULD BE ADJUSTED ACCORDINGLY  ³Û
rem ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
rem  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
SET HARBOURDIR=..
SET HARBOURLIB=%HARBOURDIR%\lib\55
SET HARBOUREXE=%HARBOURDIR%\bin\55\harbour.exe
SET _PATH=%PATH%
SET CC_DIR=..\..\BORLAND\BCC55
SET PATH=..\..\BORLAND\BCC55\BIN;%PATH%
SET DEFFILE=%HARBOURDIR%\source\vm\BCC.DEF
SET _MYLIB=codepage.lib common.lib dbffpt.lib dbfcdx.lib dbfntx.lib hbsix.lib debug.lib gtwin.lib macro.lib pp.lib rdd.lib rtl.lib vm.lib lang.lib pcrepos.lib ct.lib zlib.lib cw32MT.lib import32.lib

%HARBOUREXE% pcode.prg /n1 /i%HARBOURDIR%\include /gc2 /es2 /w /vd
%HARBOUREXE% pcode1.prg /n1 /i%HARBOURDIR%\include /gc2 /es2 /w /vd
%HARBOUREXE% pexec.prg /n0 /i%HARBOURDIR%\include /gc2 /es2 /w
BCC32 -D__EXPORT__ -I%HARBOURDIR%\include -6 -tWM -OS -O2 -c -opcode.obj pcode.c
BCC32 -D__EXPORT__ -I%HARBOURDIR%\include -6 -tWM -OS -O2 -c -opcode1.obj pcode1.c
BCC32 -I%HARBOURDIR%\include -6 -tWM -OS -O2 -c -opexec.obj pexec.c
BCC32 -I%HARBOURDIR%\include -6 -tWM -OS -O2 -c -omaindllp.obj %HARBOURDIR%\source\vm\maindllp.c
if errorlevel 1 goto end
ILINK32 -aa -Tpd -x -Gn -L%CC_DIR%\LIB -L%HARBOURLIB% pcode.obj maindllp.obj c0d32.obj,pcode.dll,,import32.lib uuid.lib cw32.lib
ILINK32 -aa -Tpd -x -Gn -L%CC_DIR%\LIB -L%HARBOURLIB% pcode1.obj maindllp.obj c0d32.obj,pcode1.dll,,import32.lib uuid.lib cw32.lib
ILINK32 -ap -Tpe -x -Gn -L%CC_DIR%\LIB -L%HARBOURLIB% pexec.obj c0x32.obj,pexec.exe,,%_MYLIB%,%DEFFILE%

:END
SET PATH=%_PATH%
SET _PATH=
SET HARBOUREXE=
SET HARBOURLIB=
SET HARBOURDIR=
SET DEFFILE=
SET _MYLIB=
SET CC_DIR=
