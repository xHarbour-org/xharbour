@echo off

REM
REM $Id: dll_vc.bat,v 1.1 2002/01/06 03:45:43 andijahja Exp $
REM
REM ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
REM ³ This is a batch file to create harbour.dll ³Û
REM ³ Please adjust envars accordingly           ³Û
REM ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
REM  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

REM Please adjust environment accordingly

SET BISON_SIMPLE=c:\windows\bison.simple
SET _LIB=%LIB%
SET _PATH=%PATH%
SET _INCLUDE=%INCLUDE%
SET LIB=C:\COMPILER\VC\LIB;%PATH%
SET PATH=C:\COMPILER\VC\BIN;BIN\VC;%PATH%
SET INCLUDE=INCLUDE;C:\COMPILER\VC\INCLUDE;%_INCLUDE%

nmake /NOLOGO /f hrbdll.vc %1 %2 %3

SET LIB=%_LIB%
SET PATH=%_PATH%
SET INCLUDE=%_INCLUDE%
SET _LIB=
SET _PATH=
SET _INCLUDE=
