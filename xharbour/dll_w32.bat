@echo off
REM
REM $Id: dll_wc.bat,v 1.2 2002/05/25 18:28:07 lculik Exp $
REM
REM ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
REM ³ This is a batch file to create harbour.dll ³Û
REM ³ Please adjust envars accordingly           ³Û
REM ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
REM  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
IF NOT EXIST obj            MKDIR obj
IF NOT EXIST obj\dll        MKDIR obj\dll
IF NOT EXIST obj\dll\watcom MKDIR obj\dll\watcom
SET BISON_SIMPLE=c:\windows\bison.simple
SET _LIB=%LIB%
SET _PATH=%PATH%
SET _INCLUDE=%INCLUDE%
SET PATH=C:\watcom\BINNT;C:\watcom\BINW;%_PATH%
SET LIB=C:\Watcom\Lib386;C:\Watcom\Lib386\NT
SET WATCOM=C:\watcom
SET EDPATH=C:\watcom\EDDAT
SET INCLUDE=C:\watcom\H;C:\watcom\H\NT
wmake -ms -h LIB_DIR=lib\watcom BIN_DIR=bin\watcom -f hrbdll.wc %1 %2 %3
if exist hdll.tmp del hdll.tmp
SET PATH=%_PATH%
SET _PATH=
SET INCLUDE=%_INCLUDE%
SET _INCLUDE=
SET LIB=%_LIB%
SET _LIB=
