@echo off
REM
REM $Id: dll_w32.bat,v 1.3 2004/11/30 20:19:53 ptsarenko Exp $
REM
REM ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
REM ³ This is a batch file to create harbour.dll ³Û
REM ³ Please adjust envars accordingly           ³Û
REM ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ
REM  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
IF NOT EXIST obj            MKDIR obj
IF NOT EXIST obj\dll        MKDIR obj\dll
IF NOT EXIST obj\dll\watcom MKDIR obj\dll\watcom
rem SET BISON_SIMPLE=F:\windows\bison.simple
SET _LIB=%LIB%
SET _PATH=%PATH%
SET _INCLUDE=%INCLUDE%
SET PATH=F:\watcom\BINNT;F:\watcom\BINW;%_PATH%
SET LIB=F:\Watcom\Lib386;F:\Watcom\Lib386\NT
SET WATCOM=F:\watcom
SET EDPATH=F:\watcom\EDDAT
SET INCLUDE=F:\watcom\H;F:\watcom\H\NT;include
wmake -ms -h LIB_DIR=lib\w32 BIN_DIR=bin\w32 -f hrbdll.wc %1 %2 %3 > dll_w32.log
if exist hdll.tmp del hdll.tmp
SET PATH=%_PATH%
SET _PATH=
SET INCLUDE=%_INCLUDE%
SET _INCLUDE=
SET LIB=%_LIB%
SET _LIB=
