@echo off
rem
rem $Id: bld.bat,v 1.5 2003/01/12 22:30:07 fsgiudice Exp $
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

set HB_ARCHITECTURE=w32
set HB_COMPILER=bcc32
set HB_INSTALL=..
SET HB_BIN_INSTALL=%HB_INSTALL%\bin
set HB_INC_INSTALL=%HB_INSTALL%\include
set HB_LIB_INSTALL=%HB_INSTALL%\lib

SET HB_MT=mt

call %HB_BIN_INSTALL%\bld.bat %1 %2 %3 %4 %5

if exist *.c   del *.c
if exist *.obj del *.obj
if exist *.tds del *.tds

