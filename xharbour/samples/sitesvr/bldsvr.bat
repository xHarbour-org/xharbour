@echo off
rem
rem $Id: bldsvr.bat,v 1.2 2003/01/11 00:25:17 ronpinkas Exp $
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
SET HB_BIN_INSTALL=..\..\bin
set HB_INC_INSTALL=..\..\include
set HB_LIB_INSTALL=..\..\lib

SET HB_MT=mt

call bld.bat sitesvr blddbf %1 %2 %3 %4 %5
