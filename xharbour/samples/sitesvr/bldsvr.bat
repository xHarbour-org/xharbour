@echo off
rem
rem $Id: bld_b32.bat,v 1.1 2001/12/22 06:36:17 ronpinkas Exp $
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
SET HB_MT=mt

call bld.bat sitesrv blddbf %1 %2 %3 %4 %5
