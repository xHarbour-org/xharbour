@echo off
rem
rem $Id: bldsvr.bat,v 1.4 2003/01/11 02:18:09 fsgiudice Exp $
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

call %HB_BIN_INSTALL%\bld.bat sitesvr blddbf %1 %2 %3 %4 %5

if exist sitesvr.c   del sitesvr.c
if exist sitesvr.obj del sitesvr.obj
if exist sitesvr.tds del sitesvr.tds
if exist blddbf.c    del blddbf.c
if exist blddbf.obj  del blddbf.obj

