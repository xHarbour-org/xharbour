@echo off
rem
rem $Id: make_vc.bat,v 1.2 2003/09/23 21:23:56 paultucker Exp $
rem

if not exist obj md obj
if not exist obj\vc md obj\vc

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\libnf.lib ..\..\lib >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

