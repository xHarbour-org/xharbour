@echo off
rem
rem $Id: make_vc.bat,v 1.1 2003/03/04 21:04:38 lculik Exp $
rem

if not exist obj md obj
if not exist obj\vc md obj\vc

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\libct.lib ..\..\lib >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

