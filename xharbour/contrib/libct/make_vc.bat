@echo off
rem
rem $Id: make_vc.bat,v 1.4 2001/11/12 23:24:38 lculik Exp $
rem

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\libct.lib ..\..\lib\*.* >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

