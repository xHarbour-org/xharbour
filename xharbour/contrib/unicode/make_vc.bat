@echo off
rem
rem $Id: make_vc.bat,v 1.1 2002/07/11 02:57:52 lculik Exp $
rem

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\hbcc.lib ..\..\lib\*.* >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

