@echo off
rem
rem $Id: make_vc.bat,v 1.1 2004/01/14 06:18:05 andijahja Exp $
rem

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\design.lib ..\..\lib\*.* >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

