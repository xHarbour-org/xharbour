@echo off
rem 
rem $Id: make_vc.bat,v 1.6 2000/05/29 07:27:40 paultucker Exp $
rem 

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\libmisc.lib ..\..\lib\*.* >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

