@echo off
rem 
rem $Id: make_vc.bat,v 1.1 2003/04/14 16:09:13 lculik Exp $
rem 

if not exist obj md obj
if not exist obj\vc md obj\vc

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\libmisc.lib ..\..\lib\*.* >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

