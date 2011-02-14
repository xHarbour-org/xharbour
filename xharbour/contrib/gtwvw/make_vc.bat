@echo off
rem 
rem $Id$
rem 

:BUILD

   cd ..\..
   make_vc contrib
   cd contrib\gtwvw
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\gtwvw.lib ..\..\lib >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT

