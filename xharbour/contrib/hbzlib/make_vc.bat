@echo off
rem
rem $Id: make_vc.bat,v 1.5 2003/09/23 04:52:34 paultucker Exp $
rem

if not exist obj md obj
if not exist obj\vc md obj\vc

:BUILD

   nmake -fmakefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy ..\..\lib\vc\hbzip.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:EXIT
