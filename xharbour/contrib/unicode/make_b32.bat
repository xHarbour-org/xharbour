@echo off
rem
rem $Id: make_b32.bat,v 1.1 2004/01/14 06:18:05 andijahja Exp $
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\hbcc.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\hbcc.lib      del ..\..\lib\b32\hbcc.lib
   if exist ..\..\lib\b32\hbcc.bak      del ..\..\lib\b32\hbcc.bak
   if exist ..\..\obj\b32\hbcc.c        del ..\..\obj\b32\hbcc.c
   if exist ..\..\obj\b32\hbcc.obj      del ..\..\obj\b32\hbcc.obj
   if exist ..\..\obj\b32\hbc7.obj      del ..\..\obj\b32\hbc7.obj
   if exist ..\..\obj\b32\hbcu.obj      del ..\..\obj\b32\hbcu.obj
   if exist ..\..\obj\b32\hbcx.obj      del ..\..\obj\b32\hbcx.obj
   if exist ..\..\obj\b32\hbcy.obj      del ..\..\obj\b32\hbcy.obj
   if exist ..\..\obj\b32\hbhex.obj     del ..\..\obj\b32\hbhex.obj
   if exist ..\..\obj\b32\hbcrc16.obj   del ..\..\obj\b32\hbcrc16.obj
   if exist ..\..\obj\b32\hbcrc32.obj   del ..\..\obj\b32\hbcrc32.obj
   if exist ..\..\obj\b32\hbcrpt32.obj  del ..\..\obj\b32\hbcrpt32.obj
   if exist ..\..\obj\b32\hbcrpt128.obj del ..\..\obj\b32\hbcrpt128.obj

   goto EXIT

:EXIT












