@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\design.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\design.lib    del ..\..\lib\b32\design.lib
   if exist ..\..\lib\b32\design.bak    del ..\..\lib\b32\design.bak
   if exist ..\..\obj\b32\general.c     del ..\..\obj\b32\general.c
   if exist ..\..\obj\b32\general.obj   del ..\..\obj\b32\general.obj
   if exist ..\..\obj\b32\tbox.c        del ..\..\obj\b32\tbox.c
   if exist ..\..\obj\b32\tbox.obj      del ..\..\obj\b32\tbox.obj
   if exist ..\..\obj\b32\tcheck.c      del ..\..\obj\b32\tcheck.c
   if exist ..\..\obj\b32\tcheck.obj    del ..\..\obj\b32\tcheck.obj
   if exist ..\..\obj\b32\tmenu.c       del ..\..\obj\b32\tmenu.c
   if exist ..\..\obj\b32\tmenu.obj     del ..\..\obj\b32\tmenu.obj
   if exist ..\..\obj\b32\tradio.c      del ..\..\obj\b32\tradio.c
   if exist ..\..\obj\b32\tradio.obj    del ..\..\obj\b32\tradio.obj
   if exist ..\..\obj\b32\twindow.c     del ..\..\obj\b32\twindow.c
   if exist ..\..\obj\b32\twindow.obj   del ..\..\obj\b32\twindow.obj

   goto EXIT

:EXIT
