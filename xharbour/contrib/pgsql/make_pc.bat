@echo off

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

rem Change paths below in accordance with your need.

if %HB_DIR%.==. set HB_DIR=C:\xHarbCVS
if %CC_DIR%.==. set CC_DIR=C:\PellesC
if %PG_DIR%.==. set PG_DIR=C:\PostgreSQL-8.2.5

:BUILD

   pomake /f makefile.pc %1 %2 %3 > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\pocc\libhbpg.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_pc.log
   goto EXIT

:CLEAN

   if exist ..\..\lib\libhbpg.lib         del ..\..\lib\libhbpg.lib
   if exist ..\..\lib\pocc\libhbpg.lib    del ..\..\lib\pocc\libhbpg.lib
   if exist ..\..\obj\pocc\postgres.obj   del ..\..\obj\pocc\postgres.obj
   if exist ..\..\obj\pocc\TPostgres.c    del ..\..\obj\pocc\TPostgres.c
   if exist ..\..\obj\pocc\TPostgres.obj  del ..\..\obj\pocc\TPostgres.obj
   goto EXIT

:EXIT

set PG_DIR=

