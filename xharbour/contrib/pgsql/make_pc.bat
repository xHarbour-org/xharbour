@echo off

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

rem Change paths below in accordance with your needs.


:BUILD

   if not exist obj md obj
   if not exist obj\pc md obj\pc
   if not exist %HB_DIR%\lib\pc md %HB_DIR%\lib\pc

   if %HB_DIR%.==. SET HB_DIR=.\..\..
   if %CC_DIR%.==. SET CC_DIR=C:\PellesC
   if %PG_DIR%.==. SET PG_DIR=C:\postgresql-8.3.1

   pomake /f makefile.pc /p > make_pc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy %HB_DIR%\lib\pc\libhbpg.lib %HB_DIR%\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   if exist %HB_DIR%\lib\libhbpg.lib      del %HB_DIR%\lib\libhbpg.lib
   if exist %HB_DIR%\lib\pc\libhbpg.lib   del %HB_DIR%\lib\pc\libhbpg.lib
   if exist obj\pc\*.c                    del obj\pc\*.c
   if exist obj\pc\*.obj                  del obj\pc\*.obj
   goto EXIT2

:EXIT

SET HB_DIR=
SET CC_DIR=
SET PG_DIR=

:EXIT2

