@echo off
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   SET CFLAGS=-D__EXPORT__ -DHB_API_MACROS -DHB_STACK_MACROS -DHB_FM_STATISTICS_OFF
   nmake /I /Fmakefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN
   del lib\*.lib
   del lib\*.bak
   del obj\*.obj
   del obj\*.c
   del make_vc.log

   goto EXIT

:EXIT

