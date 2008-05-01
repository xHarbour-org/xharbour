@echo off
rem ============================================================================
rem
rem $Id: make_w32.bat,v 1.7 2008/04/29 22:14:09 andijahja Exp $
rem
rem FILE: cont_w32.bat
rem BATCH FILE FOR OPENWATCOM (CONTRIBS)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\WATCOM
SET SUB_DIR=wc

SET _LIB=%LIB%
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BINNT;%CC_DIR%\BINW;%BISON_DIR%;%_PATH%
SET WATCOM=%CC_DIR%
SET EDPATH=%CC_DIR%\EDDAT
SET LIB=%CC_DIR%\lib386;%CC_DIR%\lib386\nt;%_LIB%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET LIBEXT=.lib
SET OBJEXT=.obj
SET DIR_SEP=\
REM SET LIBPREFIX=
rem ============================================================================

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

    @CALL mdir.bat

:BUILD
   SET HB_MT=
   SET HB_MT_DIR=
   wmake -h -ms -f contrib.wc %1 %2 %3 >cont_w32.log
   if errorlevel 1 goto BUILD_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=\mt
   REM wmake -h -ms HB_THREAD_SUPPORT=1 -f contrib.wc %1 %2 %3 >>cont_w32.log
   REM if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copycontrib
   if exist *.err del *.err
   goto EXIT

:BUILD_ERR
   if exist cont_w32.log notepad cont_w32.log
   goto EXIT

:CLEAN
   @CALL mdir.bat cleancontrib
   if exist cont_w32.log del cont_w32.log

:EXIT
   SET CC_DIR=
   SET SUB_DIR=
   SET LIB=%_LIB%
   SET PATH=%_PATH%
   SET _LIB=
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   REM SET LIBPREFIX=
   SET HB_MT=
   SET HB_MT_DIR=
   SET WATCOM=
   SET EDPATH=
