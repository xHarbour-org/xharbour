@echo off
rem ============================================================================
rem
rem $Id: makefile.bc,v 1.218 2008/04/14 06:06:20 andijahja Exp $
rem
rem FILE: make_w32.bat
rem BATCH FILE FOR OPENWATCOM
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\WATCOM
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=wc
SET HB_GT_LIB=$(GTWIN_LIB)

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
SET LIBPREFIX=
rem ============================================================================

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

    @CALL MDIR.BAT

:BUILD
   SET HB_MT=
   SET HB_MT_DIR=
   wmake -h -ms -f makefile.wc %1 %2 %3 >make_w32.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_MT=mt
   SET HB_MT_DIR=\mt
   wmake -h -ms HB_THREAD_SUPPORT=1 -f makefile.wc %1 %2 %3 >>make_w32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy bin\w32\*.exe bin\*.* > nul
   copy lib\w32\*.lib lib\*.* > nul
   if exist lib\w32\*.bak del lib\w32\*.bak
   goto EXIT

:BUILD_ERR
   notepad make_w32.log
   goto EXIT

:CLEAN
   @CALL MDIR.BAT CLEAN
   if exist make_w32.log del make_w32.log

:EXIT
   SET CC_DIR=
   SET BISON_DIR=
   SET SUB_DIR=
   SET HB_GT_LIB=
   SET LIB=%_LIB%
   SET PATH=%_PATH%
   SET _LIB=
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   SET LIBPREFIX=
   SET HB_MT=
   SET HB_MT_DIR=
   SET WATCOM=
   SET EDPATH=
