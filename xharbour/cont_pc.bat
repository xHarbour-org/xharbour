@echo off
rem ============================================================================
rem
rem $Id: make_pc.bat,v 1.25 2008/04/29 22:14:09 andijahja Exp $
rem
rem FILE: cont_pc.bat
rem BATCH FILE FOR PELLESC (CONTRIBS)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================
SET CC_DIR=C:\PELLESC
SET SUB_DIR=pc

SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%BISON_DIR%;%PATH%

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

   @CALL MDIR.BAT

:BUILD
rem ============================================================================
rem The curent POMAKE is buggy that it does not recognize macro defined, making
rem ifdef clause not working. The following envars help overcome the bug and
rem in order to build ST and MT version in one run. They will be reverted once
rem the bugs are fixed (AJ:2008-04-26)
rem ============================================================================
    REM SET HB_MT=
    REM SET HB_MT_DIR=
    REM SET __MT__=
    REM SET HB_MT_FLAGS=
    POMAKE /F contrib.pc %1 %2 %3 >cont_pc.log
    if errorlevel 1 goto BUILD_ERR

    REM SET HB_MT=mt
    REM SET HB_MT_DIR=\mt
    REM SET __MT__=-MT -DHB_THREAD_SUPPORT
    REM SET HB_MT_FLAGS=-dHB_THREAD_SUPPORT
    REM POMAKE /F makefile.pc %1 %2 %3 >>cont_pc.log
    REM if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:BUILD_ERR
   if exist cont_pc.log notepad cont_pc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat cleancontrib
   IF EXIST cont_pc.log DEL cont_pc.log

:EXIT
   SET CC_DIR=
   SET SUB_DIR=
   SET PATH=%_PATH%
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET HB_MT=
   SET DIR_SEP=
   SET HB_MT_DIR=
   SET __MT__=
   SET HB_MT_FLAGS=
