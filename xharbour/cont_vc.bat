@echo off
rem ============================================================================
rem
rem $Id: make_vc.bat,v 1.20 2008/04/29 22:14:09 andijahja Exp $
rem
rem FILE: cont_vc.bat
rem BATCH FILE FOR MSVC (CONTRIBS)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\VC
SET SUB_DIR=vc

SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%BISON_DIR%;%PATH%

rem ============================================================================
rem The followings should never change
rem Do not hard-code in makefile because there are needed for clean build
rem ============================================================================
SET LIBEXT=.lib
SET OBJEXT=.obj
SET DIR_SEP=\
rem ============================================================================

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   @CALL MDIR.BAT

:BUILD
   SET HB_MT=
   SET HB_MT_DIR=
   nmake /NOLOGO /f contrib.vc %1 %2 %3 >cont_vc.log
   if errorlevel 1 goto BUILD_ERR

   REM SET HB_MT=mt
   REM SET HB_MT_DIR=\mt
   REM nmake /NOLOGO HB_THREAD_SUPPORT=1 /f makefile.vc %1 %2 %3 >>cont_vc.log
   REM if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:BUILD_ERR
   IF EXIST cont_vc.log notepad cont_vc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat cleancontrib
   IF EXIST cont_vc.log DEL cont_vc.log
   IF EXIST vc*.idb del vc*.idb

:EXIT
   SET CC_DIR=
   SET SUB_DIR=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   SET PATH=%_PATH%
   SET _PATH=
   SET HB_MT=
   SET HB_MT_DIR=
