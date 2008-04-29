@echo off
rem ============================================================================
rem
rem $Id: make_vc.bat,v 1.19 2008/04/29 12:48:45 ronpinkas Exp $
rem
rem FILE: nake_vc.bat
rem BATCH FILE FOR MSVC
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\VC
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=vc
SET HB_GT_LIB=$(GTWIN_LIB)

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
   SET HB_MT=
   SET HB_MT_DIR=
   nmake /NOLOGO /f makefile.vc %1 %2 %3 >make_vc.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_MT=mt
   SET HB_MT_DIR=\mt
   nmake /NOLOGO HB_THREAD_SUPPORT=1 /f makefile.vc %1 %2 %3 >>make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copytobin
   goto EXIT

:BUILD_ERR
   IF EXIST make_vc.log notepad make_vc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat CLEAN
   IF EXIST make_vc.log DEL make_vc.log
   IF EXIST vc*.idb del vc*.idb

:EXIT
   REM IF EXIST BIN\%SUB_DIR%\harbour.lib  DEL BIN\%SUB_DIR%\harbour.lib
   REM IF EXIST BIN\%SUB_DIR%\ppgen.lib    DEL BIN\%SUB_DIR%\ppgen.lib
   REM IF EXIST BIN\%SUB_DIR%\hbpp.lib     DEL BIN\%SUB_DIR%\hbpp.lib
   REM IF EXIST BIN\%SUB_DIR%\hbdoc.lib    DEL BIN\%SUB_DIR%\hbdoc.lib
   REM IF EXIST BIN\%SUB_DIR%\hbmake.lib   DEL BIN\%SUB_DIR%\hbmake.lib
   REM IF EXIST BIN\%SUB_DIR%\hbrun.lib    DEL BIN\%SUB_DIR%\hbrun.lib
   REM IF EXIST BIN\%SUB_DIR%\hbrunMT.lib  DEL BIN\%SUB_DIR%\hbrunMT.lib
   REM IF EXIST BIN\%SUB_DIR%\hbtest.lib   DEL BIN\%SUB_DIR%\hbtest.lib
   REM IF EXIST BIN\%SUB_DIR%\hbtestMT.lib DEL BIN\%SUB_DIR%\hbtestMT.lib
   REM IF EXIST BIN\%SUB_DIR%\xbscript.lib DEL BIN\%SUB_DIR%\xbscript.lib
   REM IF EXIST BIN\%SUB_DIR%\harbour.exp  DEL BIN\%SUB_DIR%\harbour.exp
   REM IF EXIST BIN\%SUB_DIR%\ppgen.exp    DEL BIN\%SUB_DIR%\ppgen.exp
   REM IF EXIST BIN\%SUB_DIR%\hbpp.exp     DEL BIN\%SUB_DIR%\hbpp.exp
   REM IF EXIST BIN\%SUB_DIR%\hbdoc.exp    DEL BIN\%SUB_DIR%\hbdoc.exp
   REM IF EXIST BIN\%SUB_DIR%\hbmake.exp   DEL BIN\%SUB_DIR%\hbmake.exp
   REM IF EXIST BIN\%SUB_DIR%\hbrun.exp    DEL BIN\%SUB_DIR%\hbrun.exp
   REM IF EXIST BIN\%SUB_DIR%\hbrunMT.exp  DEL BIN\%SUB_DIR%\hbrunMT.exp
   REM IF EXIST BIN\%SUB_DIR%\hbtest.exp   DEL BIN\%SUB_DIR%\hbtest.exp
   REM IF EXIST BIN\%SUB_DIR%\hbtestMT.exp DEL BIN\%SUB_DIR%\hbtestMT.exp
   REM IF EXIST BIN\%SUB_DIR%\xbscript.exp DEL BIN\%SUB_DIR%\xbscript.exp
   SET CC_DIR=
   SET BISON_DIR=
   SET SUB_DIR=
   SET HB_GT_LIB=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   REM SET LIBPREFIX=
   SET PATH=%_PATH%
   SET _PATH=
   SET HB_MT=
   SET HB_MT_DIR=
