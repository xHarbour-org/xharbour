@echo off
rem ============================================================================
rem
rem $Id: makefile.bc,v 1.218 2008/04/14 06:06:20 andijahja Exp $
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
SET LIBPREFIX=
rem ============================================================================

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   @CALL MDIR.BAT

:BUILD
   SET HB_MT=
   SET HB_MT_DIR=
   REM nmake USE_MSVCRT=1 /f makefile.vc %1 %2 %3
   nmake /NOLOGO /f makefile.vc %1 %2 %3 >make_vc.log
   if errorlevel 1 goto BUILD_ERR

   SET HB_MT=MT
   SET HB_MT_DIR=\mt
   REM nmake USE_MSVCRT=1 HB_THREAD_SUPPORT=1 /f makefile.vc %1 %2 %3
   nmake /NOLOGO HB_THREAD_SUPPORT=1 /f makefile.vc %1 %2 %3 >>make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy bin\%SUB_DIR%\*.exe bin > nul
   copy lib\%SUB_DIR%\*.lib lib > nul
   goto EXIT

:BUILD_ERR
   notepad make_vc.log
   goto EXIT

:CLEAN
   @CALL MDIR.BAT CLEAN
   IF EXIST make_vc.log DEL make_vc.log

:EXIT
   IF EXIST BIN\%SUB_DIR%\harbour.lib  DEL BIN\%SUB_DIR%\harbour.lib
   IF EXIST BIN\%SUB_DIR%\ppgen.lib    DEL BIN\%SUB_DIR%\ppgen.lib
   IF EXIST BIN\%SUB_DIR%\hbpp.lib     DEL BIN\%SUB_DIR%\hbpp.lib
   IF EXIST BIN\%SUB_DIR%\hbdoc.lib    DEL BIN\%SUB_DIR%\hbdoc.lib
   IF EXIST BIN\%SUB_DIR%\hbmake.lib   DEL BIN\%SUB_DIR%\hbmake.lib
   IF EXIST BIN\%SUB_DIR%\hbrun.lib    DEL BIN\%SUB_DIR%\hbrun.lib
   IF EXIST BIN\%SUB_DIR%\hbrunMT.lib  DEL BIN\%SUB_DIR%\hbrunMT.lib
   IF EXIST BIN\%SUB_DIR%\hbtest.lib   DEL BIN\%SUB_DIR%\hbtest.lib
   IF EXIST BIN\%SUB_DIR%\hbtestMT.lib DEL BIN\%SUB_DIR%\hbtestMT.lib
   IF EXIST BIN\%SUB_DIR%\xbscript.lib DEL BIN\%SUB_DIR%\xbscript.lib
   IF EXIST BIN\%SUB_DIR%\harbour.exp  DEL BIN\%SUB_DIR%\harbour.exp
   IF EXIST BIN\%SUB_DIR%\ppgen.exp    DEL BIN\%SUB_DIR%\ppgen.exp
   IF EXIST BIN\%SUB_DIR%\hbpp.exp     DEL BIN\%SUB_DIR%\hbpp.exp
   IF EXIST BIN\%SUB_DIR%\hbdoc.exp    DEL BIN\%SUB_DIR%\hbdoc.exp
   IF EXIST BIN\%SUB_DIR%\hbmake.exp   DEL BIN\%SUB_DIR%\hbmake.exp
   IF EXIST BIN\%SUB_DIR%\hbrun.exp    DEL BIN\%SUB_DIR%\hbrun.exp
   IF EXIST BIN\%SUB_DIR%\hbrunMT.exp  DEL BIN\%SUB_DIR%\hbrunMT.exp
   IF EXIST BIN\%SUB_DIR%\hbtest.exp   DEL BIN\%SUB_DIR%\hbtest.exp
   IF EXIST BIN\%SUB_DIR%\hbtestMT.exp DEL BIN\%SUB_DIR%\hbtestMT.exp
   IF EXIST BIN\%SUB_DIR%\xbscript.exp DEL BIN\%SUB_DIR%\xbscript.exp
   SET CC_DIR=
   SET BISON_DIR=
   SET SUB_DIR=
   SET HB_GT_LIB=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   SET LIBPREFIX=
   SET PATH=%_PATH%
   SET _PATH=
   SET HB_MT=
   SET HB_MT_DIR=
