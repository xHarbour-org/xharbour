@echo off
rem ============================================================================
rem
rem $Id: make_dc.bat,v 1.6 2008/04/28 07:13:39 andijahja Exp $
rem
rem FILE: make_dc.bat
rem BATCH FILE FOR DIGITALMARS
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\DM
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=dc
SET HB_GT_LIB=$(GTWIN_LIB)

SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%BISON_DIR%;%_PATH%

rem ============================================================================
rem The bundled (s)make.exe of DigitalMars is found to be unable to properly
rem process the make script. For a solution, the free Borland make.exe is used.
rem If you have found a way to make DMC's (s)make.exe works, please modify this
rem file accordingly.(AJ:2008-04-26)
rem ============================================================================
SET MAKE_EXE=C:\BORLAND\BCC551\BIN\MAKE.EXE

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
rem It is not known if DMC support MT, so no MT-build is not introduced here.
rem If you have found how to build MT mode with DMC please modify this file
rem and the corresponding makefile.dc accordingly.
rem ============================================================================
   SET HB_MT=
   %MAKE_EXE% -fmakefile.dc >make_dc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copytobin
   if exist harbour.map  del harbour.map
   if exist hbdoc.map    del hbdoc.map
   if exist hbmake.map   del hbmake.map
   if exist hbpp.map     del hbpp.map
   if exist hbrun.map    del hbrun.map
   if exist hbtest.map   del hbtest.map
   if exist ppgen.map    del ppgen.map
   if exist xbscript.map del xbscript.map
   goto EXIT

:BUILD_ERR
   if exist make_dc.log notepad make_dc.log
   goto EXIT

:CLEAN
   @CALL MDIR.BAT CLEAN
   IF EXIST make_dc.log DEL make_dc.log

:EXIT
   SET CC_DIR=
   SET BISON_DIR=
   SET SUB_DIR=
   SET HB_GT_LIB=
   SET PATH=%_PATH%
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   REM SET LIBPREFIX=
   SET MAKE_EXE=
