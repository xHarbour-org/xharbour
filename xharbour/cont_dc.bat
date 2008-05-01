@echo off
rem ============================================================================
rem
rem $Id: make_dc.bat,v 1.7 2008/04/29 22:14:09 andijahja Exp $
rem
rem FILE: cont_dc.bat
rem BATCH FILE FOR DIGITALMARS (CONTRIBS)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\DM
SET SUB_DIR=dc

SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%_PATH%

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
   %MAKE_EXE% -fcontrib.dc %1 %2 %3 >cont_dc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:BUILD_ERR
   if exist cont_dc.log notepad cont_dc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat cleancontrib
   IF EXIST cont_dc.log DEL cont_dc.log

:EXIT
   SET CC_DIR=
   SET SUB_DIR=
   SET PATH=%_PATH%
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET DIR_SEP=
   SET MAKE_EXE=
