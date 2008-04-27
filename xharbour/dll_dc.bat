@echo off
rem ============================================================================
rem
rem $Id: makefile.bc,v 1.218 2008/04/14 06:06:20 andijahja Exp $
rem
rem FILE: dll_dc.bat
rem BATCH FILE FOR DIGITALMARS (DLL)
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

rem ============================================================================
rem Creating DEF File
rem ============================================================================
ECHO LIBRARY "harbour.dll" > dmcdll.def
ECHO EXETYPE NT >> dmcdll.def
ECHO SUBSYSTEM CONSOLE >> dmcdll.def
ECHO CODE SHARED EXECUTE >> dmcdll.def
ECHO DATA WRITE >> dmcdll.def

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
SET LIBPREFIX=
rem ============================================================================

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

   @CALL mdir.bat dllcreate

:BUILD
rem ============================================================================
rem It is not known if DMC support MT, so no MT-build is not introduced here.
rem If you have found how to build MT mode with DMC please modify this file
rem and the corresponding makefile.dc accordingly.
rem ============================================================================
   SET HB_MT_DIR=\dll
   %MAKE_EXE% -fhrbdll.dc > dll_dc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   @CALL mdir.bat dllcopy
   goto EXIT

:BUILD_ERR
   NOTEPAD dll_dc.log
   goto EXIT

:CLEAN
   @CALL mdir.bat dllclean
   IF EXIST dll_dc.log DEL dll_dc.log

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
   SET LIBPREFIX=
   SET MAKE_EXE=
   SET HB_MT_DIR=
   if exist dmcdll.def del dmcdll.def
   if exist make.tmp del make.tmp
