@echo off
rem ============================================================================
rem
rem $Id: make_dc.bat,v 1.7 2008/04/29 22:14:09 andijahja Exp $
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
SET PATH=%CC_DIR%\BIN;%BISON_DIR%;%PATH%
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

if "%1" == ""        goto SYNTAX
if "%1" == "clean"   goto CLEAN
if "%1" == "CLEAN"   goto CLEAN
if "%1" == "CORE"    goto BUILD
if "%1" == "core"    goto BUILD
if "%1" == "DLL"     goto DLL
if "%1" == "dll"     goto DLL
if "%1" == "CONTRIB" goto CONTRIBS
if "%1" == "contrib" goto CONTRIBS
if "%1" == "ALL"     goto BUILD_ALL
if "%1" == "all"     goto BUILD_ALL
goto SYNTAX

rem=============================================================================
:BUILD
rem=============================================================================
   @CALL MDIR.BAT
   SET __BLD__=CORE_BLD
   SET HB_MT=
   SET HB_MT_DIR=
   %MAKE_EXE% -s -l -fmakefile.dc %2 %3 >make_dc.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK

:BUILD_OK
   @CALL mdir.bat copytobin
   if "MAKEALL" == ""    goto EXIT
   if "%1" == "CORE" goto EXIT
   if "%1" == "core" goto EXIT
   goto DLL

:BUILD_ERR
   IF EXIST make_dc.log notepad make_dc.log
   goto EXIT

rem=============================================================================
:DLL
rem=============================================================================
rem
rem We use HB_MT_DIR envar for DLL object folder here
rem
   ECHO LIBRARY "harbour.dll" > dmcdll.def
   ECHO EXETYPE NT >> dmcdll.def
   ECHO SUBSYSTEM CONSOLE >> dmcdll.def
   ECHO CODE SHARED EXECUTE >> dmcdll.def
   ECHO DATA WRITE >> dmcdll.def
   @CALL mdir.bat dllcreate
   SET __BLD__=DLL_BLD
   SET HB_MT=
   SET HB_MT_DIR=\dll
   %MAKE_EXE% -s -fmakefile.dc %2 %3 >dll_dc.log
   if errorlevel 1 goto DLL_ERR
   goto DLL_OK

:DLL_OK
   @CALL mdir.bat dllcopy
   IF "MAKEALL" == ""   goto EXIT
   IF "%1" == "DLL" goto EXIT
   IF "%1" == "dll" goto EXIT
   goto CONTRIBS

:DLL_ERR
   if exist dll_dc.log notepad dll_dc.log
   goto EXIT

rem=============================================================================
:CONTRIBS
rem=============================================================================
   @CALL MDIR.BAT
   SET __BLD__=CONTRIB_BLD
   SET HB_MT_DIR=
   SET HB_MT=
   %MAKE_EXE% -s -l -fmakefile.dc %2 %3 >cont_dc.log
   if errorlevel 1 goto CONTRIBS_ERR

:CONTRIBS_OK
   @CALL mdir.bat copycontrib
   goto EXIT

:CONTRIBS_ERR
   IF EXIST cont_dc.log notepad cont_dc.log
   goto EXIT

rem=============================================================================
:BUILD_ALL
rem=============================================================================
   SET MAKEALL=yes
   goto BUILD

rem=============================================================================
:SYNTAX
rem=============================================================================
   ECHO.Syntax:
   ECHO. make_dc core    : Build xHarbour CORE files
   ECHO. make_dc dll     : Build xHarbour DLL
   ECHO. make_dc contrib : Build CONTRIB Libraries
   ECHO. make_dc all     : Build CORE, DLL and CONTRIB
   ECHO. make_dc clean   : Erase all files once built
   goto EXIT

rem=============================================================================
:CLEAN
rem=============================================================================
   @CALL mdir.bat clean
   IF EXIST make_dc.log DEL make_dc.log
   @CALL mdir.bat dllclean
   if exist dll_dc.log del dll_dc.log
   @CALL mdir.bat cleancontrib
   IF EXIST cont_dc.log DEL cont_dc.log

rem=============================================================================
:EXIT
rem=============================================================================
   @CALL mdir.bat resetenvar
   if exist *.map del *.map > NUL
   if exist dmcdll.def del dmcdll.def >NUL
