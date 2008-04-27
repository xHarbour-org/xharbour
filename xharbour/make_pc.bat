@echo off
rem ============================================================================
rem
rem $Id: makefile.bc,v 1.218 2008/04/14 06:06:20 andijahja Exp $
rem
rem FILE: make_pc.bat
rem BATCH FILE FOR PELLESC
rem
rem This is Generic File, do not change it. If you should require your own build
rem version, changes should only be made on your local copy.(AJ:2008-04-26)
rem
rem ============================================================================

SET CC_DIR=C:\PELLESC
SET BISON_DIR=C:\BISON\BIN
SET SUB_DIR=pc
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
rem ============================================================================
rem The curent POMAKE is buggy that it does not recognize macro defined, making
rem ifdef clause not working. The following envars help overcome the bug and
rem in order to build ST and MT version in one run. They will be reverted once
rem the bugs are fixed (AJ:2008-04-26)
rem ============================================================================
    SET HB_MT=
    SET HB_MT_DIR=
    SET __MT__=
    SET HB_MT_FLAGS=
    SET PROJECT=$(GTCGI_LIB) $(GTPCA_LIB) $(GTSTD_LIB) $(GTWIN_LIB) $(GTWVT_LIB) $(GTGUI_LIB) $(COMMON_LIB) $(PPGEN_EXE) $(PP_LIB) $(ZLIB_LIB) $(HARBOUR_EXE) $(VM_LIB) $(FMSTAT_LIB) $(RTL_LIB) $(MACRO_LIB) $(RDD_LIB) $(TIP_LIB) $(DBFFPT_LIB) $(DBFNTX_LIB) $(DBFCDX_LIB) $(BMDBFCDX_LIB) $(SIXCDX_LIB) $(BMSIXCDX_LIB) $(HBSIX_LIB) $(HSX_LIB) $(USRRDD_LIB) $(RDDS_LIB) $(CT_LIB) $(PCREPOS_LIB) $(HB_GT_LIBS) $(DEBUG_LIB) $(LANG_LIB) $(NULSYS_LIB) $(CODEPAGE_LIB) $(DLL_MAIN_LIB) $(ODBC_LIB) $(MISC_LIB) $(HBPP_EXE) $(HBDOC_EXE) $(HBMAKE_EXE) $(XBSCRIPT_EXE) $(HBTEST_EXE) $(HBRUN_EXE)
    POMAKE /F makefile.pc %1 %2 %3 >make_pc.log
    if errorlevel 1 goto BUILD_ERR

    SET HB_MT=mt
    SET HB_MT_DIR=\mt
    SET __MT__=-MT -DHB_THREAD_SUPPORT
    SET HB_MT_FLAGS=-dHB_THREAD_SUPPORT
    SET PROJECT=$(PP_LIB) $(VM_LIB) $(FMSTAT_LIB) $(RTL_LIB) $(MACRO_LIB) $(RDD_LIB) $(TIP_LIB) $(DBFFPT_LIB) $(DBFNTX_LIB) $(DBFCDX_LIB) $(BMDBFCDX_LIB) $(SIXCDX_LIB) $(BMSIXCDX_LIB) $(HBSIX_LIB) $(HSX_LIB) $(USRRDD_LIB) $(RDDS_LIB) $(CT_LIB) $(HBTEST_EXE) $(HBRUN_EXE)
    POMAKE /F makefile.pc %1 %2 %3 >>make_pc.log
    if errorlevel 1 goto BUILD_ERR

:BUILD_ERR
   notepad make_pc.log
   goto EXIT

:CLEAN
   @CALL MDIR.BAT CLEAN
   IF EXIST make_pc.log DEL make_pc.log

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
   SET PATH=%_PATH%
   SET _PATH=
   SET LIBEXT=
   SET OBJEXT=
   SET HB_MT=
   SET DIR_SEP=
   SET HB_MT_DIR=
   SET __MT__=
   SET HB_MT_FLAGS=
   SET PROJECT=
