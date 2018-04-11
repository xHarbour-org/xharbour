#===============================================================================
#
# $Id$
#
# FILE : common.mak
# NOTES: This file is used by all C/C++ compilers under Windows Platform whose
#        batch files are available in the root directory.
#        1. Do not convert tabs to spaces in this file
#        2. Do not comment any entry in this file
#        3. Do not include !ifdef condition in this file. If so required,
#           !ifdef should be written on individual makefile.
#	 4. Any addition to the dependencies should be accompanied by
#	    corresponding entries in compile.mak
#
#===============================================================================

#===============================================================================
# Macros for Directories. This should never change
#===============================================================================
BIN_DIR       =bin$(DIR_SEP)$(SUB_DIR)
LIB_DIR       =lib$(DIR_SEP)$(SUB_DIR)
OBJ_DIR       =obj$(DIR_SEP)$(SUB_DIR)$(HB_MT_DIR)

CGI_DIR       =source$(DIR_SEP)cgi
COMMON_DIR    =source$(DIR_SEP)common
COMPILER_DIR  =source$(DIR_SEP)compiler
DEBUG_DIR     =source$(DIR_SEP)debug
LANG_DIR      =source$(DIR_SEP)lang
MACRO_DIR     =source$(DIR_SEP)macro
PP_DIR        =source$(DIR_SEP)pp
RDD_DIR       =source$(DIR_SEP)rdd
RTL_DIR       =source$(DIR_SEP)rtl
PCREPOS_DIR   =source$(DIR_SEP)rtl$(DIR_SEP)pcre
ZLIB_DIR      =source$(DIR_SEP)rtl$(DIR_SEP)zlib
VM_DIR        =source$(DIR_SEP)vm
CODEPAGE_DIR  =source$(DIR_SEP)codepage
EXPORT_DEF    =source$(DIR_SEP)rtl$(DIR_SEP)export.def

HBRUN_DIR     =utils$(DIR_SEP)hbrun
HBTEST_DIR    =utils$(DIR_SEP)hbtest
HBDOC_DIR     =utils$(DIR_SEP)hbdoc
HBDICT_DIR    =utils$(DIR_SEP)hbdict
HBPP_DIR      =utils$(DIR_SEP)hbpp
HBMAKE_DIR    =utils$(DIR_SEP)hbmake
HBEXTERN_DIR  =utils$(DIR_SEP)hbextern
HBFORMAT_DIR  =utils$(DIR_SEP)hbformat
XBSCRIPT_DIR  =utils$(DIR_SEP)xbscript
HBFILERE_DIR  =utils$(DIR_SEP)hbfilere

CT_DIR        =source$(DIR_SEP)ct
TIP_DIR       =source$(DIR_SEP)tip
ODBC_DIR      =source$(DIR_SEP)odbc
MISC_DIR      =contrib$(DIR_SEP)misc
SDDFB_DIR     =contrib$(DIR_SEP)sddfb
SDDMY_DIR     =contrib$(DIR_SEP)sddmy
SDDOCI_DIR    =contrib$(DIR_SEP)sddoci
SDDODBC_DIR   =contrib$(DIR_SEP)sddodbc
SDDSQLT3_DIR  =contrib$(DIR_SEP)sddsqlt3
SDDPG_DIR     =contrib$(DIR_SEP)sddpg
RDDSQL_DIR    =contrib$(DIR_SEP)rddsql
HBMLZO_DIR    =contrib$(DIR_SEP)hbmlzo
HBEXPAT_DIR   =contrib$(DIR_SEP)hbexpat
HBLZF_DIR     =contrib$(DIR_SEP)hblzf
HBBZ2_DIR     =contrib$(DIR_SEP)hbbz2
HBCAIRO_DIR   =contrib$(DIR_SEP)hbcairo
HBZEBRA_DIR   =contrib$(DIR_SEP)hbzebra
HBCURL_DIR    =contrib$(DIR_SEP)hbcurl
HBSQLIT3_DIR  =contrib$(DIR_SEP)hbsqlit3
SEVENZIP_DIR  =contrib$(DIR_SEP)sevenzip
HBHPDF_DIR    =contrib$(DIR_SEP)hbhpdf
PNG_DIR       =contrib$(DIR_SEP)png
FILEMEM_DIR   =contrib$(DIR_SEP)filemem
FIREBIRD_DIR  =contrib$(DIR_SEP)firebird
FREEIMAGE_DIR =contrib$(DIR_SEP)freeimage
GDLIB_DIR     =contrib$(DIR_SEP)gd
GTWVG_DIR     =contrib$(DIR_SEP)gtwvg
GTWVW_DIR     =contrib$(DIR_SEP)gtwvw
HBMZIP_DIR    =contrib$(DIR_SEP)hbmzip
HBZIP_DIR     =contrib$(DIR_SEP)hbzlib
LIBNF_DIR     =contrib$(DIR_SEP)libnf
MYSQL_DIR     =contrib$(DIR_SEP)mysql
PGSQL_DIR     =contrib$(DIR_SEP)pgsql
RDDADS_DIR    =contrib$(DIR_SEP)rdd_ads
TELEPATH_DIR  =contrib$(DIR_SEP)tp_
HBCC_DIR      =contrib$(DIR_SEP)unicode
XWT_DIR       =contrib$(DIR_SEP)xwt
XWT2_DIR      =contrib$(DIR_SEP)xwt2
TIPSSL_DIR    =contrib$(DIR_SEP)tipssl
CGILIB_DIR    =contrib$(DIR_SEP)cgilib
HBMAGIC_DIR   =contrib$(DIR_SEP)hbmagic
HBTINYMT_DIR  =contrib$(DIR_SEP)hbtinymt
PDFLITE_DIR   =contrib$(DIR_SEP)pdflite
JPEG_DIR      =contrib$(DIR_SEP)jpeg
TIFF_DIR      =contrib$(DIR_SEP)tiff
HBBTREE_DIR   =contrib$(DIR_SEP)hbbtree
SIXAPI_DIR    =contrib$(DIR_SEP)sixapi
HBCAB_DIR     =contrib$(DIR_SEP)hbcab
HBCOMM_DIR    =contrib$(DIR_SEP)hbcomm
HBMXML_DIR    =contrib$(DIR_SEP)hbmxml
HBXDIFF_DIR   =contrib$(DIR_SEP)hbxdiff
HBSSL_DIR     =contrib$(DIR_SEP)hbssl

INCLUDE_DIR2  =include

#===============================================================================
# Macros For Standard Libraries
#===============================================================================
COMPILER_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)compiler$(LIBEXT)
HBCGI_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbcgi$(LIBEXT)
COMMON_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)common$(LIBEXT)
CODEPAGE_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)codepage$(LIBEXT)
CT_LIB      =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)ct$(LIBEXT)
DBFCDX_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)dbfcdx$(LIBEXT)
DBFMDX_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)dbfmdx$(LIBEXT)
BMDBFCDX_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)bmdbfcdx$(LIBEXT)
REDBFCDX_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)redbfcdx$(LIBEXT)
DBFMDX_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)dbfmdx$(LIBEXT)
SIXCDX_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sixcdx$(LIBEXT)
BMSIXCDX_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)bmsixcdx$(LIBEXT)
DBFFPT_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)dbffpt$(LIBEXT)
REDBFFPT_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)redbffpt$(LIBEXT)
DBFNTX_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)dbfntx$(LIBEXT)
DBFNSX_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)dbfnsx$(LIBEXT)
HBSIX_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbsix$(LIBEXT)
HSX_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hsx$(LIBEXT)
USRRDD_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)usrrdd$(LIBEXT)
RDDS_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)rdds$(LIBEXT)
DEBUG_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)debug$(LIBEXT)
LANG_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)lang$(LIBEXT)
MACRO_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)macro$(LIBEXT)
NULSYS_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)nulsys$(LIBEXT)
PP_LIB      =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)pp$(LIBEXT)
RDD_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)rdd$(LIBEXT)
RTL_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)rtl$(LIBEXT)
PCREPOS_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)pcrepos$(LIBEXT)
ZLIB_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)zlib$(LIBEXT)
TIP_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)tip$(LIBEXT)
VM_LIB      =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)vm$(HB_MT)$(LIBEXT)
ODBC_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbodbc$(LIBEXT)
DLL_MAIN_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)dllmain$(LIBEXT)
USE_DLL_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)use_dll$(LIBEXT)
MISC_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)libmisc$(LIBEXT)
GTCGI_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtcgi$(LIBEXT)
GTDOS_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtdos$(LIBEXT)
GTPCA_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtpca$(LIBEXT)
GTSTD_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtstd$(LIBEXT)
GTWIN_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtwin$(LIBEXT)
GTGUI_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtgui$(LIBEXT)
GTWVT_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtwvt$(LIBEXT)
GTALLEG_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtalleg$(LIBEXT)
HB_GT_LIBS  =$(GTCGI_LIB) $(GTPCA_LIB) $(GTSTD_LIB) $(GTWIN_LIB) $(GTWVT_LIB) $(GTGUI_LIB)

#===============================================================================
# Contrib Library Names
#===============================================================================
HBSSL_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbssl$(LIBEXT)
HBXDIFF_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbxdiff$(LIBEXT)
HBMXML_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbmxml$(LIBEXT)
HBCOMM_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbcomm$(LIBEXT)
HBCAB_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbcab$(LIBEXT)
SIXAPI_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sixapi$(LIBEXT)
HBBTREE_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbbtree$(LIBEXT)
TIFF_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)tiff$(LIBEXT)
JPEG_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)jpeg$(LIBEXT)
PDFLITE_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)pdflite$(LIBEXT)
HBTINYMT_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbtinymt$(LIBEXT)
HBMAGIC_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbmagic$(LIBEXT)
HBEXPAT_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbexpat$(LIBEXT)
SDDFB_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sddfb$(LIBEXT)
SDDOCI_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sddoci$(LIBEXT)
SDDMY_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sddmy$(LIBEXT)
SDDPG_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sddpg$(LIBEXT)
SDDODBC_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sddodbc$(LIBEXT)
SDDSQLT3_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sddsqlt3$(LIBEXT)
RDDSQL_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)rddsql$(LIBEXT)
HBMLZO_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbmlzo$(LIBEXT)
HBLZF_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hblzf$(LIBEXT)
HBBZ2_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbbz2$(LIBEXT)
HBCAIRO_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbcairo$(LIBEXT)
HBZEBRA_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbzebra$(LIBEXT)
HBCURL_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbcurl$(LIBEXT)
HBSQLIT3_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbsqlit3$(LIBEXT)
SEVENZIP_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)sevenzip$(LIBEXT)
FILEMEM_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)filemem$(LIBEXT)
FIREBIRD_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)firebird$(LIBEXT)
FREEIMAGE_LIB=$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)fi_lib$(LIBEXT)
GDLIB_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gdlib$(LIBEXT)
GTWVG_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtwvg$(LIBEXT)
GTWVW_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)gtwvw$(LIBEXT)
HBMZIP_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbmzip$(LIBEXT)
HBZIP_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbzip$(LIBEXT)
LIBNF_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)libnf$(LIBEXT)
MYSQL_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)mysql$(LIBEXT)
PGSQL_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)libhbpg$(LIBEXT)
RDDADS_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)rddads$(LIBEXT)
ACE32_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)ace32$(LIBEXT)
ACE64_LIB    =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)ace64$(LIBEXT)
TELEPATH_LIB =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)telepath$(LIBEXT)
HBCC_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbcc$(LIBEXT)
XWT_LIB      =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)xwt$(LIBEXT)
XWT2_LIB     =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)xwt2$(LIBEXT)
PNG_LIB      =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)png$(LIBEXT)
HBHPDF_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)hbhpdf$(LIBEXT)
LIBHARU_LIB  =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)libharu$(LIBEXT)
TIPSSL_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)tipssl$(LIBEXT)
CGILIB_LIB   =$(LIB_DIR)$(DIR_SEP)$(LIBPREFIX)cgilib$(LIBEXT)

#===============================================================================
# Macros For DLL Build
#===============================================================================
HB_STARTUP_OBJ  =$(OBJ_DIR)$(DIR_SEP)mainstd$(OBJEXT)
HARBOUR_DLL     =$(BIN_DIR)$(DIR_SEP)xharbour$(HB_DEBUG).dll
HBRUNDLL_EXE    =$(BIN_DIR)$(DIR_SEP)hbrundll$(HB_DEBUG).exe
HBTESTDLL_EXE   =$(BIN_DIR)$(DIR_SEP)hbtestdll$(HB_DEBUG).exe
HBMAKEDLL_EXE   =$(BIN_DIR)$(DIR_SEP)hbmakedll$(HB_DEBUG).exe
HBDOCDLL_EXE    =$(BIN_DIR)$(DIR_SEP)hbdocdll$(HB_DEBUG).exe
XBSCRIPTDLL_EXE =$(BIN_DIR)$(DIR_SEP)xbscriptdll$(HB_DEBUG).exe
HARBOUR_DLL_LIB =$(BIN_DIR)$(DIR_SEP)$(LIBPREFIX)xharbour$(LIBEXT)

#===============================================================================
# Macros For Standard Executable
#===============================================================================
HBRC_EXE    =$(BIN_DIR)$(DIR_SEP)hbrc$(HB_DEBUG).exe
HBLIB_EXE   =$(BIN_DIR)$(DIR_SEP)hblib$(HB_DEBUG).exe
HBRM_EXE    =$(BIN_DIR)$(DIR_SEP)hbrm$(HB_DEBUG).exe
HARBOUR_EXE =$(BIN_DIR)$(DIR_SEP)harbour$(HB_DEBUG).exe
HBPP_EXE    =$(BIN_DIR)$(DIR_SEP)hbpp$(HB_DEBUG).exe
PPGEN_EXE   =$(BIN_DIR)$(DIR_SEP)ppgen$(HB_DEBUG).exe
HBFILERE_EXE=$(BIN_DIR)$(DIR_SEP)hbfilere$(HB_DEBUG).exe
HBRUN_EXE   =$(BIN_DIR)$(DIR_SEP)hbrun$(HB_MT)$(HB_DEBUG).exe
HBTEST_EXE  =$(BIN_DIR)$(DIR_SEP)hbtest$(HB_MT)$(HB_DEBUG).exe
HBDOC_EXE   =$(BIN_DIR)$(DIR_SEP)hbdoc$(HB_DEBUG).exe
HBDICT_EXE  =$(BIN_DIR)$(DIR_SEP)hbdict$(HB_DEBUG).exe
HBMAKE_EXE  =$(BIN_DIR)$(DIR_SEP)hbmake$(HB_DEBUG).exe
HBEXTERN_EXE=$(BIN_DIR)$(DIR_SEP)hbextern$(HB_DEBUG).exe
HBFORMAT_EXE=$(BIN_DIR)$(DIR_SEP)hbformat$(HB_DEBUG).exe
XBSCRIPT_EXE=$(BIN_DIR)$(DIR_SEP)xbscript$(HB_DEBUG).exe

#===============================================================================
# Resource files
#===============================================================================
HARBOUR_EXE_RC  =$(OBJ_DIR)$(DIR_SEP)harbour$(HB_DEBUG).exe.rc
HBPP_EXE_RC     =$(OBJ_DIR)$(DIR_SEP)hbpp$(HB_DEBUG).exe.rc
PPGEN_EXE_RC    =$(OBJ_DIR)$(DIR_SEP)ppgen$(HB_DEBUG).exe.rc
HBFILERE_EXE_RC =$(OBJ_DIR)$(DIR_SEP)hbfilere$(HB_DEBUG).exe.rc
HBRUN_EXE_RC    =$(OBJ_DIR)$(DIR_SEP)hbrun$(HB_MT)$(HB_DEBUG).exe.rc
HBTEST_EXE_RC   =$(OBJ_DIR)$(DIR_SEP)hbtest$(HB_MT)$(HB_DEBUG).exe.rc
HBDOC_EXE_RC    =$(OBJ_DIR)$(DIR_SEP)hbdoc$(HB_DEBUG).exe.rc
HBDICT_EXE_RC   =$(OBJ_DIR)$(DIR_SEP)hbdict$(HB_DEBUG).exe.rc
HBMAKE_EXE_RC   =$(OBJ_DIR)$(DIR_SEP)hbmake$(HB_DEBUG).exe.rc
HBEXTERN_EXE_RC =$(OBJ_DIR)$(DIR_SEP)hbextern$(HB_DEBUG).exe.rc
HBFORMAT_EXE_RC =$(OBJ_DIR)$(DIR_SEP)hbformat$(HB_DEBUG).exe.rc
XBSCRIPT_EXE_RC =$(OBJ_DIR)$(DIR_SEP)xbscript$(HB_DEBUG).exe.rc
HARBOUR_EXE_RES =$(OBJ_DIR)$(DIR_SEP)harbour$(HB_DEBUG).exe$(RESEXT)
HBPP_EXE_RES    =$(OBJ_DIR)$(DIR_SEP)hbpp$(HB_DEBUG).exe$(RESEXT)
PPGEN_EXE_RES   =$(OBJ_DIR)$(DIR_SEP)ppgen$(HB_DEBUG).exe$(RESEXT)
HBFILERE_EXE_RES=$(OBJ_DIR)$(DIR_SEP)hbfilere$(HB_DEBUG).exe$(RESEXT)
HBRUN_EXE_RES   =$(OBJ_DIR)$(DIR_SEP)hbrun$(HB_MT)$(HB_DEBUG).exe$(RESEXT)
HBTEST_EXE_RES  =$(OBJ_DIR)$(DIR_SEP)hbtest$(HB_MT)$(HB_DEBUG).exe$(RESEXT)
HBDOC_EXE_RES   =$(OBJ_DIR)$(DIR_SEP)hbdoc$(HB_DEBUG).exe$(RESEXT)
HBDICT_EXE_RES  =$(OBJ_DIR)$(DIR_SEP)hbdict$(HB_DEBUG).exe$(RESEXT)
HBMAKE_EXE_RES  =$(OBJ_DIR)$(DIR_SEP)hbmake$(HB_DEBUG).exe$(RESEXT)
HBEXTERN_EXE_RES=$(OBJ_DIR)$(DIR_SEP)hbextern$(HB_DEBUG).exe$(RESEXT)
HBFORMAT_EXE_RES=$(OBJ_DIR)$(DIR_SEP)hbformat$(HB_DEBUG).exe$(RESEXT)
XBSCRIPT_EXE_RES=$(OBJ_DIR)$(DIR_SEP)xbscript$(HB_DEBUG).exe$(RESEXT)
HARBOUR_DLL_RC  =$(OBJ_DIR)$(DIR_SEP)xharbour$(HB_DEBUG).dll.rc
HARBOUR_DLL_RES =$(OBJ_DIR)$(DIR_SEP)xharbour$(HB_DEBUG).dll$(RESEXT)

#===============================================================================
# xHarbour Libraries for building standard executables
#===============================================================================
HARBOUR_LIBS=\
	$(PP_LIB)\
	$(COMMON_LIB)\
	$(DEBUG_LIB)\
	$(VM_LIB)\
	$(RTL_LIB)\
	$(PCREPOS_LIB)\
	$(HB_GT_LIB)\
	$(LANG_LIB)\
	$(RDD_LIB)\
	$(MACRO_LIB)\
	$(DBFFPT_LIB)\
	$(REDBFFPT_LIB)\
	$(DBFNTX_LIB)\
	$(DBFNSX_LIB)\
	$(DBFCDX_LIB)\
	$(DBFMDX_LIB)\
	$(BMDBFCDX_LIB)\
	$(REDBFCDX_LIB)\
	$(SIXCDX_LIB)\
	$(BMSIXCDX_LIB)\
	$(HBSIX_LIB)\
	$(HSX_LIB)\
	$(USRRDD_LIB)\
	$(RDDS_LIB)\
	$(CODEPAGE_LIB)\
	$(CT_LIB)\
	$(ZLIB_LIB)

#===============================================================================
# List Of Library and Executable Dependencies
#===============================================================================

#===============================================================================
# HBCGI.LIB
#===============================================================================
HBCGI_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_hjwindow$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_hterrsys$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_htjlist$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_htmutil$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_tcgi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_tedit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_tfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_tframe$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)CGILIB_thtm$(OBJEXT)

#===============================================================================
# HBSSL.LIB
#===============================================================================
HBSSL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)bio$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)err$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)evp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)evpciph$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)evpenc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)evpmd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)evppkey$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rand$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ssl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sslciph$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sslctx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sslsess$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ssl_hb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)x509$(OBJEXT)

#===============================================================================
# HBXDIFF.LIB
#===============================================================================
HBXDIFF_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)corexdiff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xadler32$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xalloc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xbdiff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xbpatchi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xdiffi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xemit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xmerge3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xmissing$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xpatchi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xprepare$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xrabdiff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xrabply$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xutils$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xversion$(OBJEXT)

#===============================================================================
# HBMXML.LIB
#===============================================================================
HBMXML_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbmxml$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_att$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_ent$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_fil$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_get$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_ind$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_nod$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_pri$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_sea$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_set$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mxml_str$(OBJEXT)

#===============================================================================
# HBCOMM.LIB
#===============================================================================
HBCOMM_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)commHBCOMM$(OBJEXT)

#===============================================================================
# HBCAB.LIB
#===============================================================================
HBCAB_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)cabinetHBCAB$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)compressHBCAB$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)decompressHBCAB$(OBJEXT)

#===============================================================================
# SIXAPI.LIB
#===============================================================================
SIXAPI_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)aliasSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)appendSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)baseSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bofeofSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)closeSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)commitSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)copySIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)countSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)createSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbcopySIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbdelimSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbevalSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbsortSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbstruSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)decryptSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)deleteSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)descendSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)emptySIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)encryptSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)evalSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fieldSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filterSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)finalizeSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)goSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)indexSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)isnullSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)lockSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)putSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)querySIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)recSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)replaceSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ryoSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)seekSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)selectSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)skipSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)syspropSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)useSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)arraySIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)browdbSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)browseSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbeditSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fblockSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sdeSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ttableSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ttagSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tindexSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)globalSIXAPI$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)toolsSIXAPI$(OBJEXT)

#===============================================================================
# HVMALL.OBJ
#===============================================================================
VM_ALL_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hvmall$(HB_MT)$(OBJEXT)

#===============================================================================
# HBBTREE.LIB Dependencies
#===============================================================================
HBBTREE_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hb_btree$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tbtree$(OBJEXT)

#===============================================================================
# TIFF.LIB Dependencies
#===============================================================================
TIFF_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)tif_aux$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_close$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_codec$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_color$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_compress$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dir$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dirinfo$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dirread$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dirwrite$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dumpmode$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_error$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_extension$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_fax3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_fax3sm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_flush$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_getimage$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_jbig$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_jpeg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_jpeg_12$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_luv$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_lzma$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_lzw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_next$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_ojpeg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_open$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_packbits$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_pixarlog$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_predict$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_print$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_read$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_strip$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_swab$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_thunder$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_tile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_version$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_warning$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_win32$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_write$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_zip$(OBJEXT)

#===============================================================================
# JPEG.LIB
#===============================================================================
JPEG_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)jaricom$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcapimin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcapistd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcarith$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jctrans$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcparam$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdatadst$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcinit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jmemansi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcmaster$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcmarker$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcmainct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcprepct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jccoefct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jccolor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcsample$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jchuff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcdctmgr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jfdctfst$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jfdctflt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jfdctint$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdapimin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdapistd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdtrans$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdatasrc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdmaster$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdinput$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdmarker$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdhuff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdmainct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdcoefct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdpostct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jddctmgr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jidctfst$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jidctflt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jidctint$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdsample$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdcolor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jquant1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jquant2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdmerge$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jcomapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jutils$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jerror$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jmemmgr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)jdarith$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cdjpeg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rdcolmap$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wrbmp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wrgif$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wrppm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wrtarga$(OBJEXT)

#===============================================================================
# PDFLITE.LIB Dependencies
#===============================================================================
PDFLITE_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)pdf1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbpdflib$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)ft_cid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ft_corefont$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ft_font$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ft_truetype$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ft_type1$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)pc_chartabs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_contain$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_core$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_ctype$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_digsig$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_encoding$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_file$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_geom$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_md5$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_optparse$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_output$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_resource$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_string$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_unicode$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pc_util$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)pdflib_core$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_actions$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_afm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_annots$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_bmp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_ccitt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_cid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_color$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_document$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_draw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_encoding$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_filter$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_font$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_gif$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_gstate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_hyper$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_image$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_jpeg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_jpx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_mbox$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_object$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_page$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_params$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_pattern$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_pfm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_png$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_shading$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_template$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_text$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_tiff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_truetype$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_type1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_type3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_util$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)p_xgstate$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)tif_auxx_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_close_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_codec_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_color_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_compress_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dir_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dirinfo_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dirread_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dirwrite_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_dumpmode_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_error_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_extension_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_fax3_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_fax3sm_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_flush_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_getimage_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_jpeg_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_luv_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_lzw_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_next_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_ojpeg_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_open_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_packbits_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_pixarlog_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_predict_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_print_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_read_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_strip_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_swab_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_thunder_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_tile_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_version_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_warning_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_unix_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_write_PDFLITE$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tif_zip_PDFLITE$(OBJEXT)

#===============================================================================
# HBTINYMT.LIB Dependencies
#===============================================================================
HBTINYMT_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbtinymt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tinymt32$(OBJEXT)

#===============================================================================
# HBMAGIC.LIB Dependencies
#===============================================================================
HBMAGIC_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)coreHBMAGIC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbmagis$(OBJEXT)

#===============================================================================
# HBEXPAT.LIB Dependencies
#===============================================================================
HBEXPAT_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)coreHBEXPAT$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xmlparse$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xmltok_n$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xmlrole$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xmltok$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xmltok_i$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)internal$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)unitable$(OBJEXT)

#===============================================================================
# MYSQL.LIB Dependencies
#===============================================================================
MYSQL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)mysqlrdd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tmysql$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tsqlbrw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mysql$(OBJEXT)

#===============================================================================
# CGILIB.LIB Dependencies
#===============================================================================
CGILIB_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)cgiconfig$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cgifunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)thtmlbase$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)thtmldoc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tsession$(OBJEXT)

#===============================================================================
# SDDMY.LIB Dependencies
#===============================================================================
SDDMY_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sddmy$(OBJEXT)

#===============================================================================
# SDDOCI.LIB Dependencies
#===============================================================================
SDDOCI_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sddoci$(OBJEXT)

#===============================================================================
# SDDODBC.LIB Dependencies
#===============================================================================
SDDODBC_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sddodbc$(OBJEXT)

#===============================================================================
# SDDSQLT3.LIB Dependencies
#===============================================================================
SDDSQLT3_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sddsqlt3$(OBJEXT)

#===============================================================================
# SDDPG.LIB Dependencies
#===============================================================================
SDDPG_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sddpg$(OBJEXT)

#===============================================================================
# RDDSQL.LIB rules
#===============================================================================
RDDSQL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sqlbase$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sqlmix$(OBJEXT)

#===============================================================================
# SDDFB.LIB Dependencies
#===============================================================================
SDDFB_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sddfb$(OBJEXT)

#===============================================================================
# HBMLZO.LIB rules
#===============================================================================
HBMLZO_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)coreHBMLZO$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)minilzoHBMLZO$(OBJEXT)

#===============================================================================
# HBLZF.LIB rules
#===============================================================================
HBLZF_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)coreHBLZF$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)lzf_cHBLZF$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)lzf_dHBLZF$(OBJEXT)

#===============================================================================
# HBBZ2.LIB rules
#===============================================================================
HBBZ2_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)blocksorHBBZ2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bzlibHBBZ2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)compressHBBZ2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)coreHBBZ2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)crctableHBBZ2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)decompreHBBZ2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)huffmanHBBZ2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)randtablHBBZ2$(OBJEXT)

#===============================================================================
# HBCAIRO.LIB rules
#===============================================================================
HBCAIRO_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)coreHBCAIRO$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)imageHBCAIRO$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)paths$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pdf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngHBCAIRO$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)surface$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)textHBCAIRO$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)transfor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)utilHBCAIRO$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)context$(OBJEXT)

#===============================================================================
# HBZEBRA.LIB rules
#===============================================================================
HBZEBRA_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)codabar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)code11$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)code128$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)code39$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)code93$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)coreHBZEBRA$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)coredraw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)datamtrx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)eanupc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)itf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pdf417$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)qrcode$(OBJEXT)

#===============================================================================
# HBCURL.LIB rules
#===============================================================================
HBCURL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbcurl$(OBJEXT)

#===============================================================================
# HBSQLIT3.LIB rules
#===============================================================================
HBSQLIT3_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sqlite3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xhbsqlite3$(OBJEXT)

#===============================================================================
# SEVENZIP.LIB rules
#===============================================================================
SEVENZIP_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)t7zip$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sevenzip$(OBJEXT)

#===============================================================================
# RTL.LIB rules
#===============================================================================
RTL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)abs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)accept$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ampm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)at$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)binnum$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)binnumx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)box$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cdpapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)chrasc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)chruni$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)colorind$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)console$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)copyfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)datec$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dateshb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbf2txt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)defpath$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)descend$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dirdrive$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)direct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)diskspac$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)disksphb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dllcall$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)empty$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)file$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filebuf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filehb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filenet$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filestat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filesys$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fserror$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fkmax$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fnsplit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fparse$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fssize$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fstemp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gete$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtapiu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtchrmap$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtclip$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtfunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtkeycod$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtkbstat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gttone$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtsys$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hardcr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbbitf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbchksum$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcomprs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsocket$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsockhb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbserv$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbzlibgz$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcrc32$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcrypt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbffind$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbgtcore$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbhex2n$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbip$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbjson$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbmd5$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbntos$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hboutdbg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbrandom$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsrlraw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsyslog$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbtoken$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbxml$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)idle$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)inkey$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)inkeyapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)is$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)isprint$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)langapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)left$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)len$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)lennum$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)math$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)maxrow$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)memofile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)minmax$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mlcfunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mod$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mouseapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mousex$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mouse53$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mtran$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)natmsg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)net$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)oemansi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)oldbox$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)oldclear$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pad$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)padc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)padl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)padr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)philes$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)philes53$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)philesx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)readline$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)replic$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)right$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)round$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)run$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)samples$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)saverest$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)scroll$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)seconds$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)session$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)readexit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)readins$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)regex$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setcolor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setcurs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setkey$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setpos$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setposbs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)shadow$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)soundex$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)space$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)spfiles$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)str$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)str2ptr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strcase$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strdel$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strings$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strmatch$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strpeek$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strtran$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strzero$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)stuff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)substr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tone$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tprinter$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trace$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)transfrm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trim$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)txtline$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)type$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)val$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)valtostr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)valtype$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)version$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)word$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xhelp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xsavescr$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)achoice$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)adir$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)alert$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)arrayblk$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)browdb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)browdbx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)browse$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)checkbox$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)clpclass$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)color53$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cstr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cstruct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbedit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)decode$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)devoutp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dircmd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dummy$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dumpvar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)einstvar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)error$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)errorsys$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fieldbl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getlist$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getsys$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbini$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hblog$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hblognet$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbserial$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)input$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)listbox$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)memoedit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)memvarbl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)menuto$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mssgline$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)objfunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)perfuncs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)persist$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)profiler$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pushbtn$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)radiobtn$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)radiogrp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)readkey$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)readvar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)regexrpl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)scrollbr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setfunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setta$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sprintf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)stream$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tbcolumn$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tbrowse$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tclass$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)teditor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)text$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tget$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tgetint$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tgetlist$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tlabel$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tmenuitm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tobject$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tpopup$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)traceprg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)treport$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trpc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trpccli$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tscalar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ttable$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ttextlin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ttopbar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)txml$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)typefile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wait$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)win32ole$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbping$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbwinole$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)win32prn$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)winos$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)winreg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)win_misc$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)sha1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sha1hmac$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sha2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sha2hmac$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsha1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsha1hm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsha2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbsha2hm$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)hbbffnc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbbfish$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)hbrand$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)arc4$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)hbcom$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcomhb$(OBJEXT)

#===============================================================================
# PCREPOS.LIB rules
#===============================================================================
PCREPOS_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)pcre_byte_order$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_chartables$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_compile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_config$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_dfa_exec$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_exec$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_fullinfo$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_get$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_globals$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_maketables$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_newline$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_ord2utf8$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_refcount$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_study$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_tables$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_valid_utf8$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_version$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_ucd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcre_xclass$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcreposix$(OBJEXT)

#===============================================================================
# ZLIB.LIB rules
#===============================================================================
ZLIB_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)adler32$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)compress$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)crc32$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)deflate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gzread$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gzclose$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gzlib$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gzwrite$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)infback$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)inffast$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)inflate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)inftrees$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trees$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uncompr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zutil$(OBJEXT)

#===============================================================================
# LANG.LIB rules
#===============================================================================
LANG_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)msgbgmik$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgbgwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgby866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgbywin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgca$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgcs852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgcsiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgcskam$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgcswin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgde$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgdewin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgen$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgeo$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msges$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgeswin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgeu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgfr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msggl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghe862$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghewin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghr1250$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghr437$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghr852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghriso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghu852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghucwi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msghuwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgis850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgko$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgltwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgnl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgpl852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgpliso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgplmaz$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgplwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgpt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgro$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgru866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgrukoi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgruwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgsl852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgsliso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgslwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgsr852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgsriso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgsrwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgsv$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgsvwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgua866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msguados$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msguakoi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msguawin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgzhb5$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgzhgb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msg_tpl$(OBJEXT)

#===============================================================================
# MACRO.LIB rules
#===============================================================================
MACRO_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)macroy$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)macroa$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)macrob$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)macroc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)macroslx$(OBJEXT)

#===============================================================================
# RDD.LIB rules
#===============================================================================
RDD_LIB_OBJS=\
              $(OBJ_DIR)$(DIR_SEP)dbcmd$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbcmd53$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbcmdx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbcmdhb$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbdrop$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbexists$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbrename$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)fieldhb$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)rddinfo$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)workarea$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)wacore$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)wafunc$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbf1$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbf1net$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbnubs$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)delim1$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)sdf1$(OBJEXT)\
              \
              $(OBJ_DIR)$(DIR_SEP)dbdelim$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbfuncs$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbfuncsx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbjoin$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbjoinx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbsdf$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbtotal$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbtotalx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dblist$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dblistx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbsort$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbsortx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)hbdbsort$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbstrux$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbstruxx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbstruxu$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbupdat$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)dbupdatx$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)rddord$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)rddordu$(OBJEXT)\
              $(OBJ_DIR)$(DIR_SEP)rddsys$(OBJEXT)

#===============================================================================
# NULSYS.LIB rules
#===============================================================================
NULSYS_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)nulsys$(OBJEXT)

#===============================================================================
# DBFFPT.LIB rules
#===============================================================================
DBFFPT_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)dbffpt1$(OBJEXT)\

#===============================================================================
# REDBFFPT.LIB rules
#===============================================================================
REDBFFPT_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)redbffpt1$(OBJEXT)\

#===============================================================================
# DBFNTX.LIB rules
#===============================================================================
DBFNTX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)dbfntx1$(OBJEXT)\

#===============================================================================
# DBFNSX.LIB rules
#===============================================================================
DBFNSX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)dbfnsx1$(OBJEXT)

#===============================================================================
# DBFCDX.LIB rules
#===============================================================================
DBFCDX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)dbfcdx1$(OBJEXT)

#===============================================================================
# DBFMDX.LIB rules
#===============================================================================
DBFMDX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)dbfmdx1$(OBJEXT)

#===============================================================================
# BMDBFCDX.LIB rules
#===============================================================================
BMDBFCDX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)bmdbfcdx1$(OBJEXT)

#===============================================================================
# REDBFCDX.LIB rules
#===============================================================================
REDBFCDX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)redbfcdx1$(OBJEXT)

#===============================================================================
# SIXCDX.LIB rules
#===============================================================================
SIXCDX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sixcdx1$(OBJEXT)

#===============================================================================
# BMSIXCDX.LIB rules
#===============================================================================
BMSIXCDX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)bmsixcdx1$(OBJEXT)

#===============================================================================
# HBSIX.LIB rules
#===============================================================================
HBSIX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)sxcompr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxcrypt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxdate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxsem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxfname$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxtable$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxord$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxutil$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxredir$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)sxcompat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxini$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sxtrig$(OBJEXT)

#===============================================================================
# HSX.LIB rules
#===============================================================================
HSX_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hsx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cftsfunc$(OBJEXT)

#===============================================================================
# USRRDD.LIB rules
#===============================================================================
USRRDD_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)usrrdd$(OBJEXT)

#===============================================================================
# RDDADS.LIB rules
#===============================================================================
RDDADS_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)ads1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)adsfunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)adsmgmnt$(OBJEXT)

#===============================================================================
# RDD.LIB rules
#===============================================================================
RDDS_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)arrayrdd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)logrdd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbtcdx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fcomma$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fptcdx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hscdx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rlcdx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)smtcdx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)adordd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ansirdd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)vfpcdx$(OBJEXT)

#===============================================================================
# PP.LIB rules
#===============================================================================
PP_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)pptable$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ppcore$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pplib$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pplib2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pplib3$(OBJEXT)

#===============================================================================
# DEBUG.LIB rules
#===============================================================================
DEBUG_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)dbgmenu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgtmenu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgtmitm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgtwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)debugger$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbghelp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgtarr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgthsh$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgtobj$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tbrwtext$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgaltd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgwa$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgbrwsr$(OBJEXT)

#===============================================================================
# COMMON.LIB rules
#===============================================================================
COMMON_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbarch$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbfopen$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbfsapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbgete$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbhash$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbmem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbstr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbprintf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbtrace$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbver$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)expropt1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)expropt2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)reserved$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbdate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbwince$(OBJEXT)

#===============================================================================
# VM.LIB rules
#===============================================================================
VM_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)arrays$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)arrayshb$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)asort$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)classes$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)classesc$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)codebloc$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)debug$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)do$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dynlibhb$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dynsym$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)errorapi$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)estack$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)eval$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)evalhb$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)extend$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fm$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)garbage$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hash$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hvm$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)initexit$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)itemapi$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fastitem$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)macro$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)memvars$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pcount$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)proc$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pvalue$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)set$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strapi$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)thread$(HB_MT)$(OBJEXT)

#===============================================================================
# VMMAIN Object Files, Only Linked On Static Libraries
#===============================================================================
VM_COMMON_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)cmdarg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbgentry$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbi18n$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)inet$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)break$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)initsymb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)memvclip$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)runner$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)throw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbinit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)errlogmt$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)lognetmt$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trpcvmmt$(HB_MT)$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bkgtsks$(HB_MT)$(OBJEXT)

VM_MAIN_OBJS=\
#	$(OBJ_DIR)$(DIR_SEP)maindll$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mainstd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mainwin$(OBJEXT)

#===============================================================================
# GTCGI.LIB rules
#===============================================================================
GTCGI_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtcgi$(OBJEXT)

#===============================================================================
# GTDOS.LIB rules
#===============================================================================
GTDOS_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtdos$(OBJEXT)

#===============================================================================
# GTPCA.LIB rules
#===============================================================================
GTPCA_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtpca$(OBJEXT)

#===============================================================================
# GTSTD.LIB rules
#===============================================================================
GTSTD_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtstd$(OBJEXT)

#===============================================================================
# GTWIN.LIB rules
#===============================================================================
GTWIN_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtwin$(OBJEXT)

#===============================================================================
# GTWVT.LIB rules
#===============================================================================
GTWVT_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtwvt$(OBJEXT)

#===============================================================================
# GTGUI.LIB rules
#===============================================================================
GTGUI_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtgui$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gtdef$(OBJEXT)

#===============================================================================
# GTALLEG.LIB rules
#===============================================================================
GTALLEG_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtalleg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ssf$(OBJEXT)

#===============================================================================
# TIP.LIB rules
#===============================================================================
TIP_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)base64x$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)utils$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)base64$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)client$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)credent$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)encb64$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)encoder$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)encqp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)encurl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ftpcln$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)httpcln$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mail$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)popcln$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)smtpcln$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)thtml$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sendmail$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)url$(OBJEXT)

#===============================================================================
# CODEPAGE.LIB rules
#===============================================================================
CODEPAGE_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)cpbg866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpbgiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpbgmik$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpbgwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpcs852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpcsiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpcskam$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpcswin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpde850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpdeiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpdewin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpel737$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpelwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpes850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpes850c$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpesiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpesisoc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpeswin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpeswinc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpeswinm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpfr850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpfriso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpfrwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphr437$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphr852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphrwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphu852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphu852s$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphuiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphuisos$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphuwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cphuwins$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpit437$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpit850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpitisb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpitiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpitwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpltwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cppl852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cppliso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpplmaz$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpplwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cppt850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpptiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpro852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cproiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cprowin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpru866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpruiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cprukoi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpruwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsk852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpskiso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpskkam$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpskwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsl437$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsl852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsliso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpslwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsrwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsv850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsvclip$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsviso$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpsvwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cptr857$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cptrwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpua1125$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpua866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpuakoi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cpuawin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1125$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1250$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1251$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1252$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1253$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1254$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1255$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1256$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1257$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1258$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc737$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc775$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc850$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc855$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc857$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc860$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc861$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc862$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc863$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc864$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc865$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc869$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc874$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc88591b$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_4$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_5$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_6$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_7$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_8$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc8859_9$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc885910$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc885911$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc885913$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc885914$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc885915$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc885916$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uckam$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uckoi8$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uckoi8u$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmaz$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmik$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc037$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1006$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc1026$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc424$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc500$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc856$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)uc875$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucascii$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucatari$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmacce$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmaccyr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmacgrk$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmacice$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmacrom$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucmactrk$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ucnext$(OBJEXT)\

#===============================================================================
# CT.LIB rules
#===============================================================================
CT_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)addascii$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)asciisum$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ascpos$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)atadjust$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)atnum$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)atrepl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)blank$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bit1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bit2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bit3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charevod$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charlist$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charmirr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charmix$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charone$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charonly$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charop$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charrepl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charsort$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)charswap$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)count$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctchksum$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctcrypt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctmath$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctmath2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctnet$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctpad$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctset$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctstr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctwfunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctwin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cursor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)datetime$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dattime2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dbftools$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)disk$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)envparam$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)exponent$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)files$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)finan$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ftoc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)intneg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)justify$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)keyset$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)lton$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)maxline$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)misc1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)misc2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)misc3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)num1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numcount$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numline$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numlohi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pack$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pos1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pos2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)posdiff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)print$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)range$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)relation$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)remove$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)replace$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)screen1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)settime$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setlast$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strdiff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strswap$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tab$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)token1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)token2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trig$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wordrepl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wordtoch$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)color$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ct$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctmisc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctrand$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dattime3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)diskutil$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fcopy$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getinfo$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getsecrt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)keysec$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)keytime$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)like$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numconv$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)screen2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)screen3$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)setkeys$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)scrmark$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)showtime$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)spread$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)trapkey$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cttime$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)util$(OBJEXT)

#===============================================================================
# HBDICT.EXE rules
#===============================================================================
HBDICT_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)hbdict$(OBJEXT)

#===============================================================================
# HBEXTERN.EXE rules
#===============================================================================
HBEXTERN_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)hbextern$(OBJEXT)

#===============================================================================
# HBFORMAT.EXE rules
#===============================================================================
HBFORMAT_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)hbformat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filereadHBFORMAT$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbfmtcls$(OBJEXT)

#===============================================================================
# HBPP.EXE rules
#===============================================================================
HBPP_EXE_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbpp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbppcalc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbppcomp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbppcore$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbpptbl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pragma$(OBJEXT)\

#===============================================================================
# PPGEN.EXE rules
#===============================================================================
PPGEN_EXE_OBJS=$(OBJ_DIR)$(DIR_SEP)ppgen$(OBJEXT)

#===============================================================================
# HBFILERE.EXE rules
#===============================================================================
HBFILERE_EXE_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbfilere$(OBJEXT)\

#===============================================================================
# HARBOUR.EXE rules
#===============================================================================
HARBOUR_EXE_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbmain$(OBJEXT)

COMPILER_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)harbour$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)comptool$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harboury$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cmdcheck$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbusage$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbident$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbgenerr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbpcode$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbdead$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbstripl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbfix$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genhrb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)expropta$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)exproptb$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)exproptc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbfunchk$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gencc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gencc1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gencobj$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbpcstat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbdbginf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hblbl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ppcomp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ppword$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbslex$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)harbour_expropt1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_expropt2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbstr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbgete$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbdate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbhash$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbfopen$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbfsapi$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbmem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbprintf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_hbver$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_reserved$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)harbour_pptable$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)harbour_ppcore$(OBJEXT)\
      $(OBJ_DIR)$(DIR_SEP)hbarch$(OBJEXT)

#===============================================================================
# HBRUN.EXE rules
#===============================================================================
HBRUN_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)hbrun$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)external$(OBJEXT)\

#===============================================================================
# HBTEST.EXE rules
#===============================================================================
HBTEST_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)hbtest$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_hvm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_hvma$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_math$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_date$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_str$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_stra$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_trans$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_array$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_file$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_misc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rt_ccall$(OBJEXT)

#===============================================================================
# HBDOC.EXE rules
#===============================================================================
HBDOC_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)hbdoc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genasc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)teeasc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genhpc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genhtm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genchm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genng$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genos2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)genrtf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gentrf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)html$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ng$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)os2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rtf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)troff$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fclass1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ffile1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ft_funcs$(OBJEXT)

HBDOC_PDF_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)genpdf1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pdfhbdoc$(OBJEXT)

#===============================================================================
# HBMAKE.EXE rules
#===============================================================================
HBMAKE_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)hbmake$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tmake$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbmutils$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)checks$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pickarry$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pickfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)prb_stak$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)radios$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbmlang$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)ft_funcs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fclass1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ffile1$(OBJEXT)

#===============================================================================
# XBSCRIPT.EXE rules
#===============================================================================
XBSCRIPT_EXE_OBJS=\
	$(HB_STARTUP_OBJ)\
	$(OBJ_DIR)$(DIR_SEP)xbscript$(OBJEXT)

#===============================================================================
# HBODBC.LIB rules
#===============================================================================
ODBC_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)odbc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)todbc$(OBJEXT)

#===============================================================================
# LIBMISC.LIB rules
#===============================================================================
MISC_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hb_fHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)twirlerHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numtxthuHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numtxtenHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)nconvertHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)stringpHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filereadHBMISC$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)environHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gaugeHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dateHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)timeHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)stackHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)statusHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)strfmtHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mathxHBMISC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)stringsxHBMISC$(OBJEXT)

#===============================================================================
# DLLMAIN.LIB rules
#===============================================================================
DLL_MAIN_LIB_OBJS=\
        $(OBJ_DIR)$(DIR_SEP)maindlle$(OBJEXT)

#===============================================================================
# USEDLL.LIB rules
#===============================================================================
USE_DLL_LIB_OBJS=\
        $(OBJ_DIR)$(DIR_SEP)mainstd$(OBJEXT)\
        $(OBJ_DIR)$(DIR_SEP)mainwin$(OBJEXT)\
        $(OBJ_DIR)$(DIR_SEP)usedll$(OBJEXT)

#===============================================================================
# HARBOUR.DLL Rules
#===============================================================================
HARBOUR_DLL_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)maindllh$(OBJEXT)\
	\
	$(RTL_LIB_OBJS)\
	$(PCREPOS_LIB_OBJS)\
	$(ZLIB_LIB_OBJS)\
	$(LANG_LIB_OBJS)\
	$(MACRO_LIB_OBJS)\
	$(RDD_LIB_OBJS)\
	$(DBFFPT_LIB_OBJS)\
	$(REDBFFPT_LIB_OBJS)\
	$(DBFNTX_LIB_OBJS)\
	$(DBFNSX_LIB_OBJS)\
	$(DBFCDX_LIB_OBJS)\
	$(DBFMDX_LIB_OBJS)\
	$(BMDBFCDX_LIB_OBJS)\
	$(REDBFCDX_LIB_OBJS)\
	$(SIXCDX_LIB_OBJS)\
	$(BMSIXCDX_LIB_OBJS)\
	$(HBSIX_LIB_OBJS)\
	$(HSX_LIB_OBJS)\
	$(USRRDD_LIB_OBJS)\
	$(RDDS_LIB_OBJS)\
	$(PP_LIB_OBJS)\
	$(DEBUG_LIB_OBJS)\
	$(COMMON_LIB_OBJS)\
	$(VM_LIB_OBJS)\
	$(TIP_LIB_OBJS)\
	$(CODEPAGE_LIB_OBJS)\
	$(CT_LIB_OBJS)\
	$(MISC_LIB_OBJS)\
	$(JPEG_LIB_OBJS)\
	$(PNG_LIB_OBJS)\
	\
	$(GTWIN_LIB_OBJS)\
	$(GTWVT_LIB_OBJS)\
	$(GTGUI_LIB_OBJS)

#===============================================================================
# XBSCRIPTDLL.EXE rules
#===============================================================================
XBSCRIPTDLL_EXE_OBJS=\
	$(XBSCRIPT_EXE_OBJS)

#===============================================================================
# HBRUNDLL.EXE rules
#===============================================================================
HBRUNDLL_EXE_OBJS=\
	$(HBRUN_EXE_OBJS)

#===============================================================================
# HBDOCDLL.EXE rules
#===============================================================================
HBDOCDLL_EXE_OBJS=\
	$(HBDOC_EXE_OBJS)

#===============================================================================
# HBMAKEDLL.EXE rules
#===============================================================================
HBMAKEDLL_EXE_OBJS=\
	$(HBMAKE_EXE_OBJS)

#===============================================================================
# HBTESTDLL.EXE rules
#===============================================================================
HBTESTDLL_EXE_OBJS=\
	$(HBTEST_EXE_OBJS)

#===============================================================================
# CONTRIB DEPENDENCIES
#===============================================================================

#===============================================================================
# FILEMEM.LIB dependencies
#===============================================================================
FILEMEM_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)filemem$(OBJEXT)

#===============================================================================
# GTWVG.LIB dependencies
#===============================================================================
GTWVG_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtwvg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvtcore$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvtutils$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvtclass$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvtpaint$(OBJEXT)

#===============================================================================
# GTWVW.LIB dependencies
#===============================================================================
GTWVW_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gtwvw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwdraw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwmenu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwtbar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwstbar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwpush$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwcheck$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwedit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wvwfuncs$(OBJEXT)

#===============================================================================
# FIREBIRD.LIB dependencies
#===============================================================================
FIREBIRD_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)tfirebird$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)firebird$(OBJEXT)

#===============================================================================
# FI_LIB.LIB dependencies
#===============================================================================
FREEIMAGE_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)fi_winfu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fi_wrp$(OBJEXT)

#===============================================================================
# GDLIB.LIB dependencies
#===============================================================================
GDLIB_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)gd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gdbar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gdbarcode$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gdchart$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gdimage$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gdwrp$(OBJEXT)

#===============================================================================
# HBMZIP.LIB dependencies
#===============================================================================
HBMZIP_LIB_OBJS=\
        $(OBJ_DIR)$(DIR_SEP)ioapi$(OBJEXT) \
        $(OBJ_DIR)$(DIR_SEP)mzip$(OBJEXT) \
        $(OBJ_DIR)$(DIR_SEP)unzip$(OBJEXT) \
        $(OBJ_DIR)$(DIR_SEP)hbmzip$(OBJEXT)

#===============================================================================
# HBZIP.LIB dependencies
#===============================================================================
HBZIP_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)ziparchive$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zip$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipplatform$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipstorage$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipstring$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipplatformcomm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zippathcomponent$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipmemfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipfileheader$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipnew$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipcomp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipexception$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipcompatibility$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipcentraldir$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)zipautobuffer$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)stdafx$(OBJEXT)

#===============================================================================
# LIBNF.LIB dependencies
#===============================================================================
LIBNF_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)aading$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)aavg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)acctadj$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)acctmnth$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)acctqtr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)acctweek$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)acctyear$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)adessort$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)aemaxlen$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)aeminlen$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)amedian$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)anomatch$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)any2any$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)aredit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)asum$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)at2$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bitclr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bitset$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)blink$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)byt2bit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)byt2hex$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)byteand$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)byteneg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bytenot$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)byteor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bytexor$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)calendar$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)clrsel$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cntryset$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)d2e$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)datecnfg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dayofyr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)daytobow$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dectobin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)diskfunc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dispmsg$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dosver$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)e2d$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)easter$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)elapmil$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)elapsed$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)eltime$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)findith$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)firstday$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)gcd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hex2dec$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)invclr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)isbit$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)isbiton$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)isshare$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)lastday$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)linked$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)madd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)menu1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)menutoNF$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)metaph$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)miltime$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)min2dhm$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)month$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mouse1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)netpv$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)nooccur$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ntow$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)nwlstat$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pchr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pegs$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pending$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pickday$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)popadder$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)prtesc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pvid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)qtr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rand1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)restsets$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)roundNF$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)savearr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)savesets$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)scregion$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sinkey$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sleep$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sqzn$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tbwhile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)vertmenu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)wda$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)week$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)workdays$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)woy$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xbox$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)year$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)vidmode$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)page$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tempfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)caplock$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)color2n$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)descendNF$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)numlock$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)proper$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)shift$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getvid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)chdir$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)rmdir$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)alt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ctrl$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getenvrn$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)n2color$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)origin$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)prtscr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)stod$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)kspeed$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mkdir$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)getver$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dispc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fttext$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)ftattr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)dfile$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)putkey$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)mouse$(OBJEXT)

#===============================================================================
# MYSQL.LIB dependencies
#===============================================================================
MYQSL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)mysqlrdd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tmysql$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)tsqlbrw$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mysql$(OBJEXT)

#===============================================================================
# PQSQL.LIB dependencies
#===============================================================================
PGSQL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)tpostgres$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)postgres$(OBJEXT)

#===============================================================================
# HBCC.LIB dependencies
#===============================================================================
HBCC_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hbcc$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)hbc7$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcx$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcy$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbhex$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcrc16$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcrc32HBCC$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcrpt32$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcrpt128$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbencode$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hbcctool$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)unitool$(OBJEXT)\
        \
	$(OBJ_DIR)$(DIR_SEP)bgmik$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cp1251$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cp1253$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cp852$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cp862$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)cp866$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)koi8r$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)koi8u$(OBJEXT)

#===============================================================================
# TELEPATH.LIB dependencies
#===============================================================================
TELEPATH_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)win32$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)telepath$(OBJEXT)

#===============================================================================
# XWTLIB.LIB dependencies
#===============================================================================
XWT_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)xwt_api$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)button$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)bcolumn$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)browseXWT$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)checkboxXWT$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)container$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)event$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)filesel$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)fontsel$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)framewindow$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)grid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hex$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)image$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)inputmask$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)label$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)laycontainer$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)layout$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)menu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)menuitem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)msgbox$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pane$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)radiobutton$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)splitter$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)textbox$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)togglebutton$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)treeitem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)treelist$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)viewport$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)widget$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)window$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)xwt_win$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xwt_win_menu$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xwt_win_menuitem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xwt_win_framewnd$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)xwt_win_msgbox$(OBJEXT)

#===============================================================================
# PNG.LIB dependencies
#===============================================================================
PNG_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)png$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngerror$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngget$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngmem$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngpread$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngread$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngrio$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngrtran$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngrutil$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngset$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngtrans$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngwio$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngwrite$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngwtran$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)pngwutil$(OBJEXT)

#===============================================================================
# HBHPDF.LIB dependencies
#===============================================================================
HBHPDF_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)harupdf$(OBJEXT)

LIBHARU_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)hpdf_utils$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_error$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_mmgr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_list$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_streams$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_objects$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_null$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_boolean$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_number$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_real$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_name$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_array$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_dict$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_xref$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encoder$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_string$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_binary$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encrypt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encryptdict$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_tt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_type1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_base14$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_cid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_font$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_font_type1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_font_tt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_font_cid$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_doc$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_info$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_catalog$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_page_label$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_gstate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_pages$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_page_operator$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_destination$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_annotation$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_outline$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_image$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_jp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_kr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_cns$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_cnt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_jp$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_kr$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_cns$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_cnt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_image_png$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_doc_png$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_ext_gstate$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_namedict$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_pdfa$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_3dmeasure$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_utf$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_exdata$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_image_ccitt$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)hpdf_u3d$(OBJEXT)

#===============================================================================
# TIPSSL.LIB rules
#===============================================================================
TIPSSL_LIB_OBJS=\
	$(OBJ_DIR)$(DIR_SEP)inetssl$(OBJEXT)\
	\
	$(OBJ_DIR)$(DIR_SEP)client1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)httpcln1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)mail1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)popcln1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)smtpcln1$(OBJEXT)\
	$(OBJ_DIR)$(DIR_SEP)sendmail1$(OBJEXT)

#===============================================================================
# Build Target
#===============================================================================
ST_PROJECT=\
	$(HBRM_EXE)\
	$(COMMON_LIB)\
	$(PPGEN_EXE)\
	$(PP_LIB)\
	$(COMPILER_LIB)\
	$(HBRC_EXE)\
	$(HARBOUR_EXE)\
	$(VM_LIB)\
	$(RTL_LIB)\
	$(MACRO_LIB)\
	$(RDD_LIB)\
	$(TIP_LIB)\
	$(DBFFPT_LIB)\
	$(REDBFFPT_LIB)\
	$(DBFNTX_LIB)\
	$(DBFNSX_LIB)\
	$(DBFCDX_LIB)\
	$(DBFMDX_LIB)\
	$(BMDBFCDX_LIB)\
	$(REDBFCDX_LIB)\
	$(SIXCDX_LIB)\
	$(BMSIXCDX_LIB)\
	$(HBSIX_LIB)\
	$(HSX_LIB)\
	$(USRRDD_LIB)\
	$(RDDS_LIB)\
	$(CT_LIB)\
	$(PCREPOS_LIB)\
	$(HB_GT_LIBS)\
	$(DEBUG_LIB)\
	$(LANG_LIB)\
	$(NULSYS_LIB)\
	$(CODEPAGE_LIB)\
	$(ZLIB_LIB)\
	$(DLL_MAIN_LIB)\
	$(USE_DLL_LIB)\
	$(ODBC_LIB)\
	$(MISC_LIB)\
	$(HBCGI_LIB)\
	$(JPEG_LIB)\
	$(TIFF_LIB)\
	$(PDFLITE_LIB)\
	$(PNG_LIB)\
	$(HBPP_EXE)\
	$(HBDOC_EXE)\
	$(HBMAKE_EXE)\
	$(XBSCRIPT_EXE)\
	$(HBTEST_EXE)\
	$(HBRUN_EXE)\
	$(HBDICT_EXE)\
	$(HBEXTERN_EXE)\
	$(HBFORMAT_EXE)

MT_PROJECT=\
	$(VM_LIB)\
	$(HBTEST_EXE)\
	$(HBRUN_EXE)

DLL_PROJECT=\
	$(HARBOUR_DLL)\
	$(DMC_IMPORT_LIB)\
	$(HBDOCDLL_EXE)\
	$(HBRUNDLL_EXE)\
	$(HBTESTDLL_EXE)\
	$(HBMAKEDLL_EXE)\
	$(XBSCRIPTDLL_EXE)

CONTRIB_PROJECT=\
	$(FILEMEM_LIB)\
	$(GDLIB_LIB)\
	$(LIBNF_LIB)\
	$(HBCC_LIB)\
	$(HBHPDF_LIB)\
	$(SEVENZIP_LIB)\
	$(HBZEBRA_LIB)\
	$(HBMLZO_LIB)\
	$(HBLZF_LIB)\
	$(HBBZ2_LIB)\
	$(RDDSQL_LIB)\
	$(SDDSQLT3_LIB)\
	$(SDDODBC_LIB)\
	$(FREEIMAGE_LIB)\
	$(HBMZIP_LIB)\
	$(LIBHARU_LIB)\
	$(HBEXPAT_LIB)\
	$(HBTINYMT_LIB)\
	$(HBBTREE_LIB)\
	$(SIXAPI_LIB)\
	$(HBCAB_LIB)\
	$(HBCOMM_LIB)\
	$(HBMXML_LIB)\
	$(HBXDIFF_LIB)

