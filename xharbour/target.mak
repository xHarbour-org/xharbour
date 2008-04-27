#===============================================================================
#
# $Id: makefile.bc,v 1.218 2008/04/14 06:06:20 andijahja Exp $
#
# FILE : target.mak
# NOTES: This file is used by all C/C++ compilers under Windows Platform whose
#        batch files are available in the root directory.
#
#===============================================================================

!ifdef HB_GTALLEG
HB_GT_LIBS=\
	$(HB_GT_LIBS)\
	$(GTALLEG_LIB)
!endif

!ifndef HB_THREAD_SUPPORT
PROJECT=\
	$(COMMON_LIB)\
	$(PPGEN_EXE)\
	$(PP_LIB)\
	$(HARBOUR_EXE)
!else
PROJECT=\
	$(PP_LIB)
!endif

PROJECT=\
	$(PROJECT)\
	$(VM_LIB)\
	$(FMSTAT_LIB)\
	$(RTL_LIB)\
	$(MACRO_LIB)\
	$(RDD_LIB)\
	$(TIP_LIB)\
	$(DBFFPT_LIB)\
	$(DBFNTX_LIB)\
	$(DBFCDX_LIB)\
	$(BMDBFCDX_LIB)\
	$(SIXCDX_LIB)\
	$(BMSIXCDX_LIB)\
	$(HBSIX_LIB)\
	$(HSX_LIB)\
	$(USRRDD_LIB)\
	$(RDDS_LIB)\
	$(CT_LIB)

!ifndef HB_THREAD_SUPPORT
PROJECT=\
	$(PROJECT)\
	$(PCREPOS_LIB)\
	$(HB_GT_LIBS)\
	$(DEBUG_LIB)\
	$(LANG_LIB)\
	$(NULSYS_LIB)\
	$(CODEPAGE_LIB)\
	$(ZLIB_LIB)\
	$(DLL_MAIN_LIB)\
	$(ODBC_LIB)\
	$(MISC_LIB)\
	$(HBPP_EXE)\
	$(HBDOC_EXE)\
	$(HBMAKE_EXE)\
	$(XBSCRIPT_EXE)
!endif

PROJECT=\
	$(PROJECT)\
	$(HBTEST_EXE)\
	$(HBRUN_EXE)

ALL: $(PROJECT)
