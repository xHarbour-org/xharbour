#
# $Id: common.mak,v 1.1 2006/07/20 20:15:34 map Exp $
#

#**********************************************************
#
# Common makefile.bc and makefile.vc definitions
#
#**********************************************************

#
# Macro to show/hide executed commands
#
!if "$(HB_BUILD_VERBOSE)" != "yes"
.SILENT:
!endif

#**********************************************************

#
# Directory macros. These should never have to change.
#

BIN_DIR = bin\$(CC_DIRNAME)
OBJ_DIR = obj\$(CC_DIRNAME)$(HB_MT)
LIB_DIR = lib\$(CC_DIRNAME)

FMSTAT_OBJ_DIR = $(OBJ_DIR)\fmstat

DLL_ROOTDIR = obj\dll
DLL_OBJ_DIR = $(DLL_ROOTDIR)\$(CC_DIRNAME)

# Targets Destination Directories
HB_DEST_DIRS = \
    bin lib obj     \
    $(BIN_DIR)      \
    $(OBJ_DIR)      \
    $(LIB_DIR)      \
    \
    $(FMSTAT_OBJ_DIR)\
    \
    $(DLL_ROOTDIR)  \
    $(DLL_OBJ_DIR)

!ifdef HB_MT
!endif

COMMON_DIR   = source\common
PP_DIR       = source\pp
VM_DIR       = source\vm
RTL_DIR      = source\rtl
MACRO_DIR    = source\macro
DEBUG_DIR    = source\debug
LANG_DIR     = source\lang
CODEPAGE_DIR = source\codepage
RDD_DIR      = source\rdd
NULSYS_DIR   = source\rdd\nulsys
DBFNTX_DIR   = source\rdd\dbfntx
DBFCDX_DIR   = source\rdd\dbfcdx
DBFFPT_DIR   = source\rdd\dbffpt
HBSIX_DIR    = source\rdd\hbsix
HSX_DIR      = source\rdd\hsx
USRRDD_DIR   = source\rdd\usrrdd
RDDS_DIR     = source\rdd\usrrdd\rdds

GTNUL_DIR    = source\rtl\gtnul
GTCGI_DIR    = source\rtl\gtcgi
GTPCA_DIR    = source\rtl\gtpca
GTSTD_DIR    = source\rtl\gtstd
GTWIN_DIR    = source\rtl\gtwin
GTWVT_DIR    = source\rtl\gtwvt
GTGUI_DIR    = source\rtl\gtgui
GTALG_DIR    = source\rtl\gtalleg

PCREPOS_DIR  = source\rtl\pcre
MISC_DIR     = source\misc
TIP_DIR      = source\tip
ODBC_DIR     = source\odbc
CT_DIR       = source\ct

HARBOUR_DIR  = source\compiler
HBPP_DIR     = utils\hbpp
HBRUN_DIR    = utils\hbrun
HBTEST_DIR   = utils\hbtest
HBDOC_DIR    = utils\hbdoc
HBMAKE_DIR   = utils\hbmake
XBSCRIPT_DIR = utils\xbscript

!ifdef HB_DOC_PDF
HBPDF_DIR   = contrib\pdflib
!endif

#**********************************************************

# Where Bcc-Make should look for C and PRG sources
ALL_LIB_SRC_DIRS_TMP=\
$(OBJ_DIR);\
$(COMMON_DIR);\
$(PP_DIR);\
$(VM_DIR);\
$(RTL_DIR);\
$(MACRO_DIR);\
$(DEBUG_DIR);\
$(LANG_DIR);\
$(CODEPAGE_DIR);\
$(RDD_DIR);\
$(NULSYS_DIR);\
$(DBFNTX_DIR);\
$(DBFCDX_DIR);\
$(DBFFPT_DIR);\
$(HBSIX_DIR);\
$(HSX_DIR);\
$(USRRDD_DIR);\
$(RDDS_DIR);\
\
$(GTNUL_DIR);\
$(GTCGI_DIR);\
$(GTPCA_DIR);\
$(GTSTD_DIR);\
$(GTWIN_DIR);\
$(GTWVT_DIR);\
$(GTGUI_DIR);\
$(GTALG_DIR);\
\
$(PCREPOS_DIR);\
$(MISC_DIR);\
$(ODBC_DIR);\
$(TIP_DIR);\
$(CT_DIR)\

ALL_EXE_SRC_DIRS_TMP=\
$(HBRUN_DIR);\
$(HBTEST_DIR);\
$(HBDOC_DIR);\
$(HBMAKE_DIR);\
$(XBSCRIPT_DIR)\

ALL_SRC_DIRS_TMP=\
$(ALL_LIB_SRC_DIRS_TMP);\
$(HARBOUR_DIR);\
$(HBPP_DIR);\
$(ALL_EXE_SRC_DIRS_TMP)\

ALL_LIB_SRC_DIRS = $(ALL_LIB_SRC_DIRS_TMP: =)
ALL_EXE_SRC_DIRS = $(ALL_EXE_SRC_DIRS_TMP: =)
ALL_SRC_DIRS     = $(ALL_SRC_DIRS_TMP: =)

#**********************************************************
#**********************************************************
#**********************************************************

#
# Macros to define our library and executable names
#

COMMON_LIB   = $(LIB_DIR)\common.lib
PP_LIB       = $(LIB_DIR)\pp$(HB_MT).lib
VM_LIB       = $(LIB_DIR)\vm$(HB_MT).lib
RTL_LIB      = $(LIB_DIR)\rtl$(HB_MT).lib
MACRO_LIB    = $(LIB_DIR)\macro$(HB_MT).lib
DEBUG_LIB    = $(LIB_DIR)\debug.lib
LANG_LIB     = $(LIB_DIR)\lang.lib
CODEPAGE_LIB = $(LIB_DIR)\codepage.lib
RDD_LIB      = $(LIB_DIR)\rdd$(HB_MT).lib
NULSYS_LIB   = $(LIB_DIR)\nulsys.lib
DBFNTX_LIB   = $(LIB_DIR)\dbfntx$(HB_MT).lib
DBFCDX_LIB   = $(LIB_DIR)\dbfcdx$(HB_MT).lib
DBFFPT_LIB   = $(LIB_DIR)\dbffpt$(HB_MT).lib
HBSIX_LIB    = $(LIB_DIR)\hbsix$(HB_MT).lib
HSX_LIB      = $(LIB_DIR)\hsx$(HB_MT).lib
USRRDD_LIB   = $(LIB_DIR)\usrrdd$(HB_MT).lib
RDDS_LIB     = $(LIB_DIR)\rdds$(HB_MT).lib

GTNUL_LIB    = $(LIB_DIR)\gtnul.lib
GTCGI_LIB    = $(LIB_DIR)\gtcgi.lib
GTPCA_LIB    = $(LIB_DIR)\gtpca.lib
GTSTD_LIB    = $(LIB_DIR)\gtstd.lib
GTWIN_LIB    = $(LIB_DIR)\gtwin.lib
GTWVT_LIB    = $(LIB_DIR)\gtwvt.lib
GTGUI_LIB    = $(LIB_DIR)\gtgui.lib
GTALG_LIB    = $(LIB_DIR)\gtalleg.lib

FMSTAT_LIB   = $(LIB_DIR)\fmstat$(HB_MT).lib
DLLMAIN_LIB  = $(LIB_DIR)\dllmain.lib

PCREPOS_LIB  = $(LIB_DIR)\pcrepos.lib
MISC_LIB     = $(LIB_DIR)\libmisc.lib
ODBC_LIB     = $(LIB_DIR)\hbodbc.lib
TIP_LIB      = $(LIB_DIR)\tip$(HB_MT).lib
CT_LIB       = $(LIB_DIR)\ct$(HB_MT).lib

HARBOUR_EXE  = $(BIN_DIR)\harbour.exe
HBPP_EXE     = $(BIN_DIR)\hbpp.exe
HBRUN_EXE    = $(BIN_DIR)\hbrun.exe
HBTEST_EXE   = $(BIN_DIR)\hbtest.exe
HBDOC_EXE    = $(BIN_DIR)\hbdoc.exe
HBMAKE_EXE   = $(BIN_DIR)\hbmake.exe
XBSCRIPT_EXE = $(BIN_DIR)\xbscript.exe

HARBOUR_DLL  = $(BIN_DIR)\harbour-$(CC_DIRNAME).dll
HBTESTDLL_EXE= $(BIN_DIR)\hbtest-dll.exe

#**********************************************************

#
# WinOS's GT driver list
#

HB_GT_LIBS = \
    $(GTNUL_LIB) \
    $(GTCGI_LIB) \
    $(GTPCA_LIB) \
    $(GTSTD_LIB) \
    $(GTWIN_LIB) \
    $(GTWVT_LIB) \
    $(GTGUI_LIB)

!if "$(HB_GTALLEG)" == "yes"
HB_GT_LIBS = $(HB_GT_LIBS) $(GTALG_LIB)
!endif

!ifndef HB_GT_LIB
HB_GT_LIB = $(GTWIN_LIB)
!else
HB_GT_LIB = $(LIB_DIR)\$(HB_GT_LIB).lib
!endif

#**********************************************************
#**********************************************************
#**********************************************************

# Standard Libs for HB-based executables
STANDARD_STATIC_HBLIBS = \
    $(COMMON_LIB)     \
    $(PP_LIB)         \
    $(VM_LIB)         \
    $(RTL_LIB)        \
    $(HB_GT_LIB)      \
    $(LANG_LIB)       \
    $(RDD_LIB)        \
    $(MACRO_LIB)      \
    $(DEBUG_LIB)      \
    $(DBFNTX_LIB)     \
    $(DBFCDX_LIB)     \
    $(DBFFPT_LIB)     \
    $(HBSIX_LIB)      \
    $(HSX_LIB)        \
    $(USRRDD_LIB)     \
    $(PCREPOS_LIB)    \

#**********************************************************
#**********************************************************
#**********************************************************

# OBJECT LIST definitions

#**********************************************************

COMMON_LIB_OBJS = \
    $(OBJ_DIR)\hbfhnd.obj \
    $(OBJ_DIR)\hbfsapi.obj \
    $(OBJ_DIR)\hbgete.obj \
    $(OBJ_DIR)\hbhash.obj \
    $(OBJ_DIR)\hbstr.obj \
    $(OBJ_DIR)\hbtrace.obj \
    $(OBJ_DIR)\hbver.obj \
    $(OBJ_DIR)\expropt1.obj \
    $(OBJ_DIR)\expropt2.obj \
    $(OBJ_DIR)\reserved.obj \

#**********************************************************

PP_LIB_OBJS = \
    $(OBJ_DIR)\ppcore.obj   \
    $(OBJ_DIR)\ppcomp.obj   \
    $(OBJ_DIR)\pplib.obj    \
    $(OBJ_DIR)\pptable.obj  \
    $(OBJ_DIR)\pragma.obj   \

#**********************************************************

# VM Objects common for STATIC and SHARED library
VM_COMMON_LIB_OBJS = \
    $(OBJ_DIR)\arrays.obj \
    $(OBJ_DIR)\arrayshb.obj \
    $(OBJ_DIR)\asort.obj \
    $(OBJ_DIR)\break.obj \
    $(OBJ_DIR)\classes.obj \
    $(OBJ_DIR)\cmdarg.obj \
    $(OBJ_DIR)\codebloc.obj \
    $(OBJ_DIR)\dbgentry.obj \
    $(OBJ_DIR)\debug.obj \
    $(OBJ_DIR)\do.obj \
    $(OBJ_DIR)\dynlibhb.obj \
    $(OBJ_DIR)\dynsym.obj \
    $(OBJ_DIR)\errorapi.obj \
    $(OBJ_DIR)\estack.obj \
    $(OBJ_DIR)\eval.obj \
    $(OBJ_DIR)\evalhb.obj \
    $(OBJ_DIR)\extend.obj \
    $(OBJ_DIR)\fm.obj \
    $(OBJ_DIR)\garbage.obj \
    $(OBJ_DIR)\hash.obj \
    $(OBJ_DIR)\hbi18n.obj \
    $(OBJ_DIR)\hvm.obj \
    $(OBJ_DIR)\inet.obj \
    $(OBJ_DIR)\initexit.obj \
    $(OBJ_DIR)\initsymb.obj \
    $(OBJ_DIR)\itemapi.obj \
    $(OBJ_DIR)\fastitem.obj \
    $(OBJ_DIR)\macro.obj \
    $(OBJ_DIR)\memvars.obj \
    $(OBJ_DIR)\memvclip.obj \
    $(OBJ_DIR)\pcount.obj \
    $(OBJ_DIR)\proc.obj \
    $(OBJ_DIR)\pvalue.obj \
    $(OBJ_DIR)\runner.obj \
    $(OBJ_DIR)\thread.obj \
    $(OBJ_DIR)\throw.obj \
    \
    $(OBJ_DIR)\harbinit.obj \

# Specific VM Objects for building STATIC library
VM_STATIC_LIB_OBJS = \
    $(OBJ_DIR)\mainstd.obj  \
    $(OBJ_DIR)\mainwin.obj  \

# Specific VM Objects for building SHARED (DLL) library
VM_SHARED_LIB_OBJS = \
    $(OBJ_DIR)\maindllh.obj \

# All VM Objects for building STATIC library
VM_LIB_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_STATIC_LIB_OBJS)

# All VM Objects for building SHARED (DLL) library
VM_DLL_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_SHARED_LIB_OBJS)

DISABLED_VM_OBJS = \
    $(OBJ_DIR)\maindllp.obj \
    $(OBJ_DIR)\maindll.obj  \

#**********************************************************

RTL_LIB_OBJS = \
    $(OBJ_DIR)\abs.obj \
    $(OBJ_DIR)\accept.obj \
    $(OBJ_DIR)\ampm.obj \
    $(OBJ_DIR)\at.obj \
    $(OBJ_DIR)\bkgtsks.obj \
    $(OBJ_DIR)\binnum.obj \
    $(OBJ_DIR)\binnumx.obj \
    $(OBJ_DIR)\box.obj \
    $(OBJ_DIR)\cdpapi.obj \
    $(OBJ_DIR)\chrasc.obj \
    $(OBJ_DIR)\colorind.obj \
    $(OBJ_DIR)\console.obj \
    $(OBJ_DIR)\copyfile.obj \
    $(OBJ_DIR)\datec.obj \
    $(OBJ_DIR)\dates.obj \
    $(OBJ_DIR)\dateshb.obj \
    $(OBJ_DIR)\dbf2txt.obj \
    $(OBJ_DIR)\defpath.obj \
    $(OBJ_DIR)\descend.obj \
    $(OBJ_DIR)\dirdrive.obj \
    $(OBJ_DIR)\direct.obj \
    $(OBJ_DIR)\diskspac.obj \
    $(OBJ_DIR)\disksphb.obj \
    $(OBJ_DIR)\dllcall.obj \
    $(OBJ_DIR)\empty.obj \
    $(OBJ_DIR)\file.obj \
    $(OBJ_DIR)\filehb.obj \
    $(OBJ_DIR)\filestat.obj \
    $(OBJ_DIR)\filesys.obj \
    $(OBJ_DIR)\fserror.obj \
    $(OBJ_DIR)\fkmax.obj \
    $(OBJ_DIR)\fnsplit.obj \
    $(OBJ_DIR)\fparse.obj \
    $(OBJ_DIR)\fssize.obj \
    $(OBJ_DIR)\fstemp.obj \
    $(OBJ_DIR)\gete.obj \
    $(OBJ_DIR)\gt.obj \
    $(OBJ_DIR)\gtapi.obj \
    $(OBJ_DIR)\gtapiu.obj \
    $(OBJ_DIR)\gtgraph.obj \
    $(OBJ_DIR)\gx.obj \
    $(OBJ_DIR)\hardcr.obj \
    $(OBJ_DIR)\hbbitf.obj \
    $(OBJ_DIR)\hbchksum.obj \
    $(OBJ_DIR)\hbcomprs.obj \
    $(OBJ_DIR)\hbcrc32.obj \
    $(OBJ_DIR)\hbcrypt.obj \
    $(OBJ_DIR)\hbffind.obj \
    $(OBJ_DIR)\hbhex2n.obj \
    $(OBJ_DIR)\hbmd5.obj \
    $(OBJ_DIR)\hboutdbg.obj \
    $(OBJ_DIR)\hbrandom.obj \
    $(OBJ_DIR)\hbserv.obj \
    $(OBJ_DIR)\hbsrlraw.obj \
    $(OBJ_DIR)\hbsyslog.obj \
    $(OBJ_DIR)\hbxml.obj \
    $(OBJ_DIR)\idle.obj \
    $(OBJ_DIR)\inkey.obj \
    $(OBJ_DIR)\is.obj \
    $(OBJ_DIR)\isprint.obj \
    $(OBJ_DIR)\langapi.obj \
    $(OBJ_DIR)\left.obj \
    $(OBJ_DIR)\len.obj \
    $(OBJ_DIR)\lennum.obj \
    $(OBJ_DIR)\math.obj \
    $(OBJ_DIR)\maxrow.obj \
    $(OBJ_DIR)\memofile.obj \
    $(OBJ_DIR)\minmax.obj \
    $(OBJ_DIR)\mlctopos.obj \
    $(OBJ_DIR)\mpostolc.obj \
    $(OBJ_DIR)\mod.obj \
    $(OBJ_DIR)\mouseapi.obj \
    $(OBJ_DIR)\mousex.obj \
    $(OBJ_DIR)\mtran.obj \
    $(OBJ_DIR)\natmsg.obj \
    $(OBJ_DIR)\net.obj \
    $(OBJ_DIR)\oemansi.obj \
    $(OBJ_DIR)\oldbox.obj \
    $(OBJ_DIR)\oldclear.obj \
    $(OBJ_DIR)\pad.obj \
    $(OBJ_DIR)\padc.obj \
    $(OBJ_DIR)\padl.obj \
    $(OBJ_DIR)\padr.obj \
    $(OBJ_DIR)\philes.obj \
    $(OBJ_DIR)\philes53.obj \
    $(OBJ_DIR)\philesx.obj \
    $(OBJ_DIR)\rat.obj \
    $(OBJ_DIR)\readline.obj \
    $(OBJ_DIR)\regex.obj \
    $(OBJ_DIR)\replic.obj \
    $(OBJ_DIR)\right.obj \
    $(OBJ_DIR)\round.obj \
    $(OBJ_DIR)\run.obj \
    $(OBJ_DIR)\samples.obj \
    $(OBJ_DIR)\saverest.obj \
    $(OBJ_DIR)\scroll.obj \
    $(OBJ_DIR)\seconds.obj \
    $(OBJ_DIR)\set.obj \
    $(OBJ_DIR)\readexit.obj \
    $(OBJ_DIR)\readins.obj \
    $(OBJ_DIR)\setcolor.obj \
    $(OBJ_DIR)\setcurs.obj \
    $(OBJ_DIR)\setkey.obj \
    $(OBJ_DIR)\setpos.obj \
    $(OBJ_DIR)\setposbs.obj \
    $(OBJ_DIR)\shadow.obj \
    $(OBJ_DIR)\soundex.obj \
    $(OBJ_DIR)\space.obj \
    $(OBJ_DIR)\spfiles.obj \
    $(OBJ_DIR)\str.obj \
    $(OBJ_DIR)\str2ptr.obj \
    $(OBJ_DIR)\strcase.obj \
    $(OBJ_DIR)\strdel.obj \
    $(OBJ_DIR)\strings.obj \
    $(OBJ_DIR)\strmatch.obj \
    $(OBJ_DIR)\strpeek.obj \
    $(OBJ_DIR)\strtran.obj \
    $(OBJ_DIR)\strzero.obj \
    $(OBJ_DIR)\stuff.obj \
    $(OBJ_DIR)\substr.obj \
    $(OBJ_DIR)\teditorl.obj \
    $(OBJ_DIR)\tone.obj \
    $(OBJ_DIR)\tprinter.obj \
    $(OBJ_DIR)\trace.obj \
    $(OBJ_DIR)\transfrm.obj \
    $(OBJ_DIR)\trim.obj \
    $(OBJ_DIR)\txtline.obj \
    $(OBJ_DIR)\type.obj \
    $(OBJ_DIR)\val.obj \
    $(OBJ_DIR)\valtostr.obj \
    $(OBJ_DIR)\valtype.obj \
    $(OBJ_DIR)\version.obj \
    $(OBJ_DIR)\word.obj \
    $(OBJ_DIR)\xhelp.obj \
    $(OBJ_DIR)\xsavescr.obj \
    \
    $(OBJ_DIR)\achoice.obj \
    $(OBJ_DIR)\adir.obj \
    $(OBJ_DIR)\alert.obj \
    $(OBJ_DIR)\arrayblk.obj \
    $(OBJ_DIR)\browdb.obj \
    $(OBJ_DIR)\browdbx.obj \
    $(OBJ_DIR)\browse.obj \
    $(OBJ_DIR)\checkbox.obj \
    $(OBJ_DIR)\color53.obj \
    $(OBJ_DIR)\cstr.obj \
    $(OBJ_DIR)\cstruct.obj \
    $(OBJ_DIR)\dbedit.obj \
    $(OBJ_DIR)\decode.obj \
    $(OBJ_DIR)\devoutp.obj \
    $(OBJ_DIR)\dircmd.obj \
    $(OBJ_DIR)\dummy.obj \
    $(OBJ_DIR)\dumpvar.obj \
    $(OBJ_DIR)\error.obj \
    $(OBJ_DIR)\errorsys.obj \
    $(OBJ_DIR)\fieldbl.obj \
    $(OBJ_DIR)\getlist.obj \
    $(OBJ_DIR)\getsys.obj \
    $(OBJ_DIR)\hbini.obj \
    $(OBJ_DIR)\hblog.obj \
    $(OBJ_DIR)\hblognet.obj \
    $(OBJ_DIR)\hbserial.obj \
    $(OBJ_DIR)\input.obj \
    $(OBJ_DIR)\listbox.obj \
    $(OBJ_DIR)\memoedit.obj \
    $(OBJ_DIR)\memvarbl.obj \
    $(OBJ_DIR)\menuto.obj \
    $(OBJ_DIR)\mssgline.obj \
    $(OBJ_DIR)\objfunc.obj \
    $(OBJ_DIR)\perfuncs.obj \
    $(OBJ_DIR)\persist.obj \
    $(OBJ_DIR)\profiler.obj \
    $(OBJ_DIR)\pushbtn.obj \
    $(OBJ_DIR)\radiobtn.obj \
    $(OBJ_DIR)\radiogrp.obj \
    $(OBJ_DIR)\readkey.obj \
    $(OBJ_DIR)\readvar.obj \
    $(OBJ_DIR)\regexrpl.obj \
    $(OBJ_DIR)\scrollbr.obj \
    $(OBJ_DIR)\setfunc.obj \
    $(OBJ_DIR)\setta.obj \
    $(OBJ_DIR)\sprintf.obj \
    $(OBJ_DIR)\stream.obj \
    $(OBJ_DIR)\tbcolumn.obj \
    $(OBJ_DIR)\tbrowse.obj \
    $(OBJ_DIR)\tclass.obj \
    $(OBJ_DIR)\teditor.obj \
    $(OBJ_DIR)\terror.obj \
    $(OBJ_DIR)\text.obj \
    $(OBJ_DIR)\tget.obj \
    $(OBJ_DIR)\tgetint.obj \
    $(OBJ_DIR)\tgetlist.obj \
    $(OBJ_DIR)\tlabel.obj \
    $(OBJ_DIR)\tmenuitm.obj \
    $(OBJ_DIR)\tobject.obj \
    $(OBJ_DIR)\tpopup.obj \
    $(OBJ_DIR)\traceprg.obj \
    $(OBJ_DIR)\treport.obj \
    $(OBJ_DIR)\trpc.obj \
    $(OBJ_DIR)\trpccli.obj \
    $(OBJ_DIR)\ttable.obj \
    $(OBJ_DIR)\ttextlin.obj \
    $(OBJ_DIR)\ttopbar.obj \
    $(OBJ_DIR)\txml.obj \
    $(OBJ_DIR)\typefile.obj \
    $(OBJ_DIR)\wait.obj \
    $(OBJ_DIR)\win32ole.obj \
    $(OBJ_DIR)\win32prn.obj \
    $(OBJ_DIR)\winos.obj \
    $(OBJ_DIR)\winreg.obj \

#**********************************************************

PCREPOS_LIB_OBJS = \
    $(OBJ_DIR)\chartables.obj \
    $(OBJ_DIR)\pcreposix.obj \
    $(OBJ_DIR)\pcre_compile.obj \
    $(OBJ_DIR)\pcre_config.obj \
    $(OBJ_DIR)\pcre_dfa_exec.obj \
    $(OBJ_DIR)\pcre_exec.obj \
    $(OBJ_DIR)\pcre_fullinfo.obj \
    $(OBJ_DIR)\pcre_get.obj \
    $(OBJ_DIR)\pcre_globals.obj \
    $(OBJ_DIR)\pcre_info.obj \
    $(OBJ_DIR)\pcre_maketables.obj \
    $(OBJ_DIR)\pcre_ord2utf8.obj \
    $(OBJ_DIR)\pcre_printint.obj \
    $(OBJ_DIR)\pcre_refcount.obj \
    $(OBJ_DIR)\pcre_study.obj \
    $(OBJ_DIR)\pcre_tables.obj \
    $(OBJ_DIR)\pcre_try_flipped.obj \
    $(OBJ_DIR)\pcre_ucp_findchar.obj \
    $(OBJ_DIR)\pcre_valid_utf8.obj \
    $(OBJ_DIR)\pcre_version.obj \
    $(OBJ_DIR)\pcre_xclass.obj \
    $(OBJ_DIR)\ucp_findchar.obj \

#**********************************************************

MACRO_LIB_OBJS = \
    $(OBJ_DIR)\macroy.obj \
    $(OBJ_DIR)\macroa.obj \
    $(OBJ_DIR)\macrob.obj \
    $(OBJ_DIR)\macroc.obj \
    $(OBJ_DIR)\macroslx.obj \

#**********************************************************

DEBUG_LIB_OBJS = \
    $(OBJ_DIR)\dbgmenu.obj \
    $(OBJ_DIR)\dbgtmenu.obj \
    $(OBJ_DIR)\dbgtmitm.obj \
    $(OBJ_DIR)\dbgtwin.obj \
    $(OBJ_DIR)\debugger.obj \
    $(OBJ_DIR)\dbghelp.obj \
    $(OBJ_DIR)\dbgtarr.obj \
    $(OBJ_DIR)\dbgthsh.obj \
    $(OBJ_DIR)\dbgtobj.obj \
    $(OBJ_DIR)\tbrwtext.obj \
    $(OBJ_DIR)\dbgaltd.obj \
    $(OBJ_DIR)\dbgwa.obj \
    $(OBJ_DIR)\dbgbrwsr.obj \

#**********************************************************

LANG_LIB_OBJS = \
    $(OBJ_DIR)\msgbgwin.obj \
    $(OBJ_DIR)\msgby866.obj \
    $(OBJ_DIR)\msgbywin.obj \
    $(OBJ_DIR)\msgca.obj \
    $(OBJ_DIR)\msgcs852.obj \
    $(OBJ_DIR)\msgcsiso.obj \
    $(OBJ_DIR)\msgcskam.obj \
    $(OBJ_DIR)\msgcswin.obj \
    $(OBJ_DIR)\msgde.obj \
    $(OBJ_DIR)\msgdewin.obj \
    $(OBJ_DIR)\msgen.obj \
    $(OBJ_DIR)\msgeo.obj \
    $(OBJ_DIR)\msges.obj \
    $(OBJ_DIR)\msgeswin.obj \
    $(OBJ_DIR)\msgeu.obj \
    $(OBJ_DIR)\msgfr.obj \
    $(OBJ_DIR)\msggl.obj \
    $(OBJ_DIR)\msghe862.obj \
    $(OBJ_DIR)\msghewin.obj \
    $(OBJ_DIR)\msghr1250.obj \
    $(OBJ_DIR)\msghr437.obj \
    $(OBJ_DIR)\msghr852.obj \
    $(OBJ_DIR)\msghriso.obj \
    $(OBJ_DIR)\msghu852.obj \
    $(OBJ_DIR)\msghucwi.obj \
    $(OBJ_DIR)\msghuwin.obj \
    $(OBJ_DIR)\msgid.obj \
    $(OBJ_DIR)\msgis850.obj \
    $(OBJ_DIR)\msgit.obj \
    $(OBJ_DIR)\msgko.obj \
    $(OBJ_DIR)\msgltwin.obj \
    $(OBJ_DIR)\msgpl852.obj \
    $(OBJ_DIR)\msgpliso.obj \
    $(OBJ_DIR)\msgplmaz.obj \
    $(OBJ_DIR)\msgplwin.obj \
    $(OBJ_DIR)\msgpt.obj \
    $(OBJ_DIR)\msgro.obj \
    $(OBJ_DIR)\msgru866.obj \
    $(OBJ_DIR)\msgrukoi.obj \
    $(OBJ_DIR)\msgruwin.obj \
    $(OBJ_DIR)\msgsl852.obj \
    $(OBJ_DIR)\msgsliso.obj \
    $(OBJ_DIR)\msgslwin.obj \
    $(OBJ_DIR)\msgsr852.obj \
    $(OBJ_DIR)\msgsriso.obj \
    $(OBJ_DIR)\msgsrwin.obj \
    $(OBJ_DIR)\msgua866.obj \
    $(OBJ_DIR)\msguakoi.obj \
    $(OBJ_DIR)\msguawin.obj \
    $(OBJ_DIR)\msgzhb5.obj  \
    $(OBJ_DIR)\msgzhgb.obj  \

#**********************************************************

CODEPAGE_LIB_OBJS = \
    $(OBJ_DIR)\cpbgmik.obj  \
    $(OBJ_DIR)\cpbgwin.obj  \
    $(OBJ_DIR)\cpeldos.obj  \
    $(OBJ_DIR)\cpelwin.obj  \
    $(OBJ_DIR)\cpesdos.obj  \
    $(OBJ_DIR)\cpesmwin.obj \
    $(OBJ_DIR)\cpeswin.obj  \
    $(OBJ_DIR)\cpgedos.obj  \
    $(OBJ_DIR)\cpgewin.obj  \
    $(OBJ_DIR)\cphr1250.obj \
    $(OBJ_DIR)\cphr437.obj  \
    $(OBJ_DIR)\cphr852.obj  \
    $(OBJ_DIR)\cphu852.obj  \
    $(OBJ_DIR)\cphuwin.obj  \
    $(OBJ_DIR)\cpit437.obj  \
    $(OBJ_DIR)\cpit850.obj  \
    $(OBJ_DIR)\cpitisb.obj  \
    $(OBJ_DIR)\cpitiso.obj  \
    $(OBJ_DIR)\cpltwin.obj  \
    $(OBJ_DIR)\cppl852.obj  \
    $(OBJ_DIR)\cppliso.obj  \
    $(OBJ_DIR)\cpplmaz.obj  \
    $(OBJ_DIR)\cpplwin.obj  \
    $(OBJ_DIR)\cppt850.obj  \
    $(OBJ_DIR)\cpptiso.obj  \
    $(OBJ_DIR)\cpru866.obj  \
    $(OBJ_DIR)\cprukoi.obj  \
    $(OBJ_DIR)\cpruwin.obj  \
    $(OBJ_DIR)\cpsl852.obj  \
    $(OBJ_DIR)\cpsliso.obj  \
    $(OBJ_DIR)\cpslwin.obj  \
    $(OBJ_DIR)\cpsrwin.obj  \
    $(OBJ_DIR)\cpua866.obj  \
    $(OBJ_DIR)\cpuawin.obj  \
    $(OBJ_DIR)\cpuakoi.obj  \
    $(OBJ_DIR)\uc1250.obj   \
    $(OBJ_DIR)\uc1251.obj   \
    $(OBJ_DIR)\uc1253.obj   \
    $(OBJ_DIR)\uc1257.obj   \
    $(OBJ_DIR)\uc737.obj    \
    $(OBJ_DIR)\uc850.obj    \
    $(OBJ_DIR)\uc852.obj    \
    $(OBJ_DIR)\uc866.obj    \
    $(OBJ_DIR)\uc8859_1.obj \
    $(OBJ_DIR)\uc8859_2.obj \
    $(OBJ_DIR)\uc88591b.obj \
    $(OBJ_DIR)\uckoi8.obj   \
    $(OBJ_DIR)\uckoi8u.obj  \
    $(OBJ_DIR)\ucmaz.obj    \
    $(OBJ_DIR)\ucmik.obj    \

#**********************************************************

RDD_LIB_OBJS = \
    $(OBJ_DIR)\dbcmd.obj \
    $(OBJ_DIR)\workarea.obj \
    $(OBJ_DIR)\dbf1.obj \
    $(OBJ_DIR)\dbnubs.obj \
    $(OBJ_DIR)\delim1.obj \
    $(OBJ_DIR)\sdf1.obj \
    \
    $(OBJ_DIR)\dbdelim.obj \
    $(OBJ_DIR)\dbfuncs.obj \
    $(OBJ_DIR)\dbjoin.obj \
    $(OBJ_DIR)\dbsdf.obj \
    $(OBJ_DIR)\dbtotal.obj \
    $(OBJ_DIR)\dblist.obj \
    $(OBJ_DIR)\dbsort.obj \
    $(OBJ_DIR)\hbdbsort.obj \
    $(OBJ_DIR)\dbstrux.obj \
    $(OBJ_DIR)\dbupdat.obj \
    $(OBJ_DIR)\rddord.obj \
    $(OBJ_DIR)\rddsys.obj \

#**********************************************************

NULSYS_LIB_OBJS = \
    $(OBJ_DIR)\nulsys.obj \

#**********************************************************

DBFNTX_LIB_OBJS = \
    $(OBJ_DIR)\dbfntx1.obj  \

#**********************************************************

DBFCDX_LIB_OBJS = \
    $(OBJ_DIR)\dbfcdx1.obj  \
    $(OBJ_DIR)\sixcdx1.obj  \

#**********************************************************

DBFFPT_LIB_OBJS = \
    $(OBJ_DIR)\dbffpt1.obj  \

#**********************************************************

HBSIX_LIB_OBJS = \
    $(OBJ_DIR)\sxcompr.obj  \
    $(OBJ_DIR)\sxcrypt.obj  \
    $(OBJ_DIR)\sxdate.obj   \

#**********************************************************

HSX_LIB_OBJS = \
    $(OBJ_DIR)\hsx.obj      \
    $(OBJ_DIR)\cftsfunc.obj \

#**********************************************************

USRRDD_LIB_OBJS = \
    $(OBJ_DIR)\usrrdd.obj   \

#**********************************************************

RDDS_LIB_OBJS = \
    $(OBJ_DIR)\arrayrdd.obj \
    $(OBJ_DIR)\dbtcdx.obj \
    $(OBJ_DIR)\fcomma.obj \
    $(OBJ_DIR)\fptcdx.obj \
    $(OBJ_DIR)\hscdx.obj \
    $(OBJ_DIR)\rlcdx.obj \
    $(OBJ_DIR)\smtcdx.obj \

#**********************************************************

GTCGI_LIB_OBJS = \
    $(OBJ_DIR)\gtcgi.obj    \
    $(OBJ_DIR)\mousecgi.obj \

#**********************************************************

GTPCA_LIB_OBJS = \
    $(OBJ_DIR)\gtpca.obj    \
    $(OBJ_DIR)\mousepca.obj \

#**********************************************************

GTSTD_LIB_OBJS = \
    $(OBJ_DIR)\gtstd.obj    \
    $(OBJ_DIR)\mousestd.obj \

#**********************************************************

GTWIN_LIB_OBJS = \
    $(OBJ_DIR)\gtwin.obj    \
    $(OBJ_DIR)\mousewin.obj \

#**********************************************************

GTWVT_LIB_OBJS = \
    $(OBJ_DIR)\gtwvt.obj    \

#**********************************************************

GTGUI_LIB_COMMON_OBJS = \
    $(OBJ_DIR)\gtgui.obj    \

GTGUI_LIB_STATIC_OBJS = \
    $(OBJ_DIR)\gtdef.obj    \

GTGUI_LIB_SHARED_OBJS = \

GTGUI_LIB_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_STATIC_OBJS)
GTGUI_DLL_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_SHARED_OBJS)

#**********************************************************

GTALLEG_LIB_OBJS = \
    $(OBJ_DIR)\gtalleg.obj \
    $(OBJ_DIR)\ssf.obj     \

#**********************************************************

GTNUL_LIB_OBJS = \
    $(OBJ_DIR)\gtnul.obj \
    $(OBJ_DIR)\gtext.obj \
    $(OBJ_DIR)\gtsys.obj \

#!ifdef HB_MULTI_GT
#GTNUL_LIB_OBJS = \
#    $(GTNUL_LIB_OBJS)    \
#    $(OBJ_DIR)\gtsys.obj \
#!endif

#**********************************************************

FMSTAT_LIB_OBJS = \
   $(FMSTAT_OBJ_DIR)\fm.obj \

#**********************************************************

DLLMAIN_LIB_OBJS = \
    $(OBJ_DIR)\mainstd.obj \
    $(OBJ_DIR)\mainwin.obj \
    $(OBJ_DIR)\maindll.obj \

#**********************************************************

TIP_LIB_OBJS = \
    $(OBJ_DIR)\base64x.obj \
    $(OBJ_DIR)\utils.obj \
    \
    $(OBJ_DIR)\client.obj \
    $(OBJ_DIR)\credent.obj \
    $(OBJ_DIR)\encb64.obj \
    $(OBJ_DIR)\encoder.obj \
    $(OBJ_DIR)\encqp.obj \
    $(OBJ_DIR)\encurl.obj \
    $(OBJ_DIR)\ftpcln.obj \
    $(OBJ_DIR)\httpcln.obj \
    $(OBJ_DIR)\mail.obj \
    $(OBJ_DIR)\popcln.obj \
    $(OBJ_DIR)\smtpcln.obj \
    $(OBJ_DIR)\url.obj \

#**********************************************************

MISC_LIB_OBJS = \
    $(OBJ_DIR)\hb_f.obj \

#**********************************************************

ODBC_LIB_OBJS =       \
    $(OBJ_DIR)\odbc.obj \
    $(OBJ_DIR)\todbc.obj \

#**********************************************************

CT_LIB_OBJS = \
    $(OBJ_DIR)\addascii.obj \
    $(OBJ_DIR)\asciisum.obj \
    $(OBJ_DIR)\ascpos.obj \
    $(OBJ_DIR)\atadjust.obj \
    $(OBJ_DIR)\atnum.obj \
    $(OBJ_DIR)\atrepl.obj \
    $(OBJ_DIR)\bit1.obj \
    $(OBJ_DIR)\bit2.obj \
    $(OBJ_DIR)\bit3.obj \
    $(OBJ_DIR)\charevod.obj \
    $(OBJ_DIR)\charlist.obj \
    $(OBJ_DIR)\charmirr.obj \
    $(OBJ_DIR)\charmix.obj \
    $(OBJ_DIR)\charone.obj \
    $(OBJ_DIR)\charonly.obj \
    $(OBJ_DIR)\charop.obj \
    $(OBJ_DIR)\charrepl.obj \
    $(OBJ_DIR)\charsort.obj \
    $(OBJ_DIR)\charswap.obj \
    $(OBJ_DIR)\count.obj \
    $(OBJ_DIR)\ctc.obj \
    $(OBJ_DIR)\ctchksum.obj \
    $(OBJ_DIR)\ctcrypt.obj \
    $(OBJ_DIR)\ctmath.obj \
    $(OBJ_DIR)\ctmath2.obj \
    $(OBJ_DIR)\ctnet.obj \
    $(OBJ_DIR)\ctpad.obj \
    $(OBJ_DIR)\ctset.obj \
    $(OBJ_DIR)\ctstr.obj \
    $(OBJ_DIR)\ctwin.obj \
    $(OBJ_DIR)\datetime.obj \
    $(OBJ_DIR)\dbftools.obj \
    $(OBJ_DIR)\disk.obj \
    $(OBJ_DIR)\exponent.obj \
    $(OBJ_DIR)\files.obj \
    $(OBJ_DIR)\finan.obj \
    $(OBJ_DIR)\ftoc.obj \
    $(OBJ_DIR)\justify.obj \
    $(OBJ_DIR)\keyset.obj \
    $(OBJ_DIR)\lton.obj \
    $(OBJ_DIR)\maxline.obj \
    $(OBJ_DIR)\misc1.obj \
    $(OBJ_DIR)\misc2.obj \
    $(OBJ_DIR)\misc3.obj \
    $(OBJ_DIR)\num1.obj \
    $(OBJ_DIR)\numat.obj \
    $(OBJ_DIR)\numcount.obj \
    $(OBJ_DIR)\numline.obj \
    $(OBJ_DIR)\numlohi.obj \
    $(OBJ_DIR)\pack.obj \
    $(OBJ_DIR)\pos1.obj \
    $(OBJ_DIR)\pos2.obj \
    $(OBJ_DIR)\posdiff.obj \
    $(OBJ_DIR)\print.obj \
    $(OBJ_DIR)\range.obj \
    $(OBJ_DIR)\relation.obj \
    $(OBJ_DIR)\remove.obj \
    $(OBJ_DIR)\replace.obj \
    $(OBJ_DIR)\screen1.obj \
    $(OBJ_DIR)\setclear.obj \
    $(OBJ_DIR)\settime.obj \
    $(OBJ_DIR)\strdiff.obj \
    $(OBJ_DIR)\strfile.obj \
    $(OBJ_DIR)\strswap.obj \
    $(OBJ_DIR)\tab.obj \
    $(OBJ_DIR)\token1.obj \
    $(OBJ_DIR)\token2.obj \
    $(OBJ_DIR)\trig.obj \
    $(OBJ_DIR)\wordrepl.obj \
    $(OBJ_DIR)\wordtoch.obj \
    \
    $(OBJ_DIR)\blank.obj \
    $(OBJ_DIR)\color.obj \
    $(OBJ_DIR)\ct.obj \
    $(OBJ_DIR)\ctmisc.obj \
    $(OBJ_DIR)\dattime2.obj \
    $(OBJ_DIR)\dattime3.obj \
    $(OBJ_DIR)\diskutil.obj \
    $(OBJ_DIR)\fcopy.obj \
    $(OBJ_DIR)\getinfo.obj \
    $(OBJ_DIR)\getsecrt.obj \
    $(OBJ_DIR)\invrtwin.obj \
    $(OBJ_DIR)\keysec.obj \
    $(OBJ_DIR)\keytime.obj \
    $(OBJ_DIR)\like.obj \
    $(OBJ_DIR)\numconv.obj \
    $(OBJ_DIR)\screen2.obj \
    $(OBJ_DIR)\screen3.obj \
    $(OBJ_DIR)\setkeys.obj \
    $(OBJ_DIR)\scrmark.obj \
    $(OBJ_DIR)\spread.obj \
    $(OBJ_DIR)\untext.obj \
    $(OBJ_DIR)\cttime.obj \
    $(OBJ_DIR)\util.obj \

#**********************************************************
#**********************************************************
#**********************************************************

HARBOUR_EXE_OBJS = \
    $(OBJ_DIR)\harbour.obj \
    $(OBJ_DIR)\harboury.obj \
    $(OBJ_DIR)\cmdcheck.obj \
    $(OBJ_DIR)\hbusage.obj \
    $(OBJ_DIR)\hbident.obj \
    $(OBJ_DIR)\hbgenerr.obj \
    $(OBJ_DIR)\hbpcode.obj \
    $(OBJ_DIR)\hbdead.obj \
    $(OBJ_DIR)\hbstripl.obj \
    $(OBJ_DIR)\hbfix.obj \
    $(OBJ_DIR)\genc.obj \
    $(OBJ_DIR)\gencobj.obj \
    $(OBJ_DIR)\genobj32.obj \
    $(OBJ_DIR)\genjava.obj \
    $(OBJ_DIR)\genhrb.obj \
    $(OBJ_DIR)\expropta.obj \
    $(OBJ_DIR)\exproptb.obj \
    $(OBJ_DIR)\exproptc.obj \
    $(OBJ_DIR)\hbfunchk.obj \
    $(OBJ_DIR)\calconst.obj \
    $(OBJ_DIR)\gencc.obj \
    $(OBJ_DIR)\hbpcstat.obj \
    $(OBJ_DIR)\hbdbginf.obj \
    $(OBJ_DIR)\hblbl.obj \
    $(PP_LIB) \
    $(OBJ_DIR)\hbslex.obj \

#**********************************************************

HBPP_EXE_OBJS = \
    $(OBJ_DIR)\hbpp.obj     \
    $(OBJ_DIR)\calconst.obj \

#**********************************************************

HBRUN_EXE_OBJS = \
    $(OBJ_DIR)\hbrun.obj    \
    $(OBJ_DIR)\external.obj \

#**********************************************************

HBTEST_EXE_OBJS = \
    $(OBJ_DIR)\hbtest.obj \
    $(OBJ_DIR)\rt_hvm.obj \
    $(OBJ_DIR)\rt_hvma.obj \
    $(OBJ_DIR)\rt_math.obj \
    $(OBJ_DIR)\rt_date.obj \
    $(OBJ_DIR)\rt_str.obj \
    $(OBJ_DIR)\rt_stra.obj \
    $(OBJ_DIR)\rt_trans.obj \
    $(OBJ_DIR)\rt_array.obj \
    $(OBJ_DIR)\rt_file.obj \
    $(OBJ_DIR)\rt_misc.obj \
    $(OBJ_DIR)\rt_ccall.obj \

#**********************************************************

HBDOC_EXE_OBJS = \
    $(OBJ_DIR)\hbdoc.obj \
    $(OBJ_DIR)\genasc.obj \
    $(OBJ_DIR)\teeasc.obj \
    $(OBJ_DIR)\genhpc.obj \
    $(OBJ_DIR)\genhtm.obj \
    $(OBJ_DIR)\genchm.obj \
    $(OBJ_DIR)\genng.obj \
    $(OBJ_DIR)\genos2.obj \
    $(OBJ_DIR)\genrtf.obj \
    $(OBJ_DIR)\gentrf.obj \
    $(OBJ_DIR)\html.obj \
    $(OBJ_DIR)\ng.obj \
    $(OBJ_DIR)\os2.obj \
    $(OBJ_DIR)\rtf.obj \
    $(OBJ_DIR)\troff.obj \
    $(OBJ_DIR)\fclass1.obj \
    $(OBJ_DIR)\ffile1.obj \
    $(OBJ_DIR)\ft_funcs.obj \

!ifdef HB_DOC_PDF

# PDF support for HBDOC
HBDOC_EXE_OBJS = \
    $(HBDOC_EXE_OBJS)       \
    $(OBJ_DIR)\pdfhbdoc.obj \
    $(OBJ_DIR)\genpdf1.obj  \

!endif

#**********************************************************

HBMAKE_EXE_OBJS = \
    $(OBJ_DIR)\hbmake.obj \
    $(OBJ_DIR)\tmake.obj \
    $(OBJ_DIR)\hbmutils.obj \
    $(OBJ_DIR)\checks.obj \
    $(OBJ_DIR)\pickarry.obj \
    $(OBJ_DIR)\pickfile.obj \
    $(OBJ_DIR)\prb_stak.obj \
    $(OBJ_DIR)\radios.obj \
    $(OBJ_DIR)\hbmlang.obj \
    $(OBJ_DIR)\fclass1.obj \
    $(OBJ_DIR)\ffile1.obj \
    $(OBJ_DIR)\ft_funcs.obj \

#**********************************************************

XBSCRIPT_EXE_OBJS = \
    $(OBJ_DIR)\xbscript.obj \

#**********************************************************
#**********************************************************
#**********************************************************

#
# HARBOUR_DLL objects
#

TMP_DLL_OBJS = \
    $(COMMON_LIB_OBJS)        \
    $(PP_LIB_OBJS)            \
    $(VM_DLL_OBJS)            \
    $(RTL_LIB_OBJS)           \
    $(MACRO_LIB_OBJS)         \
    $(DEBUG_LIB_OBJS)         \
    $(LANG_LIB_OBJS)          \
    $(CODEPAGE_LIB_OBJS)      \
    $(RDD_LIB_OBJS)           \
    $(DBFNTX_LIB_OBJS)        \
    $(DBFCDX_LIB_OBJS)        \
    $(DBFFPT_LIB_OBJS)        \
    $(HBSIX_LIB_OBJS)         \
    $(HSX_LIB_OBJS)           \
    $(USRRDD_LIB_OBJS)        \
    $(RDDS_LIB_OBJS)          \
    \
    $(GTNUL_LIB_OBJS)         \
    $(GTCGI_LIB_OBJS)         \
    $(GTPCA_LIB_OBJS)         \
    $(GTSTD_LIB_OBJS)         \
    $(GTWIN_LIB_OBJS)         \
    $(GTWVT_LIB_OBJS)         \
    $(GTGUI_DLL_OBJS)         \
    \
    $(PCREPOS_LIB_OBJS)       \
    $(MISC_LIB_OBJS)          \
    $(ODBC_LIB_OBJS)          \
    $(TIP_LIB_OBJS)           \
    $(CT_LIB_OBJS)            \

DISABLED_SHARED_MODULES=      \
    $(NULSYS_LIB_OBJS)        \

#**********************************************************
#**********************************************************
#**********************************************************

#
# Our default Targets
#

!ifndef HB_MT

HB_BUILD_TARGETS = \
    $(COMMON_LIB)      \
    $(PP_LIB)          \
    $(HARBOUR_EXE)     \
    $(HBPP_EXE)        \

!else

HB_BUILD_TARGETS = \
    $(PP_LIB)          \

!endif

HB_BUILD_TARGETS = $(HB_BUILD_TARGETS) \
    $(VM_LIB)          \
    $(RTL_LIB)         \
    $(MACRO_LIB)       \
    $(RDD_LIB)         \
    $(DBFNTX_LIB)      \
    $(DBFCDX_LIB)      \
    $(DBFFPT_LIB)      \
    $(HBSIX_LIB)       \
    $(HSX_LIB)         \
    $(USRRDD_LIB)      \
    $(RDDS_LIB)        \
    \
    $(FMSTAT_LIB)      \
    $(PCREPOS_LIB)     \
    $(TIP_LIB)         \
    $(CT_LIB)          \

!ifndef HB_MT

HB_BUILD_TARGETS = $(HB_BUILD_TARGETS) \
    $(DEBUG_LIB)       \
    $(LANG_LIB)        \
    $(CODEPAGE_LIB)    \
    $(NULSYS_LIB)      \
    $(HB_GT_LIBS)      \
    $(MISC_LIB)        \
    $(ODBC_LIB)        \
    \
    $(HBRUN_EXE)       \
    $(HBTEST_EXE)      \
    $(HBDOC_EXE)       \
    $(HBMAKE_EXE)      \
    $(XBSCRIPT_EXE)    \

!endif

# DLL Target is disabled by default
# It can be enabled by setting env
# variable HB_BUILD_DLL to yes

!if "$(HB_BUILD_DLL)" == "yes"
!ifndef HB_MT
HB_BUILD_TARGETS = $(HB_BUILD_TARGETS) \
    $(DLLMAIN_LIB)  \
    $(HARBOUR_DLL)  \
    $(HBTESTDLL_EXE)
!endif
!endif

#**********************************************************
#**********************************************************
#**********************************************************
