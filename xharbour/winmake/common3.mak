#===============================================================================
# $Id$
#===============================================================================

RESEXT=.res

!if ("$(DEL)"=="")
DEL=del
!endif

!if ("$(COPY)"=="")
COPY=copy
!endif

!if ("$(TYPE)"=="")
TYPE=type
!endif

!if ("$(HB_OPTIMFLAGS)"=="")
HB_OPTIMFLAGS     =-gc0
!endif

!if ("$(HB_DEBUG)"=="d")
CC_DEFINES        =-DHB_FM_STATISTICS -DHB_PRG_TRACE $(CC_DEFINES)
!if !("$(HB_DBG_VMALL)"=="1")
HB_NO_VM_ALL      =1
!endif
!if !("$(HB_DBG_DLMALLOC)"=="1")
HB_NO_FM_DL_ALLOC =1
!endif
!if !("$(HB_DBG_DVMEMCPY)"=="1")
HB_NO_DV_MEMCPY   =1
!endif
!else
HB_OPTIMFLAGS=-l $(HB_OPTIMFLAGS)
!endif

HARBOURFLAGS =$(HARBOURFLAGS)
CLIBFLAGS    =$(CLIBFLAGS)

!if !("$(HB_COMP_NO_TRACE)"=="1")
CC_COMP_DEFINES =-D__HB_COMP_TRACE__
!endif

!if ("$(HB_DEBUGGER_OFF)"=="1")
CC_DEFINES   =-DHB_NO_DEBUG $(CC_DEFINES)
!endif

!if ("$(HB_MT)"=="mt") || ("$(HB_THREAD_SUPPORT)"=="1")
CC_DEFINES   =-DHB_THREAD_SUPPORT $(CC_DEFINES)
HARBOURFLAGS =-dHB_THREAD_SUPPORT $(HARBOURFLAGS)
!endif

!if ("$(HB_GUI)"=="1")
CC_DEFINES   =-DHB_GUI $(CC_DEFINES)
!endif

!if ("$(HB_NO_BACKGROUND)"=="1")
CC_DEFINES   =-DHB_NO_BACKGROUND $(CC_DEFINES)
!endif

!if ("$(HB_NO_FM_DL_ALLOC)"=="")
CC_DEFINES   =-DHB_FM_DL_ALLOC $(CC_DEFINES)
!endif

!if ("$(HB_NO_VM_ALL)"=="")
CC_DEFINES   =-DHB_VM_ALL $(CC_DEFINES)
!endif

!if ("$(HB_NO_DV_MEMCPY)"=="1")
CC_DEFINES   =-DHB_NO_DV_MEMCPY $(CC_DEFINES)
!endif

!if ("$(HB_ARCH)"=="64")
CC_DEFINES   =-DHB_OS_WIN_64 -DNODLL $(CC_DEFINES)
HARBOURFLAGS =-dNODLL $(HARBOURFLAGS)
!endif

!if ("$(__BLD__)"=="DLL_BLD")
CC_DEFINES   =-D__EXPORT__ -DHB_NO_DUPLICATE_HVMPROCESSSYMBOL $(CC_DEFINES)
!if ("$(HB_DLL_ISMT)"=="1")
CC_DEFINES   =-D_MT -DHB_THREAD_SUPPORT $(CC_DEFINES)
HARBOURFLAGS =-dHB_THREAD_SUPPORT $(HARBOURFLAGS)
!endif
!endif

!if ("$(HB_AVOID_RESERVED_WORDS)"=="1")
CC_DEFINES=-DHB_AVOID_RESERVED_WORDS $(CC_DEFINES)
!endif

!if ("$(HB_USE_BISON)"=="1")
HARBOUR_Y    =$(COMPILER_DIR)$(DIR_SEP)harbour.sly
MACRO_Y      =$(MACRO_DIR)$(DIR_SEP)macro.y
BISON_CMD1   =bison --no-line --verbose -d $** -o$@
BISON_CMD2   =bison --no-line --verbose --name-prefix=hb_comp -d $** -o$@
!else
HARBOUR_Y    =$(COMPILER_DIR)$(DIR_SEP)harbouryy.c
MACRO_Y      =$(MACRO_DIR)$(DIR_SEP)macroyy.c
BISON_CMD1   =$(TYPE) $** > $@
BISON_CMD2   =$(BISON_CMD1)
!endif

HB_INCLUDE   =-i"include" -i"$(PDFLITE_DIR)\include" -i"$(SIXAPI_DIR)\include" -i"$(GDLIB_DIR)\include" -i"$(XWT_DIR)\include" -i"$(CGI_DIR)\include"
INCLUDE_DIR  =-I"include" -I"$(CC_DIR)\include" -I"$(OBJ_DIR)" -I"$(SIXAPI_DIR)\include" -I"$(FREEIMAGE_DIR)\include" -I"$(GDLIB_DIR)\include" -I"$(XWT_DIR)\include" -I"$(XWT_DIR)\src\xwt_win" $(INCLUDE_DIR)
INCLUDE_DIR  =-I"$(ZLIB_DIR)" -I"$(PDFLITE_DIR)\include" -I"$(PNG_DIR)" -I"$(TIFF_DIR)" -I"$(JPEG_DIR)" $(INCLUDE_DIR)
CC_DEFINES   =-D__WIN32__ -D_HAVE_SQLITE_CONFIG_H -DHAVE_CONFIG_H -DHARBOUR_CONF -DOPENSSL_NO_DEPRECATED $(CC_DEFINES)
CLIBFLAGS    =$(CC_DEFINES) $(INCLUDE_DIR) $(OPTFLAGS) $(CC_DEBUGFLAGS) $(WARNINGFLAGS) $(CLIBFLAGS) $(CFLAGS)
HARBOURFLAGS =$(HB_INCLUDE) -q0 -w3 -es2 $(HB_OPTIMFLAGS) $(HARBOURFLAGS)
HRB_LIBS     =$(COMPILER_LIB) $(COMPILERLIBS)
EXE_LIBS     =$(HARBOUR_LIBS) $(COMPILERLIBS)
CMN_LIBS     =$(COMMON_LIB) $(COMPILERLIBS)

!if !("$(HB_NO_VM_ALL)"=="1")
VM_LIB_OBJS=\
	$(VM_ALL_OBJS)
!endif

VM_LIB_OBJS=\
	$(VM_LIB_OBJS)\
	$(VM_COMMON_OBJS)

