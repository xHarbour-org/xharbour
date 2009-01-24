#===============================================================================
#
# $Id: compile.mak,v 1.36 2009/01/17 15:22:29 fsgiudice Exp $
#
# FILE  : compile.mak
# NOTES : please DO NOT convert TABS to SPACES of entries in this file.
#	  if a new entry should be included, we must follow the existing format
#         (pay attention to TAB before CMDs)
#
#===============================================================================

#===============================================================================
# LINK COMMANDS
#===============================================================================
HRB_LINK_CMD=$(LINK_CMD) $(HRB_LIBS)
EXE_LINK_CMD=$(LINK_CMD) $(EXE_LIBS)
CMN_LINK_CMD=$(LINK_CMD) $(CMN_LIBS)

#===============================================================================
# Executable dependencies and build rules
#===============================================================================
$(HARBOUR_EXE) : $(HARBOUR_EXE_OBJS)
	$(HRB_LINK_CMD)

$(PPGEN_EXE) : $(PPGEN_EXE_OBJS)
	$(CMN_LINK_CMD)

$(HBPP_EXE) : $(HBPP_EXE_OBJS)
	$(CMN_LINK_CMD)

$(HBDOC_EXE) : $(HBDOC_EXE_OBJS)
	$(EXE_LINK_CMD)

$(HBMAKE_EXE) : $(HBMAKE_EXE_OBJS)
	$(EXE_LINK_CMD)

$(XBSCRIPT_EXE) : $(XBSCRIPT_EXE_OBJS)
	$(EXE_LINK_CMD)

$(HBRUN_EXE) : $(HBRUN_EXE_OBJS)
	$(EXE_LINK_CMD)

$(HBTEST_EXE) : $(HBTEST_EXE_OBJS)
	$(EXE_LINK_CMD)

$(HBDOCDLL_EXE) : $(HBDOCDLL_EXE_OBJS)
	$(DLLEXE_LINK_CMD)

$(HBRUNDLL_EXE) : $(HBRUNDLL_EXE_OBJS)
	$(DLLEXE_LINK_CMD)

$(HBTESTDLL_EXE) : $(HBTESTDLL_EXE_OBJS)
	$(DLLEXE_LINK_CMD)

$(HBMAKEDLL_EXE) : $(HBMAKEDLL_EXE_OBJS)
	$(DLLEXE_LINK_CMD)

$(XBSCRIPTDLL_EXE) : $(XBSCRIPTDLL_EXE_OBJS)
	$(DLLEXE_LINK_CMD)

#===============================================================================
# COMMON.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)dvmemcpy$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)dvmemcpy.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbfopen$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbfopen.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbfsapi$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbfsapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbgete$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbgete.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbhash$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbhash.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbstr$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbstr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbtrace$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbtrace.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbver$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbver.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)expropt1$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)expropt1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)expropt2$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)expropt2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)reserved$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)reserved.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbdate$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbdate.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbwince$(OBJEXT) : $(COMMON_DIR)$(DIR_SEP)hbwince.c
	$(CC_CMD)

#===============================================================================
# PP.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)ppgen$(OBJEXT) : $(PP_DIR)$(DIR_SEP)ppgen.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pplib$(OBJEXT) : $(PP_DIR)$(DIR_SEP)pplib.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pplib2$(OBJEXT) : $(PP_DIR)$(DIR_SEP)pplib2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pplib3$(OBJEXT) : $(PP_DIR)$(DIR_SEP)pplib3.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ppcore$(OBJEXT) : $(PP_DIR)$(DIR_SEP)ppcore.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pptable$(OBJEXT) : $(PP_DIR)$(DIR_SEP)pptable.c
	$(CC_CMD)

$(PP_DIR)$(DIR_SEP)pptable.c : $(INCLUDE_DIR2)$(DIR_SEP)hbstdgen.ch $(INCLUDE_DIR2)$(DIR_SEP)std.ch ChangeLog $(PP_DIR)$(DIR_SEP)ppcore.c $(PP_DIR)$(DIR_SEP)ppgen.c
	$(PPGEN_EXE) $(INCLUDE_DIR2)$(DIR_SEP)hbstdgen.ch -o$(PP_DIR)$(DIR_SEP)pptable.c -cChangeLog -v$(INCLUDE_DIR2)$(DIR_SEP)hbverbld.h

#===============================================================================
# HARBOUR.EXE rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)harbour$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)harbour.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)harboury.c : $(COMPILER_DIR)$(DIR_SEP)harbour.sly
	$(BISON_CMD1)

$(OBJ_DIR)$(DIR_SEP)harboury$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)harboury.c
	$(CC_CMD)

$(COMPILER_DIR)$(DIR_SEP)expropta.c : $(INCLUDE_DIR2)$(DIR_SEP)hbexpra.c
	$(TYPE) $(COMPILER_DIR)$(DIR_SEP)expropta.c > $(OBJ_DIR)$(DIR_SEP)expropta.c.tmp
	$(COPY) $(OBJ_DIR)$(DIR_SEP)expropta.c.tmp $(COMPILER_DIR)$(DIR_SEP)expropta.c

$(COMPILER_DIR)$(DIR_SEP)exproptb.c : $(INCLUDE_DIR2)$(DIR_SEP)hbexprb.c
	$(TYPE) $(COMPILER_DIR)$(DIR_SEP)exproptb.c > $(OBJ_DIR)$(DIR_SEP)exproptb.c.tmp
	$(COPY) $(OBJ_DIR)$(DIR_SEP)exproptb.c.tmp $(COMPILER_DIR)$(DIR_SEP)exproptb.c

$(COMPILER_DIR)$(DIR_SEP)exproptc.c : $(INCLUDE_DIR2)$(DIR_SEP)hbexprc.c
	$(TYPE) $(COMPILER_DIR)$(DIR_SEP)exproptc.c > $(OBJ_DIR)$(DIR_SEP)exproptc.c.tmp
	$(COPY) $(OBJ_DIR)$(DIR_SEP)exproptc.c.tmp $(COMPILER_DIR)$(DIR_SEP)exproptc.c

$(COMPILER_DIR)$(DIR_SEP)hbslex.c : $(COMPILER_DIR)$(DIR_SEP)harbour.slx
	$(TYPE) $(COMPILER_DIR)$(DIR_SEP)hbslex.c > $(OBJ_DIR)$(DIR_SEP)hbslex.c.tmp
	$(COPY) $(OBJ_DIR)$(DIR_SEP)hbslex.c.tmp $(COMPILER_DIR)$(DIR_SEP)hbslex.c

$(OBJ_DIR)$(DIR_SEP)hbslex$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbslex.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cmdcheck$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)cmdcheck.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbusage$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbusage.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbident$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbident.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbgenerr$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbgenerr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbpcode$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbpcode.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbdead$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbdead.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbstripl$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbstripl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbfix$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbfix.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genc$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)genc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gencobj$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)gencobj.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genhrb$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)genhrb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)expropta$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)expropta.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)exproptb$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)exproptb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)exproptc$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)exproptc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbfunchk$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbfunchk.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gencc$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)gencc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbpcstat$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbpcstat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbdbginf$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hbdbginf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hblbl$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)hblbl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ppcomp$(OBJEXT) : $(COMPILER_DIR)$(DIR_SEP)ppcomp.c
	$(CC_CMD)

#===============================================================================
# RDDADS.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)ads1$(OBJEXT) : $(RDDADS_DIR)$(DIR_SEP)ads1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)adsfunc$(OBJEXT) : $(RDDADS_DIR)$(DIR_SEP)adsfunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)adsmgmnt$(OBJEXT) : $(RDDADS_DIR)$(DIR_SEP)adsmgmnt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ace32$(OBJEXT) : $(RDDADS_DIR)$(DIR_SEP)ace32.c
	$(CC_CMD)

#===============================================================================
# RDD.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)dbcmd$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbcmd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbcmd53$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbcmd53.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbcmdx$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbcmdx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbcmdhb$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbcmdhb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbdrop$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbdrop.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbexists$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbexists.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fieldhb$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)fieldhb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rddinfo$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)rddinfo.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)workarea$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)workarea.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wacore$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)wacore.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wafunc$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)wafunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbf1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbf1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbtotal.c : $(RDD_DIR)$(DIR_SEP)dbtotal.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbtotal$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbtotal.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbtotalx.c : $(RDD_DIR)$(DIR_SEP)dbtotalx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbtotalx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbtotalx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dblist.c : $(RDD_DIR)$(DIR_SEP)dblist.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dblist$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dblist.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dblistx.c : $(RDD_DIR)$(DIR_SEP)dblistx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dblistx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dblistx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbfuncs.c : $(RDD_DIR)$(DIR_SEP)dbfuncs.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbfuncs$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbfuncs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbfuncsx.c : $(RDD_DIR)$(DIR_SEP)dbfuncsx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbfuncsx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbfuncsx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbsort.c : $(RDD_DIR)$(DIR_SEP)dbsort.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbsort$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbsort.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbsortx.c : $(RDD_DIR)$(DIR_SEP)dbsortx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbsortx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbsortx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbdbsort$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbdbsort.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbnubs$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbnubs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbstrux.c : $(RDD_DIR)$(DIR_SEP)dbstrux.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbstrux$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbstrux.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbstruxx.c : $(RDD_DIR)$(DIR_SEP)dbstruxx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbstruxx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbstruxx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbstruxu.c : $(RDD_DIR)$(DIR_SEP)dbstruxu.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbstruxu$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbstruxu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbupdat.c : $(RDD_DIR)$(DIR_SEP)dbupdat.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbupdat$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbupdat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbupdatx.c : $(RDD_DIR)$(DIR_SEP)dbupdatx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbupdatx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbupdatx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sdf1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)sdf1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)delim1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)delim1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rddord.c : $(RDD_DIR)$(DIR_SEP)rddord.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rddord$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rddord.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rddordu.c : $(RDD_DIR)$(DIR_SEP)rddordu.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rddordu$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rddordu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rddsys.c : $(RDD_DIR)$(DIR_SEP)rddsys.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rddsys$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rddsys.c
	$(CC_CMD)

#===============================================================================
# VM.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)arrays$(OBJEXT) : $(VM_DIR)$(DIR_SEP)arrays.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)arrayshb$(OBJEXT) : $(VM_DIR)$(DIR_SEP)arrayshb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)asort$(OBJEXT) : $(VM_DIR)$(DIR_SEP)asort.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)break$(OBJEXT) : $(VM_DIR)$(DIR_SEP)break.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)classes$(OBJEXT) : $(VM_DIR)$(DIR_SEP)classes.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)classesc$(OBJEXT) : $(VM_DIR)$(DIR_SEP)classesc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cmdarg$(OBJEXT) : $(VM_DIR)$(DIR_SEP)cmdarg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)codebloc$(OBJEXT) : $(VM_DIR)$(DIR_SEP)codebloc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgentry$(OBJEXT) : $(VM_DIR)$(DIR_SEP)dbgentry.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)debug$(OBJEXT) : $(VM_DIR)$(DIR_SEP)debug.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)do$(OBJEXT) : $(VM_DIR)$(DIR_SEP)do.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dynlibhb$(OBJEXT) : $(VM_DIR)$(DIR_SEP)dynlibhb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dynsym$(OBJEXT) : $(VM_DIR)$(DIR_SEP)dynsym.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)errorapi$(OBJEXT) : $(VM_DIR)$(DIR_SEP)errorapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)eval$(OBJEXT) : $(VM_DIR)$(DIR_SEP)eval.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)evalhb$(OBJEXT) : $(VM_DIR)$(DIR_SEP)evalhb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)estack$(OBJEXT) : $(VM_DIR)$(DIR_SEP)estack.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)extend$(OBJEXT) : $(VM_DIR)$(DIR_SEP)extend.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fm$(OBJEXT) : $(VM_DIR)$(DIR_SEP)fm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)garbage$(OBJEXT) : $(VM_DIR)$(DIR_SEP)garbage.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)harbinit.c : $(VM_DIR)$(DIR_SEP)harbinit.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)harbinit$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)harbinit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hash$(OBJEXT) : $(VM_DIR)$(DIR_SEP)hash.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbi18n$(OBJEXT) : $(VM_DIR)$(DIR_SEP)hbi18n.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hvm$(OBJEXT) : $(VM_DIR)$(DIR_SEP)hvm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)inet$(OBJEXT) : $(VM_DIR)$(DIR_SEP)inet.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)initexit$(OBJEXT) : $(VM_DIR)$(DIR_SEP)initexit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)initsymb$(OBJEXT) : $(VM_DIR)$(DIR_SEP)initsymb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)itemapi$(OBJEXT) : $(VM_DIR)$(DIR_SEP)itemapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fastitem$(OBJEXT) : $(VM_DIR)$(DIR_SEP)fastitem.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)macro$(OBJEXT) : $(VM_DIR)$(DIR_SEP)macro.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)maindll$(OBJEXT) : $(VM_DIR)$(DIR_SEP)maindll.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mainstd$(OBJEXT) : $(VM_DIR)$(DIR_SEP)mainstd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mainwin$(OBJEXT) : $(VM_DIR)$(DIR_SEP)mainwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)maindlle$(OBJEXT) : $(VM_DIR)$(DIR_SEP)maindlle.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)maindllh$(OBJEXT) : $(VM_DIR)$(DIR_SEP)maindllh.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)memvars$(OBJEXT) : $(VM_DIR)$(DIR_SEP)memvars.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)memvclip$(OBJEXT) : $(VM_DIR)$(DIR_SEP)memvclip.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcount$(OBJEXT) : $(VM_DIR)$(DIR_SEP)pcount.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)proc$(OBJEXT) : $(VM_DIR)$(DIR_SEP)proc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pvalue$(OBJEXT) : $(VM_DIR)$(DIR_SEP)pvalue.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)runner$(OBJEXT) : $(VM_DIR)$(DIR_SEP)runner.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)thread$(OBJEXT) : $(VM_DIR)$(DIR_SEP)thread.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)throw$(OBJEXT) : $(VM_DIR)$(DIR_SEP)throw.c
	$(CC_CMD)

#===============================================================================
# FMSTAT.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)fmSTAT$(OBJEXT) : $(VM_DIR)$(DIR_SEP)fm.c
	$(CC_CMD_FMSTAT)

#===============================================================================
# RTL.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)abs$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)abs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)accept$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)accept.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)achoice.c : $(RTL_DIR)$(DIR_SEP)achoice.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)achoice$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)achoice.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)adir.c : $(RTL_DIR)$(DIR_SEP)adir.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)adir$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)adir.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)alert.c : $(RTL_DIR)$(DIR_SEP)alert.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)alert$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)alert.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ampm$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)ampm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)arrayblk.c : $(RTL_DIR)$(DIR_SEP)arrayblk.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)arrayblk$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)arrayblk.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)at$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)at.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bkgtsks$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)bkgtsks.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)binnum$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)binnum.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)binnumx$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)binnumx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)box$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)box.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)browdb.c : $(RTL_DIR)$(DIR_SEP)browdb.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)browdb$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)browdb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)browdbx.c : $(RTL_DIR)$(DIR_SEP)browdbx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)browdbx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)browdbx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)browse.c : $(RTL_DIR)$(DIR_SEP)browse.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)browse$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)browse.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cdpapi$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)cdpapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)checkbox.c : $(RTL_DIR)$(DIR_SEP)checkbox.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)checkbox$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)checkbox.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)color53.c : $(RTL_DIR)$(DIR_SEP)color53.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)color53$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)color53.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)chrasc$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)chrasc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)clpclass.c : $(RTL_DIR)$(DIR_SEP)clpclass.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)clpclass$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)clpclass.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)colorind$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)colorind.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)console$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)console.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)copyfile$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)copyfile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cstr.c : $(RTL_DIR)$(DIR_SEP)cstr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)cstr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)cstr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cstruct.c : $(RTL_DIR)$(DIR_SEP)cstruct.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)cstruct$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)cstruct.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)datec$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)datec.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)datehb$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)datehb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dateshb$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)dateshb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbedit.c : $(RTL_DIR)$(DIR_SEP)dbedit.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbedit$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbedit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbf2txt$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)dbf2txt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)decode.c : $(RTL_DIR)$(DIR_SEP)decode.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)decode$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)decode.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)defpath$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)defpath.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)descend$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)descend.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)devoutp.c : $(RTL_DIR)$(DIR_SEP)devoutp.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)devoutp$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)devoutp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dirdrive$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)dirdrive.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)direct$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)direct.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)diskspac$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)diskspac.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)disksphb$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)disksphb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dircmd.c : $(RTL_DIR)$(DIR_SEP)dircmd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dircmd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dircmd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dllcall$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)dllcall.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dummy.c : $(RTL_DIR)$(DIR_SEP)dummy.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dummy$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dummy.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dumpvar.c : $(RTL_DIR)$(DIR_SEP)dumpvar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dumpvar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dumpvar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)einstvar.c : $(RTL_DIR)$(DIR_SEP)einstvar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)einstvar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)einstvar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)empty$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)empty.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)error$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)error.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)errorsys.c : $(RTL_DIR)$(DIR_SEP)errorsys.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)errorsys$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)errorsys.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fieldbl.c : $(RTL_DIR)$(DIR_SEP)fieldbl.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)fieldbl$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)fieldbl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)getlist.c : $(RTL_DIR)$(DIR_SEP)getlist.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)getlist$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)getlist.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)getsys.c : $(RTL_DIR)$(DIR_SEP)getsys.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)getsys$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)getsys.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)file$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)file.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)filebuf$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)filebuf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)filehb$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)filehb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)filestat$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)filestat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)filesys$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)filesys.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fserror$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)fserror.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)readline$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)readline.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)txtline$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)txtline.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fkmax$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)fkmax.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fnsplit$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)fnsplit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fparse$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)fparse.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fssize$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)fssize.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fstemp$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)fstemp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gete$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gete.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gt$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtapi$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtapiu$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtapiu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtchrmap$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtchrmap.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtclip$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtclip.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtfunc$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtfunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtkbstat$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtkbstat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtkeycod$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtkeycod.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gttone$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gttone.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtsys$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtsys.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gx$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hardcr$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hardcr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbbitf$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbbitf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcomprs$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbcomprs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbchksum$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbchksum.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcrc32$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbcrc32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcrypt$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbcrypt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbffind$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbffind.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbfile$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbfile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbgtcore$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbgtcore.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hboutdbg$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hboutdbg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbntos$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbntos.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hblog.c : $(RTL_DIR)$(DIR_SEP)hblog.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hblog$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hblog.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbmd5$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbmd5.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hblognet.c : $(RTL_DIR)$(DIR_SEP)hblognet.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hblognet$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hblognet.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbhex2n$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbhex2n.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbrandom$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbrandom.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbini.c : $(RTL_DIR)$(DIR_SEP)hbini.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hbini$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbini.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbserial.c : $(RTL_DIR)$(DIR_SEP)hbserial.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hbserial$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbserial.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbserv$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbserv.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbsrlraw$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbsrlraw.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbsyslog$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbsyslog.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbtoken$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbtoken.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbxml$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)hbxml.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)idle$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)idle.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)inkey$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)inkey.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)inkeyapi$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)inkeyapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)input.c : $(RTL_DIR)$(DIR_SEP)input.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)input$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)input.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)is$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)is.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)isprint$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)isprint.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)langapi$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)langapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)left$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)left.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)len$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)len.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)lennum$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)lennum.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)listbox.c : $(RTL_DIR)$(DIR_SEP)listbox.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)listbox$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)listbox.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)math$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)math.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)maxrow$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)maxrow.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)minmax$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)minmax.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)memofile$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)memofile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)memoedit.c : $(RTL_DIR)$(DIR_SEP)memoedit.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)memoedit$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)memoedit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)memvarbl.c : $(RTL_DIR)$(DIR_SEP)memvarbl.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)memvarbl$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)memvarbl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)menuto.c : $(RTL_DIR)$(DIR_SEP)menuto.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)menuto$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)menuto.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mlcfunc$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)mlcfunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mod$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)mod.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mouseapi$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)mouseapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mousex$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)mousex.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mouse53$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)mouse53.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mssgline.c : $(RTL_DIR)$(DIR_SEP)mssgline.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)mssgline$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)mssgline.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)namespaces.c : $(RTL_DIR)$(DIR_SEP)namespaces.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)namespaces$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)namespaces.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mtran$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)mtran.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)natmsg$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)natmsg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)net$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)net.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)objfunc.c : $(RTL_DIR)$(DIR_SEP)objfunc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)objfunc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)objfunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)oemansi$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)oemansi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)oldbox$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)oldbox.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)oldclear$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)oldclear.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pad$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)pad.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)padc$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)padc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)padl$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)padl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)padr$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)padr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)philes$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)philes.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)philes53$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)philes53.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)philesx$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)philesx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)perfuncs.c : $(RTL_DIR)$(DIR_SEP)perfuncs.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)perfuncs$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)perfuncs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)persist.c : $(RTL_DIR)$(DIR_SEP)persist.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)persist$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)persist.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)profiler.c : $(RTL_DIR)$(DIR_SEP)profiler.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)profiler$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)profiler.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pushbtn.c : $(RTL_DIR)$(DIR_SEP)pushbtn.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pushbtn$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pushbtn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)radiobtn.c : $(RTL_DIR)$(DIR_SEP)radiobtn.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)radiobtn$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)radiobtn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)radiogrp.c : $(RTL_DIR)$(DIR_SEP)radiogrp.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)radiogrp$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)radiogrp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rat$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)rat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)readkey.c : $(RTL_DIR)$(DIR_SEP)readkey.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)readkey$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)readkey.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)readvar.c : $(RTL_DIR)$(DIR_SEP)readvar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)readvar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)readvar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)regex$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)regex.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)regexrpl.c : $(RTL_DIR)$(DIR_SEP)regexrpl.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)regexrpl$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)regexrpl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)replic$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)replic.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)right$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)right.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)round$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)round.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)run$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)run.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)samples$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)samples.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)saverest$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)saverest.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)scroll$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)scroll.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)scrollbr.c : $(RTL_DIR)$(DIR_SEP)scrollbr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)scrollbr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)scrollbr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)seconds$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)seconds.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)set$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)set.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)readexit$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)readexit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)readins$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)readins.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setcolor$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)setcolor.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setcurs$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)setcurs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setfunc.c : $(RTL_DIR)$(DIR_SEP)setfunc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)setfunc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)setfunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setkey$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)setkey.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setpos$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)setpos.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setposbs$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)setposbs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setta.c : $(RTL_DIR)$(DIR_SEP)setta.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)setta$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)setta.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)shadow$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)shadow.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)soundex$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)soundex.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)space$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)space.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)spfiles$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)spfiles.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)str$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)str.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)str2ptr$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)str2ptr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strcase$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)strcase.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strdel$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)strdel.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sprintf.c : $(RTL_DIR)$(DIR_SEP)sprintf.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sprintf$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sprintf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)stream.c : $(RTL_DIR)$(DIR_SEP)stream.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)stream$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)stream.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strings$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)strings.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strmatch$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)strmatch.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strpeek$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)strpeek.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strtran$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)strtran.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strzero$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)strzero.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)stuff$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)stuff.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)substr$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)substr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tbcolumn.c : $(RTL_DIR)$(DIR_SEP)tbcolumn.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tbcolumn$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tbcolumn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tbrowse.c : $(RTL_DIR)$(DIR_SEP)tbrowse.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tbrowse$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tbrowse.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tclass.c : $(RTL_DIR)$(DIR_SEP)tclass.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tclass$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tclass.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)teditor.c : $(RTL_DIR)$(DIR_SEP)teditor.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)teditor$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)teditor.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)text.c : $(RTL_DIR)$(DIR_SEP)text.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)text$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)text.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tget.c : $(RTL_DIR)$(DIR_SEP)tget.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tget$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tget.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tgetint.c : $(RTL_DIR)$(DIR_SEP)tgetint.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tgetint$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tgetint.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tgetlist.c : $(RTL_DIR)$(DIR_SEP)tgetlist.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tgetlist$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tgetlist.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tlabel.c : $(RTL_DIR)$(DIR_SEP)tlabel.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tlabel$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tlabel.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tmenuitm.c : $(RTL_DIR)$(DIR_SEP)tmenuitm.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tmenuitm$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tmenuitm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tobject.c : $(RTL_DIR)$(DIR_SEP)tobject.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tobject$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tobject.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tone$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)tone.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tpopup.c : $(RTL_DIR)$(DIR_SEP)tpopup.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tpopup$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tpopup.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tprinter$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)tprinter.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)trace$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)trace.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)transfrm$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)transfrm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)traceprg.c : $(RTL_DIR)$(DIR_SEP)traceprg.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)traceprg$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)traceprg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)treport.c : $(RTL_DIR)$(DIR_SEP)treport.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)treport$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)treport.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tscalar.c : $(RTL_DIR)$(DIR_SEP)tscalar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tscalar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tscalar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ttable.c : $(RTL_DIR)$(DIR_SEP)ttable.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ttable$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ttable.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)trim$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)trim.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ttextlin.c : $(RTL_DIR)$(DIR_SEP)ttextlin.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ttextlin$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ttextlin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ttopbar.c : $(RTL_DIR)$(DIR_SEP)ttopbar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ttopbar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ttopbar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)txml.c : $(RTL_DIR)$(DIR_SEP)txml.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)txml$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)txml.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)type$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)type.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)typefile.c : $(RTL_DIR)$(DIR_SEP)typefile.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)typefile$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)typefile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)val$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)val.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)valtostr$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)valtostr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)valtype$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)valtype.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)version$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)version.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wait.c : $(RTL_DIR)$(DIR_SEP)wait.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)wait$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)wait.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)win32ole.c : $(RTL_DIR)$(DIR_SEP)win32ole.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)win32ole$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)win32ole.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)win32prn.c : $(RTL_DIR)$(DIR_SEP)win32prn.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)win32prn$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)win32prn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)winos.c : $(RTL_DIR)$(DIR_SEP)winos.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)winos$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)winos.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)winreg.c : $(RTL_DIR)$(DIR_SEP)winreg.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)winreg$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)winreg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)word$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)word.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xhelp$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)xhelp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xsavescr$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)xsavescr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)trpc.c : $(RTL_DIR)$(DIR_SEP)trpc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)trpc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)trpc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)trpccli.c : $(RTL_DIR)$(DIR_SEP)trpccli.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)trpccli$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)trpccli.c
	$(CC_CMD)

#===============================================================================
# ZLIB.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)adler32$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)adler32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)compress$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)compress.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)crc32$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)crc32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)deflate$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)deflate.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gzio$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)gzio.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)infback$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)infback.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)inffast$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)inffast.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)inflate$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)inflate.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)inftrees$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)inftrees.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)trees$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)trees.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uncompr$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)uncompr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)zutil$(OBJEXT) : $(ZLIB_DIR)$(DIR_SEP)zutil.c
	$(CC_CMD)

#===============================================================================
# PCREPOS.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)pcre_chartables$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_chartables.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_ucd$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_ucd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_compile$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_compile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_config$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_config.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_dfa_exec$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_dfa_exec.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_exec$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_exec.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_fullinfo$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_fullinfo.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_get$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_get.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_globals$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_globals.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_info$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_info.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_maketables$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_maketables.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_newline$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_newline.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_ord2utf8$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_ord2utf8.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_refcount$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_refcount.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_study$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_study.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_tables$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_tables.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_try_flipped$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_try_flipped.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_ucp_searchfuncs$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_ucp_searchfuncs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_valid_utf8$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_valid_utf8.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_version$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_version.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcre_xclass$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcre_xclass.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pcreposix$(OBJEXT) : $(PCREPOS_DIR)$(DIR_SEP)pcreposix.c
	$(CC_CMD)

#===============================================================================
# LANG.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)msgbgwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgbgwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgbgmik$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgbgmik.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msg_tpl$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msg_tpl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgby866$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgby866.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgbywin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgbywin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgca$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgca.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgcs852$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgcs852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgcsiso$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgcsiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgcskam$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgcskam.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgcswin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgcswin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgde$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgde.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgdewin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgdewin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgen$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgen.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgeo$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgeo.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msges$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msges.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgeswin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgeswin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgeu$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgeu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgfr$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgfr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msggl$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msggl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghe862$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghe862.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghewin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghewin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghr1250$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghr1250.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghr437$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghr437.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghr852$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghr852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghriso$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghriso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghu852$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghu852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghucwi$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghucwi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msghuwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msghuwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgid$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgid.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgis850$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgis850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgit$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgko$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgko.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgltwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgltwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgpl852$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgpl852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgnl$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgnl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgpliso$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgpliso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgplmaz$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgplmaz.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgplwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgplwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgpt$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgpt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgro$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgro.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgru866$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgru866.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgrukoi$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgrukoi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgruwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgruwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgsl852$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgsl852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgsliso$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgsliso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgslwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgslwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgsr852$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgsr852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgsriso$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgsriso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgsrwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgsrwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgsv$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgsv.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgsvwin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgsvwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgua866$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgua866.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msguakoi$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msguakoi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msguawin$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msguawin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgzhb5$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgzhb5.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgzhgb$(OBJEXT) : $(LANG_DIR)$(DIR_SEP)msgzhgb.c
	$(CC_CMD)

#===============================================================================
# MACRO.LIB rules
#===============================================================================

$(OBJ_DIR)$(DIR_SEP)macroy.c : $(MACRO_DIR)$(DIR_SEP)macro.y
	$(BISON_CMD2)

$(OBJ_DIR)$(DIR_SEP)macroy$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)macroy.c
	$(CC_CMD)

$(MACRO_DIR)$(DIR_SEP)macroslx.c : $(MACRO_DIR)$(DIR_SEP)macro.slx
	$(TYPE) $(MACRO_DIR)$(DIR_SEP)macroslx.c > $(OBJ_DIR)$(DIR_SEP)macroslx.c.tmp
	$(COPY) $(OBJ_DIR)$(DIR_SEP)macroslx.c.tmp $(MACRO_DIR)$(DIR_SEP)macroslx.c

$(OBJ_DIR)$(DIR_SEP)macroslx$(OBJEXT) : $(MACRO_DIR)$(DIR_SEP)macroslx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)macroa$(OBJEXT) : $(MACRO_DIR)$(DIR_SEP)macroa.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)macrob$(OBJEXT) : $(MACRO_DIR)$(DIR_SEP)macrob.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)macroc$(OBJEXT) : $(MACRO_DIR)$(DIR_SEP)macroc.c
	$(CC_CMD)

#===============================================================================
# NULSYS.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)nulsys$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)nulsys$(DIR_SEP)nulsys.c
	$(CC_CMD)

#===============================================================================
# DBFFPT.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)dbffpt1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbffpt$(DIR_SEP)dbffpt1.c
	$(CC_CMD)

#===============================================================================
# DBFNTX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)dbfntx1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbfntx$(DIR_SEP)dbfntx1.c
	$(CC_CMD)

#===============================================================================
# DBFNSX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)dbfnsx1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbfnsx$(DIR_SEP)dbfnsx1.c
	$(CC_CMD)

#===============================================================================
# DBFCDX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)dbfcdx1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbfcdx$(DIR_SEP)dbfcdx1.c
	$(CC_CMD)

#===============================================================================
# BMDBFCDX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)bmdbfcdx1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)bmdbfcdx$(DIR_SEP)bmdbfcdx1.c
	$(CC_CMD)

#===============================================================================
# SIXCDX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)sixcdx1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)dbfcdx$(DIR_SEP)sixcdx1.c
	$(CC_CMD)

#===============================================================================
# BMSIXCDX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)bmsixcdx1$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)bmdbfcdx$(DIR_SEP)bmsixcdx1.c
	$(CC_CMD)

#===============================================================================
# HBSIX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)sxcompr$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxcompr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxcrypt$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxcrypt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxdate$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxdate.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxsem$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxsem.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxfname$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxfname.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxtable$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxtable.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxord$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxord.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxutil$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxutil.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxredir$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxredir.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxcompat.c : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxcompat.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sxcompat$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sxcompat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxini.c : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxini.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sxini$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sxini.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sxtrig.c : $(RDD_DIR)$(DIR_SEP)hbsix$(DIR_SEP)sxtrig.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sxtrig$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sxtrig.c
	$(CC_CMD)

#===============================================================================
# HSX.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hsx$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hsx$(DIR_SEP)hsx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cftsfunc$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)hsx$(DIR_SEP)cftsfunc.c
	$(CC_CMD)

#===============================================================================
# USRRDD.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)usrrdd$(OBJEXT) : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)usrrdd.c
	$(CC_CMD)

#===============================================================================
# RDDS.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)arrayrdd.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)arrayrdd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)arrayrdd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)arrayrdd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)logrdd.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)logrdd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)logrdd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)logrdd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbdelim.c : $(RDD_DIR)$(DIR_SEP)dbdelim.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbdelim$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbdelim.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbjoin.c : $(RDD_DIR)$(DIR_SEP)dbjoin.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbjoin$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbjoin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbjoinx.c : $(RDD_DIR)$(DIR_SEP)dbjoinx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbjoinx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbjoinx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbsdf.c : $(RDD_DIR)$(DIR_SEP)dbsdf.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbsdf$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbsdf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbtcdx.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)dbtcdx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbtcdx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbtcdx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fcomma.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)fcomma.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)fcomma$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)fcomma.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fptcdx.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)fptcdx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)fptcdx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)fptcdx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hscdx.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)hscdx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hscdx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hscdx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rlcdx.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)rlcdx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rlcdx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rlcdx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)smtcdx.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)smtcdx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)smtcdx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)smtcdx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)adordd.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)adordd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)adordd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)adordd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ansirdd.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)ansirdd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ansirdd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ansirdd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)vfpcdx.c : $(RDD_DIR)$(DIR_SEP)usrrdd$(DIR_SEP)rdds$(DIR_SEP)vfpcdx.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)vfpcdx$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)vfpcdx.c
	$(CC_CMD)

#===============================================================================
# DEBUG.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)dbgmenu.c : $(DEBUG_DIR)$(DIR_SEP)dbgmenu.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgmenu$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgmenu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtmenu.c : $(DEBUG_DIR)$(DIR_SEP)dbgtmenu.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtmenu$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgtmenu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtmitm.c : $(DEBUG_DIR)$(DIR_SEP)dbgtmitm.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtmitm$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgtmitm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtwin.c : $(DEBUG_DIR)$(DIR_SEP)dbgtwin.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtwin$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgtwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)debugger.c : $(DEBUG_DIR)$(DIR_SEP)debugger.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)debugger$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)debugger.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbghelp.c : $(DEBUG_DIR)$(DIR_SEP)dbghelp.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbghelp$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbghelp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tbrwtext.c : $(DEBUG_DIR)$(DIR_SEP)tbrwtext.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tbrwtext$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tbrwtext.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtarr.c : $(DEBUG_DIR)$(DIR_SEP)dbgtarr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtarr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgtarr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgthsh.c : $(DEBUG_DIR)$(DIR_SEP)dbgthsh.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgthsh$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgthsh.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtobj.c : $(DEBUG_DIR)$(DIR_SEP)dbgtobj.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgtobj$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgtobj.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgwa.c : $(DEBUG_DIR)$(DIR_SEP)dbgwa.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgwa$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgwa.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgbrwsr.c : $(DEBUG_DIR)$(DIR_SEP)dbgbrwsr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgbrwsr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgbrwsr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgaltd.c : $(DEBUG_DIR)$(DIR_SEP)dbgaltd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dbgaltd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dbgaltd.c
	$(CC_CMD)

#===============================================================================
# GTCGI.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtcgi$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtcgi$(DIR_SEP)gtcgi.c
	$(CC_CMD)

#===============================================================================
# GTDOS.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtdos$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtdos$(DIR_SEP)gtdos.c
	$(CC_CMD)

#===============================================================================
# GTPCA.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtpca$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtpca$(DIR_SEP)gtpca.c
	$(CC_CMD)

#===============================================================================
# GTSTD.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtstd$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtstd$(DIR_SEP)gtstd.c
	$(CC_CMD)

#===============================================================================
# GTWIN.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtwin$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtwin$(DIR_SEP)gtwin.c
	$(CC_CMD)

#===============================================================================
# GTWVT.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtwvt$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtwvt$(DIR_SEP)gtwvt.c
	$(CC_CMD)

#===============================================================================
# GTWVG.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtwvg$(OBJEXT) : $(GTWVG_DIR)$(DIR_SEP)gtwvg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wvtcore$(OBJEXT) : $(GTWVG_DIR)$(DIR_SEP)wvtcore.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wvtutils$(OBJEXT) : $(GTWVG_DIR)$(DIR_SEP)wvtutils.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wvtclass$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)wvtclass.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wvtclass.c : $(GTWVG_DIR)$(DIR_SEP)wvtclass.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)wvtpaint$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)wvtpaint.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wvtpaint.c : $(GTWVG_DIR)$(DIR_SEP)wvtpaint.prg
	$(HB_CMD)

#===============================================================================
# GTGUI.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtgui$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtgui$(DIR_SEP)gtgui.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gtdef$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtgui$(DIR_SEP)gtdef.c
	$(CC_CMD)

#===============================================================================
# GTALLEG.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtalleg$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtalleg$(DIR_SEP)gtalleg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ssf$(OBJEXT) : $(RTL_DIR)$(DIR_SEP)gtalleg$(DIR_SEP)ssf.c
	$(CC_CMD)

#===============================================================================
# CODEPAGE.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)cpbg866$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpbg866.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpbgiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpbgiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpbgmik$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpbgmik.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpbgwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpbgwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpcs852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpcs852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpcsiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpcsiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpcskam$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpcskam.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpcswin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpcswin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpde850$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpde850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpdeiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpdeiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpdewin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpdewin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpel737$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpel737.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpelwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpelwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpes850$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpes850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpes850c$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpes850c.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpesiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpesiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpesisoc$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpesisoc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpeswin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpeswin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpeswinc$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpeswinc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpeswinm$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpeswinm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpfr850$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpfr850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpfriso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpfriso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpfrwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpfrwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphr437$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphr437.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphr852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphr852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphrwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphrwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphu852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphu852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphu852s$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphu852s.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphuiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphuiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphuisos$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphuisos.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphuwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphuwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cphuwins$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cphuwins.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpit437$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpit437.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpit850$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpit850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpitisb$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpitisb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpitiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpitiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpitwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpitwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpltwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpltwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cppl852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cppl852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cppliso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cppliso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpplmaz$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpplmaz.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpplwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpplwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cppt850$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cppt850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpptiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpptiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpro852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpro852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cproiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cproiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cprowin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cprowin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpru866$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpru866.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpruiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpruiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cprukoi$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cprukoi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpruwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpruwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsk852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsk852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpskiso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpskiso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpskkam$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpskkam.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpskwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpskwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsl437$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsl437.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsl852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsl852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsliso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsliso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpslwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpslwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsrwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsrwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsv850$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsv850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsvclip$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsvclip.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsviso$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsviso.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpsvwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpsvwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cptr857$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cptr857.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cptrwin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cptrwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpua866$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpua866.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpuakoi$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpuakoi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cpuawin$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)cpuawin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1250$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1250.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1251$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1251.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1252$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1252.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1253$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1253.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1254$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1254.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1255$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1255.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1256$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1256.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1257$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1257.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc1258$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc1258.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc737$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc737.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc775$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc775.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc850$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc850.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc852$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc852.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc855$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc855.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc857$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc857.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc860$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc860.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc861$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc861.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc862$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc862.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc863$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc863.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc864$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc864.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc865$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc865.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc866$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc866.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc869$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc869.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc874$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc874.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_1$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc88591b$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc88591b.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_2$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_3$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_3.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_4$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_4.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_5$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_5.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_6$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_6.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_7$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_7.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_8$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_8.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc8859_9$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc8859_9.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc885910$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc885910.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc885911$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc885911.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc885913$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc885913.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc885914$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc885914.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc885915$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc885915.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uc885916$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uc885916.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uckam$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uckam.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uckoi8$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uckoi8.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)uckoi8u$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)uckoi8u.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ucmaz$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)ucmaz.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ucmik$(OBJEXT) : $(CODEPAGE_DIR)$(DIR_SEP)ucmik.c
	$(CC_CMD)

#===============================================================================
# HBODBC.lib rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)odbc$(OBJEXT) : $(ODBC_DIR)$(DIR_SEP)odbc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)todbc.c : $(ODBC_DIR)$(DIR_SEP)todbc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)todbc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)todbc.c
	$(CC_CMD)

#===============================================================================
# LIBMISC.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hb_f$(OBJEXT) : $(MISC_DIR)$(DIR_SEP)hb_f.c
	$(CC_CMD)

#===============================================================================
# TIP.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)base64x$(OBJEXT) : $(TIP_DIR)$(DIR_SEP)base64x.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)utils$(OBJEXT) : $(TIP_DIR)$(DIR_SEP)utils.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)client.c : $(TIP_DIR)$(DIR_SEP)client.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)client$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)client.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)credent.c : $(TIP_DIR)$(DIR_SEP)credent.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)credent$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)credent.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)encb64.c : $(TIP_DIR)$(DIR_SEP)encb64.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)encb64$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)encb64.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sendmail.c : $(TIP_DIR)$(DIR_SEP)sendmail.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sendmail$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sendmail.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)encoder.c : $(TIP_DIR)$(DIR_SEP)encoder.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)encoder$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)encoder.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)encqp.c : $(TIP_DIR)$(DIR_SEP)encqp.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)encqp$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)encqp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)encurl.c : $(TIP_DIR)$(DIR_SEP)encurl.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)encurl$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)encurl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ftpcln.c : $(TIP_DIR)$(DIR_SEP)ftpcln.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ftpcln$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ftpcln.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)httpcln.c : $(TIP_DIR)$(DIR_SEP)httpcln.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)httpcln$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)httpcln.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mail.c : $(TIP_DIR)$(DIR_SEP)mail.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)mail$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)mail.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)popcln.c : $(TIP_DIR)$(DIR_SEP)popcln.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)popcln$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)popcln.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)smtpcln.c : $(TIP_DIR)$(DIR_SEP)smtpcln.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)smtpcln$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)smtpcln.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)thtml.c : $(TIP_DIR)$(DIR_SEP)thtml.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)thtml$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)thtml.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)url.c : $(TIP_DIR)$(DIR_SEP)url.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)url$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)url.c
	$(CC_CMD)

#===============================================================================
# CT.LIB rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)addascii$(OBJEXT) : $(CT_DIR)$(DIR_SEP)addascii.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)asciisum$(OBJEXT) : $(CT_DIR)$(DIR_SEP)asciisum.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ascpos$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ascpos.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)atadjust$(OBJEXT) : $(CT_DIR)$(DIR_SEP)atadjust.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)atnum$(OBJEXT) : $(CT_DIR)$(DIR_SEP)atnum.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)atrepl$(OBJEXT) : $(CT_DIR)$(DIR_SEP)atrepl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bit1$(OBJEXT) : $(CT_DIR)$(DIR_SEP)bit1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bit2$(OBJEXT) : $(CT_DIR)$(DIR_SEP)bit2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bit3$(OBJEXT) : $(CT_DIR)$(DIR_SEP)bit3.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charevod$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charevod.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charlist$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charlist.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charmirr$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charmirr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charmix$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charmix.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charone$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charone.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charonly$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charonly.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charop$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charop.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charrepl$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charrepl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charsort$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charsort.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)charswap$(OBJEXT) : $(CT_DIR)$(DIR_SEP)charswap.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)count$(OBJEXT) : $(CT_DIR)$(DIR_SEP)count.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctc$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctchksum$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctchksum.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctcrypt$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctcrypt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctmath$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctmath.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctmath2$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctmath2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctnet$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctnet.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctset$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctset.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctstr$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctstr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)datetime$(OBJEXT) : $(CT_DIR)$(DIR_SEP)datetime.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dattime2$(OBJEXT) : $(CT_DIR)$(DIR_SEP)dattime2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dbftools$(OBJEXT) : $(CT_DIR)$(DIR_SEP)dbftools.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)disk$(OBJEXT) : $(CT_DIR)$(DIR_SEP)disk.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)envparam$(OBJEXT) : $(CT_DIR)$(DIR_SEP)envparam.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)exponent$(OBJEXT) : $(CT_DIR)$(DIR_SEP)exponent.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)files$(OBJEXT) : $(CT_DIR)$(DIR_SEP)files.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)finan$(OBJEXT) : $(CT_DIR)$(DIR_SEP)finan.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ftoc$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ftoc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)intneg$(OBJEXT) : $(CT_DIR)$(DIR_SEP)intneg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)justify$(OBJEXT) : $(CT_DIR)$(DIR_SEP)justify.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)keyset$(OBJEXT) : $(CT_DIR)$(DIR_SEP)keyset.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)lton$(OBJEXT) : $(CT_DIR)$(DIR_SEP)lton.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)maxline$(OBJEXT) : $(CT_DIR)$(DIR_SEP)maxline.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)misc1$(OBJEXT) : $(CT_DIR)$(DIR_SEP)misc1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)misc2$(OBJEXT) : $(CT_DIR)$(DIR_SEP)misc2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)misc3$(OBJEXT) : $(CT_DIR)$(DIR_SEP)misc3.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)num1$(OBJEXT) : $(CT_DIR)$(DIR_SEP)num1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)numat$(OBJEXT) : $(CT_DIR)$(DIR_SEP)numat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)numcount$(OBJEXT) : $(CT_DIR)$(DIR_SEP)numcount.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)numline$(OBJEXT) : $(CT_DIR)$(DIR_SEP)numline.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)numlohi$(OBJEXT) : $(CT_DIR)$(DIR_SEP)numlohi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctpad$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctpad.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pack$(OBJEXT) : $(CT_DIR)$(DIR_SEP)pack.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pos1$(OBJEXT) : $(CT_DIR)$(DIR_SEP)pos1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pos2$(OBJEXT) : $(CT_DIR)$(DIR_SEP)pos2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)posdiff$(OBJEXT) : $(CT_DIR)$(DIR_SEP)posdiff.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)print$(OBJEXT) : $(CT_DIR)$(DIR_SEP)print.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)range$(OBJEXT) : $(CT_DIR)$(DIR_SEP)range.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)relation$(OBJEXT) : $(CT_DIR)$(DIR_SEP)relation.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)remove$(OBJEXT) : $(CT_DIR)$(DIR_SEP)remove.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)replace$(OBJEXT) : $(CT_DIR)$(DIR_SEP)replace.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)screen1$(OBJEXT) : $(CT_DIR)$(DIR_SEP)screen1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setlast$(OBJEXT) : $(CT_DIR)$(DIR_SEP)setlast.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)settime$(OBJEXT) : $(CT_DIR)$(DIR_SEP)settime.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strdiff$(OBJEXT) : $(CT_DIR)$(DIR_SEP)strdiff.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strfile$(OBJEXT) : $(CT_DIR)$(DIR_SEP)strfile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)strswap$(OBJEXT) : $(CT_DIR)$(DIR_SEP)strswap.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tab$(OBJEXT) : $(CT_DIR)$(DIR_SEP)tab.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)token1$(OBJEXT) : $(CT_DIR)$(DIR_SEP)token1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)token2$(OBJEXT) : $(CT_DIR)$(DIR_SEP)token2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)trig$(OBJEXT) : $(CT_DIR)$(DIR_SEP)trig.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wordrepl$(OBJEXT) : $(CT_DIR)$(DIR_SEP)wordrepl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wordtoch$(OBJEXT) : $(CT_DIR)$(DIR_SEP)wordtoch.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)blank.c : $(CT_DIR)$(DIR_SEP)blank.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)blank$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)blank.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)color.c : $(CT_DIR)$(DIR_SEP)color.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)color$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)color.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ct.c : $(CT_DIR)$(DIR_SEP)ct.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ct$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ct.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctmisc.c : $(CT_DIR)$(DIR_SEP)ctmisc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ctmisc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ctmisc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctrand.c : $(CT_DIR)$(DIR_SEP)ctrand.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ctrand$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ctrand.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cttime.c : $(CT_DIR)$(DIR_SEP)cttime.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)cttime$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)cttime.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctwfunc$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctwfunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctwin$(OBJEXT) : $(CT_DIR)$(DIR_SEP)ctwin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cursor$(OBJEXT) : $(CT_DIR)$(DIR_SEP)cursor.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dattime3$(OBJEXT) : $(CT_DIR)$(DIR_SEP)dattime3.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)diskutil.c : $(CT_DIR)$(DIR_SEP)diskutil.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)diskutil$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)diskutil.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fcopy.c : $(CT_DIR)$(DIR_SEP)fcopy.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)fcopy$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)fcopy.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)getinfo.c : $(CT_DIR)$(DIR_SEP)getinfo.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)getinfo$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)getinfo.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)getsecrt.c : $(CT_DIR)$(DIR_SEP)getsecrt.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)getsecrt$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)getsecrt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)keysec.c : $(CT_DIR)$(DIR_SEP)keysec.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)keysec$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)keysec.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)keytime.c : $(CT_DIR)$(DIR_SEP)keytime.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)keytime$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)keytime.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)like.c : $(CT_DIR)$(DIR_SEP)like.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)like$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)like.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)numconv.c : $(CT_DIR)$(DIR_SEP)numconv.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)numconv$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)numconv.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)screen2$(OBJEXT) : $(CT_DIR)$(DIR_SEP)screen2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)screen3.c : $(CT_DIR)$(DIR_SEP)screen3.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)screen3$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)screen3.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)setkeys.c : $(CT_DIR)$(DIR_SEP)setkeys.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)setkeys$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)setkeys.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)scrmark.c : $(CT_DIR)$(DIR_SEP)scrmark.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)scrmark$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)scrmark.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)showtime.c : $(CT_DIR)$(DIR_SEP)showtime.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)showtime$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)showtime.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)spread.c : $(CT_DIR)$(DIR_SEP)spread.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)spread$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)spread.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)trapkey.c : $(CT_DIR)$(DIR_SEP)trapkey.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)trapkey$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)trapkey.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)util.c : $(CT_DIR)$(DIR_SEP)util.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)util$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)util.c
	$(CC_CMD)

#===============================================================================
# HBPP.EXE rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hbpp$(OBJEXT) : $(HBPP_DIR)$(DIR_SEP)hbpp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbppcalc$(OBJEXT) : $(HBPP_DIR)$(DIR_SEP)hbppcalc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbppcomp$(OBJEXT) : $(HBPP_DIR)$(DIR_SEP)hbppcomp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbppcore$(OBJEXT) : $(HBPP_DIR)$(DIR_SEP)hbppcore.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbpptbl$(OBJEXT) : $(HBPP_DIR)$(DIR_SEP)hbpptbl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pragma$(OBJEXT) : $(HBPP_DIR)$(DIR_SEP)pragma.c
	$(CC_CMD)

#===============================================================================
# HBRUN.EXE rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hbrun.c : $(HBRUN_DIR)$(DIR_SEP)hbrun.prg
	$(HB_CMD_MAIN)

$(OBJ_DIR)$(DIR_SEP)hbrun$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbrun.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)external.c : $(HBRUN_DIR)$(DIR_SEP)external.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)external$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)external.c
	$(CC_CMD)

#===============================================================================
# HBTEST.EXE rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hbtest.c : $(HBTEST_DIR)$(DIR_SEP)hbtest.prg
	$(HB_CMD_MAIN)

$(OBJ_DIR)$(DIR_SEP)hbtest$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbtest.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_hvm.c : $(HBTEST_DIR)$(DIR_SEP)rt_hvm.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_hvm$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_hvm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_hvma.c : $(HBTEST_DIR)$(DIR_SEP)rt_hvma.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_hvma$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_hvma.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_math.c : $(HBTEST_DIR)$(DIR_SEP)rt_math.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_math$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_math.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_date.c : $(HBTEST_DIR)$(DIR_SEP)rt_date.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_date$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_date.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_str.c : $(HBTEST_DIR)$(DIR_SEP)rt_str.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_str$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_str.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_stra.c : $(HBTEST_DIR)$(DIR_SEP)rt_stra.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_stra$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_stra.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_trans.c : $(HBTEST_DIR)$(DIR_SEP)rt_trans.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_trans$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_trans.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_array.c : $(HBTEST_DIR)$(DIR_SEP)rt_array.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_array$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_array.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_file.c : $(HBTEST_DIR)$(DIR_SEP)rt_file.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_file$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_file.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_misc.c : $(HBTEST_DIR)$(DIR_SEP)rt_misc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_misc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_misc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_ccall.c : $(HBTEST_DIR)$(DIR_SEP)rt_ccall.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rt_ccall$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rt_ccall.c
	$(CC_CMD)

#===============================================================================
# HBDOC.EXE rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hbdoc.c : $(HBDOC_DIR)$(DIR_SEP)hbdoc.prg
	$(HB_CMD_MAIN)

$(OBJ_DIR)$(DIR_SEP)hbdoc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbdoc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genasc.c : $(HBDOC_DIR)$(DIR_SEP)genasc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genasc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genasc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)teeasc.c : $(HBDOC_DIR)$(DIR_SEP)teeasc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)teeasc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)teeasc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genhpc.c : $(HBDOC_DIR)$(DIR_SEP)genhpc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genhpc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genhpc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genhtm.c : $(HBDOC_DIR)$(DIR_SEP)genhtm.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genhtm$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genhtm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genchm.c : $(HBDOC_DIR)$(DIR_SEP)genchm.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genchm$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genchm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genng.c : $(HBDOC_DIR)$(DIR_SEP)genng.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genng$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genng.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genos2.c : $(HBDOC_DIR)$(DIR_SEP)genos2.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genos2$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genos2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genrtf.c : $(HBDOC_DIR)$(DIR_SEP)genrtf.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genrtf$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genrtf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gentrf.c : $(HBDOC_DIR)$(DIR_SEP)gentrf.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)gentrf$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)gentrf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)html.c : $(HBDOC_DIR)$(DIR_SEP)html.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)html$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)html.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ng.c : $(HBDOC_DIR)$(DIR_SEP)ng.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ng$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ng.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)os2.c : $(HBDOC_DIR)$(DIR_SEP)os2.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)os2$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)os2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rtf.c : $(HBDOC_DIR)$(DIR_SEP)rtf.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rtf$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rtf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)troff.c : $(HBDOC_DIR)$(DIR_SEP)troff.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)troff$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)troff.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fclass1.c : $(HBDOC_DIR)$(DIR_SEP)fclass1.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)fclass1$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)fclass1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ffile1.c : $(HBDOC_DIR)$(DIR_SEP)ffile1.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ffile1$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ffile1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ft_funcs.c : $(HBDOC_DIR)$(DIR_SEP)ft_funcs.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ft_funcs$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ft_funcs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)genpdf1.c : $(HBDOC_DIR)$(DIR_SEP)genpdf1.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)genpdf1$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)genpdf1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pdfhbdoc$(OBJEXT) : $(HBPDF_DIR)$(DIR_SEP)pdfhbdoc.c
	$(CC_CMD)

#===============================================================================
# HBMAKE.EXE rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hbmake.c : $(HBMAKE_DIR)$(DIR_SEP)hbmake.prg
	$(HB_CMD_MAIN)

$(OBJ_DIR)$(DIR_SEP)hbmake$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbmake.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tmake.c : $(HBMAKE_DIR)$(DIR_SEP)tmake.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tmake$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tmake.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbmutils.c : $(HBMAKE_DIR)$(DIR_SEP)hbmutils.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hbmutils$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbmutils.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)radios.c : $(HBMAKE_DIR)$(DIR_SEP)radios.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)radios$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)radios.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)checks.c : $(HBMAKE_DIR)$(DIR_SEP)checks.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)checks$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)checks.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pickarry.c : $(HBMAKE_DIR)$(DIR_SEP)pickarry.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pickarry$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pickarry.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pickfile.c : $(HBMAKE_DIR)$(DIR_SEP)pickfile.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pickfile$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pickfile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)prb_stak.c : $(HBMAKE_DIR)$(DIR_SEP)prb_stak.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)prb_stak$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)prb_stak.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbmlang$(OBJEXT) : $(HBMAKE_DIR)$(DIR_SEP)hbmlang.c
	$(CC_CMD)

#===============================================================================
# XBSCRIPT.EXE rules
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)xbscript.c : $(XBSCRIPT_DIR)$(DIR_SEP)xbscript.prg
	$(HB_CMD_MAIN)

$(OBJ_DIR)$(DIR_SEP)xbscript$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)xbscript.c
	$(CC_CMD)

#
# CONTRIB FILES
#
#===============================================================================
# FIREBIRD.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)TFirebird$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)TFirebird.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)TFirebird.c : $(FIREBIRD_DIR)$(DIR_SEP)TFirebird.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)firebird$(OBJEXT) : $(FIREBIRD_DIR)$(DIR_SEP)firebird.c
	$(CC_CMD)

#===============================================================================
# FI_LIB.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)fi_winfu$(OBJEXT) : $(FREEIMAGE_DIR)$(DIR_SEP)source$(DIR_SEP)fi_winfu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fi_wrp$(OBJEXT) : $(FREEIMAGE_DIR)$(DIR_SEP)source$(DIR_SEP)fi_wrp.c
	$(CC_CMD)

#===============================================================================
# GDLIB.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)gd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gdbar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)gdbar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gdbarcode$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)gdbarcode.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gdchart$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)gdchart.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gdimage$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)gdimage.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gdwrp$(OBJEXT) : $(GDLIB_DIR)$(DIR_SEP)source$(DIR_SEP)gdwrp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gd.c : $(GDLIB_DIR)$(DIR_SEP)source$(DIR_SEP)gd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)gdbar.c : $(GDLIB_DIR)$(DIR_SEP)source$(DIR_SEP)gdbar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)gdbarcode.c : $(GDLIB_DIR)$(DIR_SEP)source$(DIR_SEP)gdbarcode.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)gdchart.c : $(GDLIB_DIR)$(DIR_SEP)source$(DIR_SEP)gdchart.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)gdimage.c : $(GDLIB_DIR)$(DIR_SEP)source$(DIR_SEP)gdimage.prg
	$(HB_CMD)

#===============================================================================
# GTWVW.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)gtwvw$(OBJEXT) : $(GTWVW_DIR)$(DIR_SEP)gtwvw.c
	$(CC_CMD)


#===============================================================================
# HBMLIB_LIB.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)ioapi$(OBJEXT) : $(HBMZIP_DIR)$(DIR_SEP)ioapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mzip$(OBJEXT) : $(HBMZIP_DIR)$(DIR_SEP)zip.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)unzip$(OBJEXT) : $(HBMZIP_DIR)$(DIR_SEP)unzip.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbmzip$(OBJEXT) : $(HBMZIP_DIR)$(DIR_SEP)hbmzip.c
	$(CC_CMD)

#===============================================================================
# HBZIP.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)zip$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zip.c
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipstorage$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipstorage.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipstring$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipstring.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipplatformcomm$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipplatformcomm.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipplatform$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipplatform.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zippathcomponent$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zippathcomponent.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipmemfile$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipmemfile.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipnew$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipnew.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipcomp$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipcomp.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipfileheader$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipfileheader.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipfile$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipfile.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipexception$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipexception.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipcompatibility$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipcompatibility.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipcentraldir$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipcentraldir.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)zipautobuffer$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)zipautobuffer.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)ziparchive$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)ziparchive.cpp
	$(CC_CMD_ZIP)

$(OBJ_DIR)$(DIR_SEP)stdafx$(OBJEXT) : $(HBZIP_DIR)$(DIR_SEP)stdafx.cpp
	$(CC_CMD_ZIP)

#===============================================================================
# LIBNF.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)isbiton.c : $(LIBNF_DIR)$(DIR_SEP)isbiton.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)isbiton$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)isbiton.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mouse1.c : $(LIBNF_DIR)$(DIR_SEP)mouse1.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)acctadj.c : $(LIBNF_DIR)$(DIR_SEP)acctadj.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)aeminlen.c : $(LIBNF_DIR)$(DIR_SEP)aeminlen.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)lastday.c : $(LIBNF_DIR)$(DIR_SEP)lastday.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)restsets.c : $(LIBNF_DIR)$(DIR_SEP)restsets.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)acctyear.c : $(LIBNF_DIR)$(DIR_SEP)acctyear.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)datecnfg.c : $(LIBNF_DIR)$(DIR_SEP)datecnfg.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)adessort.c : $(LIBNF_DIR)$(DIR_SEP)adessort.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)anomatch.c : $(LIBNF_DIR)$(DIR_SEP)anomatch.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)any2any.c : $(LIBNF_DIR)$(DIR_SEP)any2any.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)asum.c : $(LIBNF_DIR)$(DIR_SEP)asum.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)bitclr.c : $(LIBNF_DIR)$(DIR_SEP)bitclr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)bitset.c : $(LIBNF_DIR)$(DIR_SEP)bitset.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tbwhile.c : $(LIBNF_DIR)$(DIR_SEP)tbwhile.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)blink.c : $(LIBNF_DIR)$(DIR_SEP)blink.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)byt2bit.c : $(LIBNF_DIR)$(DIR_SEP)byt2bit.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)byt2hex.c : $(LIBNF_DIR)$(DIR_SEP)byt2hex.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)byteand.c : $(LIBNF_DIR)$(DIR_SEP)byteand.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)byteneg.c : $(LIBNF_DIR)$(DIR_SEP)byteneg.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)byteor.c : $(LIBNF_DIR)$(DIR_SEP)byteor.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)daytobow.c : $(LIBNF_DIR)$(DIR_SEP)daytobow.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)clrsel.c : $(LIBNF_DIR)$(DIR_SEP)clrsel.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)cntryset.c : $(LIBNF_DIR)$(DIR_SEP)cntryset.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)elapsed.c : $(LIBNF_DIR)$(DIR_SEP)elapsed.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)woy.c : $(LIBNF_DIR)$(DIR_SEP)woy.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dectobin.c : $(LIBNF_DIR)$(DIR_SEP)dectobin.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hex2dec.c : $(LIBNF_DIR)$(DIR_SEP)hex2dec.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)e2d.c : $(LIBNF_DIR)$(DIR_SEP)e2d.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)vidcur.c : $(LIBNF_DIR)$(DIR_SEP)vidcur.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)elapmil.c : $(LIBNF_DIR)$(DIR_SEP)elapmil.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dosver.c : $(LIBNF_DIR)$(DIR_SEP)dosver.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)findith.c : $(LIBNF_DIR)$(DIR_SEP)findith.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)firstday.c : $(LIBNF_DIR)$(DIR_SEP)firstday.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)gcd.c : $(LIBNF_DIR)$(DIR_SEP)gcd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)min2dhm.c : $(LIBNF_DIR)$(DIR_SEP)min2dhm.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)invclr.c : $(LIBNF_DIR)$(DIR_SEP)invclr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)isbit.c : $(LIBNF_DIR)$(DIR_SEP)isbit.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)isshare.c : $(LIBNF_DIR)$(DIR_SEP)isshare.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)popadder.c : $(LIBNF_DIR)$(DIR_SEP)popadder.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)menutoNF.c : $(LIBNF_DIR)$(DIR_SEP)menuto.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)month.c : $(LIBNF_DIR)$(DIR_SEP)month.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)metaph.c : $(LIBNF_DIR)$(DIR_SEP)metaph.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)miltime.c : $(LIBNF_DIR)$(DIR_SEP)miltime.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)netpv.c : $(LIBNF_DIR)$(DIR_SEP)netpv.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)nooccur.c : $(LIBNF_DIR)$(DIR_SEP)nooccur.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pickday.c : $(LIBNF_DIR)$(DIR_SEP)pickday.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)prtesc.c : $(LIBNF_DIR)$(DIR_SEP)prtesc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)week.c : $(LIBNF_DIR)$(DIR_SEP)week.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)rand1.c : $(LIBNF_DIR)$(DIR_SEP)rand1.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)wda.c : $(LIBNF_DIR)$(DIR_SEP)wda.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sqzn.c : $(LIBNF_DIR)$(DIR_SEP)sqzn.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)menu1.c : $(LIBNF_DIR)$(DIR_SEP)menu1.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)vertmenu.c : $(LIBNF_DIR)$(DIR_SEP)vertmenu.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)aavg.c : $(LIBNF_DIR)$(DIR_SEP)aavg.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)acctweek.c : $(LIBNF_DIR)$(DIR_SEP)acctweek.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)bytenot.c : $(LIBNF_DIR)$(DIR_SEP)bytenot.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pchr.c : $(LIBNF_DIR)$(DIR_SEP)pchr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)d2e.c : $(LIBNF_DIR)$(DIR_SEP)d2e.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)xbox.c : $(LIBNF_DIR)$(DIR_SEP)xbox.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)acctqtr.c : $(LIBNF_DIR)$(DIR_SEP)acctqtr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)savesets.c : $(LIBNF_DIR)$(DIR_SEP)savesets.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dayofyr.c : $(LIBNF_DIR)$(DIR_SEP)dayofyr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dispmsg.c : $(LIBNF_DIR)$(DIR_SEP)dispmsg.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)page.c : $(LIBNF_DIR)$(DIR_SEP)page.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pending.c : $(LIBNF_DIR)$(DIR_SEP)pending.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)amedian.c : $(LIBNF_DIR)$(DIR_SEP)amedian.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)calendar.c : $(LIBNF_DIR)$(DIR_SEP)calendar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)dfile.c : $(LIBNF_DIR)$(DIR_SEP)dfile.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)year.c : $(LIBNF_DIR)$(DIR_SEP)year.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)easter.c : $(LIBNF_DIR)$(DIR_SEP)easter.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)roundNF.c : $(LIBNF_DIR)$(DIR_SEP)round.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)aemaxlen.c : $(LIBNF_DIR)$(DIR_SEP)aemaxlen.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)aading.c : $(LIBNF_DIR)$(DIR_SEP)aading.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)qtr.c : $(LIBNF_DIR)$(DIR_SEP)qtr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)madd.c : $(LIBNF_DIR)$(DIR_SEP)madd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)scregion.c : $(LIBNF_DIR)$(DIR_SEP)scregion.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)linked.c : $(LIBNF_DIR)$(DIR_SEP)linked.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sleep.c : $(LIBNF_DIR)$(DIR_SEP)sleep.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ntow.c : $(LIBNF_DIR)$(DIR_SEP)ntow.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)aredit.c : $(LIBNF_DIR)$(DIR_SEP)aredit.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)at2.c : $(LIBNF_DIR)$(DIR_SEP)at2.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)nwlstat.c : $(LIBNF_DIR)$(DIR_SEP)nwlstat.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tempfile.c : $(LIBNF_DIR)$(DIR_SEP)tempfile.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)sinkey.c : $(LIBNF_DIR)$(DIR_SEP)sinkey.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)vidmode.c : $(LIBNF_DIR)$(DIR_SEP)vidmode.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)eltime.c : $(LIBNF_DIR)$(DIR_SEP)eltime.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)diskfunc.c : $(LIBNF_DIR)$(DIR_SEP)diskfunc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pvid.c : $(LIBNF_DIR)$(DIR_SEP)pvid.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)bytexor.c : $(LIBNF_DIR)$(DIR_SEP)bytexor.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)acctmnth.c : $(LIBNF_DIR)$(DIR_SEP)acctmnth.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pegs.c : $(LIBNF_DIR)$(DIR_SEP)pegs.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)workdays.c : $(LIBNF_DIR)$(DIR_SEP)workdays.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)savearr.c : $(LIBNF_DIR)$(DIR_SEP)savearr.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)mouse1$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)mouse1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)acctadj$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)acctadj.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)aeminlen$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)aeminlen.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)lastday$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)lastday.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)restsets$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)restsets.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)acctyear$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)acctyear.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)datecnfg$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)datecnfg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)adessort$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)adessort.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)anomatch$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)anomatch.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)any2any$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)any2any.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)asum$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)asum.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bitclr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)bitclr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bitset$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)bitset.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tbwhile$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tbwhile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)blink$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)blink.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)byt2bit$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)byt2bit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)byt2hex$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)byt2hex.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)byteand$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)byteand.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)byteneg$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)byteneg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)byteor$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)byteor.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)daytobow$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)daytobow.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)clrsel$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)clrsel.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)cntryset$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)cntryset.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)elapsed$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)elapsed.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)woy$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)woy.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dectobin$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dectobin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hex2dec$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hex2dec.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)e2d$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)e2d.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)elapmil$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)elapmil.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dosver$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dosver.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)findith$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)findith.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)firstday$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)firstday.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)gcd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)gcd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)min2dhm$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)min2dhm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)invclr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)invclr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)isbit$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)isbit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)isshare$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)isshare.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)popadder$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)popadder.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)menutoNF$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)menutoNF.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)month$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)month.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)metaph$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)metaph.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)miltime$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)miltime.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)netpv$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)netpv.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)nooccur$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)nooccur.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pickday$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pickday.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)prtesc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)prtesc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)week$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)week.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rand1$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)rand1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)wda$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)wda.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sqzn$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sqzn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)menu1$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)menu1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)vertmenu$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)vertmenu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)aavg$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)aavg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)acctweek$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)acctweek.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bytenot$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)bytenot.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pchr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pchr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)d2e$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)d2e.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xbox$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)xbox.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)acctqtr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)acctqtr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)savesets$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)savesets.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dayofyr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dayofyr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dispmsg$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dispmsg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)page$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)page.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pending$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pending.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)amedian$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)amedian.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)calendar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)calendar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)year$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)year.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)easter$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)easter.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)roundNF$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)roundNF.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)aemaxlen$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)aemaxlen.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)aading$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)aading.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)qtr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)qtr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)madd$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)madd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)scregion$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)scregion.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)linked$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)linked.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sleep$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sleep.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ntow$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ntow.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)aredit$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)aredit.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)at2$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)at2.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)nwlstat$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)nwlstat.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tempfile$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tempfile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)sinkey$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)sinkey.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)vidmode$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)vidmode.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)eltime$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)eltime.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)diskfunc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)diskfunc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pvid$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pvid.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bytexor$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)bytexor.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dfile$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)dfile.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)acctmnth$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)acctmnth.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pegs$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pegs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)workdays$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)workdays.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)savearr$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)savearr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)caplock$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)caplock.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)shift$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)shift.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)color2n$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)color2n.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)descendNF$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)descend.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)numlock$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)numlock.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)proper$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)proper.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mouse$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)mouse.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)getvid$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)getvid.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)chdir$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)chdir.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)rmdir$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)rmdir.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)alt$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)alt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ctrl$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)ctrl.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)getenvrn$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)getenvrn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)n2color$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)n2color.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)origin$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)origin.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)prtscr$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)prtscr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)stod$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)stod.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)kspeed$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)kspeed.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mkdir$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)mkdir.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)getver$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)getver.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ftattr$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)ftattr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)dispc$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)dispc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fttext$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)fttext.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)putkey$(OBJEXT) : $(LIBNF_DIR)$(DIR_SEP)putkey.c
	$(CC_CMD)

#===============================================================================
# MYSQL.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)mysqlrdd$(OBJEXT): $(OBJ_DIR)$(DIR_SEP)mysqlrdd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tmysql$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tmysql.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)tsqlbrw$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)tsqlbrw.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mysql$(OBJEXT) : $(MYSQL_DIR)$(DIR_SEP)mysql.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)mysqlrdd.c : $(MYSQL_DIR)$(DIR_SEP)mysqlrdd.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tmysql.c : $(MYSQL_DIR)$(DIR_SEP)tmysql.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)tsqlbrw.c : $(MYSQL_DIR)$(DIR_SEP)tsqlbrw.prg
	$(HB_CMD)

#===============================================================================
# PDFLIB.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)pdf1$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pdf1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pdfhbdocs$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pdfhbdocs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pdf1.c : $(PDFLIB_DIR)$(DIR_SEP)pdf1.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pdfhbdocs.c : $(PDFLIB_DIR)$(DIR_SEP)pdfhbdocs.prg
	$(HB_CMD)

#===============================================================================
# PQSQL.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)postgres$(OBJEXT) : $(PGSQL_DIR)$(DIR_SEP)postgres.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)TPostgres$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)TPostgres.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)TPostgres.c : $(PGSQL_DIR)$(DIR_SEP)TPostgres.prg
	$(HB_CMD)

#===============================================================================
# TELEPATH.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)win32$(OBJEXT) : $(TELEPATH_DIR)$(DIR_SEP)win32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)telepath$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)telepath.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)telepath.C : $(TELEPATH_DIR)$(DIR_SEP)telepath.PRG
	$(HB_CMD)

#===============================================================================
# HBCC.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)hbcc$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbcc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcc.c : $(HBCC_DIR)$(DIR_SEP)hbcc.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hbc7$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbc7.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcu$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbcu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcx$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbcx.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcy$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbcy.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbhex$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbhex.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcrc16$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbcrc16.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcrc32HBCC$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbcrc32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcrpt32$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbcrpt32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbcrpt128$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbcrpt128.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbencode$(OBJEXT) : $(HBCC_DIR)$(DIR_SEP)hbencode.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbdecode$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hbdecode.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hbdecode.c : $(HBCC_DIR)$(DIR_SEP)hbdecode.prg
	$(HB_CMD)

#===============================================================================
# WHAT32.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)_winbmp$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winbmp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winbrsh$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winbrsh.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wincall$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wincall.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wincdlg$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wincdlg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winclpb$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winclpb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wincomm$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wincomm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wincret$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wincret.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_windate$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_windate.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_windc$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_windc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_windir$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_windir.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_windlg$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_windlg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_windll$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_windll.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_windraw$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_windraw.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winfont$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winfont.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wingdi$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wingdi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winhead$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winhead.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winicon$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winicon.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winilst$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winilst.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winINet$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winINet.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winini$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winini.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winkbrd$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winkbrd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winlv$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winlv.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmain$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmain.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmapi$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmapi.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmem$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmem.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmenu$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmenu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmeta$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmeta.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmisc$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmisc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmmcap$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmmcap.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmous$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmous.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winmsg$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winmsg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winpen$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winpen.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winprn$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winprn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winrect$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winrect.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winreg$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winreg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winrgn$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winrgn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winscrlb$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winscrlb.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winshell$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winshell.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winsock$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winsock.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winsys$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winsys.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wintab$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wintab.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wintbar$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wintbar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_wintext$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_wintext.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winTree$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winTree.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winview$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winview.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winwnd$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winwnd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)_winserial$(OBJEXT) : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)_winserial.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)DebugWHAT32$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)DebugWHAT32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)ErrorSysWHAT32$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)ErrorSysWHAT32.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WhatUtil$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WhatUtil.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WinCDlg$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WinCDlg.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WinComm$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WinComm.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WinCore$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WinCore.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WinIni$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WinIni.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WinRBar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WinRBar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WinTBar$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WinTBar.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)WinTabs$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)WinTabs.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)DebugWHAT32.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)Debug.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)ErrorSysWHAT32.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)ErrorSys.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WhatUtil.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WhatUtil.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WinCDlg.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WinCDlg.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WinComm.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WinComm.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WinCore.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WinCore.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WinIni.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WinIni.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WinRBar.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WinRBar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WinTBar.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WinTBar.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)WinTabs.c : $(WHAT32_DIR)$(DIR_SEP)source$(DIR_SEP)WinTabs.prg
	$(HB_CMD)

#===============================================================================
# XWT.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)xwt_api$(OBJEXT) : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)xwt_api.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xwt_win$(OBJEXT) : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)xwt_win$(DIR_SEP)xwt_win.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xwt_win_menu$(OBJEXT) : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)xwt_win$(DIR_SEP)xwt_win_menu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xwt_win_menuitem$(OBJEXT) : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)xwt_win$(DIR_SEP)xwt_win_menuitem.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xwt_win_framewnd$(OBJEXT) : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)xwt_win$(DIR_SEP)xwt_win_framewnd.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)xwt_win_msgbox$(OBJEXT) : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)xwt_win$(DIR_SEP)xwt_win_msgbox.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)button$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)button.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)bcolumn$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)bcolumn.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)browseXWT$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)browseXWT.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)checkboxXWT$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)checkboxXWT.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)container$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)container.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)event$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)event.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)filesel$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)filesel.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)fontsel$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)fontsel.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)framewindow$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)framewindow.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)grid$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)grid.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hex$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)hex.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)image$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)image.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)inputmask$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)inputmask.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)label$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)label.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)laycontainer$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)laycontainer.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)layout$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)layout.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)menu$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)menu.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)menuitem$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)menuitem.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)msgbox$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)msgbox.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pane$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)pane.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)radiobutton$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)radiobutton.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)splitter$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)splitter.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)textbox$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)textbox.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)togglebutton$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)togglebutton.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)treeitem$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)treeitem.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)treelist$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)treelist.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)viewport$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)viewport.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)widget$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)widget.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)window$(OBJEXT) : $(OBJ_DIR)$(DIR_SEP)window.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)button.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)button.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)bcolumn.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)bcolumn.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)browseXWT.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)browse.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)checkboxXWT.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)checkbox.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)container.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)container.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)event.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)event.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)filesel.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)filesel.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)fontsel.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)fontsel.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)framewindow.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)framewindow.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)grid.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)grid.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)hex.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)hex.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)image.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)image.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)inputmask.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)inputmask.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)label.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)label.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)laycontainer.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)laycontainer.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)layout.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)layout.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)menu.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)menu.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)menuitem.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)menuitem.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)msgbox.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)msgbox.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)pane.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)pane.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)radiobutton.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)radiobutton.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)splitter.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)splitter.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)textbox.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)textbox.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)togglebutton.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)togglebutton.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)treeitem.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)treeitem.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)treelist.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)treelist.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)viewport.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)viewport.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)widget.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)widget.prg
	$(HB_CMD)

$(OBJ_DIR)$(DIR_SEP)window.c : $(XWT_DIR)$(DIR_SEP)src$(DIR_SEP)window.prg
	$(HB_CMD)

#===============================================================================
# PNG.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)png$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)png.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngerror$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngerror.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngget$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngget.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngmem$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngmem.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngpread$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngpread.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngread$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngread.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngrio$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngrio.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngrtran$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngrtran.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngrutil$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngrutil.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngset$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngset.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngtrans$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngtrans.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngwio$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngwio.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngwrite$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngwrite.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngwtran$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngwtran.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)pngwutil$(OBJEXT) : $(PNG_DIR)$(DIR_SEP)pngwutil.c
	$(CC_CMD)

#===============================================================================
# HBHPDF.LIB dependencies
#===============================================================================
$(OBJ_DIR)$(DIR_SEP)harupdf$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)harupdf.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_utils$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_utils.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_error$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_error.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_mmgr$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_mmgr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_list$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_list.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_streams$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_streams.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_objects$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_objects.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_null$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_null.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_boolean$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_boolean.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_number$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_number.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_real$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_real.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_name$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_name.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_array$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_array.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_dict$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_dict.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_xref$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_xref.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_encoder$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_encoder.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_string$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_string.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_binary$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_binary.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_encrypt$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_encrypt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_encryptdict$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_encryptdict.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_tt$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_tt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_type1$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_type1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_base14$(OBJEXT): $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_base14.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_cid$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_cid.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_font$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_font.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_font_type1$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_font_type1.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_font_tt$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_font_tt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_font_cid$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_font_cid.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_doc$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_doc.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_info$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_info.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_catalog$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_catalog.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_page_label$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_page_label.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_gstate$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_gstate.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_pages$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_pages.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_page_operator$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_page_operator.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_destination$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_destination.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_annotation$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_annotation.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_outline$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_outline.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_image$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_image.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_jp$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_encoder_jp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_kr$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_encoder_kr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_cns$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_encoder_cns.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_encoder_cnt$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_encoder_cnt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_jp$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_jp.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_kr$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_kr.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_cns$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_cns.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_fontdef_cnt$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_fontdef_cnt.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_image_png$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_image_png.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_doc_png$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_doc_png.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_ext_gstate$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_ext_gstate.c
	$(CC_CMD)

$(OBJ_DIR)$(DIR_SEP)hpdf_u3d$(OBJEXT) : $(HBHPDF_DIR)$(DIR_SEP)hpdf_u3d.c
	$(CC_CMD)

