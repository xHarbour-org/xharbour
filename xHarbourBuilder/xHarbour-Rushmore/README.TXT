/*
 * $Id$
 */

   EMPTY() function optimization - IMHO impossible - such optimization
                                   will be buggy for strings because
                                   empty string may contain any combination
                                   of TABs, LFs, CRs and SPACEs
                                   It could be done for spaces only.

   DTOS() function optimization
      index on DATE to TAG1; SET FILTER TO DTOS(DATE) <op> <cVal>
      index on DTOS(DATE) to TAG1; SET FILTER TO DATE <op> <dVal>

   (done) Abberivated function names:
      SUBSTR
      DELETED
      REPLICATE


TODO:
=====
 1. Some function detection in L-value like DTOS, DTOC, YEAR
 2. Optimize the stack before evaluation (DONE)
 3. Optimize for noninclusive ranges: a > x .and. a < y (DONE)
 4. The error system sth like m6_error() to report different errors
    and early expression type checking (DONE but it's not MT safe)
    Should I optimize UDFs R-value??? - now it's done.
 5. Some optimization technics which are now only in my head ;-)

 1   is IMHO necessary and I'll do it ASAP
 2-3 are for speed improvement (DONE)
 4   is mostly for future SIX m6_error() compatibility.
 5 ??? somewhere in the future

History:

 $Log$
 Revision 1.24  2007/01/13 21:57:28  Przemyslaw Czerpak
 2007-01-13 22:55 UTC+0100 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmcdx1.c
     + added automatic WA filter resizing

 Revision 1.23  2006/12/11 14:41:34  Przemyslaw Czerpak
 2006-12-11 15:40 UTC+0100 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/m6funcs.c
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
     * cleaned BCC warnings

 Revision 1.22  2006/11/10 01:18:06  Przemyslaw Czerpak
 2006-11-10 02:15 UTC+0100 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap2.c
     ! added missing line with:
         pRM->iOptLvl = iOptLvl;

 Revision 1.21  2006/09/29 10:18:44  Przemyslaw Czerpak
 2006-09-29 12:15 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmcdx1.c
     % retrive information directly from bitmap filter when ordKeyPos() or
       ordKeyNo() are executed and current index is set to 0 to not enable
       slow emulation of filters respecting in pure DBFCDX.
     % optimized SKIP() when bitmap filter is set and current index is 0
     + added rlPosRecNo( <rl>, <nRecNo> ) -> <nLogicalPos>

 Revision 1.20  2006/07/11 19:38:13  Przemyslaw Czerpak
 2006-07-11 21:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap2.c
     ! fixed GPF reported by Ron

   * xHarbour-Rushmore/dbrmap/rmcdx1.c
     * updated to recent core code modifications

 Revision 1.19  2005/11/25 11:41:32  Przemyslaw Czerpak
 2005-11-25 12:43 UTC+0100 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap2.c
     * code cleaned

   * xHarbour-Rushmore/dbrmap/rmcdx1.c
     * update work area bitmap filter when record is changed and written
       to disk - COMIX compatibility

 Revision 1.18  2005/11/07 02:42:33  Przemyslaw Czerpak
 2005-11-07 03:47 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap2.c
     - removed hb_itemClone()

 Revision 1.17  2005/10/24 16:25:51  Przemyslaw Czerpak
 2005-10-24 18:28 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/README.TXT
   * xHarbour-Rushmore/rmtst.prg
   * xHarbour-Rushmore/dbrmap/Makefile
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
   * xHarbour-Rushmore/dbrmap/m6funcs.c
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
     * updated for recent CVS modifications and some minor cleanups

 Revision 1.16  2005/10/04 10:49:37  Przemyslaw Czerpak
 2005-10-04 12:52 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/Makefile
   * xHarbour-Rushmore/dbrmap/rmcdx1.c
   - xHarbour-Rushmore/dbrmap/rmcdx0.prg
   - xHarbour-Rushmore/dbrmap/rmntx0.prg
     * updated to recent core RDD code in xHarbour CVS

 Revision 1.15  2005/09/20 19:17:22  Przemyslaw Czerpak
 2005-09-20 21:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/Makefile
     * minor cleanup

   * xHarbour-Rushmore/dbrmap/rmap1.c
     * RLOPTLEVEL() can receive now as parameter also RL filter handle
       and returns optimization level reach for this filter

   * xHarbour-Rushmore/dbrmap/rmcdx1.c
     * updated for core RDD modifications

   * xHarbour-Rushmore/dbrmap/rmntx0.prg
     * by default use DBT memo in RMDBFNTX

 Revision 1.14  2005/09/11 20:04:10  Przemyslaw Czerpak
 2005-09-11 22:06 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
   * xHarbour-Rushmore/dbrmap/rmcdx1.c
    * updated for recent RDD modification in xHarbour core code
    * minor modification in returned nonopt expression

 Revision 1.13  2005/09/05 14:31:24  Przemyslaw Czerpak
 2005-09-05 16:30 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
   * xHarbour-Rushmore/dbrmap/rmcdx1.c
     * changed extracting of nonoptimized part of expression - now it's
       absolutely not SIX3 compatible but return more accurate data
     * added new function RLNONOP( <cExp> ) -> <cNonOptExp> to return
       nonoptimized part of expression
     ! fixed area filter detaching i broke in previous commit

 Revision 1.12  2005/08/23 16:27:47  Przemyslaw Czerpak
 2005-08-23 18:30 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
   * xHarbour-Rushmore/dbrmap/m6compat.prg
   * xHarbour-Rushmore/dbrmap/m6funcs.c
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmcdx1.c
     * updated for recen RDD modifications
     * M6_FILTERJOIN() should not destroy the second filter if it's
       own by system
     + RL_RESIZE( <rl>, <newSize> ) -> <rl> function added

 Revision 1.11  2005/08/06 19:36:10  Przemyslaw Czerpak
 2005-08-06 21:30 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
     + macros to set/get/remove RM filters in WA

   * xHarbour-Rushmore/dbrmap/m6compat.prg
     ! restore previous filter if any in m6_dbEval()

   * xHarbour-Rushmore/dbrmap/rmap1.c
     * use DBI_* interface instead of direct AREAP access

   * xHarbour-Rushmore/dbrmap/m6funcs.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
     * formatting

   * xHarbour-Rushmore/dbrmap/Makefile
   - xHarbour-Rushmore/dbrmap/rmap0.prg
   - xHarbour-Rushmore/dbrmap/rmap3.c
   + xHarbour-Rushmore/dbrmap/rmcdx0.prg
   + xHarbour-Rushmore/dbrmap/rmcdx1.c
     * file names changed

   * xHarbour-Rushmore/dbrmap/Makefile
   + xHarbour-Rushmore/dbrmap/rmntx0.prg
   + xHarbour-Rushmore/dbrmap/rmntx1.c
     + added RMDBFNTX

 Revision 1.10  2005/08/05 08:33:17  Przemyslaw Czerpak
 2005-08-05 10:30 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap0.prg
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
   * xHarbour-Rushmore/dbrmap/rmap3.c
     * updated for recent base RDD modifications

 Revision 1.9  2005/04/21 16:12:26  Przemyslaw Czerpak
 2005-04-21 18:00 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/Makefile
   + xHarbour-Rushmore/dbrmap/machsix.ch
   - xHarbour-Rushmore/dbrmap/rmerror.ch
     * renamed the include file

   * xHarbour-Rushmore/dbrmap/cmcompat.prg
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
   * xHarbour-Rushmore/dbrmap/rmap3.c
     * updated for recent modifications in xHarbour CVS
     + updated for HSX support
     + extended for MachSIX support - all low level functions
       implemented
     ! fixed bug in rlDupNew() which caused GPF.
     + copy in rlDupNew() the filter expressions (MachSIX extension)

   + xHarbour-Rushmore/dbrmap/m6compat.prg
   + xHarbour-Rushmore/dbrmap/m6funcs.c
     + all MachSIX functions implemented. m6_FiltSave()/m6_filtRestore()
       use differ binary format then SIX3 and cannot be shared. I made
       it intentionally - SIX dumps memory structures as is and it does
       not support some extensions like "maybe" records which xHarbour does.

 Revision 1.8  2005/02/04 01:11:31  Przemyslaw Czerpak
 2005-02-05 02:05 UTC+0100 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap1.c
     * clear the unused bits in negated filter.

   * xHarbour-Rushmore/dbrmap/cmcompat.prg
     ! cmFiltCount() should return rlCount() for current RM filter.
       deleted records for fully optimized expressions and never
       check scopes.
     * modified cm(x)Key*() functions to use DBOI_KEYGOTO[RAW] and
       DBOI_KEYNO[RAW] and DBOI_KEYCOUNT[RAW] to avoid confusion
       what they exactly do ;-)

   * xHarbour-Rushmore/dbrmap/rmap2.c
     * disabled DELETED() and logical position optimization in
       ordKey{Count,Goto,Pos}() functions.

   * xHarbour-Rushmore/dbrmap/rmap3.c
     ! fixed typo in FOR condition

 Revision 1.7  2005/01/31 01:53:13  Przemyslaw Czerpak
 2005-31-01 02:15 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/cmcompat.prg
     + added missing cmxKeyCount()

   * xHarbour-Rushmore/dbrmap/rmap0.prg
     + added REQUEST DBFCDX

   * xHarbour-Rushmore/dbrmap/rmap1.c
     * changed hb_rmCountRecords(), hb_rmNextRecord(), hb_rmPrevRecord()
       to respect "maybe" records - I hope it's ClipMore compatible.

   * xHarbour-Rushmore/dbrmap/rmap3.c
     + added logic to detect abberivated function names

 Revision 1.6  2004/09/21 22:04:53  Przemyslaw Czerpak
 2004-09-22 00:05 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/cmcompat.prg
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
     * updated to the last changes in xHarbour SF CVS.
       RMDBFCDX has to be rebuild !!! to work with last CVS.

 Revision 1.5  2004/09/12 23:45:58  Przemyslaw Czerpak
 2004-09-13 01:50 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/README.TXT
   * xHarbour-Rushmore/rmtst.prg
   * xHarbour-Rushmore/dbrmap/Makefile
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
   * xHarbour-Rushmore/dbrmap/rmap3.c
     % detect ranges in AND operation - it will speedup some operation
     % some other improvements
     ! some fixes
     IMHO now it's ready for tests

 Revision 1.4  2004/09/06 14:37:01  Przemyslaw Czerpak
 2004-09-06 16:30 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap3.c
    ! added missing braces.

 Revision 1.3  2004/08/29 18:19:35  Przemyslaw Czerpak
 2004-08-29 20:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/dbrmap/rmap3.c
    * Detect non full L-value in character expressions, f.e.
      set filter to a='x' when we have index on a+b
   * xHarbour-Rushmore/README.TXT
    * TODO list updated

 Revision 1.2  2004/08/27 22:36:33  Przemyslaw Czerpak
 2004-08-28 00:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   * xHarbour-Rushmore/README.TXT
   * xHarbour-Rushmore/dbrmap/Makefile
   * xHarbour-Rushmore/dbrmap/hbrddrm.h
   * xHarbour-Rushmore/dbrmap/rmap1.c
   * xHarbour-Rushmore/dbrmap/rmap2.c
   + xHarbour-Rushmore/dbrmap/rmap3.c
    + query analizer implemented
      the DBSETFILTER() autooptimization is already done.
      It's not yet as functional as I want but I'll try implement
      the missing functionality ASAP - see README.TXT
      The QUERY analyzer is a little bit differ then in SIX and COMIX.
      It can and try to use also conditional indexes when the FOR
      expression is part of filter query - such optimization when
      available is even more efficient then checking for KEY expression
      because in record scanning process we do not have to check for the
      ranges.

 Revision 1.1  2004/08/17 22:52:27  Przemyslaw Czerpak
 2004-08-18 00:50 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
   + xHarbour-Rushmore/dbrmap/Makefile
   + xHarbour-Rushmore/dbrmap/cmcompat.prg
   + xHarbour-Rushmore/dbrmap/hbrddrm.h
   + xHarbour-Rushmore/dbrmap/rmap0.prg
   + xHarbour-Rushmore/dbrmap/rmap1.c
   + xHarbour-Rushmore/dbrmap/rmap2.c
   + xHarbour-Rushmore/README.TXT
   + xHarbour-Rushmore/rmtst.prg
     + initial version uploaded



The following COMIX functions has been implemented:
===================================================
cmxClrScope()     Clear top or bottom index scope                     3.00.00
cmxCount()        Get the count of indexes in use                     3.00.00
cmxCustom()       Get/set Custom Built Index flag for an index        3.00.00
cmxDescend()      Get/set descending flag of an index                 3.00.00
cmxFor()          Get/set FOR condition of an index                   3.00.00
cmxIndexInfo()    Return an array of index information                3.00.00
cmxKeyAdd()       Add a key to a Custom Built Index                   3.00.00
cmxKeyCount()     Get the number of keys in an index                  3.00.00
cmxKeyDel()       Delete a key from a Custom Built Index              3.00.00
cmxKeyGoto()      Goto the specified key number                       3.00.00
cmxKeyNo()        Get the logical record number of key in an index    3.00.00
cmxKeysIncluded() Get number of keys included in index so far         3.00.05
cmxKeySkip()      Skip specified number of index keys                 1.00.13
cmxKeyVal()       Get current key value from master index             3.00.00
cmxMemoBlock()    Get/set memo block size                             3.00.00
cmxMemoExt()      Get memo file extension                             3.00.02
cmxRollback()     Rollback changes made to current record             3.00.03
cmxSeekLast()     Seek to the last occurrence of a key                3.00.00
cmxSetRelation()  Set a scoped relation                               3.00.00
cmxSetScope()     Get/set top or bottom index scope                   3.00.00
cmxShared()       Get/set shared/locking mode for database & indexes  1.00.19
cmxSkipUnique()   Skip to next/previous unique key                    3.00.00
cmxSys()          Provide low-level control over Comix's operation    1.00.22
cmxUnique()       Get the unique flag for an index                    3.00.00
cmxVersion()      Get version of the Comix RDD                        1.00.00
ordBagExt()       Get the default extension of an index               1.00.14
ordBagName()      Get the index file name of an index                 1.00.14
ordDestroy()      Delete an index from an index file                  3.00.00
ordFor()          Get the FOR condition for an index                  3.00.00
ordKey()          Get the key expression for an index                 3.00.00
ordName()         Get the name of the nOrder'th index                 1.00.10
ordNumber()       Get the number of an index                          3.00.00
ordSetFocus()     Make the specified index the master index           3.00.00

The following CLIPMORE functions has been implemented:
======================================================
cm2Str()          Convert variable to equivalent string constant      3.00.00
cmClrFilter()     Clear index filter previously set up by cmFilter()  2.00.00
cmDestroyFilter() Destroy a saved filter without restoring it         2.00.00
cmDoLinear()      Perform Linear Optimization/eliminate maybe's       2.00.00
cmFiltCount()     Get the number of records which meet the condition  2.00.00
cmFilter()        Rapidly isolate records meeting a condition         2.00.00
cmGetLinear()     Get the current setting of Linear Optimization      2.00.00
cmKeyCount()      Return the number of keys in the index (filtered)   2.00.03
cmKeyGoto()       Goto the specified logical key number (filtered)    2.00.03
cmKeyNo()         Return the logical key number (filtered)            2.00.03
cmKeySkip()       Skip the specified number of keys (filtered)        2.00.03
cmReFilter()      Rapidly isolate records meeting additive condition  2.00.00
cmRestFilter()    Restore a saved filter                              2.00.00
cmSaveFilter()    Save the current filter state                       2.00.00
cmSetLinear()     Set whether or not to use Linear Optimization       2.00.00
cmSmartFilter()   Get/set smart filter setting                        3.00.00
cmVersion()       Get ClipMore version                                3.00.00
rlAnd()           AND together two RecordList's                       2.00.00
rlClear()         Remove a record from (clear a bit in) a RecordList  2.00.00
rlCount()         Count the number of records in a RecordList         2.00.00
rlDestroy()       Destroy (free memory for) a RecordList              2.00.00
rlExFilter()      Extract the Filter RecordList                       2.00.00
rlGetFilter()     Get the Filter RecordList                           2.00.00
rlHasMaybe()      Determine if the RecordList has any Maybe's         2.00.00
rlMaybeEval()     Remove Maybe's from a RecordList                    2.00.00
rlLen()           Get the length of a RecordList                      2.00.00
rlOptLevel()      Get optimization level rlNewQuery() achieved        2.00.02
rlOr()            OR together two RecordList's                        2.00.00
rlNew()           Create a RecordList                                 2.00.00
rlNewDup()        Create a duplicate of a RecordList                  2.00.00
rlNewQuery()      Create a RecordList of recs meeting the condition   2.00.00
rlNextRecno()     Get the next record in a RecordList                 2.00.00
rlNot()           NOT a RecordList                                    2.00.00
rlPrevRecno()     Get the previous record in a RecordList             2.00.00
rlSet()           Add a record to (set bit in) a RecordList           2.00.00
rlSetFilter()     Set the Filter RecordList                           2.00.00
rlSetLoHi()       Add records in range to a RecordList                2.00.00
rlTest()          Determine if a record is in a RecordList            2.00.00

The following MachSIx functions has been implemented:
=====================================================
m6_AddScoped()     Adds records in a range to a filter
m6_ChgOwner()      Set ownership of RYO filter to user
m6_dbEval()        Optimized version of dbEval()'s with FOR conditions
m6_Error()         Get error code for last command/function
m6_EvalPartial()   Resolve partially optimizable filters
m6_FiltAddRec()    Adds record to a filter
m6_FiltChgRec()    Toggle record status in a filter
m6_FiltCopy()      Make copy of a specified filter
m6_FiltCount()     Returns number of records in a filter
m6_FiltDropRec()   Drop a record from a filter
m6_FiltInfo()      Get information about a specified filter
m6_FiltInverse()   Change state of all filter records
m6_FiltJoin()      Join two filters
m6_FiltRestore()   Restore filter from a file
m6_FiltSave()      Save filter to a file
m6_FiltSkip()      Position pointer to the next filtered record
m6_FiltBott()      Position pointer to last filtered record
m6_FiltGoRec()     Position pointer to specified record
m6_FiltTop()       Position pointer to first filtered record
m6_FreeFilter()    Release a Roll-Your-Own filter from memory
m6_GetAreaFilter() Gets a handle to workarea filter
m6_IsFilter()      Returns the optimization level of the active filter
m6_IsFiltRec()     Test if record is in a Roll-Your-Own filter
m6_IsOptimize()    Measures the optimizability of a FOR expression
m6_NewFilter()     Creates a new Roll-Your-Own filter
m6_RecCount()      Returns the number of records found in last query
m6_RefreshFilter() Refreshes the current filter on shared files
m6_SetAreaFilter() Sets workarea filter to a Roll-Your-Own filter
m6_SetFilter()     Hide records not meeting a condition
m6_SetTemp()       Set path for temporary files
m6_Version()       Returns the Mach SIx version information
m6_Error()         Get error code for last command/function

The following HiPer-SEEK functions has been implemented (xHarbour CVS):
=======================================================================
hs_Add()          Adds a text string entry to a HiPer-SEEK index file
hs_Close()        Closes a previously opened HiPer-SEEK index file
hs_Create()       Creates a new, empty HiPer-SEEK index file
hs_Delete()       Deletes specifed index record from HiPer-SEEK index file
hs_Filter()       Sets a filter using a HiPer-SEEK index
hs_IfDel()        Determines if a HiPer-SEEK record is marked as deleted
hs_Index()        Creates and populates a new HiPer-SEEK index
hs_KeyCount()     Returns the number of entries in a HiPer-SEEK index
hs_Next()         Searches a HiPer-SEEK index file for first/next match
hs_Open()         Opens an existing HiPer-SEEK index file
hs_Replace()      Replaces current HiPer-SEEK index entry with a new value
hs_Set()          Sets up parameters for a subsequent hs_Next() call
hs_Undelete()     Unmarks the specified HiPer-SEEK record as being deleted
hs_Verify()       Verifies hs_Next() hit against code block expression

The following CFTS functions has been implemented (xHarbour CVS):
=================================================================
CftsAdd()         Add a new record to the CFTS index.
CftsClose()       Close an open CFTS index.
CftsCrea()        Create CFTS index.
CftsDelete()      Mark CFTS index record as deleted.
CftsIfdel()       Test if CFTS index record is deleted.
CftsNext()        Get next CFTS record matching the criteria set by CftsSet.
CftsOpen()        Open a CFTS index.
CftsRecn()        Get number of records in CFTS index.
CftsReplac()      Replace or update a CFTS record.
CftsSet()         Prepare CFTS index for search by CftsNext.
CftsUndel()       Undelete a CFTS index record that is marked deleted.
CftsVeri()        Verify a target string against the .DBF source string.
CftsVers()        Returns version information of the CFTS library as a string.
