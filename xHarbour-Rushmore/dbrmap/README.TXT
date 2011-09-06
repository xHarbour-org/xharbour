/*
 * $Id$
 */

GNU make build
==============
put this file in: xharbour/source/rdd/dbrmap/ directory
and set environment variable: HB_DB_DRVEXT to dbrmap, f.e.:
   set HB_DB_DRVEXT=dbrmap
then run GNU make. It should build dbrmap library.


TODO:
=====
   EMPTY() function optimization - IMHO impossible - such optimization
                                   will be buggy for strings because
                                   empty string may contain any combination
                                   of TABs, LFs, CRs and SPACEs
                                   It could be done for spaces only.

   DTOS() function optimization (DONE)
      index on DATE to TAG1; SET FILTER TO DTOS(DATE) <op> <cVal>
      index on DTOS(DATE) to TAG1; SET FILTER TO DATE <op> <dVal>

   (done) Abberivated function names:
      SUBSTR
      DELETED
      REPLICATE
   TODO?
      LOWER
      UPPER
      LTRIM
      RTRIM
      SPACE
      TRANSFORM
      VALTYPE
      RECNO
      MONTH
      ROUND

 1. Some function detection in L-value like DTOS (DONE), DTOC, YEAR
 2. Optimize the stack before evaluation (DONE)
 3. Optimize for noninclusive ranges: a > x .and. a < y (DONE)
 4. The error system sth like m6_error() to report different errors
    and early expression type checking (DONE)
    Should I optimize UDFs R-value??? - now it's done.
 5. Some optimization technics which are now only in my head ;-)

 1   is IMHO necessary and I'll do it ASAP
 2-3 are for speed improvement (DONE)
 4   is mostly for future SIX m6_error() compatibility (DONE).
 5 ??? somewhere in the future



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

Undocumented MachSIx functions, not implemented
===============================================
   m6_ordCondSet()
   m6_createIndex()
   m6_ordCreate()
   m6_dbFilter()
   m6_dbContinue()
   m6_dbLocate()
   m6_copyDelim()
   m6_copySdf()
   m6_copy()
   m6_copyToArray()
   m6_countFor()
   m6_listFor()
   m6_sortFor()
   m6_totalFor()
   m6_labelForm()
   m6_reportForm()
   m6_search()


The following HiPer-SEEK functions has been implemented ([x]Harbour SVN):
=========================================================================
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

The following CFTS functions has been implemented ([x]Harbour SVN):
===================================================================
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
