/*
=============================================================================
=============================================================================
 Source file: SDE61.H
 Description: SDE C/C++ header file for SDE60.DLL

 Notice     : Copyright ©1999-2000 Vista Software

 Last Updated: 1/8/2003 (SDE 6.1.0.4)
=============================================================================
=============================================================================
*/

#ifndef _INCLUDE_SDE_H
#define _INCLUDE_SDE_H

#include <windef.h>

#ifndef SDEAPI
#ifdef _SDELIB_
#define SDEAPI _declspec(dllexport)
#else
#define SDEAPI _declspec(dllimport)
#endif
#endif

/* ******************************************************* */
/* SDE defines                                             */
/* ******************************************************* */

#define READWRITE        0
#define READONLY         1
#define EXCLUSIVE        2

/* ********* */
/* RDE types */
/* ********* */
#define SDENTX		1
#define SDEFOX		2
#define SDENSX		3
#define SDENSXDBT	4
#define SDEVFOX		5

/* text file types */
#define COMMA_DELIM  21
#define SDF_FILE     22
#define TAB_DELIM    23
#define SPACE_DELIM  24
#define USER_DELIM   25

/* OEM Source types for AppendFrom */
#define OEMNTX   31
#define OEMFOX   32
#define OEMNSX   33

/* *********** */
/* Index Types */
/* *********** */
#define INDEX_STANDARD           1
#define INDEX_STANDARD_UNIQUE    2
#define INDEX_CONDITIONAL        3
#define INDEX_CONDITIONAL_UNIQUE 4

/* ************* */
/*   date types  */
/* ************* */
#define AMERICAN 0
#define ANSI     1
#define BRITISH  2
#define FRENCH   3
#define GERMAN   4
#define ITALIAN  5
#define SPANISH  6
#define WIN_DEFAULT 99

/* ************************************ */
/* Data type identifiers for sx_Replace */
/* ************************************ */
#define R_INTEGER       1
#define R_LONG          2
#define R_DOUBLE        8
#define R_JULIAN       32
#define R_LOGICAL     128
#define R_CHAR       1024
#define R_DATESTR    1056
#define R_MEMO       3072
#define R_BITMAP     4096
#define R_BLOBFILE   8192
#define R_BLOBPTR    8193
#define R_GENERAL    8195

/* ******************************** */
/* sx_QueryTest Results             */
/* ******************************** */
#define OPTIMIZE_NONE  0
#define OPTIMIZE_PART  1
#define OPTIMIZE_FULL  2

/* ******************************** */
/* sx_EvalTest Results              */
/* ******************************** */
#define EVAL_CHARACTER  1
#define EVAL_NUMERIC    2
#define EVAL_LOGICAL    3
#define EVAL_DATE       4

/* ******************************** */
/* sx_Index(tag) iOptions           */
/* ******************************** */
#define IDX_NONE         0
#define IDX_UNIQUE       1
#define IDX_EMPTY        2

/* ******************************** */
/* sx_ErrorLevel uiErrorLevels      */
/* ******************************** */
#define ERRLEVEL_NONE     0
#define ERRLEVEL_FATAL    1
#define ERRLEVEL_STANDARD 2

/* ***************************************** */
/* RYO BOOL Operations for RYOFilterActivate */
/* ***************************************** */
#define RYOFILTER_NEW    1
#define RYOFILTER_AND    2
#define RYOFILTER_OR     3 
#define RYOFILTER_XOR    4
#define RYOFILTER_ANDNOT 5
#define RYOFILTER_ORNOT  6
#define RYOFILTER_XORNOT 7

/* ***************************************** */
/* Collation rule type                       */
/* ***************************************** */
#define ALPHABETICAL		0	// usual linguistic
#define SPELLING			1	// == Duden
#define EXPANDING			2	// additonal groups coalltion rule
#define MACHINE				3	// simple value ordering

/* ***************************************** */
/* Collation rule order                      */
/* ***************************************** */
#define DEFAULT_SET			0 // ALPHABETICAL or duden or expanding default

/* ******************************** */
/* sx_SysProp Constants             */
/* ******************************** */
//  Global Task Information
//  Gets should always be even numbers
#define SDE_SP_GETSOFTSEEK   1000   // Get the softseek flag
#define SDE_SP_SETSOFTSEEK   1001   // Set the softseek flag
#define SDE_SP_GETEXACT      1002   // Get the extact flag
#define SDE_SP_SETEXACT      1003   // Set the extact flag
#define SDE_SP_GETDELETED    1006   // Get the deleted flag
#define SDE_SP_PUTOBUFFER	 1007   // Write the optimistic buffer on commit
#define SDE_SP_GETOBUFFER    1008   // Get the optimistic buffer flag
#define SDE_SP_SETOBUFFER    1009   // Set the optimistic buffer flag
#define SDE_SP_GETSTRINGTYPE 1010   // Get the stringtype flag
#define SDE_SP_SETSTRINGTYPE 1011   // Set the stringtype flag
#define SDE_SP_GETDISABLEAUTO 1012  // Get the disable auto open flag
#define SDE_SP_SETDISABLEAUTO 1013  // Set the disable auto open flag

#define SDE_SP_SETOEMCOLLATE     1101   // Set the collation sequence for OEM tables.
#define SDE_SP_GETOEMCOLLATE     1111   // Get the collation sequence for OEM tables.
#define SDE_SP_SETCHRCOLLATE     1102   // Set the collation sequence for Win tables.
#define SDE_SP_GETCHRCOLLATE     1122   // Get the collation sequence for Win tables.
#define SDE_SP_SETLGTRCOLLATE    1103   // Set the ligatures collation dimmension
#define SDE_SP_GETLGTRCOLLATE    1133   // Get the ligatures collation dimmension

#define SDE_SP_SETSPECIALCOLLATE    1108   // Set the international collation like DUDEN collate flag
#define SDE_SP_GETSPECIALCOLLATE    1109   // Set the international collation like DUDEN collate flag

#define SDE_SP_GETLANGUAGECOLLATE    1110   // Get language, according to collation done

#define SDE_SP_GETDUDENCOLLATE   1104   // get the German DUDEN collate flag
#define SDE_SP_SETDUDENCOLLATE   1105   // set the German DUDEN collate flag
#define SDE_SP_GETLIMITCASECONV  1106   // limit case conv to A-Z, a-z if TRUE
#define SDE_SP_SETLIMITCASECONV  1107   // limit case conv to A-Z, a-z if TRUE

//Behavior settings which bridge the differences between 1.40 and 2.00
#define SDE_SP_GETADDQUERY   1300   //Get the AddQueryFlag
#define SDE_SP_SETADDQUERY   1301   //Set the AddQueryFlag
#define SDE_SP_GETUSECONDITIONAL 1302 //Get the bUseConditional flag
#define SDE_SP_SETUSECONDITIONAL 1303 //Get the bUseConditional flag
#define SDE_SP_SETWRITEBLOBHDR   1305 //Set the bWriteBlobHdr
#define SDE_SP_GETQUERYRELAXFLAG 1306 // Get flag that dictates rules of query
#define SDE_SP_SETQUERYRELAXFLAG 1307 // Set flag that dictates rules of query

//WorkArea information
#define SDE_SP_GETDRIVER     2000   //Get the active driver

#define SDE_SP_SETSTRDEFLEN  2001   //Set the default lenght for STR, if 2nd parameter is absent and field lenght zero
#define SDE_SP_SETSTRDEFDEC	 2002	//Set the default decimals for STR, if 3d parameter is absent and field lenght zero

#define SDE_SP_SETDEFAPPEND		2003   //Set default behavior for ordering ordering for non-unique key like FOX/Clipper
#define SDE_SP_SETMEMOMIXED		2004   //Set pure Clipper's memo for NSX driver
#define SDE_SP_BDESPECIFIC		2005	//Set the treatment of LIKE operator in accoring to BDE
#define SDE_SP_DBASEDATEHEADER	2006	//Set the using of DBF header in according to DbaseIII+ specification
#define SDE_SP_SETAUTOPAD		2007
#define SDE_SP_GETAUTOPAD		2008
#define SDE_SP_SETNOVELLFIX		2009


//Index information for current workarea
#define SDE_SP_GETINDEXCOUNT 3000   //Get the number of indexes
#define SDE_SP_GETDESCENDING 3002   //Get the descending flag
#define SDE_SP_GETEMPTY      3004   //Get the empty index flag

#define NIL		'\0'

/* ******************************************************** */
/* SDE    exported functions                                */
/* ******************************************************** */
#ifdef __cplusplus
extern "C" {
#endif
	VOID	SDEAPI WINAPI sx_AddEtecCollation(VOID);
	VOID	SDEAPI WINAPI sx_AddDudenCollation(VOID);
	LONG	SDEAPI WINAPI sx_Alias         (WORD uiWorkArea);
	VOID	SDEAPI WINAPI sx_Append        (VOID);
	SHORT	SDEAPI WINAPI sx_AppendEx      (VOID);
	VOID	SDEAPI WINAPI sx_AppendBlank   (VOID);
	SHORT	SDEAPI WINAPI sx_AppendBlankEx (VOID);
	BOOL	SDEAPI WINAPI sx_AppendFrom    (PBYTE cpFileName, SHORT iSourceType, PBYTE cpScopeExpr);
	LONG	SDEAPI WINAPI sx_BaseDate      (VOID);
	LONG	SDEAPI WINAPI sx_BaseName      (VOID);
	BOOL	SDEAPI WINAPI sx_BlobToFile    (PBYTE cpFieldName, PBYTE cpFileName);
	BOOL	SDEAPI WINAPI sx_Bof           (VOID);
	VOID	SDEAPI WINAPI sx_Close         (VOID);
	VOID	SDEAPI WINAPI sx_CloseAll      (VOID);
	VOID	SDEAPI WINAPI sx_CloseIndexes  (VOID);
	VOID	SDEAPI WINAPI sx_Commit        (VOID);
	INT		SDEAPI WINAPI sx_CommitLevel   (INT nNewLevel);
	BOOL	SDEAPI WINAPI sx_CopyFile      (PBYTE cpToFileName);
	BOOL	SDEAPI WINAPI sx_CopyFileText  (PBYTE cpTextFileName, SHORT iFileType);
	BOOL	SDEAPI WINAPI sx_CopyStructure (char * cpFileName, PBYTE cpAlias);
	BOOL	SDEAPI WINAPI sx_CopyStructureExtended (char * cpFileName);
	LONG	SDEAPI WINAPI sx_Count         (VOID);
	BOOL	SDEAPI WINAPI sx_CreateExec    (VOID);
	VOID	SDEAPI WINAPI sx_CreateField   (PBYTE cpName, PBYTE cpType, SHORT iLength, SHORT iDecimals);
	BOOL	SDEAPI WINAPI sx_CreateFrom    (const char * cpFileName, const char * cpAlias, PBYTE cpStruFile, SHORT iRDEType);
	SHORT	SDEAPI WINAPI sx_CreateNew     (const char * cpFileName, const char * cpAlias, SHORT iRdeType, SHORT iNumFields);
	SHORT	SDEAPI WINAPI sx_CreateNewEx   (const char * cpFileName, const char * cpAlias, SHORT iRdeType, SHORT iNumFields, UINT uiModeFlag);
	BOOL	SDEAPI WINAPI sx_DbfDecrypt    (VOID);
	BOOL	SDEAPI WINAPI sx_DbfEncrypt    (VOID);
	LONG	SDEAPI WINAPI sx_DBFilter      (VOID);
	VOID	SDEAPI WINAPI sx_DBRlockList   (PULONG ulpArray);
	LONG	SDEAPI WINAPI sx_Decrypt       (PBYTE pbBuffer, PBYTE cpPassword, int iLen);
	VOID	SDEAPI WINAPI sx_Delete        (VOID);
	BOOL	SDEAPI WINAPI sx_Deleted       (VOID);
	LONG	SDEAPI WINAPI sx_Descend       (PBYTE cpKeyString);
	BOOL	SDEAPI WINAPI sx_Empty         (PBYTE cpFieldName);
	BOOL	SDEAPI WINAPI sx_Eof           (VOID);
	LONG	SDEAPI WINAPI sx_Encrypt       (PBYTE pbBuffer, PBYTE cpPassword, int iLen);
	SHORT	SDEAPI WINAPI sx_ErrorLevel    (SHORT iErrorLevel);
	HRESULT	SDEAPI WINAPI sx_ErrorTest     (VOID);
	BOOL	SDEAPI WINAPI sx_EvalLogical   (PBYTE cpExpression);
	double	SDEAPI WINAPI sx_EvalNumeric   (PBYTE cpExpression);
	LONG	SDEAPI WINAPI sx_EvalString    (PBYTE cpExpression);
	SHORT	SDEAPI WINAPI sx_EvalTest      (PBYTE cpExpression);
	WORD	SDEAPI WINAPI sx_FieldCount    (VOID);
	WORD	SDEAPI WINAPI sx_FieldDecimals (PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_FieldName     (WORD uiFieldNum);
	WORD	SDEAPI WINAPI sx_FieldNum      (PBYTE cpFieldName);
	WORD	SDEAPI WINAPI sx_FieldOffset   (PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_FieldType     (PBYTE cpFieldName);
	WORD	SDEAPI WINAPI sx_FieldWidth    (PBYTE cpFieldName);
	BOOL	SDEAPI WINAPI sx_FilterAlias   (PBYTE cpAliasName, PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_FilterDlg     (HWND hwnd, PBYTE cpExpr, PBYTE cpCaption, SHORT iHasIndexList);
	VOID	SDEAPI WINAPI sx_FinalizeSession(VOID);
	BOOL	SDEAPI WINAPI sx_Flock         (VOID);
	VOID	SDEAPI WINAPI sx_FlushBuffers  (VOID);
	BOOL	SDEAPI WINAPI sx_Found         (VOID);
	BOOL	SDEAPI WINAPI sx_GetBitMap     (PBYTE cpFieldName, HWND hwnd);
	LONG	SDEAPI WINAPI sx_GetBlob       (PBYTE cpFieldName, PVOID vpVar);
	ULONG	SDEAPI WINAPI sx_GetBlobLength (PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_GetByte       (PBYTE cpFieldName);
	INT		SDEAPI WINAPI sx_GetCommitLevel(WORD uiWorkArea);
	INT		SDEAPI WINAPI sx_GetDateFormat (VOID);
	LONG	SDEAPI WINAPI sx_GetDateJulian (PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_GetDateString (PBYTE cpFieldName);
	double	SDEAPI WINAPI sx_GetDouble     (PBYTE cpFieldName);
	SHORT	SDEAPI WINAPI sx_GetInteger    (PBYTE cpFieldName);
	SHORT	SDEAPI WINAPI sx_GetLogical    (PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_GetLong       (PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_GetMemo       (PBYTE cpFieldName, WORD uiLineWidth);
	LONG	SDEAPI WINAPI sx_GetPrinter    (VOID);
	BOOL	SDEAPI WINAPI sx_GetQueryBit   (LONG lRecNo);
	VOID	SDEAPI WINAPI sx_GetRecord     (PBYTE cpRecord);
	LONG	SDEAPI WINAPI sx_GetScope      (SHORT iWhichScope);
	LONG	SDEAPI WINAPI sx_GetString     (PBYTE cpFieldName);
	LPSTR	SDEAPI WINAPI sx_GetSystemCharOrder (VOID);
	LPSTR	SDEAPI WINAPI sx_GetSystemLocale(VOID);
	LONG	SDEAPI WINAPI sx_GetTrimString (PBYTE cpFieldName);
	LONG	SDEAPI WINAPI sx_GetVariant    (PBYTE cpFieldName);
	LPSTR	SDEAPI WINAPI sx_GetXML        (VOID);
	VOID	SDEAPI WINAPI sx_Go            (LONG lRecNum);
	VOID	SDEAPI WINAPI sx_GoBottom      (VOID);
	VOID	SDEAPI WINAPI sx_GoTop         (VOID);
	SHORT	SDEAPI WINAPI sx_Index         (const char * cpFileName, char * cpExpr, SHORT iOption, BOOL bDescend, char * cpCondition);
	VOID	SDEAPI WINAPI sx_IndexClose    (VOID);
	LONG	SDEAPI WINAPI sx_IndexCondition(VOID);
	BOOL	SDEAPI WINAPI sx_IndexFlip     (VOID);
	LONG	SDEAPI WINAPI sx_IndexKey      (VOID);
	LONG	SDEAPI WINAPI sx_IndexKeyField (VOID);
	LONG	SDEAPI WINAPI sx_IndexName     (SHORT iIndex);
	SHORT	SDEAPI WINAPI sx_IndexOpen     (char * cpFileName);
	SHORT	SDEAPI WINAPI sx_IndexOrd      (VOID);
	SHORT	SDEAPI WINAPI sx_IndexTag      (const char * cpFileName, const char * cpTagName, char * cpExpr, SHORT iOption, BOOL bDescend, char * cpCondition);
	SHORT	SDEAPI WINAPI sx_IndexType     (VOID);
	BOOL	SDEAPI WINAPI sx_IsEncrypted   (SHORT iFileOrRec);
	BOOL	SDEAPI WINAPI sx_IsNull        (PBYTE cpFieldName);
	BOOL	SDEAPI WINAPI sx_KeyAdd        (PBYTE cpTagname);
	LONG	SDEAPI WINAPI sx_KeyData       (VOID);
	BOOL	SDEAPI WINAPI sx_KeyDrop       (PBYTE cpTagname);
	SHORT	SDEAPI WINAPI sx_KeyLength     (VOID);
	LONG	SDEAPI WINAPI sx_Locate        (PBYTE cpExpression, SHORT iDirection, BOOL bContinue);
	WORD	SDEAPI WINAPI sx_LockCount     (VOID);
	BOOL	SDEAPI WINAPI sx_Locked        (LONG lRecNum);
	PBYTE	SDEAPI WINAPI sx_MemAlloc      (LONG lNum);
	VOID	SDEAPI WINAPI sx_MemDealloc    (PVOID vpPtr);
	PBYTE	SDEAPI WINAPI sx_MemRealloc    (PVOID vpPtr, LONG lSize);
	SHORT	SDEAPI WINAPI sx_OpenMode      (VOID);
	double	SDEAPI WINAPI sx_OrderPosGet   (VOID);
	VOID	SDEAPI WINAPI sx_OrderPosSet   (double dPosition);
	LONG	SDEAPI WINAPI sx_OrderRecNo    (VOID);
	BOOL	SDEAPI WINAPI sx_Pack          (VOID);
	LONG	SDEAPI WINAPI sx_PutBlob       (PBYTE cpFieldName, PVOID vpVar, LONG lSize);
	VOID	SDEAPI WINAPI sx_PutRecord     (PBYTE cpRecord);
	VOID	SDEAPI WINAPI sx_PutVariant    (PBYTE cpFieldName, LPVOID lpVariant);

	// M.O. -> 18.09.2002 by Med's request for NET support
	VOID	SDEAPI WINAPI sx_PutDateString (PBYTE cpFieldName, PBYTE cpDateString);
	VOID	SDEAPI WINAPI sx_PutDouble	   (PBYTE cpFieldName, double dVal);
	VOID	SDEAPI WINAPI sx_PutInteger    (PBYTE cpFieldName, SHORT lValue);
	VOID	SDEAPI WINAPI sx_PutLogical	   (PBYTE cpFieldName, SHORT sbVal);
	VOID	SDEAPI WINAPI sx_PutLong       (PBYTE cpFieldName, LONG lValue);
	VOID	SDEAPI WINAPI sx_PutString	   (PBYTE cpFieldName, PBYTE cpString);
	VOID	SDEAPI WINAPI sx_PutMemo	   (PBYTE cpFieldName, PBYTE cpMemoString);
	//////////////////////////////////////
	
	LONG	SDEAPI WINAPI sx_Query         (PBYTE cpExpression);
	LONG	SDEAPI WINAPI sx_QueryRecCount (VOID);
	BOOL	SDEAPI WINAPI sx_QuerySetExact (BOOL);
	SHORT	SDEAPI WINAPI sx_QueryTest     (PBYTE cpExpression);
	VOID	SDEAPI WINAPI sx_Recall        (VOID);
	LONG	SDEAPI WINAPI sx_RecCount      (VOID);
	ULONG	SDEAPI WINAPI sx_RecNo         (VOID);
	LONG	SDEAPI WINAPI sx_RecSize       (VOID);
	LONG	SDEAPI WINAPI sx_RecToString   (PBYTE cpRecStruc, SHORT iLength);
	BOOL	SDEAPI WINAPI sx_Reindex       (VOID);
	VOID	SDEAPI WINAPI sx_Replace       (PBYTE cpFieldname, SHORT iDataType, PVOID vpData);
	BOOL	SDEAPI WINAPI sx_Rlock         (LONG lRecNum);
	BOOL	SDEAPI WINAPI sx_RYOFilterActivate(SHORT iFilterHandle, SHORT iBoolOperation);
	SHORT	SDEAPI WINAPI sx_RYOFilterCopy    (VOID);
	SHORT	SDEAPI WINAPI sx_RYOFilterCreate  (VOID);
	BOOL	SDEAPI WINAPI sx_RYOFilterDestroy (SHORT iFilterHandle);
	BOOL	SDEAPI WINAPI sx_RYOFilterGetBit  (SHORT iFilterHandle, LONG lRecNo);
	BOOL	SDEAPI WINAPI sx_RYOFilterRestore (PBYTE cpFileName);
	BOOL	SDEAPI WINAPI sx_RYOFilterSave    (SHORT iFilterHandle, PBYTE cpFileName);
	BOOL	SDEAPI WINAPI sx_RYOFilterSetBit  (SHORT iFilterHandle, LONG lRecNo, SHORT iOnOrOff);
	BOOL	SDEAPI WINAPI sx_RYOKeyAdd     (PBYTE cpTagname, PBYTE cpKey);
	BOOL	SDEAPI WINAPI sx_RYOKeyDrop    (PBYTE cpTagname);
	BOOL	SDEAPI WINAPI sx_Seek          (PBYTE cpKeyValue);
	BOOL	SDEAPI WINAPI sx_SeekBin       (PBYTE cpKeyValue, WORD uiLength);
	WORD	SDEAPI WINAPI sx_Select        (WORD uiBaseArea);
	VOID	SDEAPI WINAPI sx_SetCentury    (SHORT iValue);
	BOOL	SDEAPI WINAPI sx_SetCollationRule	(INT nRuleType, PCHAR szSourceSymbolSet, PCHAR szDestSymbolSet, BOOL bResetPrevious, BOOL bOem, LONG nReserved);
	VOID	SDEAPI WINAPI sx_SetDateFormat (WORD uiDateType);
	VOID	SDEAPI WINAPI sx_SetDeleted    (WORD uiDeleted);
	WORD	SDEAPI WINAPI sx_SetEpoch      (WORD uiBaseYear);
	LONG	SDEAPI WINAPI sx_SetErrorFunc  (LONG pUserErrorFunc, LONG pUserErrorInfo);
	VOID	SDEAPI WINAPI sx_SetErrorHook  (WORD uiErrorHook);
	VOID	SDEAPI WINAPI sx_SetExact      (WORD uiOnOff);
	VOID	SDEAPI WINAPI sx_SetFilter     (PBYTE cpExpression);
	VOID	SDEAPI WINAPI sx_SetGaugeHook  (HWND hwndGauge);
	SHORT	SDEAPI WINAPI sx_SetHandles    (SHORT iNumHandles);
	VOID	SDEAPI WINAPI sx_SetLockTimeout(SHORT iSeconds);
	VOID	SDEAPI WINAPI sx_SetMachineCollation(VOID);
	WORD	SDEAPI WINAPI sx_SetMemoBlockSize(WORD uiBlockSize);
	SHORT	SDEAPI WINAPI sx_SetOrder      (SHORT iIndex);
	VOID	SDEAPI WINAPI sx_SetPassword   (PBYTE cpEncodeKey);
	BOOL	SDEAPI WINAPI sx_SetPrinter    (PBYTE cpPrinterName);
	VOID	SDEAPI WINAPI sx_SetQueryBit   (LONG lRecNo, BOOL bValue);
	VOID	SDEAPI WINAPI sx_SetRelation   (WORD uiChildArea, char * cpKeyExpr);
	BOOL	SDEAPI WINAPI sx_SetScope      (PBYTE cpLowVal, PBYTE cpHighVal);
	VOID	SDEAPI WINAPI sx_SetSoftSeek   (WORD uiOnOff);
	VOID	SDEAPI WINAPI sx_SetStringType (WORD uiStringType);
	VOID	SDEAPI WINAPI sx_SetSystemCollation(VOID);
	VOID	SDEAPI WINAPI sx_SetTranslate  (WORD uiOnOff);
	VOID	SDEAPI WINAPI sx_SetTurboRead  (WORD uiOnOff);
	VOID	SDEAPI WINAPI sx_SetUDFPath	   (PBYTE pbPath);
	CHAR	SDEAPI WINAPI sx_SetUserDelimiter(CHAR cDelimiter, BOOL bMakeTrimmWhenCopy, BOOL bRespectMemo);
	VOID	SDEAPI WINAPI sx_GetUDFPath	   (PBYTE pbPath, INT nMaxPathLen);
	VOID	SDEAPI WINAPI sx_Skip          (LONG lNumRecs);
	LONG	SDEAPI WINAPI sx_SysProp       (WORD uiSysItem, PVOID vpData);
	SHORT	SDEAPI WINAPI sx_TagArea       (PBYTE cpTagName);
	BOOL	SDEAPI WINAPI sx_TagDelete     (PBYTE cpTagName);
	LONG	SDEAPI WINAPI sx_TagName       (SHORT iTagArea);
	VOID	SDEAPI WINAPI sx_Unlock        (LONG lRecNum);
	SHORT	SDEAPI WINAPI sx_Use           (char * cpFileName, const char * cpAlias, SHORT iOpenMode, SHORT iRdeType);
	SHORT	SDEAPI WINAPI sx_UseEx         (char * cpFileName, const char * cpAlias, SHORT iOpenMode, SHORT iRdeType, UINT uiModeFlag);
	LONG	SDEAPI WINAPI sx_Version       (VOID);
	WORD	SDEAPI WINAPI sx_WorkArea      (PBYTE cpAlias);
	VOID	SDEAPI WINAPI sx_Zap           (VOID);
	
#ifdef __cplusplus
    }
#endif

#endif // #ifndef _INCLUDE_SDE_H
/*
=============================================================================
 END
=============================================================================
*/
