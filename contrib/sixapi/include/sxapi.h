/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 */
#ifndef __SIXAPIC__
#define __SIXAPIC__

#if defined( __WATCOMC__ )
   #pragma disable_message ( 136 )
#endif

#if defined( __DMC__ )
   #include <windows.h>
   #include <tchar.h>
   #include <ctype.h>
#else
   #define HB_OS_WIN_USED
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapirdd.h"
#include "hbset.h"
#include "hbdate.h"
#include "error.ch"
#include "hbvm.h"
#include "hbstack.h"
#include "sxver.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ******************************************************* */
/* SDE defines                                             */
/* ******************************************************* */
#define READWRITE                   0
#define READONLY                    1
#define EXCLUSIVE                   2

/* ********* */
/* RDE types */
/* ********* */
#define SDENTX                      1
#define SDEFOX                      2
#define SDENSX                      3
#define SDENSXDBT                   4

/* text file types */
#define COMMA_DELIM                 21
#define SDF_FILE                    22
#define TAB_DELIM                   23
#define SPACE_DELIM                 24
#define USER_DELIM                  25

/* OEM Source types for AppendFrom */
#define OEMNTX                      31
#define OEMFOX                      32
#define OEMNSX                      33

/* Outdated RDE Names */
#define DBFNTX                      SDENTX
#define DBFIDX                      SDEFOX
#define DBFNSX                      SDENSX
#define DBFNSXDBT                   SDENSXDBT

/* *********** */
/* Index Types */
/* *********** */
#define INDEX_STANDARD              1
#define INDEX_STANDARD_UNIQUE       2
#define INDEX_CONDITIONAL           3
#define INDEX_CONDITIONAL_UNIQUE    4

/* ************* */
/*   date types  */
/* ************* */
#define AMERICAN                    0
#define ANSI                        1
#define BRITISH                     2
#define FRENCH                      3
#define GERMAN                      4
#define ITALIAN                     5
#define SPANISH                     6
#define WIN_DEFAULT                 99

/* ************************************ */
/* Data type identifiers for sx_Replace */
/* ************************************ */
#define R_INTEGER                   1
#define R_LONG                      2
#define R_DOUBLE                    8
#define R_JULIAN                    32
#define R_LOGICAL                   128
#define R_CHAR                      1024
#define R_DATESTR                   1056
#define R_MEMO                      3072
#define R_BITMAP                    4096
#define R_BLOBFILE                  8192
#define R_BLOBPTR                   8193
#define R_GENERAL                   8195

/* ******************************** */
/* sx_QueryTest Results             */
/* ******************************** */
#define OPTIMIZE_NONE               0
#define OPTIMIZE_PART               1
#define OPTIMIZE_FULL               2

/* ******************************** */
/* sx_EvalTest Results              */
/* ******************************** */
#define EVAL_CHARACTER              1
#define EVAL_NUMERIC                2
#define EVAL_LOGICAL                3

/* ******************************** */
/* sx_Index(tag) iOptions           */
/* ******************************** */
#define IDX_NONE                    0
#define IDX_UNIQUE                  1
#define IDX_EMPTY                   2

/* ******************************** */
/* sx_ErrorLevel uiErrorLevels      */
/* ******************************** */
#define ERRLEVEL_NONE               0
#define ERRLEVEL_FATAL              1
#define ERRLEVEL_STANDARD           2

/* ***************************************** */
/* RYO BOOL Operations for RYOFilterActivate */
/* ***************************************** */
#define RYOFILTER_NEW               1
#define RYOFILTER_AND               2
#define RYOFILTER_OR                3
#define RYOFILTER_XOR               4
#define RYOFILTER_ANDNOT            5
#define RYOFILTER_ORNOT             6
#define RYOFILTER_XORNOT            7

/* ***************************************** */
/* Collation rule type                       */
/* ***************************************** */
#define ALPHABETICAL                0     // usual linguistic
#define SPELLING                    1     // == Duden
#define EXPANDING                   2     // additonal groups coalltion rule
#define MACHINE                     3     // simple value ordering

/* ***************************************** */
/* Collation rule order                      */
/* ***************************************** */
#define DEFAULT_SET                 0     // ALPHABETICAL or duden or expanding default

/* ******************************** */
/* sx_SysProp Constants             */
/* ******************************** */

//  Global Task Information
//  Gets should always be even numbers
#define SDE_SP_GETSOFTSEEK          1000  // Get the softseek flag
#define SDE_SP_SETSOFTSEEK          1001  // Set the softseek flag
#define SDE_SP_GETEXACT             1002  // Get the extact flag
#define SDE_SP_SETEXACT             1003  // Set the extact flag
#define SDE_SP_GETDELETED           1006  // Get the deleted flag
#define SDE_SP_PUTOBUFFER           1007  // Write the optimistic buffer on commit
#define SDE_SP_GETOBUFFER           1008  // Get the optimistic buffer flag
#define SDE_SP_SETOBUFFER           1009  // Set the optimistic buffer flag
#define SDE_SP_GETSTRINGTYPE        1010  // Get the stringtype flag
#define SDE_SP_SETSTRINGTYPE        1011  // Set the stringtype flag
#define SDE_SP_GETDISABLEAUTO       1012  // Get the disable auto open flag
#define SDE_SP_SETDISABLEAUTO       1013  // Set the disable auto open flag
#define SDE_SP_SETOEMCOLLATE        1101  // Set the collation sequence for OEM tables.
#define SDE_SP_GETOEMCOLLATE        1111  // Get the collation sequence for OEM tables.
#define SDE_SP_SETCHRCOLLATE        1102  // Set the collation sequence for Win tables.
#define SDE_SP_GETCHRCOLLATE        1122  // Get the collation sequence for Win tables.
#define SDE_SP_SETLGTRCOLLATE       1103  // Set the ligatures collation dimmension
#define SDE_SP_GETLGTRCOLLATE       1133  // Get the ligatures collation dimmension
#define SDE_SP_SETSPECIALCOLLATE    1108  // Set the international collation like DUDEN collate flag
#define SDE_SP_GETSPECIALCOLLATE    1109  // Set the international collation like DUDEN collate flag
#define SDE_SP_GETLANGUAGECOLLATE   1110  // Get language, according to collation done
#define SDE_SP_GETDUDENCOLLATE      1104  // get the German DUDEN collate flag
#define SDE_SP_SETDUDENCOLLATE      1105  // set the German DUDEN collate flag
#define SDE_SP_GETLIMITCASECONV     1106  // limit case conv to A-Z, a-z if TRUE
#define SDE_SP_SETLIMITCASECONV     1107  // limit case conv to A-Z, a-z if TRUE

//Behavior settings which bridge the differences between 1.40 and 2.00
#define SDE_SP_GETADDQUERY          1300  //Get the AddQueryFlag
#define SDE_SP_SETADDQUERY          1301  //Set the AddQueryFlag
#define SDE_SP_GETUSECONDITIONAL    1302  //Get the bUseConditional flag
#define SDE_SP_SETUSECONDITIONAL    1303  //Get the bUseConditional flag
#define SDE_SP_SETWRITEBLOBHDR      1305  //Set the bWriteBlobHdr
#define SDE_SP_GETQUERYRELAXFLAG    1306  // Get flag that dictates rules of query
#define SDE_SP_SETQUERYRELAXFLAG    1307  // Set flag that dictates rules of query

//WorkArea information
#define SDE_SP_GETDRIVER            2000  //Get the active driver
#define SDE_SP_SETSTRDEFLEN         2001  //Set the default lenght for STR, if 2nd parameter is absent and field lenght zero
#define SDE_SP_SETSTRDEFDEC         2002  //Set the default decimals for STR, if 3d parameter is absent and field lenght zero
#define SDE_SP_SETDEFAPPEND         2003  //Set default behavior for ordering ordering for non-unique key like FOX/Clipper
#define SDE_SP_SETMEMOMIXED         2004  //Set pure Clipper's memo for NSX driver
#define SDE_SP_BDESPECIFIC          2005  //Set the treatment of LIKE operator in accoring to BDE
#define SDE_SP_DBASEDATEHEADER      2006  //Set the using of DBF header in according to DbaseIII+ specification
#define SDE_SP_SETAUTOPAD           2007
#define SDE_SP_GETAUTOPAD           2008

//Index information for current workarea
#define SDE_SP_GETINDEXCOUNT        3000  //Get the number of indexes
#define SDE_SP_GETDESCENDING        3002  //Get the descending flag
#define SDE_SP_GETEMPTY             3004  //Get the empty index flag
#define NIL                         '\0'

/* Verify Types */
#define VBEG                        1  /* verify at begining of string only */
#define VEND                        2  /* verify at end of string only */
#define VAND                        3  /* verify all tokens in target */
#define VONE                        4  /* don't tokenize target */

/* FTS File Open Modes */
#define FTS_SHARE                   0x0         /* SHARE */
#define FTS_EXCL                    0x1         /* EXCLUSIVE */
#define FTS_RDONLY                  0x2         /* READ-ONLY */

#define FTSifdelete                 FTSisdelete /* permit old name */

/* FTS Error Codes */
#define FTS_CREATEFAIL              -1
#define FTS_MEMERR                  -2
#define FTS_NULLPTR                 -3
#define FTS_BADSEEK                 -4
#define FTS_BADREAD                 -5
#define FTS_BADWRITE                -6
#define FTS_RECBOUND                -7
#define FTS_ISDELETED               -8
#define FTS_NOTDELETED              -9
#define FTS_OPENERR                 -10
#define FTS_INTERR                  -11
#define FTS_NORECS                  -13
#define FTS_BADPARMS                -16
#define FTS_NOMOREHANDLES           -17
#define FTS_BADHANDLE               -18
#define FTS_BADIHANDLE              -19
#define FTS_LOCKFAILED              -20
#define FTS_NOMORELOCKS             -21
#define FTS_CANNOTUNLOCK            -22
#define FTS_BADCOMMIT               -23

#define SX_DUMMY_NUMBER             9999

typedef struct SX_DBOPENINFO
{
   USHORT uiArea;
   const char * cFilename;
   const char * cAlias;
   BOOL fShared;
   BOOL fReadonly;
   USHORT iRDEType;
   USHORT iMode;
   const char * cRDD;
   USHORT iCommitLevel;
   USHORT iRecSize;
   USHORT iFieldCount;
   PHB_ITEM aFieldInfo;
} SX_DBOPENINFO;

extern LONG       _sx_SysProp( WORD uiSysItem, PVOID vpData );
extern char       * _sx_randomname( const char * szPrefix );
extern PHB_ITEM   _sx_FieldNames( void );
extern int        _sx_CheckRDD( const char * sSetDefault );
extern BOOL       _sx_Eval( PHB_ITEM pItem );
extern BOOL       _sx_Used( VOID );
extern BOOL       _sx_SetCentury( VOID );
extern BOOL       _sx_CopyStructure( PBYTE cpFileName, PHB_ITEM paFields );
extern char       * _sx_insertchar( char * strbuf, char chrtoins, int pos );
extern char       * _sx_ltrim( char * string );
extern char       * _sx_rtrim( char * string );
extern char       * _sx_alltrim( char * string );
extern char       * _sx_padl( char * strbuf, char chrtofill, unsigned len );
extern char       * _sx_padr( char * strbuf, char chrtofill, unsigned len );
extern char       * _sx_upper( char * string );
extern char       * _sx_strcat( char * dest, const char * src, ... );
extern char       * _sx_AutoAlias( const char * cpFileName );
extern PHB_ITEM _sx_DbStruct( VOID );
extern WORD       _sx_select( PHB_ITEM vParam );
extern int        _sx_CheckOpenMode( const char * sxOpenMode );
extern char       * _sx_GetDateValue( PBYTE cFieldName );
extern void       _sx_SetDBFInfo( int iOpenedArea, const char * szAlias,
                                  int iOpenMode, int iRDEType );
extern void       _sx_DelOpenInfo( const char * szAlias );
extern const char * _sx_CheckFileExt( const char * szFileName );
extern PHB_ITEM   _sx_GetAlias( void );

#if defined( __SXAPI_INIT )
   int               i_sxApi_MemoBlock_Size;    /* Default Memo Block Size */
   int               i_sxApi_Error_Level;       /* Default ErrorLevel */
   int               i_sxApi_RDD_Default;       /* Default RDD Driver */
   BOOL              bSetTrimmedON       = FALSE;
   PHB_ITEM          Opened_DBF_Property = NULL;
#else
   extern int        i_sxApi_MemoBlock_Size;    /* Default Memo Block Size */
   extern int        i_sxApi_Error_Level;       /* Default ErrorLevel */
   extern int        i_sxApi_RDD_Default;       /* Default RDD Driver */
   extern BOOL       bSetTrimmedON;
   extern PHB_ITEM   Opened_DBF_Property;
#endif

#define IF( x, y, z ) ( ( x ) ? ( y ) : ( z ) )

#ifndef HB_VM_STACK
   #define HB_VM_STACK  hb_stack
#endif

#define AMERICAN        0  /* MM/DD/YY */
#define ANSI            1  /* YY.MM.DD */
#define BRITISH         2  /* DD/MM/YY */
#define FRENCH          3  /* DD/MM/YY */
#define GERMAN          4  /* DD.MM.YY */
#define ITALIAN         5  /* DD-MM-YY */
#define SPANISH         6  /* DD-MM-YY */

#define STACK_RETURN    hb_stackReturnItem()
#define SELF            hb_stackSelfItem()

#if defined( _HB_API_INTERNAL_ )
   #define HB_PUSH_SYMB( p )     p->pSymbol
   #define HB_GETC( p )          p->item.asString.value
   #define HB_GETNI( p )         p->item.asInteger.value
   #define HB_GETNL( p )         p->item.asLong.value
   #define HB_GETL( p )          p->item.asLogical.value
   #define HB_L()                ( &( HB_VM_STACK ).Return )->item.asLogical.value
   #define HB_PUSHEVALSYM()      hb_vmPushSymbol( &hb_symEval )
   #define HB_STACK_RETURN()     & ( HB_VM_STACK ).Return )
   #if defined( __XHARBOUR__ )
      #define HB_ARRAY_LEN( p )  p->item.asArray.value->ulLen
   #else
      #define HB_ARRAY_LEN( p )  p->item.asArray.value->nLen
   #endif
#else
   #define HB_PUSH_SYMB( p )     hb_dynsymSymbol( p )
   #define HB_GETC( p )          hb_itemGetCPtr( p )
   #define HB_GETNI( p )         hb_itemGetNI( p )
   #define HB_GETL( p )          hb_itemGetL( p )
   #define HB_GETNL( p )         hb_itemGetNL( p )
   #define HB_PUSHEVALSYM()      hb_vmPushEvalSym()
   #define HB_ARRAY_LEN( p )     hb_arrayLen( p )
   #define HB_STACK_RETURN()     hb_stackReturnItem()
   #define HB_L()                hb_itemGetL( hb_stackReturnItem() )
#endif

#if defined( __XHARBOUR__ )
   #define HB_STORNI hb_storni
   #define HB_STORND hb_stornd
   #define HB_STORDS hb_stords
   #define HB_STORC  hb_storc
   #define HB_STORL  hb_storl
   #define HB_ARRAY_CLONE( p1, p2 ) p1 = hb_arrayClone( p2, NULL )
#else
   #define HB_STORNI hb_storvni
   #define HB_STORDS hb_storvds
   #define HB_STORND hb_storvnd
   #define HB_STORC  hb_storvc
   #define HB_STORL  hb_storvl
   #define HB_ARRAY_CLONE( p1, p2 ) hb_arrayCloneTo( p1, p2 )
#endif

typedef VOID ( WINAPI * SX_ADDETECCOLLATION )( VOID );
extern VOID sx_AddDetecCollation( VOID );

typedef VOID ( WINAPI * SX_ADDDUDENCOLLATION )( VOID );
extern VOID sx_AddDudenCollation( VOID );

typedef PBYTE ( WINAPI * SX_ALIAS )( WORD uiWorkArea );
extern PBYTE sx_Alias( WORD uiWorkArea );

typedef VOID ( WINAPI * SX_APPEND )( VOID );
extern VOID    sx_Append( VOID );

typedef SHORT ( WINAPI * SX_APPENDEX )( VOID );
extern SHORT   sx_AppendEx( VOID );

typedef VOID ( WINAPI * SX_APPENDBLANK )( VOID );
extern VOID    sx_AppendBlank( VOID );

typedef SHORT ( WINAPI * SX_APPENDBLANKEX )( VOID );
extern SHORT   sx_AppendBlankEx( VOID );

typedef BOOL ( WINAPI * SX_APPENDFROM )( PBYTE cpFileName, SHORT iSourceType, PBYTE cpScopeExpr );
extern BOOL   sx_AppendFrom( PBYTE cpFileName, SHORT iSourceType, PBYTE cpScopeExpr );

typedef LONG ( WINAPI * SX_BASEDATE )( VOID );
extern LONG sx_BaseDate( VOID );

typedef LONG ( WINAPI * SX_BASENAME )( VOID );
extern LONG sx_BaseName( VOID );

typedef BOOL ( WINAPI * SX_BLOBTOFILE )( PBYTE cpFieldName, PBYTE cpFileName );
extern BOOL sx_BlobToFile( PBYTE cpFieldName, PBYTE cpFileName );

typedef BOOL ( WINAPI * SX_BOF )( VOID );
extern BOOL sx_Bof( VOID );

typedef VOID ( WINAPI * SX_CLOSE )( VOID );
extern VOID sx_Close( VOID );

typedef VOID ( WINAPI * SX_CLOSEALL )( VOID );
extern VOID sx_CloseAll( VOID );

typedef VOID ( WINAPI * SX_CLOSEINDEXES )( VOID );
extern VOID sx_CloseIndexes( VOID );

typedef VOID ( WINAPI * SX_COMMIT )( VOID );
extern VOID sx_Commit( VOID );

typedef INT ( WINAPI * SX_COMMITLEVEL )( INT nNewLevel );
extern INT sx_CommitLevel( INT nNewLevel );

typedef BOOL ( WINAPI * SX_COPYFILE )( PBYTE cpToFileName );
extern BOOL sx_CopyFile( PBYTE cpToFileName );

typedef BOOL ( WINAPI * SX_COPYFILETEXT )( PBYTE cpTextFileName, SHORT iFileType );
extern BOOL sx_CopyFileText( PBYTE cpTextFileName, SHORT iFileType );

typedef BOOL ( WINAPI * SX_COPYSTRUCTURE )( PBYTE cpFileName, PBYTE cpAlias );
extern BOOL sx_CopyStructure( PBYTE cpFileName, PBYTE cpAlias );

typedef BOOL ( WINAPI * SX_COPYSTRUCTUREEXTENDED )( PBYTE cpFileName );
extern BOOL sx_CopyStructureExtended( PBYTE cpFileName );

typedef LONG ( WINAPI * SX_COUNT )( VOID );
extern LONG sx_Count( VOID );

typedef BOOL ( WINAPI * SX_CREATEEXEC )( VOID );
extern BOOL sx_CreateExec( VOID );

typedef VOID ( WINAPI * SX_CREATEFIELD )( PBYTE cpName, PBYTE cpType, SHORT iLength, SHORT iDecimals );
extern VOID sx_CreateField( PBYTE cpName, PBYTE cpType, SHORT iLength, SHORT iDecimals );

typedef BOOL ( WINAPI * SX_CREATEFROM )( PBYTE cpFileName, PBYTE cpAlias, PBYTE cpStruFile, SHORT iRDEType );
extern BOOL sx_CreateFrom( PBYTE cpFileName, PBYTE cpAlias, PBYTE cpStruFile, SHORT iRDEType );

typedef SHORT ( WINAPI * SX_CREATENEW )( PBYTE cpFileName, PBYTE cpAlias, SHORT iRdeType, SHORT iNumFields );
extern SHORT sx_CreateNew( PBYTE cpFileName, PBYTE cpAlias, SHORT iRdeType, SHORT iNumFields );

typedef SHORT ( WINAPI * SX_CREATENEWEX )( PBYTE cpFileName, PBYTE cpAlias, SHORT iRdeType, SHORT iNumFields, UINT uiModeFlag );
extern SHORT sx_CreateNewEx( PBYTE cpFileName, PBYTE cpAlias, SHORT iRdeType, SHORT iNumFields, UINT uiModeFlag );

typedef BOOL ( WINAPI * SX_DBFDECRYPT )( VOID );
extern BOOL sx_DbfDecrypt( VOID );

typedef BOOL ( WINAPI * SX_DBFENCRYPT )( VOID );
extern BOOL sx_DbfEncrypt( VOID );

typedef LONG ( WINAPI * SX_DBFILTER )( VOID );
extern LONG sx_DBFilter( VOID );

typedef VOID ( WINAPI * SX_DBRLOCKLIST )( PULONG ulpArray );
extern VOID sx_DBRlockList( PULONG ulpArray );

typedef LONG ( WINAPI * SX_DECRYPT )( PBYTE pbBuffer, PBYTE cpPassword, int iLen );
extern LONG sx_Decrypt( PBYTE pbBuffer, PBYTE cpPassword, int iLen );

typedef VOID ( WINAPI * SX_DELETE )( VOID );
extern VOID sx_Delete( VOID );

typedef BOOL ( WINAPI * SX_DELETED )( VOID );
extern BOOL sx_Deleted( VOID );

typedef LONG ( WINAPI * SX_DESCEND )( PBYTE cpKeyString );
extern LONG sx_Descend( PBYTE cpKeyString );

typedef BOOL ( WINAPI * SX_EMPTY )( PBYTE cpFieldName );
extern BOOL sx_Empty( PBYTE cpFieldName );

typedef BOOL ( WINAPI * SX_EOF )( VOID );
extern BOOL sx_Eof( VOID );

typedef LONG ( WINAPI * SX_ENCRYPT )( PBYTE pbBuffer, PBYTE cpPassword, int iLen );
extern LONG sx_Encrypt( PBYTE pbBuffer, PBYTE cpPassword, int iLen );

typedef SHORT ( WINAPI * SX_ERRORLEVEL )( SHORT iErrorLevel );
extern SHORT sx_ErrorLevel( SHORT iErrorLevel );

typedef HRESULT ( WINAPI * SX_ERRORTEST )( void );
extern HRESULT sx_ErrorTest( void );

typedef BOOL ( WINAPI * SX_EVALLOGICAL )( PBYTE cpExpression );
extern BOOL sx_EvalLogical( PBYTE cpExpression );

typedef double ( WINAPI * SX_EVALNUMERIC )( PBYTE cpExpression );
extern double sx_EvalNumeric( PBYTE cpExpression );

typedef LONG ( WINAPI * SX_EVALSTRING )( PBYTE cpExpression );
extern LONG sx_EvalString( PBYTE cpExpression );

typedef SHORT ( WINAPI * SX_EVALTEST )( PBYTE cpExpression );
extern SHORT sx_EvalTest( PBYTE cpExpression );

typedef WORD ( WINAPI * SX_FIELDCOUNT )( VOID );
extern WORD sx_FieldCount( VOID );

typedef WORD ( WINAPI * SX_FIELDDECIMALS )( PBYTE cpFieldName );
extern WORD sx_FieldDecimals( PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_FIELDNAME )( WORD uiFieldNum );
extern LONG sx_FieldName( WORD uiFieldNum );

typedef WORD ( WINAPI * SX_FIELDNUM )( PBYTE cpFieldName );
extern WORD sx_FieldNum( PBYTE cpFieldName );

typedef WORD ( WINAPI * SX_FIELDOFFSET )( PBYTE cpFieldName );
extern WORD sx_FieldOffset( PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_FIELDTYPE )( PBYTE cpFieldName );
extern LONG sx_FieldType( PBYTE cpFieldName );

typedef WORD ( WINAPI * SX_FIELDWIDTH )( PBYTE cpFieldName );
extern WORD sx_FieldWidth( PBYTE cpFieldName );

typedef BOOL ( WINAPI * SX_FILTERALIAS )( PBYTE cpAliasName, PBYTE cpFieldName );
extern BOOL sx_FilterAlias( PBYTE cpAliasName, PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_FILTERDLG )( HWND hwnd, PBYTE cpExpr, PBYTE cpCaption, SHORT iHasIndexList );
extern LONG sx_FilterDlg( HWND hwnd, PBYTE cpExpr, PBYTE cpCaption, SHORT iHasIndexList );

typedef VOID ( WINAPI * SX_FINALIZESESSION )( VOID );
extern VOID sx_FinalizeSession( VOID );

typedef BOOL ( WINAPI * SX_FLOCK )( VOID );
extern BOOL sx_Flock( VOID );

typedef VOID ( WINAPI * SX_FLUSHBUFFERS )( VOID );
extern VOID sx_FlushBuffers( VOID );

typedef BOOL ( WINAPI * SX_FOUND )( VOID );
extern BOOL sx_Found( VOID );

typedef BOOL ( WINAPI * SX_GETBITMAP )( PBYTE cpFieldName, HWND hwnd );
extern BOOL sx_GetBitMap( PBYTE cpFieldName, HWND hwnd );

typedef LONG ( WINAPI * SX_GETBLOB )( PBYTE cpFieldName, PVOID vpVar );
extern LONG sx_GetBlob( PBYTE cpFieldName, PVOID vpVar );

typedef ULONG ( WINAPI * SX_GETBLOBLENGTH )( PBYTE cpFieldName );
extern ULONG sx_GetBlobLength( PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_GETBYTE )( PBYTE cpFieldName );
extern LONG sx_GetByte( PBYTE cpFieldName );

typedef INT ( WINAPI * SX_GETCOMMITLEVEL )( WORD uiWorkArea );
extern INT sx_GetCommitLevel( WORD uiWorkArea );

typedef INT ( WINAPI * SX_GETDATEFORMAT )( VOID );
extern INT sx_GetDateFormat( VOID );

typedef LONG ( WINAPI * SX_GETDATEJULIAN )( PBYTE cpFieldName );
extern LONG sx_GetDateJulian( PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_GETDATESTRING )( PBYTE cpFieldName );
extern LONG sx_GetDateString( PBYTE cpFieldName );

typedef double ( WINAPI * SX_GETDOUBLE )( PBYTE cpFieldName );
extern double sx_GetDouble( PBYTE cpFieldName );

typedef SHORT ( WINAPI * SX_GETINTEGER )( PBYTE cpFieldName );
extern SHORT sx_GetInteger( PBYTE cpFieldName );

typedef SHORT ( WINAPI * SX_GETLOGICAL )( PBYTE cpFieldName );
extern SHORT sx_GetLogical( PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_GETLONG )( PBYTE cpFieldName );
extern LONG sx_GetLong( PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_GETMEMO )( PBYTE cpFieldName, WORD uiLineWidth );
extern LONG sx_GetMemo( PBYTE cpFieldName, WORD uiLineWidth );

typedef LONG ( WINAPI * SX_GETPRINTER )( VOID );
extern LONG sx_GetPrinter( VOID );

typedef BOOL ( WINAPI * SX_GETQUERYBIT )( LONG lRecNo );
extern BOOL sx_GetQueryBit( LONG lRecNo );

typedef VOID ( WINAPI * SX_GETRECORD )( PBYTE cpRecord );
extern VOID sx_GetRecord( PBYTE cpRecord );

typedef LONG ( WINAPI * SX_GETSCOPE )( SHORT iWhichScope );
extern LONG sx_GetScope( SHORT iWhichScope );

typedef LONG ( WINAPI * SX_GETSTRING )( PBYTE cpFieldName );
extern LONG sx_GetString( PBYTE cpFieldName );

typedef LPSTR ( WINAPI * SX_GETSYSTEMCHARORDER )( VOID );
extern LPSTR   sx_GetSystemCharOrder( VOID );

typedef LPSTR ( WINAPI * SX_GETSYSTEMLOCALE )( VOID );
extern LPSTR   sx_GetSystemLocale( VOID );

typedef LONG ( WINAPI * SX_GETTRIMSTRING )( PBYTE cpFieldName );
extern LONG sx_GetTrimString( PBYTE cpFieldName );

typedef LONG ( WINAPI * SX_GETVARIANT )( PBYTE cpFieldName );
extern LONG sx_GetVariant( PBYTE cpFieldName );

typedef VOID ( WINAPI * SX_GO )( LONG lRecNum );
extern VOID sx_Go( LONG lRecNum );

typedef VOID ( WINAPI * SX_GOBOTTOM )( VOID );
extern VOID sx_GoBottom( VOID );

typedef VOID ( WINAPI * SX_GOTOP )( VOID );
extern VOID sx_GoTop( VOID );

typedef SHORT ( WINAPI * SX_INDEX )( PBYTE cpFileName, PBYTE cpExpr, SHORT iOption, BOOL bDescend, PBYTE cpCondition );
extern SHORT sx_Index( PBYTE cpFileName, PBYTE cpExpr, SHORT iOption, BOOL bDescend, PBYTE cpCondition );

typedef VOID ( WINAPI * SX_INDEXCLOSE )( VOID );
extern VOID sx_IndexClose( VOID );

typedef LONG ( WINAPI * SX_INDEXCONDITION )( VOID );
extern LONG sx_IndexCondition( VOID );

typedef BOOL ( WINAPI * SX_INDEXFLIP )( VOID );
extern BOOL sx_IndexFlip( VOID );

typedef LONG ( WINAPI * SX_INDEXKEY )( VOID );
extern LONG sx_IndexKey( VOID );

typedef LONG ( WINAPI * SX_INDEXKEYFIELD )( VOID );
extern LONG sx_IndexKeyField( VOID );

typedef LONG ( WINAPI * SX_INDEXNAME )( SHORT iIndex );
extern LONG sx_IndexName( SHORT iIndex );

typedef SHORT ( WINAPI * SX_INDEXOPEN )( PBYTE cpFileName );
extern SHORT sx_IndexOpen( PBYTE cpFileName );

typedef SHORT ( WINAPI * SX_INDEXORD )( VOID );
extern SHORT sx_IndexOrd( VOID );

typedef SHORT ( WINAPI * SX_INDEXTAG )( PBYTE cpFileName, PBYTE cpTagName, PBYTE cpExpr, SHORT iOption, BOOL bDescend, PBYTE cpCondition );
extern SHORT sx_IndexTag( PBYTE cpFileName, PBYTE cpTagName, PBYTE cpExpr, SHORT iOption, BOOL bDescend, PBYTE cpCondition );

typedef SHORT ( WINAPI * SX_INDEXTYPE )( VOID );
extern SHORT sx_IndexType( VOID );

typedef BOOL ( WINAPI * SX_ISENCRYPTED )( SHORT iFileOrRec );
extern BOOL sx_IsEncrypted( SHORT iFileOrRec );

typedef BOOL ( WINAPI * SX_ISNULL )( PBYTE cpFieldName );
extern BOOL sx_IsNull( PBYTE cpFieldName );

typedef BOOL ( WINAPI * SX_KEYADD )( PBYTE cpTagname );
extern BOOL sx_KeyAdd( PBYTE cpTagname );

typedef LONG ( WINAPI * SX_KEYDATA )( VOID );
extern LONG sx_KeyData( VOID );

typedef BOOL ( WINAPI * SX_KEYDROP )( PBYTE cpTagname );
extern BOOL sx_KeyDrop( PBYTE cpTagname );

typedef SHORT ( WINAPI * SX_KEYLENGTH )( VOID );
extern SHORT sx_KeyLength( VOID );

typedef LONG ( WINAPI * SX_LOCATE )( PBYTE cpExpression, SHORT iDirection, BOOL bContinue );
extern LONG sx_Locate( PBYTE cpExpression, SHORT iDirection, BOOL bContinue );

typedef WORD ( WINAPI * SX_LOCKCOUNT )( VOID );
extern WORD sx_LockCount( VOID );

typedef BOOL ( WINAPI * SX_LOCKED )( LONG lRecNum );
extern BOOL sx_Locked( LONG lRecNum );

typedef PBYTE ( WINAPI * SX_MEMALLOC )( LONG lNum );
extern PBYTE sx_MemAlloc( LONG lNum );

typedef VOID ( WINAPI * SX_MEMDEALLOC )( PVOID vpPtr );
extern VOID sx_MemDealloc( PVOID vpPtr );

typedef PBYTE ( WINAPI * SX_MEMREALLOC )( PVOID vpPtr, LONG lSize );
extern PBYTE sx_MemRealloc( PVOID vpPtr, LONG lSize );

typedef SHORT ( WINAPI * SX_OPENMODE )( VOID );
extern SHORT   sx_OpenMode( VOID );

typedef double ( WINAPI * SX_ORDERPOSGET )( VOID );
extern double   sx_OrderPosGet( VOID );

typedef VOID ( WINAPI * SX_ORDERPOSSET )( double dPosition );
extern VOID sx_OrderPosSet( double dPosition );

typedef LONG ( WINAPI * SX_ORDERRECNO )( VOID );
extern LONG sx_OrderRecNo( VOID );

typedef VOID ( WINAPI * SX_PACK )( VOID );
extern VOID sx_Pack( VOID );

typedef LONG ( WINAPI * SX_PUTBLOB )( PBYTE cpFieldName, PVOID vpVar, LONG lSize );
extern LONG sx_PutBlob( PBYTE cpFieldName, PVOID vpVar, LONG lSize );

typedef VOID ( WINAPI * SX_PUTRECORD )( PBYTE cpRecord );
extern VOID sx_PutRecord( PBYTE cpRecord );

typedef VOID ( WINAPI * SX_PUTVARIANT )( PBYTE cpFieldName, LPVOID lpVariant );
extern VOID sx_PutVariant( PBYTE cpFieldName, LPVOID lpVariant );

typedef LONG ( WINAPI * SX_QUERY )( PBYTE cpExpression );
extern LONG sx_Query( PBYTE cpExpression );

typedef LONG ( WINAPI * SX_QUERYRECCOUNT )( VOID );
extern LONG sx_QueryRecCount( VOID );

typedef BOOL ( WINAPI * SX_QUERYSETEXACT )( BOOL );
extern BOOL sx_QuerySetExact( BOOL );

typedef SHORT ( WINAPI * SX_QUERYTEST )( PBYTE cpExpression );
extern SHORT sx_QueryTest( PBYTE cpExpression );

typedef VOID ( WINAPI * SX_RECALL )( VOID );
extern VOID    sx_Recall( VOID );

typedef LONG ( WINAPI * SX_RECCOUNT )( VOID );
extern LONG    sx_RecCount( VOID );

typedef ULONG ( WINAPI * SX_RECNO )( VOID );
extern ULONG   sx_RecNo( VOID );

typedef LONG ( WINAPI * SX_RECSIZE )( VOID );
extern LONG    sx_RecSize( VOID );

typedef LONG ( WINAPI * SX_RECTOSTRING )( PBYTE cpRecStruc, SHORT iLength );
extern LONG sx_RecToString( PBYTE cpRecStruc, SHORT iLength );

typedef VOID ( WINAPI * SX_REINDEX )( VOID );
extern VOID sx_Reindex( VOID );

typedef VOID ( WINAPI * SX_REPLACE )( PBYTE cpFieldname, SHORT iDataType, PVOID vpData );
extern VOID sx_Replace( PBYTE cpFieldname, SHORT iDataType, PVOID vpData );

typedef BOOL ( WINAPI * SX_RLOCK )( LONG lRecNum );
extern BOOL sx_Rlock( LONG lRecNum );

typedef BOOL ( WINAPI * SX_RYOFILTERACTIVATE )( SHORT iFilterHandle, SHORT iBoolOperation );
extern BOOL sx_RYOFilterActivate( SHORT iFilterHandle, SHORT iBoolOperation );

typedef SHORT ( WINAPI * SX_RYOFILTERCOPY )( VOID );
extern SHORT   sx_RYOFilterCopy( VOID );

typedef SHORT ( WINAPI * SX_RYOFILTERCREATE )( VOID );
extern SHORT   sx_RYOFilterCreate( VOID );

typedef BOOL ( WINAPI * SX_RYOFILTERDESTROY )( SHORT iFilterHandle );
extern BOOL sx_RYOFilterDestroy( SHORT iFilterHandle );

typedef BOOL ( WINAPI * SX_RYOFILTERGETBIT )( SHORT iFilterHandle, LONG lRecNo );
extern BOOL sx_RYOFilterGetBit( SHORT iFilterHandle, LONG lRecNo );

typedef BOOL ( WINAPI * SX_RYOFILTERRESTORE )( PBYTE cpFileName );
extern BOOL sx_RYOFilterRestore( PBYTE cpFileName );

typedef BOOL ( WINAPI * SX_RYOFILTERSAVE )( SHORT iFilterHandle, PBYTE cpFileName );
extern BOOL sx_RYOFilterSave( SHORT iFilterHandle, PBYTE cpFileName );

typedef BOOL ( WINAPI * SX_RYOFILTERSETBIT )( SHORT iFilterHandle, LONG lRecNo, SHORT iOnOrOff );
extern BOOL sx_RYOFilterSetBit( SHORT iFilterHandle, LONG lRecNo, SHORT iOnOrOff );

typedef BOOL ( WINAPI * SX_RYOKEYADD )( PBYTE cpTagname, PBYTE cpKey );
extern BOOL sx_RYOKeyAdd( PBYTE cpTagname, PBYTE cpKey );

typedef BOOL ( WINAPI * SX_RYOKEYDROP )( PBYTE cpTagname );
extern BOOL sx_RYOKeyDrop( PBYTE cpTagname );

typedef BOOL ( WINAPI * SX_SEEK )( PBYTE cpKeyValue );
extern BOOL sx_Seek( PBYTE cpKeyValue );

typedef BOOL ( WINAPI * SX_SEEKBIN )( PBYTE cpKeyValue, WORD uiLength );
extern BOOL sx_SeekBin( PBYTE cpKeyValue, WORD uiLength );

typedef WORD ( WINAPI * SX_SELECT )( WORD uiBaseArea );
extern WORD sx_Select( WORD uiBaseArea );

typedef VOID ( WINAPI * SX_SETCENTURY )( SHORT iValue );
extern VOID sx_SetCentury( SHORT iValue );

typedef BOOL ( WINAPI * SX_SETCOLLATIONRULE )( INT nRuleType, PCHAR szSourceSymbolSet, PCHAR szDestSymbolSet, BOOL bResetPrevious, BOOL bOem, LONG nReserved );
extern BOOL sx_SetCollationRule( INT nRuleType, PCHAR szSourceSymbolSet, PCHAR szDestSymbolSet, BOOL bResetPrevious, BOOL bOem, LONG nReserved );

typedef VOID ( WINAPI * SX_SETDATEFORMAT )( WORD uiDateType );
extern VOID sx_SetDateFormat( WORD uiDateType );

typedef VOID ( WINAPI * SX_SETDELETED )( WORD uiDeleted );
extern VOID sx_SetDeleted( WORD uiDeleted );

typedef WORD ( WINAPI * SX_SETEPOCH )( WORD uiBaseYear );
extern WORD sx_SetEpoch( WORD uiBaseYear );

typedef LONG ( WINAPI * SX_SETERRORFUNC )( LONG pUserErrorFunc, LONG pUserErrorInfo );
extern LONG sx_SetErrorFunc( LONG pUserErrorFunc, LONG pUserErrorInfo );

typedef VOID ( WINAPI * SX_SETERRORHOOK )( WORD uiErrorHook );
extern VOID sx_SetErrorHook( WORD uiErrorHook );

typedef VOID ( WINAPI * SX_SETEXACT )( WORD uiOnOff );
extern VOID sx_SetExact( WORD uiOnOff );

typedef VOID ( WINAPI * SX_SETFILTER )( PBYTE cpExpression );
extern VOID sx_SetFilter( PBYTE cpExpression );

typedef VOID ( WINAPI * SX_SETGAUGEHOOK )( HWND hwndGauge );
extern VOID sx_SetGaugeHook( HWND hwndGauge );

typedef SHORT ( WINAPI * SX_SETHANDLES )( SHORT iNumHandles );
extern SHORT sx_SetHandles( SHORT iNumHandles );

typedef VOID ( WINAPI * SX_SETLOCKTIMEOUT )( SHORT iSeconds );
extern VOID sx_SetLockTimeout( SHORT iSeconds );

typedef VOID ( WINAPI * SX_SETMACHINECOLLATION )( VOID );
extern VOID sx_SetMachineCollation( VOID );

typedef WORD ( WINAPI * SX_SETMEMOBLOCKSIZE )( WORD uiBlockSize );
extern WORD sx_SetMemoBlockSize( WORD uiBlockSize );

typedef SHORT ( WINAPI * SX_SETORDER )( SHORT iIndex );
extern SHORT sx_SetOrder( SHORT iIndex );

typedef VOID ( WINAPI * SX_SETPASSWORD )( PBYTE cpEncodeKey );
extern VOID sx_SetPassword( PBYTE cpEncodeKey );

typedef BOOL ( WINAPI * SX_SETPRINTER )( PBYTE cpPrinterName );
extern BOOL sx_SetPrinter( PBYTE cpPrinterName );

typedef VOID ( WINAPI * SX_SETQUERYBIT )( LONG lRecNo, BOOL bValue );
extern VOID sx_SetQueryBit( LONG lRecNo, BOOL bValue );

typedef VOID ( WINAPI * SX_SETRELATION )( WORD uiChildArea, PBYTE cpKeyExpr );
extern VOID sx_SetRelation( WORD uiChildArea, PBYTE cpKeyExpr );

typedef BOOL ( WINAPI * SX_SETSCOPE )( PBYTE cpLowVal, PBYTE cpHighVal );
extern BOOL sx_SetScope( PBYTE cpLowVal, PBYTE cpHighVal );

typedef VOID ( WINAPI * SX_SETSOFTSEEK )( WORD uiOnOff );
extern VOID sx_SetSoftSeek( WORD uiOnOff );

typedef VOID ( WINAPI * SX_SETSTRINGTYPE )( WORD uiStringType );
extern VOID sx_SetStringType( WORD uiStringType );

typedef VOID ( WINAPI * SX_SETSYSTEMCOLLATION )( VOID );
extern VOID sx_SetSystemCollation( VOID );

typedef VOID ( WINAPI * SX_SETTRANSLATE )( WORD uiOnOff );
extern VOID sx_SetTranslate( WORD uiOnOff );

typedef VOID ( WINAPI * SX_SETTURBOREAD )( WORD uiOnOff );
extern VOID sx_SetTurboRead( WORD uiOnOff );

typedef VOID ( WINAPI * SX_SETUDFPATH )( PBYTE pbPath );
extern VOID sx_SetUDFPath( PBYTE pbPath );

typedef CHAR ( WINAPI * SX_SETUSERDELIMITER )( CHAR cDelimiter, BOOL bMakeTrimmWhenCopy, BOOL bRespectMemo );
extern CHAR sx_SetUserDelimiter( CHAR cDelimiter, BOOL bMakeTrimmWhenCopy, BOOL bRespectMemo );

typedef VOID ( WINAPI * SX_GETUDFPATH )( PBYTE pbPath, INT nMaxPathLen );
extern VOID sx_GetUDFPath( PBYTE pbPath, INT nMaxPathLen );

typedef VOID ( WINAPI * SX_SKIP )( LONG lNumRecs );
extern VOID sx_Skip( LONG lNumRecs );

typedef LONG ( WINAPI * SX_SYSPROP )( WORD uiSysItem, PVOID vpData );
extern LONG sx_SysProp( WORD uiSysItem, PVOID vpData );

typedef SHORT ( WINAPI * SX_TAGAREA )( PBYTE cpTagName );
extern SHORT sx_TagArea( PBYTE cpTagName );

typedef BOOL ( WINAPI * SX_TAGDELETE )( PBYTE cpTagName );
extern BOOL sx_TagDelete( PBYTE cpTagName );

typedef LONG ( WINAPI * SX_TAGNAME )( SHORT iTagArea );
extern LONG sx_TagName( SHORT iTagArea );

typedef VOID ( WINAPI * SX_UNLOCK )( LONG lRecNum );
extern VOID sx_Unlock( LONG lRecNum );

typedef SHORT ( WINAPI * SX_USE )( PBYTE cpFileName, PBYTE cpAlias, SHORT iOpenMode, SHORT iRdeType );
extern SHORT sx_Use( PBYTE cpFileName, PBYTE cpAlias, SHORT iOpenMode, SHORT iRdeType );

typedef SHORT ( WINAPI * SX_USEEX )( PBYTE cpFileName, PBYTE cpAlias, SHORT iOpenMode, SHORT iRdeType, UINT uiModeFlag );
extern SHORT sx_UseEx( PBYTE cpFileName, PBYTE cpAlias, SHORT iOpenMode, SHORT iRdeType, UINT uiModeFlag );

typedef LONG ( WINAPI * SX_VERSION )( VOID );
extern LONG sx_Version( VOID );

typedef WORD ( WINAPI * SX_WORKAREA )( PBYTE cpAlias );
extern WORD sx_WorkArea( PBYTE cpAlias );

typedef VOID ( WINAPI * SX_ZAP )( VOID );
extern VOID sx_Zap( VOID );

typedef VOID ( WINAPI * SX_PUTMEMO )( PBYTE cpFieldName, LPVOID lpVariant );
extern VOID sx_PutMemo( PBYTE cpFieldName, LPVOID lpVariant );

typedef VOID ( WINAPI * SX_PUTSTRING )( PBYTE cpFieldName, LPVOID lpVariant );
extern VOID sx_PutString( PBYTE cpFieldName, LPVOID lpVariant );

typedef VOID ( WINAPI * SX_PUTDOUBLE )( PBYTE cpFieldName, LPVOID lpVariant );
extern VOID sx_PutDouble( PBYTE cpFieldName, LPVOID lpVariant );

typedef VOID ( WINAPI * SX_PUTDATESTRING )( PBYTE cpFieldName, LPVOID lpVariant );
extern VOID sx_PutDateString( PBYTE cpFieldName, LPVOID lpVariant );

typedef long ( WINAPI * FTSADD )( long, unsigned char * );
extern long   FtsAdd( long, unsigned char * );

typedef short ( WINAPI * FTSCLOSE )( long );
extern short  FtsClose( long );

typedef long ( WINAPI * FTSCREATE )( char *, short, short, char, short );
extern long   FtsCreate( char *, short, short, char, short );

typedef short ( WINAPI * FTSDELETE )( long, long );
extern short  FtsDelete( long, long );

typedef int ( WINAPI * FTSSET )( long, unsigned char * );
extern int    FtsSet( long, unsigned char * );

typedef short ( WINAPI * FTSISDELETE )( long, long );
extern short  FtsIsDelete( long, long );

typedef long ( WINAPI * FTSOPEN )( char *, short, short );
extern long   FtsOpen( char *, short, short );

typedef long ( WINAPI * FTSNEXTREC )( long );
extern long   FtsNextRec( long );

typedef long ( WINAPI * FTSNUMRECS )( long );
extern long   FtsNumRecs( long );

typedef short ( WINAPI * FTSREPLACE )( long, unsigned char *, long );
extern short  FtsReplace( long, unsigned char *, long );

typedef short ( WINAPI * FTSUNDELETE )( long, long );
extern short  FtsUnDelete( long, long );

typedef short ( WINAPI * FTSVERIFY )( long, unsigned char *, unsigned char *, short );
extern short  FtsVerify( long, unsigned char *, unsigned char *, short );

typedef char * ( WINAPI * FTSVERSION )( void );
extern char *  FtsVersion( void );

#if defined( __MINGW32__ ) && defined( HB_OS_WIN_64 )
   #define SX_CONVFUNC( funcname )   ( HB_LONG ) funcname
#else
   #define SX_CONVFUNC( funcname )  funcname
#endif

#ifdef __cplusplus
}
#endif

#endif //  end __SIXAPI__
