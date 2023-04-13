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
#define __SXAPI_INIT
#include "sxapi.h"

#define SDE_DLL   "sde7.dll"
#define FTS_DLL   "fts32.dll"

static HMODULE hModule    = NULL;
static HMODULE hFTSModule = NULL;

#ifdef __cplusplus
extern "C" {
#endif

extern PHB_ITEM Opened_DBF_Property;

#ifdef __cplusplus
}
#endif

static FARPROC sx_GetProcAddress( char * szFuncName )
{
   FARPROC pFunc = GetProcAddress( hModule, szFuncName );

   if( pFunc )
   {
      return pFunc;
   }
   else
   {
      char __szError[ 256 ];
      hb_snprintf( __szError, sizeof( __szError ),
                   "Cannot find function address: %s", szFuncName );
      MessageBox( NULL, __szError, szFuncName, MB_ICONSTOP );
      hb_vmRequestQuit();
      return NULL;
   }
}

static FARPROC fts_GetProcAddress( char * szFuncName )
{
   FARPROC pFunc = GetProcAddress( hFTSModule, szFuncName );

   if( pFunc )
   {
      return pFunc;
   }
   else
   {
      char __szError[ 256 ];
      hb_snprintf( __szError, sizeof( __szError ),
                   "Cannot find function address: %s", szFuncName );
      MessageBox( NULL, __szError, szFuncName, MB_ICONSTOP );
      hb_vmRequestQuit();
      return NULL;
   }
}

VOID sx_AddDetecCollation( VOID )
{
#ifdef __cplusplus
   static SX_ADDETECCOLLATION
#else
   static SX_ADDETECCOLLATION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ADDETECCOLLATION ) sx_GetProcAddress( "sx_AddDetecCollation" );

   pFunc();
}

VOID sx_AddDudenCollation( VOID )
{
#ifdef __cplusplus
   static SX_ADDDUDENCOLLATION
#else
   static SX_ADDDUDENCOLLATION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ADDDUDENCOLLATION ) sx_GetProcAddress( "sx_AddDudenCollation" );

   pFunc();
}

PBYTE sx_Alias( WORD uiWorkArea )
{
#ifdef __cplusplus
   static SX_ALIAS
#else
   static SX_ALIAS pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ALIAS ) sx_GetProcAddress( "sx_Alias" );

   return pFunc( uiWorkArea );
}

VOID sx_Append( VOID )
{
#ifdef __cplusplus
   static SX_APPEND
#else
   static SX_APPEND pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_APPEND ) sx_GetProcAddress( "sx_Append" );

   pFunc();
}

SHORT sx_AppendEx( VOID )
{
#ifdef __cplusplus
   static SX_APPENDEX
#else
   static SX_APPENDEX pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_APPENDEX ) sx_GetProcAddress( "sx_AppendEx" );

   return pFunc();
}

VOID sx_AppendBlank( VOID )
{
#ifdef __cplusplus
   static SX_APPENDBLANK
#else
   static SX_APPENDBLANK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_APPENDBLANK ) sx_GetProcAddress( "sx_AppendBlank" );

   pFunc();
}

SHORT sx_AppendBlankEx( VOID )
{
#ifdef __cplusplus
   static SX_APPENDBLANKEX
#else
   static SX_APPENDBLANKEX pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_APPENDBLANKEX ) sx_GetProcAddress( "sx_AppendBlankEx" );

   return pFunc();
}

BOOL sx_AppendFrom( PBYTE cpFileName, SHORT iSourceType,
                    PBYTE cpScopeExpr )
{
#ifdef __cplusplus
   static SX_APPENDFROM
#else
   static SX_APPENDFROM pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_APPENDFROM ) sx_GetProcAddress( "sx_AppendFrom" );

   return pFunc( cpFileName, iSourceType, cpScopeExpr );
}

LONG sx_BaseDate( VOID )
{
#ifdef __cplusplus
   static SX_BASEDATE
#else
   static SX_BASEDATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_BASEDATE ) sx_GetProcAddress( "sx_BaseDate" );

   return pFunc();
}

LONG sx_BaseName( VOID )
{
#ifdef __cplusplus
   static SX_BASENAME
#else
   static SX_BASENAME pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_BASENAME ) sx_GetProcAddress( "sx_BaseName" );

   return pFunc();
}

BOOL sx_BlobToFile( PBYTE cpFieldName, PBYTE cpFileName )
{
#ifdef __cplusplus
   static SX_BLOBTOFILE
#else
   static SX_BLOBTOFILE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_BLOBTOFILE ) sx_GetProcAddress( "sx_BlobToFile" );

   return pFunc( cpFieldName, cpFileName );
}

BOOL sx_Bof( VOID )
{
#ifdef __cplusplus
   static SX_BOF
#else
   static SX_BOF pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_BOF ) sx_GetProcAddress( "sx_Bof" );

   return pFunc();
}

VOID sx_Close( VOID )
{
#ifdef __cplusplus
   static SX_CLOSE
#else
   static SX_CLOSE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CLOSE ) sx_GetProcAddress( "sx_Close" );

   pFunc();
}

VOID sx_CloseAll( VOID )
{
#ifdef __cplusplus
   static SX_CLOSEALL
#else
   static SX_CLOSEALL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CLOSEALL ) sx_GetProcAddress( "sx_CloseAll" );

   pFunc();
}

VOID sx_CloseIndexes( VOID )
{
#ifdef __cplusplus
   static SX_CLOSEINDEXES
#else
   static SX_CLOSEINDEXES pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CLOSEINDEXES ) sx_GetProcAddress( "sx_CloseIndexes" );

   pFunc();
}

VOID sx_Commit( VOID )
{
#ifdef __cplusplus
   static SX_COMMIT
#else
   static SX_COMMIT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_COMMIT ) sx_GetProcAddress( "sx_Commit" );

   pFunc();
}

INT sx_CommitLevel( INT nNewLevel )
{
#ifdef __cplusplus
   static SX_COMMITLEVEL
#else
   static SX_COMMITLEVEL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_COMMITLEVEL ) sx_GetProcAddress( "sx_CommitLevel" );

   return pFunc( nNewLevel );
}

BOOL sx_CopyFile( PBYTE cpToFileName )
{
#ifdef __cplusplus
   static SX_COPYFILE
#else
   static SX_COPYFILE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_COPYFILE ) sx_GetProcAddress( "sx_CopyFile" );

   return pFunc( cpToFileName );
}

BOOL sx_CopyFileText( PBYTE cpTextFileName, SHORT iFileType )
{
#ifdef __cplusplus
   static SX_COPYFILETEXT
#else
   static SX_COPYFILETEXT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_COPYFILETEXT ) sx_GetProcAddress( "sx_CopyFileText" );

   return pFunc( cpTextFileName, iFileType );
}

BOOL sx_CopyStructure( PBYTE cpFileName, PBYTE cpAlias )
{
#ifdef __cplusplus
   static SX_COPYSTRUCTURE
#else
   static SX_COPYSTRUCTURE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_COPYSTRUCTURE ) sx_GetProcAddress( "sx_CopyStructure" );

   return pFunc( cpFileName, cpAlias );
}

BOOL sx_CopyStructureExtended( PBYTE cpFileName )
{
#ifdef __cplusplus
   static SX_COPYSTRUCTUREEXTENDED
#else
   static SX_COPYSTRUCTUREEXTENDED pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_COPYSTRUCTUREEXTENDED ) sx_GetProcAddress( "sx_CopyStructureExtended" );

   return pFunc( cpFileName );
}

LONG sx_Count( VOID )
{
#ifdef __cplusplus
   static SX_COUNT
#else
   static SX_COUNT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_COUNT ) sx_GetProcAddress( "sx_Count" );

   return pFunc();
}

BOOL sx_CreateExec( VOID )
{
#ifdef __cplusplus
   static SX_CREATEEXEC
#else
   static SX_CREATEEXEC pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CREATEEXEC ) sx_GetProcAddress( "sx_CreateExec" );

   return pFunc();
}

VOID sx_CreateField( PBYTE cpName, PBYTE cpType, SHORT iLength,
                     SHORT iDecimals )
{
#ifdef __cplusplus
   static SX_CREATEFIELD
#else
   static SX_CREATEFIELD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CREATEFIELD ) sx_GetProcAddress( "sx_CreateField" );

   pFunc( cpName, cpType, iLength, iDecimals );
}

BOOL sx_CreateFrom( PBYTE cpFileName, PBYTE cpAlias, PBYTE cpStruFile,
                    SHORT iRDEType )
{
#ifdef __cplusplus
   static SX_CREATEFROM
#else
   static SX_CREATEFROM pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CREATEFROM ) sx_GetProcAddress( "sx_CreateFrom" );

   return pFunc( cpFileName, cpAlias, cpStruFile, iRDEType );
}

SHORT sx_CreateNew( PBYTE cpFileName, PBYTE cpAlias, SHORT iRdeType,
                    SHORT iNumFields )
{
#ifdef __cplusplus
   static SX_CREATENEW
#else
   static SX_CREATENEW pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CREATENEW ) sx_GetProcAddress( "sx_CreateNew" );

   return pFunc( cpFileName, cpAlias, iRdeType, iNumFields );
}

SHORT sx_CreateNewEx( PBYTE cpFileName, PBYTE cpAlias, SHORT iRdeType,
                      SHORT iNumFields, UINT uiModeFlag )
{
#ifdef __cplusplus
   static SX_CREATENEWEX
#else
   static SX_CREATENEWEX pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_CREATENEWEX ) sx_GetProcAddress( "sx_CreateNewEx" );

   return pFunc( cpFileName, cpAlias, iRdeType, iNumFields, uiModeFlag );
}

BOOL sx_DbfDecrypt( VOID )
{
#ifdef __cplusplus
   static SX_DBFDECRYPT
#else
   static SX_DBFDECRYPT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DBFDECRYPT ) sx_GetProcAddress( "sx_DbfDecrypt" );

   return pFunc();
}

BOOL sx_DbfEncrypt( VOID )
{
#ifdef __cplusplus
   static SX_DBFENCRYPT
#else
   static SX_DBFENCRYPT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DBFENCRYPT ) sx_GetProcAddress( "sx_DbfEncrypt" );

   return pFunc();
}

LONG sx_DBFilter( VOID )
{
#ifdef __cplusplus
   static SX_DBFILTER
#else
   static SX_DBFILTER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DBFILTER ) sx_GetProcAddress( "sx_DBFilter" );

   return pFunc();
}

VOID sx_DBRlockList( PULONG ulpArray )
{
#ifdef __cplusplus
   static SX_DBRLOCKLIST
#else
   static SX_DBRLOCKLIST pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DBRLOCKLIST ) sx_GetProcAddress( "sx_DBRlockList" );

   pFunc( ulpArray );
}

LONG sx_Decrypt( PBYTE pbBuffer, PBYTE cpPassword, int iLen )
{
#ifdef __cplusplus
   static SX_DECRYPT
#else
   static SX_DECRYPT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DECRYPT ) sx_GetProcAddress( "sx_Decrypt" );

   return pFunc( pbBuffer, cpPassword, iLen );
}

VOID sx_Delete( VOID )
{
#ifdef __cplusplus
   static SX_DELETE
#else
   static SX_DELETE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DELETE ) sx_GetProcAddress( "sx_Delete" );

   pFunc();
}

BOOL sx_Deleted( VOID )
{
#ifdef __cplusplus
   static SX_DELETED
#else
   static SX_DELETED pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DELETED ) sx_GetProcAddress( "sx_Deleted" );

   return pFunc();
}

LONG sx_Descend( PBYTE cpKeyString )
{
#ifdef __cplusplus
   static SX_DESCEND
#else
   static SX_DESCEND pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_DESCEND ) sx_GetProcAddress( "sx_Descend" );

   return pFunc( cpKeyString );
}

BOOL sx_Empty( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_EMPTY
#else
   static SX_EMPTY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_EMPTY ) sx_GetProcAddress( "sx_Empty" );

   return pFunc( cpFieldName );
}

BOOL sx_Eof( VOID )
{
#ifdef __cplusplus
   static SX_EOF
#else
   static SX_EOF pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_EOF ) sx_GetProcAddress( "sx_Eof" );

   return pFunc();
}

LONG sx_Encrypt( PBYTE pbBuffer, PBYTE cpPassword, int iLen )
{
#ifdef __cplusplus
   static SX_ENCRYPT
#else
   static SX_ENCRYPT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ENCRYPT ) sx_GetProcAddress( "sx_Encrypt" );

   return pFunc( pbBuffer, cpPassword, iLen );
}

SHORT sx_ErrorLevel( SHORT iErrorLevel )
{
#ifdef __cplusplus
   static SX_ERRORLEVEL
#else
   static SX_ERRORLEVEL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ERRORLEVEL ) sx_GetProcAddress( "sx_ErrorLevel" );

   return pFunc( iErrorLevel );
}

HRESULT sx_ErrorTest( VOID )
{
#ifdef __cplusplus
   static SX_ERRORTEST
#else
   static SX_ERRORTEST pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ERRORTEST ) sx_GetProcAddress( "sx_ErrorTest" );

   return pFunc();
}

BOOL sx_EvalLogical( PBYTE cpExpression )
{
#ifdef __cplusplus
   static SX_EVALLOGICAL
#else
   static SX_EVALLOGICAL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_EVALLOGICAL ) sx_GetProcAddress( "sx_EvalLogical" );

   return pFunc( cpExpression );
}

double sx_EvalNumeric( PBYTE cpExpression )
{
#ifdef __cplusplus
   static SX_EVALNUMERIC
#else
   static SX_EVALNUMERIC pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_EVALNUMERIC ) sx_GetProcAddress( "sx_EvalNumeric" );

   return pFunc( cpExpression );
}

LONG sx_EvalString( PBYTE cpExpression )
{
#ifdef __cplusplus
   static SX_EVALSTRING
#else
   static SX_EVALSTRING pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_EVALSTRING ) sx_GetProcAddress( "sx_EvalString" );

   return pFunc( cpExpression );
}

SHORT sx_EvalTest( PBYTE cpExpression )
{
#ifdef __cplusplus
   static SX_EVALTEST
#else
   static SX_EVALTEST pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_EVALTEST ) sx_GetProcAddress( "sx_EvalTest" );

   return pFunc( cpExpression );
}

WORD sx_FieldCount( VOID )
{
#ifdef __cplusplus
   static SX_FIELDCOUNT
#else
   static SX_FIELDCOUNT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FIELDCOUNT ) sx_GetProcAddress( "sx_FieldCount" );

   return pFunc();
}

WORD sx_FieldDecimals( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_FIELDDECIMALS
#else
   static SX_FIELDDECIMALS pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FIELDDECIMALS ) sx_GetProcAddress( "sx_FieldDecimals" );

   return pFunc( cpFieldName );
}

LONG sx_FieldName( WORD uiFieldNum )
{
#ifdef __cplusplus
   static SX_FIELDNAME
#else
   static SX_FIELDNAME pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FIELDNAME ) sx_GetProcAddress( "sx_FieldName" );

   return pFunc( uiFieldNum );
}

WORD sx_FieldNum( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_FIELDNUM
#else
   static SX_FIELDNUM pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FIELDNUM ) sx_GetProcAddress( "sx_FieldNum" );

   return pFunc( cpFieldName );
}

WORD sx_FieldOffset( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_FIELDOFFSET
#else
   static SX_FIELDOFFSET pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FIELDOFFSET ) sx_GetProcAddress( "sx_FieldOffset" );

   return pFunc( cpFieldName );
}

LONG sx_FieldType( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_FIELDTYPE
#else
   static SX_FIELDTYPE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FIELDTYPE ) sx_GetProcAddress( "sx_FieldType" );

   return pFunc( cpFieldName );
}

WORD sx_FieldWidth( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_FIELDWIDTH
#else
   static SX_FIELDWIDTH pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FIELDWIDTH ) sx_GetProcAddress( "sx_FieldWidth" );

   return pFunc( cpFieldName );
}

BOOL sx_FilterAlias( PBYTE cpAliasName, PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_FILTERALIAS
#else
   static SX_FILTERALIAS pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FILTERALIAS ) sx_GetProcAddress( "sx_FilterAlias" );

   return pFunc( cpAliasName, cpFieldName );
}

LONG sx_FilterDlg( HWND hwnd, PBYTE cpExpr, PBYTE cpCaption,
                   SHORT iHasIndexList )
{
#ifdef __cplusplus
   static SX_FILTERDLG
#else
   static SX_FILTERDLG pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FILTERDLG ) sx_GetProcAddress( "sx_FilterDlg" );

   return pFunc( hwnd, cpExpr, cpCaption, iHasIndexList );
}

VOID sx_FinalizeSession( VOID )
{
#ifdef __cplusplus
   static SX_FINALIZESESSION
#else
   static SX_FINALIZESESSION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FINALIZESESSION ) sx_GetProcAddress( "sx_FinalizeSession" );

   pFunc();
}

BOOL sx_Flock( VOID )
{
#ifdef __cplusplus
   static SX_FLOCK
#else
   static SX_FLOCK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FLOCK ) sx_GetProcAddress( "sx_Flock" );

   return pFunc();
}

VOID sx_FlushBuffers( VOID )
{
#ifdef __cplusplus
   static SX_FLUSHBUFFERS
#else
   static SX_FLUSHBUFFERS pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FLUSHBUFFERS ) sx_GetProcAddress( "sx_FlushBuffers" );

   pFunc();
}

BOOL sx_Found( VOID )
{
#ifdef __cplusplus
   static SX_FOUND
#else
   static SX_FOUND pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_FOUND ) sx_GetProcAddress( "sx_Found" );

   return pFunc();
}

BOOL sx_GetBitMap( PBYTE cpFieldName, HWND hwnd )
{
#ifdef __cplusplus
   static SX_GETBITMAP
#else
   static SX_GETBITMAP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETBITMAP ) sx_GetProcAddress( "sx_GetBitMap" );

   return pFunc( cpFieldName, hwnd );
}

LONG sx_GetBlob( PBYTE cpFieldName, PVOID vpVar )
{
#ifdef __cplusplus
   static SX_GETBLOB
#else
   static SX_GETBLOB pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETBLOB ) sx_GetProcAddress( "sx_GetBlob" );

   return pFunc( cpFieldName, vpVar );
}

ULONG sx_GetBlobLength( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETBLOBLENGTH
#else
   static SX_GETBLOBLENGTH pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETBLOBLENGTH ) sx_GetProcAddress( "sx_GetBlobLength" );

   return pFunc( cpFieldName );
}

LONG sx_GetByte( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETBYTE
#else
   static SX_GETBYTE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETBYTE ) sx_GetProcAddress( "sx_GetByte" );

   return pFunc( cpFieldName );
}

INT sx_GetCommitLevel( WORD uiWorkArea )
{
#ifdef __cplusplus
   static SX_GETCOMMITLEVEL
#else
   static SX_GETCOMMITLEVEL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETCOMMITLEVEL ) sx_GetProcAddress( "sx_GetCommitLevel" );

   return pFunc( uiWorkArea );
}

INT sx_GetDateFormat( VOID )
{
#ifdef __cplusplus
   static SX_GETDATEFORMAT
#else
   static SX_GETDATEFORMAT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETDATEFORMAT ) sx_GetProcAddress( "sx_GetDateFormat" );

   return pFunc();
}

LONG sx_GetDateJulian( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETDATEJULIAN
#else
   static SX_GETDATEJULIAN pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETDATEJULIAN ) sx_GetProcAddress( "sx_GetDateJulian" );

   return pFunc( cpFieldName );
}

LONG sx_GetDateString( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETDATESTRING
#else
   static SX_GETDATESTRING pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETDATESTRING ) sx_GetProcAddress( "sx_GetDateString" );

   return pFunc( cpFieldName );
}

double sx_GetDouble( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETDOUBLE
#else
   static SX_GETDOUBLE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETDOUBLE ) sx_GetProcAddress( "sx_GetDouble" );

   return pFunc( cpFieldName );
}

SHORT sx_GetInteger( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETINTEGER
#else
   static SX_GETINTEGER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETINTEGER ) sx_GetProcAddress( "sx_GetInteger" );

   return pFunc( cpFieldName );
}

SHORT sx_GetLogical( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETLOGICAL
#else
   static SX_GETLOGICAL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETLOGICAL ) sx_GetProcAddress( "sx_GetLogical" );

   return pFunc( cpFieldName );
}

LONG sx_GetLong( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETLONG
#else
   static SX_GETLONG pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETLONG ) sx_GetProcAddress( "sx_GetLong" );

   return pFunc( cpFieldName );
}

LONG sx_GetMemo( PBYTE cpFieldName, WORD uiLineWidth )
{
#ifdef __cplusplus
   static SX_GETMEMO
#else
   static SX_GETMEMO pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETMEMO ) sx_GetProcAddress( "sx_GetMemo" );

   return pFunc( cpFieldName, uiLineWidth );
}

LONG sx_GetPrinter( VOID )
{
#ifdef __cplusplus
   static SX_GETPRINTER
#else
   static SX_GETPRINTER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETPRINTER ) sx_GetProcAddress( "sx_GetPrinter" );

   return pFunc();
}

BOOL sx_GetQueryBit( LONG lRecNo )
{
#ifdef __cplusplus
   static SX_GETQUERYBIT
#else
   static SX_GETQUERYBIT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETQUERYBIT ) sx_GetProcAddress( "sx_GetQueryBit" );

   return pFunc( lRecNo );
}

VOID sx_GetRecord( PBYTE cpRecord )
{
#ifdef __cplusplus
   static SX_GETRECORD
#else
   static SX_GETRECORD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETRECORD ) sx_GetProcAddress( "sx_GetRecord" );

   pFunc( cpRecord );
}

LONG sx_GetScope( SHORT iWhichScope )
{
#ifdef __cplusplus
   static SX_GETSCOPE
#else
   static SX_GETSCOPE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETSCOPE ) sx_GetProcAddress( "sx_GetScope" );

   return pFunc( iWhichScope );
}

LONG sx_GetString( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETSTRING
#else
   static SX_GETSTRING pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETSTRING ) sx_GetProcAddress( "sx_GetString" );

   return pFunc( cpFieldName );
}

LPSTR sx_GetSystemCharOrder( VOID )
{
#ifdef __cplusplus
   static SX_GETSYSTEMCHARORDER
#else
   static SX_GETSYSTEMCHARORDER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETSYSTEMCHARORDER ) sx_GetProcAddress( "sx_GetSystemCharOrder" );

   return pFunc();
}

LPSTR sx_GetSystemLocale( VOID )
{
#ifdef __cplusplus
   static SX_GETSYSTEMLOCALE
#else
   static SX_GETSYSTEMLOCALE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETSYSTEMLOCALE ) sx_GetProcAddress( "sx_GetSystemLocale" );

   return pFunc();
}

LONG sx_GetTrimString( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETTRIMSTRING
#else
   static SX_GETTRIMSTRING pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETTRIMSTRING ) sx_GetProcAddress( "sx_GetTrimString" );

   return pFunc( cpFieldName );
}

LONG sx_GetVariant( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_GETVARIANT
#else
   static SX_GETVARIANT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETVARIANT ) sx_GetProcAddress( "sx_GetVariant" );

   return pFunc( cpFieldName );
}

VOID sx_Go( LONG lRecNum )
{
#ifdef __cplusplus
   static SX_GO
#else
   static SX_GO pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GO ) sx_GetProcAddress( "sx_Go" );

   pFunc( lRecNum );
}

VOID sx_GoBottom( VOID )
{
#ifdef __cplusplus
   static SX_GOBOTTOM
#else
   static SX_GOBOTTOM pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GOBOTTOM ) sx_GetProcAddress( "sx_GoBottom" );

   pFunc();
}

VOID sx_GoTop( VOID )
{
#ifdef __cplusplus
   static SX_GOTOP
#else
   static SX_GOTOP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GOTOP ) sx_GetProcAddress( "sx_GoTop" );

   pFunc();
}

SHORT sx_Index( PBYTE cpFileName, PBYTE cpExpr, SHORT iOption,
                BOOL bDescend, PBYTE cpCondition )
{
#ifdef __cplusplus
   static SX_INDEX
#else
   static SX_INDEX pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEX ) sx_GetProcAddress( "sx_Index" );

   return pFunc( cpFileName, cpExpr, iOption, bDescend, cpCondition );
}

VOID sx_IndexClose( VOID )
{
#ifdef __cplusplus
   static SX_INDEXCLOSE
#else
   static SX_INDEXCLOSE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXCLOSE ) sx_GetProcAddress( "sx_IndexClose" );

   pFunc();
}

LONG sx_IndexCondition( VOID )
{
#ifdef __cplusplus
   static SX_INDEXCONDITION
#else
   static SX_INDEXCONDITION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXCONDITION ) sx_GetProcAddress( "sx_IndexCondition" );

   return pFunc();
}

BOOL sx_IndexFlip( VOID )
{
#ifdef __cplusplus
   static SX_INDEXFLIP
#else
   static SX_INDEXFLIP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXFLIP ) sx_GetProcAddress( "sx_IndexFlip" );

   return pFunc();
}

LONG sx_IndexKey( VOID )
{
#ifdef __cplusplus
   static SX_INDEXKEY
#else
   static SX_INDEXKEY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXKEY ) sx_GetProcAddress( "sx_IndexKey" );

   return pFunc();
}

LONG sx_IndexKeyField( VOID )
{
#ifdef __cplusplus
   static SX_INDEXKEYFIELD
#else
   static SX_INDEXKEYFIELD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXKEYFIELD ) sx_GetProcAddress( "sx_IndexKeyField" );

   return pFunc();
}

LONG sx_IndexName( SHORT iIndex )
{
#ifdef __cplusplus
   static SX_INDEXNAME
#else
   static SX_INDEXNAME pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXNAME ) sx_GetProcAddress( "sx_IndexName" );

   return pFunc( iIndex );
}

SHORT sx_IndexOpen( PBYTE cpFileName )
{
#ifdef __cplusplus
   static SX_INDEXOPEN
#else
   static SX_INDEXOPEN pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXOPEN ) sx_GetProcAddress( "sx_IndexOpen" );

   return pFunc( cpFileName );
}

SHORT sx_IndexOrd( VOID )
{
#ifdef __cplusplus
   static SX_INDEXORD
#else
   static SX_INDEXORD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXORD ) sx_GetProcAddress( "sx_IndexOrd" );

   return pFunc();
}

SHORT sx_IndexTag( PBYTE cpFileName, PBYTE cpTagName, PBYTE cpExpr,
                   SHORT iOption, BOOL bDescend, PBYTE cpCondition )
{
#ifdef __cplusplus
   static SX_INDEXTAG
#else
   static SX_INDEXTAG pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXTAG ) sx_GetProcAddress( "sx_IndexTag" );

   return pFunc( cpFileName, cpTagName, cpExpr, iOption, bDescend, cpCondition );
}

SHORT sx_IndexType( VOID )
{
#ifdef __cplusplus
   static SX_INDEXTYPE
#else
   static SX_INDEXTYPE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_INDEXTYPE ) sx_GetProcAddress( "sx_IndexType" );

   return pFunc();
}

BOOL sx_IsEncrypted( SHORT iFileOrRec )
{
#ifdef __cplusplus
   static SX_ISENCRYPTED
#else
   static SX_ISENCRYPTED pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ISENCRYPTED ) sx_GetProcAddress( "sx_IsEncrypted" );

   return pFunc( iFileOrRec );
}

BOOL sx_IsNull( PBYTE cpFieldName )
{
#ifdef __cplusplus
   static SX_ISNULL
#else
   static SX_ISNULL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ISNULL ) sx_GetProcAddress( "sx_IsNull" );

   return pFunc( cpFieldName );
}

BOOL sx_KeyAdd( PBYTE cpTagname )
{
#ifdef __cplusplus
   static SX_KEYADD
#else
   static SX_KEYADD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_KEYADD ) sx_GetProcAddress( "sx_KeyAdd" );

   return pFunc( cpTagname );
}

LONG sx_KeyData( VOID )
{
#ifdef __cplusplus
   static SX_KEYDATA
#else
   static SX_KEYDATA pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_KEYDATA ) sx_GetProcAddress( "sx_KeyData" );

   return pFunc();
}

BOOL sx_KeyDrop( PBYTE cpTagname )
{
#ifdef __cplusplus
   static SX_KEYDROP
#else
   static SX_KEYDROP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_KEYDROP ) sx_GetProcAddress( "sx_KeyDrop" );

   return pFunc( cpTagname );
}

SHORT sx_KeyLength( VOID )
{
#ifdef __cplusplus
   static SX_KEYLENGTH
#else
   static SX_KEYLENGTH pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_KEYLENGTH ) sx_GetProcAddress( "sx_KeyLength" );

   return pFunc();
}

LONG sx_Locate( PBYTE cpExpression, SHORT iDirection, BOOL bContinue )
{
#ifdef __cplusplus
   static SX_LOCATE
#else
   static SX_LOCATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_LOCATE ) sx_GetProcAddress( "sx_Locate" );

   return pFunc( cpExpression, iDirection, bContinue );
}

WORD sx_LockCount( VOID )
{
#ifdef __cplusplus
   static SX_LOCKCOUNT
#else
   static SX_LOCKCOUNT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_LOCKCOUNT ) sx_GetProcAddress( "sx_LockCount" );

   return pFunc();
}

BOOL sx_Locked( LONG lRecNum )
{
#ifdef __cplusplus
   static SX_LOCKED
#else
   static SX_LOCKED pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_LOCKED ) sx_GetProcAddress( "sx_Locked" );

   return pFunc( lRecNum );
}

PBYTE sx_MemAlloc( LONG lNum )
{
#ifdef __cplusplus
   static SX_MEMALLOC
#else
   static SX_MEMALLOC pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_MEMALLOC ) sx_GetProcAddress( "sx_MemAlloc" );

   return pFunc( lNum );
}

VOID sx_MemDealloc( PVOID vpPtr )
{
#ifdef __cplusplus
   static SX_MEMDEALLOC
#else
   static SX_MEMDEALLOC pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_MEMDEALLOC ) sx_GetProcAddress( "sx_MemDealloc" );

   pFunc( vpPtr );
}

PBYTE sx_MemRealloc( PVOID vpPtr, LONG lSize )
{
#ifdef __cplusplus
   static SX_MEMREALLOC
#else
   static SX_MEMREALLOC pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_MEMREALLOC ) sx_GetProcAddress( "sx_MemReAlloc" );

   return pFunc( vpPtr, lSize );
}

SHORT sx_OpenMode( VOID )
{
#ifdef __cplusplus
   static SX_OPENMODE
#else
   static SX_OPENMODE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_OPENMODE ) sx_GetProcAddress( "sx_OpenMode" );

   return pFunc();
}

double sx_OrderPosGet( VOID )
{
#ifdef __cplusplus
   static SX_ORDERPOSGET
#else
   static SX_ORDERPOSGET pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ORDERPOSGET ) sx_GetProcAddress( "sx_OrderPosGet" );

   return pFunc();
}

VOID sx_OrderPosSet( double dPosition )
{
#ifdef __cplusplus
   static SX_ORDERPOSSET
#else
   static SX_ORDERPOSSET pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ORDERPOSSET ) sx_GetProcAddress( "sx_OrderPosSet" );

   pFunc( dPosition );
}

LONG sx_OrderRecNo( VOID )
{
#ifdef __cplusplus
   static SX_ORDERRECNO
#else
   static SX_ORDERRECNO pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ORDERRECNO ) sx_GetProcAddress( "sx_OrderRecNo" );

   return pFunc();
}

VOID sx_Pack( VOID )
{
#ifdef __cplusplus
   static SX_PACK
#else
   static SX_PACK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PACK ) sx_GetProcAddress( "sx_Pack" );

   pFunc();
}

LONG sx_PutBlob( PBYTE cpFieldName, PVOID vpVar, LONG lSize )
{
#ifdef __cplusplus
   static SX_PUTBLOB
#else
   static SX_PUTBLOB pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PUTBLOB ) sx_GetProcAddress( "sx_PutBlob" );

   return pFunc( cpFieldName, vpVar, lSize );
}

VOID sx_PutRecord( PBYTE cpRecord )
{
#ifdef __cplusplus
   static SX_PUTRECORD
#else
   static SX_PUTRECORD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PUTRECORD ) sx_GetProcAddress( "sx_PutRecord" );

   pFunc( cpRecord );
}

VOID sx_PutVariant( PBYTE cpFieldName, LPVOID lpVariant )
{
#ifdef __cplusplus
   static SX_PUTVARIANT
#else
   static SX_PUTVARIANT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PUTVARIANT ) sx_GetProcAddress( "sx_PutVariant" );

   pFunc( cpFieldName, lpVariant );
}

LONG sx_Query( PBYTE cpExpression )
{
#ifdef __cplusplus
   static SX_QUERY
#else
   static SX_QUERY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_QUERY ) sx_GetProcAddress( "sx_Query" );

   return pFunc( cpExpression );
}

LONG sx_QueryRecCount( VOID )
{
#ifdef __cplusplus
   static SX_QUERYRECCOUNT
#else
   static SX_QUERYRECCOUNT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_QUERYRECCOUNT ) sx_GetProcAddress( "sx_QueryRecCount" );

   return pFunc();
}

BOOL sx_QuerySetExact( BOOL bExact )
{
#ifdef __cplusplus
   static SX_QUERYSETEXACT
#else
   static SX_QUERYSETEXACT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_QUERYSETEXACT ) sx_GetProcAddress( "sx_QuerySetExact" );

   return pFunc( bExact );
}

SHORT sx_QueryTest( PBYTE cpExpression )
{
#ifdef __cplusplus
   static SX_QUERYTEST
#else
   static SX_QUERYTEST pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_QUERYTEST ) sx_GetProcAddress( "sx_QueryTest" );

   return pFunc( cpExpression );
}

VOID sx_Recall( VOID )
{
#ifdef __cplusplus
   static SX_RECALL
#else
   static SX_RECALL pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RECALL ) sx_GetProcAddress( "sx_Recall" );

   pFunc();
}

LONG sx_RecCount( VOID )
{
#ifdef __cplusplus
   static SX_RECCOUNT
#else
   static SX_RECCOUNT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RECCOUNT ) sx_GetProcAddress( "sx_RecCount" );

   return pFunc();
}

ULONG sx_RecNo( VOID )
{
#ifdef __cplusplus
   static SX_RECNO
#else
   static SX_RECNO pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RECNO ) sx_GetProcAddress( "sx_RecNo" );

   return pFunc();
}

LONG sx_RecSize( VOID )
{
#ifdef __cplusplus
   static SX_RECSIZE
#else
   static SX_RECSIZE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RECSIZE ) sx_GetProcAddress( "sx_RecSize" );

   return pFunc();
}

LONG sx_RecToString( PBYTE cpRecStruc, SHORT iLength )
{
#ifdef __cplusplus
   static SX_RECTOSTRING
#else
   static SX_RECTOSTRING pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RECTOSTRING ) sx_GetProcAddress( "sx_RecToString" );

   return pFunc( cpRecStruc, iLength );
}

VOID sx_Reindex( VOID )
{
#ifdef __cplusplus
   static SX_REINDEX
#else
   static SX_REINDEX pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_REINDEX ) sx_GetProcAddress( "sx_Reindex" );

   pFunc();
}

VOID sx_Replace( PBYTE cpFieldname, SHORT iDataType, PVOID vpData )
{
#ifdef __cplusplus
   static SX_REPLACE
#else
   static SX_REPLACE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_REPLACE ) sx_GetProcAddress( "sx_Replace" );

   pFunc( cpFieldname, iDataType, vpData );
}

BOOL sx_Rlock( LONG lRecNum )
{
#ifdef __cplusplus
   static SX_RLOCK
#else
   static SX_RLOCK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RLOCK ) sx_GetProcAddress( "sx_Rlock" );

   return pFunc( lRecNum );
}

BOOL sx_RYOFilterActivate( SHORT iFilterHandle, SHORT iBoolOperation )
{
#ifdef __cplusplus
   static SX_RYOFILTERACTIVATE
#else
   static SX_RYOFILTERACTIVATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERACTIVATE ) sx_GetProcAddress( "sx_RYOFilterActivate" );

   return pFunc( iFilterHandle, iBoolOperation );
}

SHORT sx_RYOFilterCopy( VOID )
{
#ifdef __cplusplus
   static SX_RYOFILTERCOPY
#else
   static SX_RYOFILTERCOPY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERCOPY ) sx_GetProcAddress( "sx_RYOFilterCopy" );

   return pFunc();
}

SHORT sx_RYOFilterCreate( VOID )
{
#ifdef __cplusplus
   static SX_RYOFILTERCREATE
#else
   static SX_RYOFILTERCREATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERCREATE ) sx_GetProcAddress( "sx_RYOFilterCreate" );

   return pFunc();
}

BOOL sx_RYOFilterDestroy( SHORT iFilterHandle )
{
#ifdef __cplusplus
   static SX_RYOFILTERDESTROY
#else
   static SX_RYOFILTERDESTROY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERDESTROY ) sx_GetProcAddress( "sx_RYOFilterDestroy" );

   return pFunc( iFilterHandle );
}

BOOL sx_RYOFilterGetBit( SHORT iFilterHandle, LONG lRecNo )
{
#ifdef __cplusplus
   static SX_RYOFILTERGETBIT
#else
   static SX_RYOFILTERGETBIT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERGETBIT ) sx_GetProcAddress( "sx_RYOFilterGetBit" );

   return pFunc( iFilterHandle, lRecNo );
}

BOOL sx_RYOFilterRestore( PBYTE cpFileName )
{
#ifdef __cplusplus
   static SX_RYOFILTERRESTORE
#else
   static SX_RYOFILTERRESTORE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERRESTORE ) sx_GetProcAddress( "sx_RYOFilterRestore" );

   return pFunc( cpFileName );
}

BOOL sx_RYOFilterSave( SHORT iFilterHandle, PBYTE cpFileName )
{
#ifdef __cplusplus
   static SX_RYOFILTERSAVE
#else
   static SX_RYOFILTERSAVE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERSAVE ) sx_GetProcAddress( "sx_RYOFilterSave" );

   return pFunc( iFilterHandle, cpFileName );
}

BOOL sx_RYOFilterSetBit( SHORT iFilterHandle, LONG lRecNo,
                         SHORT iOnOrOff )
{
#ifdef __cplusplus
   static SX_RYOFILTERSETBIT
#else
   static SX_RYOFILTERSETBIT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOFILTERSETBIT ) sx_GetProcAddress( "sx_RYOFilterSetBit" );

   return pFunc( iFilterHandle, lRecNo, iOnOrOff );
}

BOOL sx_RYOKeyAdd( PBYTE cpTagname, PBYTE cpKey )
{
#ifdef __cplusplus
   static SX_RYOKEYADD
#else
   static SX_RYOKEYADD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOKEYADD ) sx_GetProcAddress( "sx_RYOKeyAdd" );

   return pFunc( cpTagname, cpKey );
}

BOOL sx_RYOKeyDrop( PBYTE cpTagname )
{
#ifdef __cplusplus
   static SX_RYOKEYDROP
#else
   static SX_RYOKEYDROP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_RYOKEYDROP ) sx_GetProcAddress( "sx_RYOKeyDrop" );

   return pFunc( cpTagname );
}

BOOL sx_Seek( PBYTE cpKeyValue )
{
#ifdef __cplusplus
   static SX_SEEK
#else
   static SX_SEEK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SEEK ) sx_GetProcAddress( "sx_Seek" );

   return pFunc( cpKeyValue );
}

BOOL sx_SeekBin( PBYTE cpKeyValue, WORD uiLength )
{
#ifdef __cplusplus
   static SX_SEEKBIN
#else
   static SX_SEEKBIN pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SEEKBIN ) sx_GetProcAddress( "sx_SeekBin" );

   return pFunc( cpKeyValue, uiLength );
}

WORD sx_Select( WORD uiBaseArea )
{
#ifdef __cplusplus
   static SX_SELECT
#else
   static SX_SELECT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SELECT ) sx_GetProcAddress( "sx_Select" );

   return pFunc( uiBaseArea );
}

VOID sx_SetCentury( SHORT iValue )
{
#ifdef __cplusplus
   static SX_SETCENTURY
#else
   static SX_SETCENTURY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETCENTURY ) sx_GetProcAddress( "sx_SetCentury" );

   pFunc( iValue );
}

BOOL sx_SetCollationRule( INT nRuleType, PCHAR szSourceSymbolSet,
                          PCHAR szDestSymbolSet, BOOL bResetPrevious,
                          BOOL bOem, LONG nReserved )
{
#ifdef __cplusplus
   static SX_SETCOLLATIONRULE
#else
   static SX_SETCOLLATIONRULE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETCOLLATIONRULE ) sx_GetProcAddress( "sx_SetCollationRule" );

   return pFunc( nRuleType, szSourceSymbolSet, szDestSymbolSet, bResetPrevious,
                 bOem, nReserved );
}

VOID sx_SetDateFormat( WORD uiDateType )
{
#ifdef __cplusplus
   static SX_SETDATEFORMAT
#else
   static SX_SETDATEFORMAT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETDATEFORMAT ) sx_GetProcAddress( "sx_SetDateFormat" );

   pFunc( uiDateType );
}

VOID sx_SetDeleted( WORD uiDeleted )
{
#ifdef __cplusplus
   static SX_SETDELETED
#else
   static SX_SETDELETED pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETDELETED ) sx_GetProcAddress( "sx_SetDeleted" );

   pFunc( uiDeleted );
}

WORD sx_SetEpoch( WORD uiBaseYear )
{
#ifdef __cplusplus
   static SX_SETEPOCH
#else
   static SX_SETEPOCH pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETEPOCH ) sx_GetProcAddress( "sx_SetEpoch" );

   return pFunc( uiBaseYear );
}

LONG sx_SetErrorFunc( LONG pUserErrorFunc, LONG pUserErrorInfo )
{
#ifdef __cplusplus
   static SX_SETERRORFUNC
#else
   static SX_SETERRORFUNC pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETERRORFUNC ) sx_GetProcAddress( "sx_SetErrorFunc" );

   return pFunc( pUserErrorFunc, pUserErrorInfo );
}

VOID sx_SetErrorHook( WORD uiErrorHook )
{
#ifdef __cplusplus
   static SX_SETERRORHOOK
#else
   static SX_SETERRORHOOK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETERRORHOOK ) sx_GetProcAddress( "sx_SetErrorHook" );

   pFunc( uiErrorHook );
}

VOID sx_SetExact( WORD uiOnOff )
{
#ifdef __cplusplus
   static SX_SETEXACT
#else
   static SX_SETEXACT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETEXACT ) sx_GetProcAddress( "sx_SetExact" );

   pFunc( uiOnOff );
}

VOID sx_SetFilter( PBYTE cpExpression )
{
#ifdef __cplusplus
   static SX_SETFILTER
#else
   static SX_SETFILTER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETFILTER ) sx_GetProcAddress( "sx_SetFilter" );

   pFunc( cpExpression );
}

VOID sx_SetGaugeHook( HWND hwndGauge )
{
#ifdef __cplusplus
   static SX_SETGAUGEHOOK
#else
   static SX_SETGAUGEHOOK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETGAUGEHOOK ) sx_GetProcAddress( "sx_SetGaugeHook" );

   pFunc( hwndGauge );
}

SHORT sx_SetHandles( SHORT iNumHandles )
{
#ifdef __cplusplus
   static SX_SETHANDLES
#else
   static SX_SETHANDLES pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETHANDLES ) sx_GetProcAddress( "sx_SetHandles" );

   return pFunc( iNumHandles );
}

VOID sx_SetLockTimeout( SHORT iSeconds )
{
#ifdef __cplusplus
   static SX_SETLOCKTIMEOUT
#else
   static SX_SETLOCKTIMEOUT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETLOCKTIMEOUT ) sx_GetProcAddress( "sx_SetLockTimeout" );

   pFunc( iSeconds );
}

VOID sx_SetMachineCollation( VOID )
{
#ifdef __cplusplus
   static SX_SETMACHINECOLLATION
#else
   static SX_SETMACHINECOLLATION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETMACHINECOLLATION ) sx_GetProcAddress( "sx_SetMachineCollation" );

   pFunc();
}

WORD sx_SetMemoBlockSize( WORD uiBlockSize )
{
#ifdef __cplusplus
   static SX_SETMEMOBLOCKSIZE
#else
   static SX_SETMEMOBLOCKSIZE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETMEMOBLOCKSIZE ) sx_GetProcAddress( "sx_SetMemoBlockSize" );

   return pFunc( uiBlockSize );
}

SHORT sx_SetOrder( SHORT iIndex )
{
#ifdef __cplusplus
   static SX_SETORDER
#else
   static SX_SETORDER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETORDER ) sx_GetProcAddress( "sx_SetOrder" );

   return pFunc( iIndex );
}

VOID sx_SetPassword( PBYTE cpEncodeKey )
{
#ifdef __cplusplus
   static SX_SETPASSWORD
#else
   static SX_SETPASSWORD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETPASSWORD ) sx_GetProcAddress( "sx_SetPassword" );

   pFunc( cpEncodeKey );
}

BOOL sx_SetPrinter( PBYTE cpPrinterName )
{
#ifdef __cplusplus
   static SX_SETPRINTER
#else
   static SX_SETPRINTER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETPRINTER ) sx_GetProcAddress( "sx_SetPrinter" );

   return pFunc( cpPrinterName );
}

VOID sx_SetQueryBit( LONG lRecNo, BOOL bValue )
{
#ifdef __cplusplus
   static SX_SETQUERYBIT
#else
   static SX_SETQUERYBIT pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETQUERYBIT ) sx_GetProcAddress( "sx_SetQueryBit" );

   pFunc( lRecNo, bValue );
}

VOID sx_SetRelation( WORD uiChildArea, PBYTE cpKeyExpr )
{
#ifdef __cplusplus
   static SX_SETRELATION
#else
   static SX_SETRELATION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETRELATION ) sx_GetProcAddress( "sx_SetRelation" );

   pFunc( uiChildArea, cpKeyExpr );
}

BOOL sx_SetScope( PBYTE cpLowVal, PBYTE cpHighVal )
{
#ifdef __cplusplus
   static SX_SETSCOPE
#else
   static SX_SETSCOPE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETSCOPE ) sx_GetProcAddress( "sx_SetScope" );

   return pFunc( cpLowVal, cpHighVal );
}

VOID sx_SetSoftSeek( WORD uiOnOff )
{
#ifdef __cplusplus
   static SX_SETSOFTSEEK
#else
   static SX_SETSOFTSEEK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETSOFTSEEK ) sx_GetProcAddress( "sx_SetSoftSeek" );

   pFunc( uiOnOff );
}

VOID sx_SetStringType( WORD uiStringType )
{
#ifdef __cplusplus
   static SX_SETSTRINGTYPE
#else
   static SX_SETSTRINGTYPE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETSTRINGTYPE ) sx_GetProcAddress( "sx_SetStringType" );

   pFunc( uiStringType );
}

VOID sx_SetSystemCollation( VOID )
{
#ifdef __cplusplus
   static SX_SETSYSTEMCOLLATION
#else
   static SX_SETSYSTEMCOLLATION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETSYSTEMCOLLATION ) sx_GetProcAddress( "sx_SetSystemCollation" );

   pFunc();
}

VOID sx_SetTranslate( WORD uiOnOff )
{
#ifdef __cplusplus
   static SX_SETTRANSLATE
#else
   static SX_SETTRANSLATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETTRANSLATE ) sx_GetProcAddress( "sx_SetTranslate" );

   pFunc( uiOnOff );
}

VOID sx_SetTurboRead( WORD uiOnOff )
{
#ifdef __cplusplus
   static SX_SETTURBOREAD
#else
   static SX_SETTURBOREAD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETTURBOREAD ) sx_GetProcAddress( "sx_SetTurboRead" );

   pFunc( uiOnOff );
}

VOID sx_SetUDFPath( PBYTE pbPath )
{
#ifdef __cplusplus
   static SX_SETUDFPATH
#else
   static SX_SETUDFPATH pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETUDFPATH ) sx_GetProcAddress( "sx_SetUDFPath" );

   pFunc( pbPath );
}

CHAR sx_SetUserDelimiter( CHAR cDelimiter, BOOL bMakeTrimmWhenCopy,
                          BOOL bRespectMemo )
{
#ifdef __cplusplus
   static SX_SETUSERDELIMITER
#else
   static SX_SETUSERDELIMITER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SETUSERDELIMITER ) sx_GetProcAddress( "sx_SetUserDelimiter" );

   return pFunc( cDelimiter, bMakeTrimmWhenCopy, bRespectMemo );
}

VOID sx_GetUDFPath( PBYTE pbPath, INT nMaxPathLen )
{
#ifdef __cplusplus
   static SX_GETUDFPATH
#else
   static SX_GETUDFPATH pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_GETUDFPATH ) sx_GetProcAddress( "sx_GetUDFPath" );

   pFunc( pbPath, nMaxPathLen );
}

VOID sx_Skip( LONG lNumRecs )
{
#ifdef __cplusplus
   static SX_SKIP
#else
   static SX_SKIP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SKIP ) sx_GetProcAddress( "sx_Skip" );

   pFunc( lNumRecs );
}

LONG _sx_SysProp( WORD uiSysItem, PVOID vpData )
{
#ifdef __cplusplus
   static SX_SYSPROP
#else
   static SX_SYSPROP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SYSPROP ) sx_GetProcAddress( "sx_SysProp" );

   return pFunc( uiSysItem, &vpData );
}

LONG sx_SysProp( WORD uiSysItem, PVOID vpData )
{
#ifdef __cplusplus
   static SX_SYSPROP
#else
   static SX_SYSPROP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_SYSPROP ) sx_GetProcAddress( "sx_SysProp" );

   return pFunc( uiSysItem, vpData );
}

SHORT sx_TagArea( PBYTE cpTagName )
{
#ifdef __cplusplus
   static SX_TAGAREA
#else
   static SX_TAGAREA pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_TAGAREA ) sx_GetProcAddress( "sx_TagArea" );

   return pFunc( cpTagName );
}

BOOL sx_TagDelete( PBYTE cpTagName )
{
#ifdef __cplusplus
   static SX_TAGDELETE
#else
   static SX_TAGDELETE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_TAGDELETE ) sx_GetProcAddress( "sx_TagDelete" );

   return pFunc( cpTagName );
}

LONG sx_TagName( SHORT iTagArea )
{
#ifdef __cplusplus
   static SX_TAGNAME
#else
   static SX_TAGNAME pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_TAGNAME ) sx_GetProcAddress( "sx_TagName" );

   return pFunc( iTagArea );
}

VOID sx_Unlock( LONG lRecNum )
{
#ifdef __cplusplus
   static SX_UNLOCK
#else
   static SX_UNLOCK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_UNLOCK ) sx_GetProcAddress( "sx_Unlock" );

   pFunc( lRecNum );
}

SHORT sx_UseEx( PBYTE cpFileName, PBYTE cpAlias, SHORT iOpenMode, SHORT iRdeType, UINT uiModeFlag )
{
#ifdef __cplusplus
   static SX_USEEX
#else
   static SX_USEEX pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_USEEX ) sx_GetProcAddress( "sx_UseEx" );

   return pFunc( cpFileName, cpAlias, iOpenMode, iRdeType, uiModeFlag );
}

SHORT sx_Use( PBYTE cpFileName, PBYTE cpAlias, SHORT iOpenMode, SHORT iRdeType )
{
#ifdef __cplusplus
   static SX_USE
#else
   static SX_USE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_USE ) sx_GetProcAddress( "sx_Use" );

   return pFunc( cpFileName, cpAlias, iOpenMode, iRdeType );
}

LONG sx_Version( VOID )
{
#ifdef __cplusplus
   static SX_VERSION
#else
   static SX_VERSION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_VERSION ) sx_GetProcAddress( "sx_Version" );

   return pFunc();
}

WORD sx_WorkArea( PBYTE cpAlias )
{
#ifdef __cplusplus
   static SX_WORKAREA
#else
   static SX_WORKAREA pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_WORKAREA ) sx_GetProcAddress( "sx_WorkArea" );

   return pFunc( cpAlias );
}

VOID sx_Zap( VOID )
{
#ifdef __cplusplus
   static SX_ZAP
#else
   static SX_ZAP pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_ZAP ) sx_GetProcAddress( "sx_Zap" );

   pFunc();
}

VOID sx_PutMemo( PBYTE cpFieldName, LPVOID lpVariant )
{
#ifdef __cplusplus
   static SX_PUTMEMO
#else
   static SX_PUTMEMO pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PUTMEMO ) sx_GetProcAddress( "sx_PutMemo" );

   pFunc( cpFieldName, lpVariant );
}

VOID sx_PutString( PBYTE cpFieldName, LPVOID lpVariant )
{
#ifdef __cplusplus
   static SX_PUTSTRING
#else
   static SX_PUTSTRING pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PUTSTRING ) sx_GetProcAddress( "sx_PutString" );

   pFunc( cpFieldName, lpVariant );
}

VOID sx_PutDouble( PBYTE cpFieldName, LPVOID lpVariant )
{
#ifdef __cplusplus
   static SX_PUTDOUBLE
#else
   static SX_PUTDOUBLE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PUTDOUBLE ) sx_GetProcAddress( "sx_PutDouble" );

   pFunc( cpFieldName, lpVariant );
}

VOID sx_PutDateString( PBYTE cpFieldName, LPVOID lpVariant )
{
#ifdef __cplusplus
   static SX_PUTDATESTRING
#else
   static SX_PUTDATESTRING pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( SX_PUTDATESTRING ) sx_GetProcAddress( "sx_PutDateString" );

   pFunc( cpFieldName, lpVariant );
}

long FtsAdd( long l, unsigned char * uc )
{
#ifdef __cplusplus
   static FTSADD
#else
   static FTSADD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSADD ) fts_GetProcAddress( "FtsAdd" );

   return pFunc( l, uc );
}

short FtsClose( long l )
{
#ifdef __cplusplus
   static FTSCLOSE
#else
   static FTSCLOSE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSCLOSE ) fts_GetProcAddress( "FtsClose" );

   return pFunc( l );
}

long FtsCreate( char * c, short s1, short s2, char c1, short s3 )
{
#ifdef __cplusplus
   static FTSCREATE
#else
   static FTSCREATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSCREATE ) fts_GetProcAddress( "FtsCreate" );

   return pFunc( c, s1, s2, c1, s3 );
}

short FtsDelete( long l1, long l2 )
{
#ifdef __cplusplus
   static FTSDELETE
#else
   static FTSDELETE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSDELETE ) fts_GetProcAddress( "FtsDelete" );

   return pFunc( l1, l2 );
}

int FtsSet( long l, unsigned char * uc )
{
#ifdef __cplusplus
   static FTSSET
#else
   static FTSSET pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSSET ) fts_GetProcAddress( "FtsSet" );

   return pFunc( l, uc );
}

short FtsIsDelete( long l1, long l2 )
{
#ifdef __cplusplus
   static FTSISDELETE
#else
   static FTSISDELETE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSISDELETE ) fts_GetProcAddress( "FtsIsDelete" );

   return pFunc( l1, l2 );
}

long FtsOpen( char * c, short s1, short s2 )
{
#ifdef __cplusplus
   static FTSOPEN
#else
   static FTSOPEN pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSOPEN ) fts_GetProcAddress( "FtsOpen" );

   return pFunc( c, s1, s2 );
}

long FtsNextRec( long l )
{
#ifdef __cplusplus
   static FTSNEXTREC
#else
   static FTSNEXTREC pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSNEXTREC ) fts_GetProcAddress( "FtsNextRec" );

   return pFunc( l );
}

long FtsNumRecs( long l )
{
#ifdef __cplusplus
   static FTSNUMRECS
#else
   static FTSNUMRECS pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSNUMRECS ) fts_GetProcAddress( "FtsNumRecs" );

   return pFunc( l );
}

short FtsReplace( long l1, unsigned char * uc, long l2 )
{
#ifdef __cplusplus
   static FTSREPLACE
#else
   static FTSREPLACE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSREPLACE ) fts_GetProcAddress( "FtsReplace" );

   return pFunc( l1, uc, l2 );
}

short FtsUnDelete( long l1, long l2 )
{
#ifdef __cplusplus
   static FTSUNDELETE
#else
   static FTSUNDELETE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSUNDELETE ) fts_GetProcAddress( "FtsUnDelete" );

   return pFunc( l1, l2 );
}

short FtsVerify( long l, unsigned char * c, unsigned char * uc, short s )
{
#ifdef __cplusplus
   static FTSVERIFY
#else
   static FTSVERIFY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSVERIFY ) fts_GetProcAddress( "FtsVerify" );

   return pFunc( l, c, uc, s );
}

char * FtsVersion( void )
{
#ifdef __cplusplus
   static FTSVERSION
#else
   static FTSVERSION pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FTSVERSION ) fts_GetProcAddress( "FtsVersion" );

   return pFunc();
}

//------------------------------------------------------------------------------
static void __sx_CreateINITFile( const char * pIniFile )
{
   FILE * FIniHandle = hb_fopen( pIniFile, "w" );

   fprintf( FIniHandle, ";==========================\n" );
   fprintf( FIniHandle, ";SIXAPI initialization file\n" );
   fprintf( FIniHandle, ";==========================\n" );

   /* Notes */
   fprintf( FIniHandle, ";DateFormat AMERICAN  0 /* MM/DD/YY */\n" );
   fprintf( FIniHandle, ";DateFormat ANSI      1 /* YY.MM.DD */\n" );
   fprintf( FIniHandle, ";DateFormat BRITISH   2 /* DD/MM/YY */\n" );
   fprintf( FIniHandle, ";DateFormat FRENCH    3 /* DD/MM/YY */\n" );
   fprintf( FIniHandle, ";DateFormat GERMAN    4 /* DD.MM.YY */\n" );
   fprintf( FIniHandle, ";DateFormat ITALIAN   5 /* DD-MM-YY */\n" );
   fprintf( FIniHandle, ";DateFormat SPANISH   6 /* DD-MM-YY */\n\n" );
   fprintf( FIniHandle, ";RDEType SDENTX       1\n" );
   fprintf( FIniHandle, ";RDEType SDEFOX       2\n" );
   fprintf( FIniHandle, ";RDEType SDENSX       3\n" );
   fprintf( FIniHandle, ";RDEType SDENSXDBT    4\n\n" );
   fprintf( FIniHandle, ";RDEType DBFNTX       1\n" );
   fprintf( FIniHandle, ";RDEType DBFFOX       2\n" );
   fprintf( FIniHandle, ";RDEType DBFNSX       3\n" );
   fprintf( FIniHandle, ";RDEType DBFNSXDBT    4\n\n" );

   fprintf( FIniHandle, ";Commit Level\n" );
   fprintf( FIniHandle, ";0. Full commit. Always write data to disk. Do not use SDE buffers and force\n" );
   fprintf( FIniHandle, ";Windows to write any cached data. This offers the slowest performance\n" );
   fprintf( FIniHandle, ";(100x slower than level = 2 in some cases) but the data is guaranteed to\n" );
   fprintf( FIniHandle, ";always be saved upon each write.\n" );
   fprintf( FIniHandle, ";1. Normal commit. Do not use the SDE buffers, but do not force Windows to\n" );
   fprintf( FIniHandle, ";write the cached data. This offers very good performance and allows Windows\n" );
   fprintf( FIniHandle, ";to manage the data caching. (Usually 50-100%s slower than level = 2)\n", "%" );
   fprintf( FIniHandle, ";2. None. Let the SDE use its buffering mechanisms and do not force Windows\n" );
   fprintf( FIniHandle, ";to write the cached data. This offers the best performance\n" );
   fprintf( FIniHandle, ";(100x faster than level = 0 in some cases).\n\n" );

   fprintf( FIniHandle, ";ErrorLevel\n" );
   fprintf( FIniHandle, ";0  No error message at all.\n" );
   fprintf( FIniHandle, ";1  Only report Fatal errors (Default).\n" );
   fprintf( FIniHandle, ";2  Report all errors.\n\n" );

   fprintf( FIniHandle, ";MemoBlock Size\n" );
   fprintf( FIniHandle, ";This does not apply to CA-Clipper .DBT memo files, which use\n" );
   fprintf( FIniHandle, ";fixed 512 byte blocks.\n" );
   fprintf( FIniHandle, ";The default .FPT memo block size is 32 bytes. The default .SMT memo block size\n" );
   fprintf( FIniHandle, ";is 1. This function sets a new default block size that will be used when creating\n" );
   fprintf( FIniHandle, ";any new table that has memos and will also change the block size in memo files\n" );
   fprintf( FIniHandle, ";when the DBF is packed. It does not affect existing memo files except when the\n" );
   fprintf( FIniHandle, ";corresponding DBF is packed.\n" );
   fprintf( FIniHandle, ";The size must be a value from 1 through 1024.\n\n" );

   /* Preset Defaults */
   fprintf( FIniHandle, "\n[Setup]\n" );
   fprintf( FIniHandle, "SetErrorLevel=1\n" );
   fprintf( FIniHandle, "SetCommitLevel=2\n" );
   fprintf( FIniHandle, "SetDateFormat=2\n" );
   fprintf( FIniHandle, "SetCentury=1\n" );
   fprintf( FIniHandle, "SetDeleted=1\n" );
   fprintf( FIniHandle, "SetRDDDefault=3\n" );
   fprintf( FIniHandle, "SetMemoBlockSize=1\n" );
   fprintf( FIniHandle, "DllName=%s\n", SDE_DLL );
   fprintf( FIniHandle, "FTSDllName=%s\n", FTS_DLL );
   fprintf( FIniHandle, "AutoOpenIndexIsDisabled=1\n" );
   fclose( FIniHandle );
}

//------------------------------------------------------------------------------
static void hb_sixapiRddInit( void * cargo )
{
   BYTE  bBufferEXE[ 250 ];
   BYTE  bBuffer[ 1024 ];
   BYTE  bFTSBuffer[ 1024 ];
   char  pIniFile[ 256 ];
   int   iDateFormat,
         iCentury,
         iDeleted,
         iAutoOpenDisabled,
         iCommitLevel;
   PHB_FNAME pFileName;

   HB_SYMBOL_UNUSED( cargo );

   GetModuleFileName( NULL, ( char * ) bBufferEXE, 249 );

   pFileName = hb_fsFNameSplit( ( const char * ) bBufferEXE );

   /* Modifiable SET-UP in SDE.INI */
   hb_snprintf( pIniFile, sizeof( pIniFile ), "%s/%s", pFileName->szPath, "sde.ini" );

   hb_xfree( pFileName );

   if( ! hb_fsFileExists( ( const char * ) pIniFile ) )
      __sx_CreateINITFile( ( const char * ) pIniFile );

   i_sxApi_Error_Level    = GetPrivateProfileInt( "Setup", "SetErrorLevel", 1, pIniFile );
   i_sxApi_RDD_Default    = GetPrivateProfileInt( "Setup", "SetRDDDefault", 3, pIniFile );
   iDateFormat            = GetPrivateProfileInt( "Setup", "SetDateFormat", 2, pIniFile );
   iDeleted               = GetPrivateProfileInt( "Setup", "SetDeleted", 1, pIniFile );
   iCentury               = GetPrivateProfileInt( "Setup", "SetCentury", 1, pIniFile );
   iCommitLevel           = GetPrivateProfileInt( "Setup", "SetCommitLevel", 2, pIniFile );
   iAutoOpenDisabled      = GetPrivateProfileInt( "Setup", "AutoOpenIndexIsDisabled", 1, pIniFile );
   i_sxApi_MemoBlock_Size = GetPrivateProfileInt( "Setup", "SetMemoBlockSize", 32, pIniFile );
   GetPrivateProfileString( "Setup", "DllName", SDE_DLL, ( char * ) bBuffer, sizeof( bBuffer ) - 1, pIniFile );
   GetPrivateProfileString( "Setup", "FTSDllName", FTS_DLL, ( char * ) bFTSBuffer, sizeof( bFTSBuffer ) - 1, pIniFile );

   /* DEFAULT SETTINGS */

   hModule    = LoadLibrary( ( LPCSTR ) bBuffer );
   hFTSModule = LoadLibrary( ( LPCSTR ) bFTSBuffer );

   if( ! hModule )
   {
      char __szError[ 256 ];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot load %s", bBuffer );
      hb_errInternal( 5178, __szError, NULL, NULL );
      return;
   }

   if( ! hFTSModule )
   {
      char __szError[ 256 ];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot load %s", bFTSBuffer );
      hb_errInternal( 5178, __szError, NULL, NULL );
      return;
   }

   /* Disable AutoOpen Index File */
   if( iAutoOpenDisabled == 1 )
      _sx_SysProp( SDE_SP_SETDISABLEAUTO, ( PVOID ) 1 );

   sx_ErrorLevel( ( WORD ) i_sxApi_Error_Level );
   sx_CommitLevel( ( WORD ) iCommitLevel );
   sx_SetMemoBlockSize( ( WORD ) i_sxApi_MemoBlock_Size );

   /*
      C Declaration
      INT SDEAPI WINAPI sx_CommitLevel (INT nNewLevel);

      Description
      Set how and the SDE and Windows writes data to disk.

      Parameters
      nNewLevel: Value to set the commit level to. Possible values are 0,1 or 2.

      0. Full commit. Always write data to disk. Do not use SDE buffers and force
      Windows to write any cached data. This offers the slowest performance
      (100x slower than level = 2 in some cases) but the data is guaranteed to
      always be saved upon each write.

      1. Normal commit. Do not use the SDE buffers, but do not force Windows to
      write the cached data. This offers very good performance and allows Windows
      to manage the data caching. (Usually 50-100% slower than level = 2)

      2. None. Let the SDE use its buffering mechanisms and do not force Windows
      to write the cached data. This offers the best performance
      (100x faster than level = 0 in some cases).

      Return Value
      Current commit level value.
    */

   /* Global Set Ups */
   sx_SetStringType( 1 );                    // C strings
   sx_SetCentury( ( WORD ) iCentury );       // dates to display century
   sx_SetDateFormat( ( WORD ) iDateFormat ); // date format DD/MM/CCYY
   sx_SetDeleted( ( WORD ) iDeleted );       // filter deleted records

   /*
      Both dBase and Clipper UPPER() and LOWER() case conversion functions
      limit the characters eligible for case conversion. With UPPER(), only
      characters a-z are converted to upper case. With LOWER(), only
      characters A-Z are converted. Characters with diacritical marks ARE NOT
      CONVERTED when this switch is TRUE if sx_SetTranslate is also set to TRUE.
      To limit case conversion using this switch, set sx_SetTranslate to TRUE
      and set the sx_SysProp value on as well.

      sx_SetTranslate( TRUE );
      sx_SysProp( SDE_SP_SETLIMITCASECONV, (VOIDP)1 );
    */

   //required database to be opened
   //sx_SetTranslate( TRUE );
   //sx_SysProp( SDE_SP_SETLIMITCASECONV, (PVOID)1 );
   // Init PHB_ITEM To Hold Opened DBF
   if( Opened_DBF_Property == NULL )
   {
      Opened_DBF_Property = hb_itemNew( NULL );
      hb_arrayNew( Opened_DBF_Property, 0 );
   }
}

//------------------------------------------------------------------------------
static void hb_sixapiRddExit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hModule )
   {
      sx_CloseAll();
      sx_FinalizeSession();

      FreeLibrary( hModule );

      if( Opened_DBF_Property )
      {
         hb_itemRelease( Opened_DBF_Property );
         Opened_DBF_Property = NULL;
      }
   }

   if( hFTSModule )
      FreeLibrary( hFTSModule );
}

//------------------------------------------------------------------------------
#define __PRG_SOURCE__        __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER   HB_PCODE_VER
#endif

HB_CALL_ON_STARTUP_BEGIN( _hb_sixapi_rdd_init_ )
hb_vmAtInit( hb_sixapiRddInit, NULL );
hb_vmAtExit( hb_sixapiRddExit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sixapi_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_sixapi_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( _hb_sixapi_rdd_init_ )
   #include "hbiniseg.h"
#endif
