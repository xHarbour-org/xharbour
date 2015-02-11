/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Advantage Database Server RDD ( functions interface )
 *
 * Copyright 2008 Andi Jahja <harbour/AT/cbn/net/id>
 * www - http://www.harbour-project.org
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
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


#include "rddads.h"

static HMODULE hModule = NULL;

HB_EXTERN_BEGIN
extern HB_EXPORT void hb_errInternal( ULONG ulIntCode, const char * szText, const char * szPar1, const char * szPar2 );
extern UNSIGNED32 ENTRYPOINT hb_AdsGetVersionN( void );
HB_EXTERN_END

UNSIGNED32 ENTRYPOINT hb_AdsGetVersionN( void )
{
   UNSIGNED32 ulMajor;
   UNSIGNED32 ulMinor;
   UNSIGNED8  ucLetter;
   UNSIGNED8  ucDesc[ 128 ];
   UNSIGNED16 usDescLen = sizeof( ucDesc ) - 1;

   AdsGetVersion( &ulMajor,
                  &ulMinor,
                  &ucLetter,
                  ucDesc,
                  &usDescLen );

   return ( ( ulMajor * 100 ) + ulMinor );
}

static FARPROC Ace32_GetProcAddress( char* szFuncName )
{
   if ( !hModule )
      hModule = LoadLibrary( "ace32.dll" );

   if ( hModule )
   {
      FARPROC pFunc = GetProcAddress( hModule, szFuncName );

      if ( pFunc )
      {
         return pFunc;
      }
      else
      {
         char __szError[256];

         hb_snprintf( __szError, sizeof( __szError ), "Cannot find function: %s in ace32.dll v. %lu", szFuncName, hb_AdsGetVersionN() );
         hb_errInternal( 5178, __szError, NULL, NULL );
         return NULL;
      }
   }
   else
   {
      char __szError[256];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot load ace32.dll" );
      hb_errInternal( 5178, __szError, NULL, NULL );
      return NULL;
   }

}

HB_FUNC( ADSVERSION_N )
{
   hb_retnl( hb_AdsGetVersionN() );
}

HB_FUNC( TESTACEAPI )
{
   hb_retl( GetProcAddress( hModule, hb_parc( 1 ) ) != NULL );
}

UNSIGNED32 ENTRYPOINT AdsSetFieldRaw( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
   static ADSSETFIELDRAW_PTR
#else
   static ADSSETFIELDRAW_PTR pFunc = NULL;
   if ( !pFunc )
#endif
      pFunc = (ADSSETFIELDRAW_PTR) Ace32_GetProcAddress( "AdsSetFieldRaw" );

   return pFunc( hObj, pucFldName, pucBuf, ulLen  );
}

UNSIGNED32 ENTRYPOINT AdsDeleteFile( ADSHANDLE hConnection, UNSIGNED8* pucFileName )
{
#if defined(__cplusplus)
   static ADSDELETEFILE_PTR
#else
   static ADSDELETEFILE_PTR pFunc = NULL;
   if ( !pFunc )
#endif
      pFunc = (ADSDELETEFILE_PTR) Ace32_GetProcAddress( "AdsDeleteFile" );

   return pFunc( hConnection, pucFileName );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldRaw( ADSHANDLE hTbl, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen )
{
#if defined(__cplusplus)
   static ADSGETFIELDRAW_PTR
#else
   static ADSGETFIELDRAW_PTR pFunc = NULL;
   if ( !pFunc )
#endif
      pFunc = (ADSGETFIELDRAW_PTR) Ace32_GetProcAddress( "AdsGetFieldRaw" );

   return pFunc( hTbl, pucFldName, pucBuf, pulLen );
}

UNSIGNED32 ENTRYPOINT AdsAddCustomKey( ADSHANDLE hIndex )
{
#if defined(__cplusplus)
    static ADSADDCUSTOMKEY_PTR
#else
    static ADSADDCUSTOMKEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSADDCUSTOMKEY_PTR) Ace32_GetProcAddress( "AdsAddCustomKey" );

    return pFunc( hIndex );
}

UNSIGNED32 ENTRYPOINT AdsAppendRecord( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSAPPENDRECORD_PTR
#else
    static ADSAPPENDRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSAPPENDRECORD_PTR) Ace32_GetProcAddress( "AdsAppendRecord" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsApplicationExit( void )
{
#if defined(__cplusplus)
    static ADSAPPLICATIONEXIT_PTR
#else
    static ADSAPPLICATIONEXIT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSAPPLICATIONEXIT_PTR) Ace32_GetProcAddress( "AdsApplicationExit" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsAtBOF( ADSHANDLE hTable, UNSIGNED16 *pbBof )
{
#if defined(__cplusplus)
    static ADSATBOF_PTR
#else
    static ADSATBOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSATBOF_PTR) Ace32_GetProcAddress( "AdsAtBOF" );

    return pFunc( hTable, pbBof );
}

UNSIGNED32 ENTRYPOINT AdsAtEOF( ADSHANDLE hTable, UNSIGNED16 *pbEof )
{
#if defined(__cplusplus)
    static ADSATEOF_PTR
#else
    static ADSATEOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSATEOF_PTR) Ace32_GetProcAddress( "AdsAtEOF" );

    return pFunc( hTable, pbEof );
}

UNSIGNED32 ENTRYPOINT AdsBeginTransaction( ADSHANDLE hConnect )
{
#if defined(__cplusplus)
    static ADSBEGINTRANSACTION_PTR
#else
    static ADSBEGINTRANSACTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSBEGINTRANSACTION_PTR) Ace32_GetProcAddress( "AdsBeginTransaction" );

    return pFunc( hConnect );
}

UNSIGNED32 ENTRYPOINT AdsBinaryToFile( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucFileName )
{
#if defined(__cplusplus)
    static ADSBINARYTOFILE_PTR
#else
    static ADSBINARYTOFILE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSBINARYTOFILE_PTR) Ace32_GetProcAddress( "AdsBinaryToFile" );

    return pFunc( hTable, pucFldName, pucFileName );
}

UNSIGNED32 ENTRYPOINT AdsCacheOpenCursors( UNSIGNED16 usOpen )
{
#if defined(__cplusplus)
    static ADSCACHEOPENCURSORS_PTR
#else
    static ADSCACHEOPENCURSORS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCACHEOPENCURSORS_PTR) Ace32_GetProcAddress( "AdsCacheOpenCursors" );

    return pFunc( usOpen );
}

UNSIGNED32 ENTRYPOINT AdsCacheOpenTables( UNSIGNED16 usOpen )
{
#if defined(__cplusplus)
    static ADSCACHEOPENTABLES_PTR
#else
    static ADSCACHEOPENTABLES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCACHEOPENTABLES_PTR) Ace32_GetProcAddress( "AdsCacheOpenTables" );

    return pFunc( usOpen );
}

UNSIGNED32 ENTRYPOINT AdsCacheRecords( ADSHANDLE hTable, UNSIGNED16 usNumRecords )
{
#if defined(__cplusplus)
    static ADSCACHERECORDS_PTR
#else
    static ADSCACHERECORDS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCACHERECORDS_PTR) Ace32_GetProcAddress( "AdsCacheRecords" );

    return pFunc( hTable, usNumRecords );
}

UNSIGNED32 ENTRYPOINT AdsCancelUpdate( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSCANCELUPDATE_PTR
#else
    static ADSCANCELUPDATE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCANCELUPDATE_PTR) Ace32_GetProcAddress( "AdsCancelUpdate" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsCancelUpdate90( ADSHANDLE hTable,  UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSCANCELUPDATE90_PTR
#else
    static ADSCANCELUPDATE90_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCANCELUPDATE90_PTR) Ace32_GetProcAddress( "AdsCancelUpdate90" );

    return pFunc( hTable, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsCheckExistence( ADSHANDLE hConnect, UNSIGNED8 *pucFileName, UNSIGNED16 *pusOnDisk )
{
#if defined(__cplusplus)
    static ADSCHECKEXISTENCE_PTR
#else
    static ADSCHECKEXISTENCE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCHECKEXISTENCE_PTR) Ace32_GetProcAddress( "AdsCheckExistence" );

    return pFunc( hConnect, pucFileName, pusOnDisk );
}

UNSIGNED32 ENTRYPOINT AdsClearAllScopes( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSCLEARALLSCOPES_PTR
#else
    static ADSCLEARALLSCOPES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARALLSCOPES_PTR) Ace32_GetProcAddress( "AdsClearAllScopes" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsClearDefault( void )
{
#if defined(__cplusplus)
    static ADSCLEARDEFAULT_PTR
#else
    static ADSCLEARDEFAULT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARDEFAULT_PTR) Ace32_GetProcAddress( "AdsClearDefault" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsClearFilter( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSCLEARFILTER_PTR
#else
    static ADSCLEARFILTER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARFILTER_PTR) Ace32_GetProcAddress( "AdsClearFilter" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsClearRelation( ADSHANDLE hTableParent )
{
#if defined(__cplusplus)
    static ADSCLEARRELATION_PTR
#else
    static ADSCLEARRELATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARRELATION_PTR) Ace32_GetProcAddress( "AdsClearRelation" );

    return pFunc( hTableParent );
}

UNSIGNED32 ENTRYPOINT AdsClearScope( ADSHANDLE hIndex, UNSIGNED16 usScopeOption )
{
#if defined(__cplusplus)
    static ADSCLEARSCOPE_PTR
#else
    static ADSCLEARSCOPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARSCOPE_PTR) Ace32_GetProcAddress( "AdsClearScope" );

    return pFunc( hIndex, usScopeOption );
}

UNSIGNED32 ENTRYPOINT AdsCloneTable( ADSHANDLE hTable, ADSHANDLE *phClone )
{
#if defined(__cplusplus)
    static ADSCLONETABLE_PTR
#else
    static ADSCLONETABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLONETABLE_PTR) Ace32_GetProcAddress( "AdsCloneTable" );

    return pFunc( hTable, phClone );
}

UNSIGNED32 ENTRYPOINT AdsCloseAllIndexes( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSCLOSEALLINDEXES_PTR
#else
    static ADSCLOSEALLINDEXES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLOSEALLINDEXES_PTR) Ace32_GetProcAddress( "AdsCloseAllIndexes" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsCloseAllTables( void )
{
#if defined(__cplusplus)
    static ADSCLOSEALLTABLES_PTR
#else
    static ADSCLOSEALLTABLES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLOSEALLTABLES_PTR) Ace32_GetProcAddress( "AdsCloseAllTables" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsCloseIndex( ADSHANDLE hIndex )
{
#if defined(__cplusplus)
    static ADSCLOSEINDEX_PTR
#else
    static ADSCLOSEINDEX_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLOSEINDEX_PTR) Ace32_GetProcAddress( "AdsCloseIndex" );

    return pFunc( hIndex );
}

UNSIGNED32 ENTRYPOINT AdsCloseTable( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSCLOSETABLE_PTR
#else
    static ADSCLOSETABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLOSETABLE_PTR) Ace32_GetProcAddress( "AdsCloseTable" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsCloseCachedTables( ADSHANDLE hConnection )
{
#if defined(__cplusplus)
    static ADSCLOSECACHEDTABLES_PTR
#else
    static ADSCLOSECACHEDTABLES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLOSECACHEDTABLES_PTR) Ace32_GetProcAddress( "AdsCloseCachedTables" );

    return pFunc( hConnection );
}

UNSIGNED32 ENTRYPOINT AdsCommitTransaction( ADSHANDLE hConnect )
{
#if defined(__cplusplus)
    static ADSCOMMITTRANSACTION_PTR
#else
    static ADSCOMMITTRANSACTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCOMMITTRANSACTION_PTR) Ace32_GetProcAddress( "AdsCommitTransaction" );

    return pFunc( hConnect );
}

UNSIGNED32 ENTRYPOINT AdsConnect( UNSIGNED8 *pucServerName, ADSHANDLE *phConnect )
{
#if defined(__cplusplus)
    static ADSCONNECT_PTR
#else
    static ADSCONNECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCONNECT_PTR) Ace32_GetProcAddress( "AdsConnect" );

    return pFunc( pucServerName, phConnect );
}

UNSIGNED32 ENTRYPOINT AdsConnect26( UNSIGNED8 *pucServerName, UNSIGNED16 usServerTypes, ADSHANDLE *phConnect )
{
#if defined(__cplusplus)
    static ADSCONNECT26_PTR
#else
    static ADSCONNECT26_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCONNECT26_PTR) Ace32_GetProcAddress( "AdsConnect26" );

    return pFunc( pucServerName, usServerTypes, phConnect );
}

UNSIGNED32 ENTRYPOINT AdsConnect60( UNSIGNED8 *pucServerPath, UNSIGNED16 usServerTypes, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED32 ulOptions, ADSHANDLE *phConnect )
{
#if defined(__cplusplus)
    static ADSCONNECT60_PTR
#else
    static ADSCONNECT60_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCONNECT60_PTR) Ace32_GetProcAddress( "AdsConnect60" );

    return pFunc( pucServerPath, usServerTypes, pucUserName, pucPassword, ulOptions, phConnect );
}

UNSIGNED32 ENTRYPOINT AdsIsConnectionAlive(ADSHANDLE hConnect, UNSIGNED16 *pbConnectionIsAlive )
{
#if defined(__cplusplus)
    static ADSISCONNECTIONALIVE_PTR
#else
    static ADSISCONNECTIONALIVE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISCONNECTIONALIVE_PTR) Ace32_GetProcAddress( "AdsIsConnectionAlive" );

    return pFunc( hConnect, pbConnectionIsAlive );
}

UNSIGNED32 ENTRYPOINT AdsContinue( ADSHANDLE hTable, UNSIGNED16 *pbFound )
{
#if defined(__cplusplus)
    static ADSCONTINUE_PTR
#else
    static ADSCONTINUE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCONTINUE_PTR) Ace32_GetProcAddress( "AdsContinue" );

    return pFunc( hTable, pbFound );
}

UNSIGNED32 ENTRYPOINT AdsConvertTable( ADSHANDLE hObj, UNSIGNED16 usFilterOption, UNSIGNED8 *pucFile, UNSIGNED16 usTableType )
{
#if defined(__cplusplus)
    static ADSCONVERTTABLE_PTR
#else
    static ADSCONVERTTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCONVERTTABLE_PTR) Ace32_GetProcAddress( "AdsConvertTable" );

    return pFunc( hObj, usFilterOption, pucFile, usTableType );
}

UNSIGNED32 ENTRYPOINT AdsCopyTable( ADSHANDLE hObj, UNSIGNED16 usFilterOption, UNSIGNED8 *pucFile )
{
#if defined(__cplusplus)
    static ADSCOPYTABLE_PTR
#else
    static ADSCOPYTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCOPYTABLE_PTR) Ace32_GetProcAddress( "AdsCopyTable" );

    return pFunc( hObj, usFilterOption, pucFile );
}

UNSIGNED32 ENTRYPOINT AdsCopyTableContents( ADSHANDLE hObjFrom, ADSHANDLE hTableTo, UNSIGNED16 usFilterOption )
{
#if defined(__cplusplus)
    static ADSCOPYTABLECONTENTS_PTR
#else
    static ADSCOPYTABLECONTENTS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCOPYTABLECONTENTS_PTR) Ace32_GetProcAddress( "AdsCopyTableContents" );

    return pFunc( hObjFrom, hTableTo, usFilterOption );
}

UNSIGNED32 ENTRYPOINT AdsCopyTableStructure( ADSHANDLE hTable, UNSIGNED8 *pucFile )
{
#if defined(__cplusplus)
    static ADSCOPYTABLESTRUCTURE_PTR
#else
    static ADSCOPYTABLESTRUCTURE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCOPYTABLESTRUCTURE_PTR) Ace32_GetProcAddress( "AdsCopyTableStructure" );

    return pFunc( hTable, pucFile );
}

UNSIGNED32 ENTRYPOINT AdsCreateIndex61( ADSHANDLE hObj, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucExpr, UNSIGNED8 *pucCondition, UNSIGNED8 *pucWhile, UNSIGNED32 ulOptions, UNSIGNED32 ulPageSize, ADSHANDLE *phIndex )
{
    UNSIGNED32 u_ADS_LIB_VERSION = hb_AdsGetVersionN();
#if defined(__cplusplus)
    static ADSCREATEINDEX61_PTR
       pFunc = (u_ADS_LIB_VERSION>=610) ? (ADSCREATEINDEX61_PTR) Ace32_GetProcAddress( "AdsCreateIndex61" ) : 0;
#else
    static ADSCREATEINDEX61_PTR pFunc = NULL;
    if ( (u_ADS_LIB_VERSION>=610) && !pFunc )
       pFunc = (ADSCREATEINDEX61_PTR) Ace32_GetProcAddress( "AdsCreateIndex61" );
#endif

    return ( pFunc ? pFunc( hObj, pucFileName, pucTag, pucExpr, pucCondition, pucWhile, ulOptions, ulPageSize, phIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateIndex( ADSHANDLE hObj, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucExpr, UNSIGNED8 *pucCondition, UNSIGNED8 *pucWhile, UNSIGNED32 ulOptions, ADSHANDLE *phIndex )
{
#if defined(__cplusplus)
    static ADSCREATEINDEX_PTR
#else
    static ADSCREATEINDEX_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATEINDEX_PTR) Ace32_GetProcAddress( "AdsCreateIndex" );

    return pFunc( hObj, pucFileName, pucTag, pucExpr, pucCondition, pucWhile, ulOptions, phIndex );
}

UNSIGNED32 ENTRYPOINT AdsCreateIndex90( ADSHANDLE hObj, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucExpr, UNSIGNED8 *pucCondition, UNSIGNED8 *pucWhile, UNSIGNED32 ulOptions, UNSIGNED32 ulPageSize, UNSIGNED8 *pucCollation, ADSHANDLE *phIndex )
{
#if defined(__cplusplus)
    static ADSCREATEINDEX90_PTR
#else
    static ADSCREATEINDEX90_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATEINDEX90_PTR) Ace32_GetProcAddress( "AdsCreateIndex90" );

    return pFunc( hObj, pucFileName, pucTag, pucExpr, pucCondition, pucWhile, ulOptions, ulPageSize, pucCollation, phIndex );
}

UNSIGNED32 ENTRYPOINT AdsCreateFTSIndex( ADSHANDLE hTable, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucField, UNSIGNED32 ulPageSize, UNSIGNED32 ulMinWordLen, UNSIGNED32 ulMaxWordLen, UNSIGNED16 usUseDefaultDelim, VOID *pvDelimiters, UNSIGNED16 usUseDefaultNoise, VOID *pvNoiseWords, UNSIGNED16 usUseDefaultDrop, VOID *pvDropChars, UNSIGNED16 usUseDefaultConditionals, VOID *pvConditionalChars, UNSIGNED8 *pucCollation, UNSIGNED8 *pucReserved1, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSCREATEFTSINDEX_PTR
#else
    static ADSCREATEFTSINDEX_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATEFTSINDEX_PTR) Ace32_GetProcAddress( "AdsCreateFTSIndex" );

    return pFunc( hTable, pucFileName, pucTag, pucField, ulPageSize, ulMinWordLen, ulMaxWordLen, usUseDefaultDelim, pvDelimiters, usUseDefaultNoise, pvNoiseWords, usUseDefaultDrop, pvDropChars, usUseDefaultConditionals, pvConditionalChars, pucCollation, pucReserved1, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsCreateTable( ADSHANDLE hConnection, UNSIGNED8 *pucName, UNSIGNED8 *pucAlias, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED16 usMemoSize, UNSIGNED8 *pucFields, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSCREATETABLE_PTR
#else
    static ADSCREATETABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATETABLE_PTR) Ace32_GetProcAddress( "AdsCreateTable" );

    return pFunc( hConnection, pucName, pucAlias, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, pucFields, phTable );
}

UNSIGNED32 ENTRYPOINT AdsCreateTable71( ADSHANDLE hConnection, UNSIGNED8 *pucName, UNSIGNED8 *pucDBObjName, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED16 usMemoSize, UNSIGNED8 *pucFields, UNSIGNED32 ulOptions, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSCREATETABLE71_PTR
#else
    static ADSCREATETABLE71_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATETABLE71_PTR) Ace32_GetProcAddress( "AdsCreateTable71" );

    return pFunc( hConnection, pucName, pucDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, pucFields, ulOptions, phTable );
}

UNSIGNED32 ENTRYPOINT AdsCreateTable90( ADSHANDLE hConnection, UNSIGNED8 *pucName, UNSIGNED8 *pucDBObjName, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED16 usMemoSize, UNSIGNED8 *pucFields, UNSIGNED32 ulOptions, UNSIGNED8 *pucCollation, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSCREATETABLE90_PTR
#else
    static ADSCREATETABLE90_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATETABLE90_PTR) Ace32_GetProcAddress( "AdsCreateTable90" );

    return pFunc( hConnection, pucName, pucDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, pucFields, ulOptions, pucCollation, phTable );
}

UNSIGNED32 ENTRYPOINT AdsDDCreate( UNSIGNED8 *pucDictionaryPath, UNSIGNED16 usEncrypt, UNSIGNED8 *pucDescription, ADSHANDLE *phDictionary )
{
#if defined(__cplusplus)
    static ADSDDCREATE_PTR
#else
    static ADSDDCREATE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATE_PTR) Ace32_GetProcAddress( "AdsDDCreate" );

    return pFunc( pucDictionaryPath, usEncrypt, pucDescription, phDictionary );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateRefIntegrity( ADSHANDLE hDictionary, UNSIGNED8 *pucRIName, UNSIGNED8 *pucFailTable, UNSIGNED8 *pucParentTableName, UNSIGNED8 *pucParentTagName, UNSIGNED8 *pucChildTableName, UNSIGNED8 *pucChildTagName, UNSIGNED16 usUpdateRule, UNSIGNED16 usDeleteRule )
{
#if defined(__cplusplus)
    static ADSDDCREATEREFINTEGRITY_PTR
#else
    static ADSDDCREATEREFINTEGRITY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATEREFINTEGRITY_PTR) Ace32_GetProcAddress( "AdsDDCreateRefIntegrity" );

    return pFunc( hDictionary, pucRIName, pucFailTable, pucParentTableName, pucParentTagName, pucChildTableName, pucChildTagName, usUpdateRule, usDeleteRule );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateRefIntegrity62( ADSHANDLE hDictionary, UNSIGNED8 *pucRIName, UNSIGNED8 *pucFailTable, UNSIGNED8 *pucParentTableName, UNSIGNED8 *pucParentTagName, UNSIGNED8 *pucChildTableName, UNSIGNED8 *pucChildTagName, UNSIGNED16 usUpdateRule, UNSIGNED16 usDeleteRule, UNSIGNED8 *pucNoPrimaryError, UNSIGNED8 *pucCascadeError )
{
#if defined(__cplusplus)
    static ADSDDCREATEREFINTEGRITY62_PTR
#else
    static ADSDDCREATEREFINTEGRITY62_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATEREFINTEGRITY62_PTR) Ace32_GetProcAddress( "AdsDDCreateRefIntegrity62" );

    return pFunc( hDictionary, pucRIName, pucFailTable, pucParentTableName, pucParentTagName, pucChildTableName, pucChildTagName, usUpdateRule, usDeleteRule, pucNoPrimaryError, pucCascadeError );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveRefIntegrity( ADSHANDLE hDictionary, UNSIGNED8 *pucRIName )
{
#if defined(__cplusplus)
    static ADSDDREMOVEREFINTEGRITY_PTR
#else
    static ADSDDREMOVEREFINTEGRITY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREMOVEREFINTEGRITY_PTR) Ace32_GetProcAddress( "AdsDDRemoveRefIntegrity" );

    return pFunc( hDictionary, pucRIName );
}

UNSIGNED32 ENTRYPOINT AdsDDGetDatabaseProperty( ADSHANDLE hObject, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETDATABASEPROPERTY_PTR
#else
    static ADSDDGETDATABASEPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETDATABASEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetDatabaseProperty" );

    return pFunc( hObject, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetFieldProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED8 *pucFieldName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETFIELDPROPERTY_PTR
#else
    static ADSDDGETFIELDPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETFIELDPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetFieldProperty" );

    return pFunc( hObject, pucTableName, pucFieldName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetIndexFileProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexFileName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETINDEXFILEPROPERTY_PTR
#else
    static ADSDDGETINDEXFILEPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETINDEXFILEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetIndexFileProperty" );

    return pFunc( hObject, pucTableName, pucIndexFileName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetIndexProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETINDEXPROPERTY_PTR
#else
    static ADSDDGETINDEXPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETINDEXPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetIndexProperty" );

    return pFunc( hObject, pucTableName, pucIndexName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetLinkProperty( ADSHANDLE hConnect, UNSIGNED8 *pucLinkName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETLINKPROPERTY_PTR
#else
    static ADSDDGETLINKPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETLINKPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetLinkProperty" );

    return pFunc( hConnect, pucLinkName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetTableProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETTABLEPROPERTY_PTR
#else
    static ADSDDGETTABLEPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETTABLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetTableProperty" );

    return pFunc( hObject, pucTableName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetUserGroupProperty( ADSHANDLE hObject, UNSIGNED8 *pucUserGroupName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETUSERGROUPPROPERTY_PTR
#else
    static ADSDDGETUSERGROUPPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETUSERGROUPPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetUserGroupProperty" );

    return pFunc( hObject, pucUserGroupName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetUserProperty( ADSHANDLE hObject, UNSIGNED8 *pucUserName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETUSERPROPERTY_PTR
#else
    static ADSDDGETUSERPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETUSERPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetUserProperty" );

    return pFunc( hObject, pucUserName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetViewProperty( ADSHANDLE hObject, UNSIGNED8 *pucViewName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETVIEWPROPERTY_PTR
#else
    static ADSDDGETVIEWPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETVIEWPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetViewProperty" );

    return pFunc( hObject, pucViewName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetTriggerProperty( ADSHANDLE hObject, UNSIGNED8 *pucTriggerName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETTRIGGERPROPERTY_PTR
#else
    static ADSDDGETTRIGGERPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETTRIGGERPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetTriggerProperty" );

    return pFunc( hObject, pucTriggerName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetProcedureProperty( ADSHANDLE hObject, UNSIGNED8 *pucProcName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETPROCEDUREPROPERTY_PTR
#else
    static ADSDDGETPROCEDUREPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETPROCEDUREPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetProcedureProperty" );

    return pFunc( hObject, pucProcName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetRefIntegrityProperty( ADSHANDLE hObject, UNSIGNED8 *pucRIName, UNSIGNED16 usPropertyID, UNSIGNED8 *pucProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETREFINTEGRITYPROPERTY_PTR
#else
    static ADSDDGETREFINTEGRITYPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETREFINTEGRITYPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetRefIntegrityProperty" );

    return pFunc( hObject, pucRIName, usPropertyID, pucProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDGetPermissions( ADSHANDLE hDBConn, UNSIGNED8 *pucGrantee, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucParentName, UNSIGNED16 usGetInherited, UNSIGNED32 *pulPermissions )
{
#if defined(__cplusplus)
    static ADSDDGETPERMISSIONS_PTR
#else
    static ADSDDGETPERMISSIONS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETPERMISSIONS_PTR) Ace32_GetProcAddress( "AdsDDGetPermissions" );

    return pFunc( hDBConn, pucGrantee, usObjectType, pucObjectName, pucParentName, usGetInherited, pulPermissions );
}

UNSIGNED32 ENTRYPOINT AdsDDGrantPermission( ADSHANDLE hAdminConn, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucParentName, UNSIGNED8 *pucGrantee, UNSIGNED32 ulPermissions )
{
#if defined(__cplusplus)
    static ADSDDGRANTPERMISSION_PTR
#else
    static ADSDDGRANTPERMISSION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGRANTPERMISSION_PTR) Ace32_GetProcAddress( "AdsDDGrantPermission" );

    return pFunc( hAdminConn, usObjectType, pucObjectName, pucParentName, pucGrantee, ulPermissions );
}

UNSIGNED32 ENTRYPOINT AdsDDRevokePermission( ADSHANDLE hAdminConn, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucParentName, UNSIGNED8 *pucGrantee, UNSIGNED32 ulPermissions )
{
#if defined(__cplusplus)
    static ADSDDREVOKEPERMISSION_PTR
#else
    static ADSDDREVOKEPERMISSION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREVOKEPERMISSION_PTR) Ace32_GetProcAddress( "AdsDDRevokePermission" );

    return pFunc( hAdminConn, usObjectType, pucObjectName, pucParentName, pucGrantee, ulPermissions );
}

UNSIGNED32 ENTRYPOINT AdsDDSetDatabaseProperty( ADSHANDLE hDictionary, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETDATABASEPROPERTY_PTR
#else
    static ADSDDSETDATABASEPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETDATABASEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetDatabaseProperty" );

    return pFunc( hDictionary, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetFieldProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucFieldName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen, UNSIGNED16 usValidateOption, UNSIGNED8 *pucFailTable )
{
#if defined(__cplusplus)
    static ADSDDSETFIELDPROPERTY_PTR
#else
    static ADSDDSETFIELDPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETFIELDPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetFieldProperty" );

    return pFunc( hDictionary, pucTableName, pucFieldName, usPropertyID, pvProperty, usPropertyLen, usValidateOption, pucFailTable );
}

UNSIGNED32 ENTRYPOINT AdsDDSetProcedureProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucProcedureName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETPROCEDUREPROPERTY_PTR
#else
    static ADSDDSETPROCEDUREPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETPROCEDUREPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetProcedureProperty" );

    return pFunc( hDictionary, pucProcedureName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetTableProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen, UNSIGNED16 usValidateOption, UNSIGNED8 *pucFailTable )
{
#if defined(__cplusplus)
    static ADSDDSETTABLEPROPERTY_PTR
#else
    static ADSDDSETTABLEPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETTABLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetTableProperty" );

    return pFunc( hDictionary, pucTableName, usPropertyID, pvProperty, usPropertyLen, usValidateOption, pucFailTable );
}

UNSIGNED32 ENTRYPOINT AdsDDSetUserGroupProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucUserGroupName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETUSERGROUPPROPERTY_PTR
#else
    static ADSDDSETUSERGROUPPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETUSERGROUPPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetUserGroupProperty" );

    return pFunc( hDictionary, pucUserGroupName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetUserProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucUserName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETUSERPROPERTY_PTR
#else
    static ADSDDSETUSERPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETUSERPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetUserProperty" );

    return pFunc( hDictionary, pucUserName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetViewProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucViewName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETVIEWPROPERTY_PTR
#else
    static ADSDDSETVIEWPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETVIEWPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetViewProperty" );

    return pFunc( hDictionary, pucViewName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetObjectAccessRights( ADSHANDLE hDictionary, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucAccessorName, UNSIGNED8 *pucAllowedAccess )
{
#if defined(__cplusplus)
    static ADSDDSETOBJECTACCESSRIGHTS_PTR
#else
    static ADSDDSETOBJECTACCESSRIGHTS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETOBJECTACCESSRIGHTS_PTR) Ace32_GetProcAddress( "AdsDDSetObjectAccessRights" );

    return pFunc( hDictionary, pucObjectName, pucAccessorName, pucAllowedAccess );
}

UNSIGNED32 ENTRYPOINT AdsDDAddProcedure( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucContainer, UNSIGNED8 *pucProcName, UNSIGNED32 ulInvokeOption, UNSIGNED8 *pucInParams, UNSIGNED8 *pucOutParams, UNSIGNED8 *pucComments )
{
#if defined(__cplusplus)
    static ADSDDADDPROCEDURE_PTR
#else
    static ADSDDADDPROCEDURE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDPROCEDURE_PTR) Ace32_GetProcAddress( "AdsDDAddProcedure" );

    return pFunc( hDictionary, pucName, pucContainer, pucProcName, ulInvokeOption, pucInParams, pucOutParams, pucComments );
}

UNSIGNED32 ENTRYPOINT AdsDDAddProcedure100( ADSHANDLE hDictionary, UNSIGNED8 *pucName, WCHAR *pwcContainer, UNSIGNED8 *pucProcName, UNSIGNED32 ulInvokeOption, UNSIGNED8 *pucInParams, UNSIGNED8 *pucOutParams, UNSIGNED8 *pucComments )
{
#if defined(__cplusplus)
    static ADSDDADDPROCEDURE100_PTR
#else
    static ADSDDADDPROCEDURE100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDPROCEDURE100_PTR) Ace32_GetProcAddress( "AdsDDAddProcedure100" );

    return pFunc( hDictionary, pucName, pwcContainer, pucProcName, ulInvokeOption, pucInParams, pucOutParams, pucComments );
}

UNSIGNED32 ENTRYPOINT AdsDDAddTable( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucTablePath, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED8 *pucIndexFiles, UNSIGNED8 *pucComments )
{
#if defined(__cplusplus)
    static ADSDDADDTABLE_PTR
#else
    static ADSDDADDTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDTABLE_PTR) Ace32_GetProcAddress( "AdsDDAddTable" );

    return pFunc( hDictionary, pucTableName, pucTablePath, usTableType, usCharType, pucIndexFiles, pucComments );
}

UNSIGNED32 ENTRYPOINT AdsDDAddTable90( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucTablePath, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED8 *pucIndexFiles, UNSIGNED8 *pucComments, UNSIGNED8 *pucCollation )
{
#if defined(__cplusplus)
    static ADSDDADDTABLE90_PTR
#else
    static ADSDDADDTABLE90_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDTABLE90_PTR) Ace32_GetProcAddress( "AdsDDAddTable90" );

    return pFunc( hDictionary, pucTableName, pucTablePath, usTableType, usCharType, pucIndexFiles, pucComments, pucCollation );
}

UNSIGNED32 ENTRYPOINT AdsDDAddView( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucComments, UNSIGNED8 *pucSQL )
{
#if defined(__cplusplus)
    static ADSDDADDVIEW_PTR
#else
    static ADSDDADDVIEW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDVIEW_PTR) Ace32_GetProcAddress( "AdsDDAddView" );

    return pFunc( hDictionary, pucName, pucComments, pucSQL );
}

UNSIGNED32 ENTRYPOINT AdsDDAddView100( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucComments, WCHAR *pwcSQL )
{
#if defined(__cplusplus)
    static ADSDDADDVIEW100_PTR
#else
    static ADSDDADDVIEW100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDVIEW100_PTR) Ace32_GetProcAddress( "AdsDDAddView100" );

    return pFunc( hDictionary, pucName, pucComments, pwcSQL );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateTrigger( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucTableName, UNSIGNED32 ulTriggerType, UNSIGNED32 ulEventTypes, UNSIGNED32 ulContainerType, UNSIGNED8 *pucContainer, UNSIGNED8 *pucFunctionName, UNSIGNED32 ulPriority, UNSIGNED8 *pucComments, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDCREATETRIGGER_PTR
#else
    static ADSDDCREATETRIGGER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATETRIGGER_PTR) Ace32_GetProcAddress( "AdsDDCreateTrigger" );

    return pFunc( hDictionary, pucName, pucTableName, ulTriggerType, ulEventTypes, ulContainerType, pucContainer, pucFunctionName, ulPriority, pucComments, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateTrigger100( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucTableName, UNSIGNED32 ulTriggerType, UNSIGNED32 ulEventTypes, UNSIGNED32 ulContainerType, WCHAR *pwcContainer, UNSIGNED8 *pucFunctionName, UNSIGNED32 ulPriority, UNSIGNED8 *pucComments, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDCREATETRIGGER100_PTR
#else
    static ADSDDCREATETRIGGER100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATETRIGGER100_PTR) Ace32_GetProcAddress( "AdsDDCreateTrigger100" );

    return pFunc( hDictionary, pucName, pucTableName, ulTriggerType, ulEventTypes, ulContainerType, pwcContainer, pucFunctionName, ulPriority, pucComments, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveTrigger( ADSHANDLE hDictionary, UNSIGNED8 *pucName )
{
#if defined(__cplusplus)
    static ADSDDREMOVETRIGGER_PTR
#else
    static ADSDDREMOVETRIGGER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREMOVETRIGGER_PTR) Ace32_GetProcAddress( "AdsDDRemoveTrigger" );

    return pFunc( hDictionary, pucName );
}

UNSIGNED32 ENTRYPOINT AdsDDAddIndexFile( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexFilePath, UNSIGNED8 *pucComment )
{
#if defined(__cplusplus)
    static ADSDDADDINDEXFILE_PTR
#else
    static ADSDDADDINDEXFILE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDINDEXFILE_PTR) Ace32_GetProcAddress( "AdsDDAddIndexFile" );

    return pFunc( hDictionary, pucTableName, pucIndexFilePath, pucComment );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateUser( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED8 *pucDescription )
{
#if defined(__cplusplus)
    static ADSDDCREATEUSER_PTR
#else
    static ADSDDCREATEUSER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATEUSER_PTR) Ace32_GetProcAddress( "AdsDDCreateUser" );

    return pFunc( hDictionary, pucGroupName, pucUserName, pucPassword, pucDescription );
}

UNSIGNED32 ENTRYPOINT AdsDDAddUserToGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucUserName )
{
#if defined(__cplusplus)
    static ADSDDADDUSERTOGROUP_PTR
#else
    static ADSDDADDUSERTOGROUP_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDADDUSERTOGROUP_PTR) Ace32_GetProcAddress( "AdsDDAddUserToGroup" );

    return pFunc( hDictionary, pucGroupName, pucUserName );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveUserFromGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucUserName )
{
#if defined(__cplusplus)
    static ADSDDREMOVEUSERFROMGROUP_PTR
#else
    static ADSDDREMOVEUSERFROMGROUP_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREMOVEUSERFROMGROUP_PTR) Ace32_GetProcAddress( "AdsDDRemoveUserFromGroup" );

    return pFunc( hDictionary, pucGroupName, pucUserName );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteUser( ADSHANDLE hDictionary, UNSIGNED8 *pucUserName )
{
#if defined(__cplusplus)
    static ADSDDDELETEUSER_PTR
#else
    static ADSDDDELETEUSER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDELETEUSER_PTR) Ace32_GetProcAddress( "AdsDDDeleteUser" );

    return pFunc( hDictionary, pucUserName );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateUserGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucDescription )
{
#if defined(__cplusplus)
    static ADSDDCREATEUSERGROUP_PTR
#else
    static ADSDDCREATEUSERGROUP_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATEUSERGROUP_PTR) Ace32_GetProcAddress( "AdsDDCreateUserGroup" );

    return pFunc( hDictionary, pucGroupName, pucDescription );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteUserGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName )
{
#if defined(__cplusplus)
    static ADSDDDELETEUSERGROUP_PTR
#else
    static ADSDDDELETEUSERGROUP_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDELETEUSERGROUP_PTR) Ace32_GetProcAddress( "AdsDDDeleteUserGroup" );

    return pFunc( hDictionary, pucGroupName );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteIndex( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexName )
{
#if defined(__cplusplus)
    static ADSDDDELETEINDEX_PTR
#else
    static ADSDDDELETEINDEX_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDELETEINDEX_PTR) Ace32_GetProcAddress( "AdsDDDeleteIndex" );

    return pFunc( hDictionary, pucTableName, pucIndexName );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveIndexFile( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexFileName, UNSIGNED16 usDeleteFile )
{
#if defined(__cplusplus)
    static ADSDDREMOVEINDEXFILE_PTR
#else
    static ADSDDREMOVEINDEXFILE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREMOVEINDEXFILE_PTR) Ace32_GetProcAddress( "AdsDDRemoveIndexFile" );

    return pFunc( hDictionary, pucTableName, pucIndexFileName, usDeleteFile );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveProcedure( ADSHANDLE hDictionary, UNSIGNED8 *pucName )
{
#if defined(__cplusplus)
    static ADSDDREMOVEPROCEDURE_PTR
#else
    static ADSDDREMOVEPROCEDURE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREMOVEPROCEDURE_PTR) Ace32_GetProcAddress( "AdsDDRemoveProcedure" );

    return pFunc( hDictionary, pucName );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveTable( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED16 usDeleteFiles )
{
#if defined(__cplusplus)
    static ADSDDREMOVETABLE_PTR
#else
    static ADSDDREMOVETABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREMOVETABLE_PTR) Ace32_GetProcAddress( "AdsDDRemoveTable" );

    return pFunc( hObject, pucTableName, usDeleteFiles );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveView( ADSHANDLE hDictionary, UNSIGNED8 *pucName )
{
#if defined(__cplusplus)
    static ADSDDREMOVEVIEW_PTR
#else
    static ADSDDREMOVEVIEW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDREMOVEVIEW_PTR) Ace32_GetProcAddress( "AdsDDRemoveView" );

    return pFunc( hDictionary, pucName );
}

UNSIGNED32 ENTRYPOINT AdsDDRenameObject( ADSHANDLE hDictionary, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucNewObjectName, UNSIGNED16 usObjectType, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDRENAMEOBJECT_PTR
#else
    static ADSDDRENAMEOBJECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDRENAMEOBJECT_PTR) Ace32_GetProcAddress( "AdsDDRenameObject" );

    return pFunc( hDictionary, pucObjectName, pucNewObjectName, usObjectType, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDMoveObjectFile( ADSHANDLE hDictionary, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucNewPath, UNSIGNED8 *pucIndexFiles, UNSIGNED8 *pucParent, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDMOVEOBJECTFILE_PTR
#else
    static ADSDDMOVEOBJECTFILE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDMOVEOBJECTFILE_PTR) Ace32_GetProcAddress( "AdsDDMoveObjectFile" );

    return pFunc( hDictionary, usObjectType, pucObjectName, pucNewPath, pucIndexFiles, pucParent, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDFindFirstObject( ADSHANDLE hObject, UNSIGNED16 usFindObjectType, UNSIGNED8 *pucParentName, UNSIGNED8 *pucObjectName, UNSIGNED16 *pusObjectNameLen, ADSHANDLE *phFindHandle )
{
#if defined(__cplusplus)
    static ADSDDFINDFIRSTOBJECT_PTR
#else
    static ADSDDFINDFIRSTOBJECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDFINDFIRSTOBJECT_PTR) Ace32_GetProcAddress( "AdsDDFindFirstObject" );

    return pFunc( hObject, usFindObjectType, pucParentName, pucObjectName, pusObjectNameLen, phFindHandle );
}

UNSIGNED32 ENTRYPOINT AdsDDFindNextObject( ADSHANDLE hObject, ADSHANDLE hFindHandle, UNSIGNED8 *pucObjectName, UNSIGNED16 *pusObjectNameLen )
{
#if defined(__cplusplus)
    static ADSDDFINDNEXTOBJECT_PTR
#else
    static ADSDDFINDNEXTOBJECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDFINDNEXTOBJECT_PTR) Ace32_GetProcAddress( "AdsDDFindNextObject" );

    return pFunc( hObject, hFindHandle, pucObjectName, pusObjectNameLen );
}

UNSIGNED32 ENTRYPOINT AdsDDFindClose( ADSHANDLE hObject,  ADSHANDLE hFindHandle )
{
#if defined(__cplusplus)
    static ADSDDFINDCLOSE_PTR
#else
    static ADSDDFINDCLOSE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDFINDCLOSE_PTR) Ace32_GetProcAddress( "AdsDDFindClose" );

    return pFunc( hObject, hFindHandle );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateLink( ADSHANDLE hDBConn, UNSIGNED8 *pucLinkAlias, UNSIGNED8 *pucLinkedDDPath, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDCREATELINK_PTR
#else
    static ADSDDCREATELINK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATELINK_PTR) Ace32_GetProcAddress( "AdsDDCreateLink" );

    return pFunc( hDBConn, pucLinkAlias, pucLinkedDDPath, pucUserName, pucPassword, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDModifyLink( ADSHANDLE hDBConn, UNSIGNED8 *pucLinkAlias, UNSIGNED8 *pucLinkedDDPath, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDMODIFYLINK_PTR
#else
    static ADSDDMODIFYLINK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDMODIFYLINK_PTR) Ace32_GetProcAddress( "AdsDDModifyLink" );

    return pFunc( hDBConn, pucLinkAlias, pucLinkedDDPath, pucUserName, pucPassword, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDDropLink( ADSHANDLE hDBConn, UNSIGNED8 *pucLinkedDD, UNSIGNED16 usDropGlobal )
{
#if defined(__cplusplus)
    static ADSDDDROPLINK_PTR
#else
    static ADSDDDROPLINK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDROPLINK_PTR) Ace32_GetProcAddress( "AdsDDDropLink" );

    return pFunc( hDBConn, pucLinkedDD, usDropGlobal );
}

UNSIGNED32 ENTRYPOINT AdsDDCreatePublication( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucComments, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDCREATEPUBLICATION_PTR
#else
    static ADSDDCREATEPUBLICATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATEPUBLICATION_PTR) Ace32_GetProcAddress( "AdsDDCreatePublication" );

    return pFunc( hDictionary, pucPublicationName, pucComments, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDGetPublicationProperty( ADSHANDLE hObject, UNSIGNED8 *pucPublicationName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETPUBLICATIONPROPERTY_PTR
#else
    static ADSDDGETPUBLICATIONPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETPUBLICATIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetPublicationProperty" );

    return pFunc( hObject, pucPublicationName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetPublicationProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETPUBLICATIONPROPERTY_PTR
#else
    static ADSDDSETPUBLICATIONPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETPUBLICATIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetPublicationProperty" );

    return pFunc( hDictionary, pucPublicationName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDDeletePublication( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName )
{
#if defined(__cplusplus)
    static ADSDDDELETEPUBLICATION_PTR
#else
    static ADSDDDELETEPUBLICATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDELETEPUBLICATION_PTR) Ace32_GetProcAddress( "AdsDDDeletePublication" );

    return pFunc( hDictionary, pucPublicationName );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateArticle( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucRowIdentColumns, UNSIGNED8 *pucFilter, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDCREATEARTICLE_PTR
#else
    static ADSDDCREATEARTICLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATEARTICLE_PTR) Ace32_GetProcAddress( "AdsDDCreateArticle" );

    return pFunc( hDictionary, pucPublicationName, pucObjectName, pucRowIdentColumns, pucFilter, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateArticle100( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucRowIdentColumns, WCHAR *pwcFilter, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDCREATEARTICLE100_PTR
#else
    static ADSDDCREATEARTICLE100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATEARTICLE100_PTR) Ace32_GetProcAddress( "AdsDDCreateArticle100" );

    return pFunc( hDictionary, pucPublicationName, pucObjectName, pucRowIdentColumns, pwcFilter, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDGetArticleProperty( ADSHANDLE hObject, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETARTICLEPROPERTY_PTR
#else
    static ADSDDGETARTICLEPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETARTICLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetArticleProperty" );

    return pFunc( hObject, pucPublicationName, pucObjectName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetArticleProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETARTICLEPROPERTY_PTR
#else
    static ADSDDSETARTICLEPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETARTICLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetArticleProperty" );

    return pFunc( hDictionary, pucPublicationName, pucObjectName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteArticle( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName )
{
#if defined(__cplusplus)
    static ADSDDDELETEARTICLE_PTR
#else
    static ADSDDDELETEARTICLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDELETEARTICLE_PTR) Ace32_GetProcAddress( "AdsDDDeleteArticle" );

    return pFunc( hDictionary, pucPublicationName, pucObjectName );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateSubscription( ADSHANDLE hDictionary, UNSIGNED8 *pucSubscriptionName, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucTarget, UNSIGNED8 *pucUser, UNSIGNED8 *pucPassword, UNSIGNED8 *pucReplicationQueue, UNSIGNED16 usForward, UNSIGNED8 *pucComments, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDCREATESUBSCRIPTION_PTR
#else
    static ADSDDCREATESUBSCRIPTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDCREATESUBSCRIPTION_PTR) Ace32_GetProcAddress( "AdsDDCreateSubscription" );

    return pFunc( hDictionary, pucSubscriptionName, pucPublicationName, pucTarget, pucUser, pucPassword, pucReplicationQueue, usForward, pucComments, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDGetSubscriptionProperty( ADSHANDLE hObject, UNSIGNED8 *pucSubscriptionName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDGETSUBSCRIPTIONPROPERTY_PTR
#else
    static ADSDDGETSUBSCRIPTIONPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDGETSUBSCRIPTIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetSubscriptionProperty" );

    return pFunc( hObject, pucSubscriptionName, usPropertyID, pvProperty, pusPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDSetSubscriptionProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucSubscriptionName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETSUBSCRIPTIONPROPERTY_PTR
#else
    static ADSDDSETSUBSCRIPTIONPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETSUBSCRIPTIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetSubscriptionProperty" );

    return pFunc( hDictionary, pucSubscriptionName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteSubscription( ADSHANDLE hDictionary, UNSIGNED8 *pucSubscriptionName )
{
#if defined(__cplusplus)
    static ADSDDDELETESUBSCRIPTION_PTR
#else
    static ADSDDDELETESUBSCRIPTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDELETESUBSCRIPTION_PTR) Ace32_GetProcAddress( "AdsDDDeleteSubscription" );

    return pFunc( hDictionary, pucSubscriptionName );
}

UNSIGNED32 ENTRYPOINT AdsDecryptRecord( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSDECRYPTRECORD_PTR
#else
    static ADSDECRYPTRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDECRYPTRECORD_PTR) Ace32_GetProcAddress( "AdsDecryptRecord" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsDecryptTable( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSDECRYPTTABLE_PTR
#else
    static ADSDECRYPTTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDECRYPTTABLE_PTR) Ace32_GetProcAddress( "AdsDecryptTable" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsDeleteCustomKey( ADSHANDLE hIndex )
{
#if defined(__cplusplus)
    static ADSDELETECUSTOMKEY_PTR
#else
    static ADSDELETECUSTOMKEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDELETECUSTOMKEY_PTR) Ace32_GetProcAddress( "AdsDeleteCustomKey" );

    return pFunc( hIndex );
}

UNSIGNED32 ENTRYPOINT AdsDeleteIndex( ADSHANDLE hIndex )
{
#if defined(__cplusplus)
    static ADSDELETEINDEX_PTR
#else
    static ADSDELETEINDEX_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDELETEINDEX_PTR) Ace32_GetProcAddress( "AdsDeleteIndex" );

    return pFunc( hIndex );
}

UNSIGNED32 ENTRYPOINT AdsDeleteRecord( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSDELETERECORD_PTR
#else
    static ADSDELETERECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDELETERECORD_PTR) Ace32_GetProcAddress( "AdsDeleteRecord" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyColumn( ADSHANDLE hCursor, UNSIGNED8 *pucKeyColumn, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETKEYCOLUMN_PTR
#else
    static ADSGETKEYCOLUMN_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETKEYCOLUMN_PTR) Ace32_GetProcAddress( "AdsGetKeyColumn" );

    return pFunc( hCursor, pucKeyColumn, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsDisableEncryption( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSDISABLEENCRYPTION_PTR
#else
    static ADSDISABLEENCRYPTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDISABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsDisableEncryption" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsDisableLocalConnections( void )
{
#if defined(__cplusplus)
    static ADSDISABLELOCALCONNECTIONS_PTR
#else
    static ADSDISABLELOCALCONNECTIONS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDISABLELOCALCONNECTIONS_PTR) Ace32_GetProcAddress( "AdsDisableLocalConnections" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsDisconnect( ADSHANDLE hConnect )
{
#if defined(__cplusplus)
    static ADSDISCONNECT_PTR
#else
    static ADSDISCONNECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDISCONNECT_PTR) Ace32_GetProcAddress( "AdsDisconnect" );

    return pFunc( hConnect );
}

UNSIGNED32 ENTRYPOINT AdsEnableEncryption( ADSHANDLE hTable, UNSIGNED8 *pucPassword )
{
#if defined(__cplusplus)
    static ADSENABLEENCRYPTION_PTR
#else
    static ADSENABLEENCRYPTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSENABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsEnableEncryption" );

    return pFunc( hTable, pucPassword );
}

UNSIGNED32 ENTRYPOINT AdsEncryptRecord( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSENCRYPTRECORD_PTR
#else
    static ADSENCRYPTRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSENCRYPTRECORD_PTR) Ace32_GetProcAddress( "AdsEncryptRecord" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsEncryptTable( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSENCRYPTTABLE_PTR
#else
    static ADSENCRYPTTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSENCRYPTTABLE_PTR) Ace32_GetProcAddress( "AdsEncryptTable" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsEvalLogicalExpr(ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 *pbResult )
{
#if defined(__cplusplus)
    static ADSEVALLOGICALEXPR_PTR
#else
    static ADSEVALLOGICALEXPR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEVALLOGICALEXPR_PTR) Ace32_GetProcAddress( "AdsEvalLogicalExpr" );

    return pFunc( hTable, pucExpr, pbResult );
}

UNSIGNED32 ENTRYPOINT AdsEvalLogicalExprW(ADSHANDLE hTable, WCHAR *pwcExpr, UNSIGNED16 *pbResult )
{
#if defined(__cplusplus)
    static ADSEVALLOGICALEXPRW_PTR
#else
    static ADSEVALLOGICALEXPRW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEVALLOGICALEXPRW_PTR) Ace32_GetProcAddress( "AdsEvalLogicalExprW" );

    return pFunc( hTable, pwcExpr, pbResult );
}

UNSIGNED32 ENTRYPOINT AdsEvalNumericExpr(ADSHANDLE hTable, UNSIGNED8 *pucExpr, DOUBLE *pdResult )
{
#if defined(__cplusplus)
    static ADSEVALNUMERICEXPR_PTR
#else
    static ADSEVALNUMERICEXPR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEVALNUMERICEXPR_PTR) Ace32_GetProcAddress( "AdsEvalNumericExpr" );

    return pFunc( hTable, pucExpr, pdResult );
}

UNSIGNED32 ENTRYPOINT AdsEvalStringExpr(ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED8 *pucResult, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSEVALSTRINGEXPR_PTR
#else
    static ADSEVALSTRINGEXPR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEVALSTRINGEXPR_PTR) Ace32_GetProcAddress( "AdsEvalStringExpr" );

    return pFunc( hTable, pucExpr, pucResult, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsEvalTestExpr(ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 *pusType )
{
#if defined(__cplusplus)
    static ADSEVALTESTEXPR_PTR
#else
    static ADSEVALTESTEXPR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEVALTESTEXPR_PTR) Ace32_GetProcAddress( "AdsEvalTestExpr" );

    return pFunc( hTable, pucExpr, pusType );
}

UNSIGNED32 ENTRYPOINT AdsExtractKey(ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSEXTRACTKEY_PTR
#else
    static ADSEXTRACTKEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEXTRACTKEY_PTR) Ace32_GetProcAddress( "AdsExtractKey" );

    return pFunc( hIndex, pucKey, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsFailedTransactionRecovery( UNSIGNED8 *pucServer )
{
#if defined(__cplusplus)
    static ADSFAILEDTRANSACTIONRECOVERY_PTR
#else
    static ADSFAILEDTRANSACTIONRECOVERY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFAILEDTRANSACTIONRECOVERY_PTR) Ace32_GetProcAddress( "AdsFailedTransactionRecovery" );

    return pFunc( pucServer );
}

UNSIGNED32 ENTRYPOINT AdsFileToBinary(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 usBinaryType, UNSIGNED8 *pucFileName )
{
#if defined(__cplusplus)
    static ADSFILETOBINARY_PTR
#else
    static ADSFILETOBINARY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFILETOBINARY_PTR) Ace32_GetProcAddress( "AdsFileToBinary" );

    return pFunc( hTable, pucFldName, usBinaryType, pucFileName );
}

UNSIGNED32 ENTRYPOINT AdsFindConnection(UNSIGNED8 *pucServerName, ADSHANDLE *phConnect )
{
#if defined(__cplusplus)
    static ADSFINDCONNECTION_PTR
#else
    static ADSFINDCONNECTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDCONNECTION_PTR) Ace32_GetProcAddress( "AdsFindConnection" );

    return pFunc( pucServerName, phConnect );
}

UNSIGNED32 ENTRYPOINT AdsFindConnection25(UNSIGNED8 *pucFullPath, ADSHANDLE *phConnect )
{
#if defined(__cplusplus)
    static ADSFINDCONNECTION25_PTR
#else
    static ADSFINDCONNECTION25_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDCONNECTION25_PTR) Ace32_GetProcAddress( "AdsFindConnection25" );

    return pFunc( pucFullPath, phConnect );
}

UNSIGNED32 ENTRYPOINT AdsFindClose( ADSHANDLE hConnect,  ADSHANDLE lHandle )
{
#if defined(__cplusplus)
    static ADSFINDCLOSE_PTR
#else
    static ADSFINDCLOSE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDCLOSE_PTR) Ace32_GetProcAddress( "AdsFindClose" );

    return pFunc( hConnect, lHandle );
}

UNSIGNED32 ENTRYPOINT AdsFindFirstTable( ADSHANDLE hConnect, UNSIGNED8 *pucFileMask, UNSIGNED8 *pucFirstFile, UNSIGNED16 *pusFileLen, ADSHANDLE *plHandle )
{
#if defined(__cplusplus)
    static ADSFINDFIRSTTABLE_PTR
#else
    static ADSFINDFIRSTTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDFIRSTTABLE_PTR) Ace32_GetProcAddress( "AdsFindFirstTable" );

    return pFunc( hConnect, pucFileMask, pucFirstFile, pusFileLen, plHandle );
}

UNSIGNED32 ENTRYPOINT AdsFindNextTable( ADSHANDLE hConnect, ADSHANDLE lHandle, UNSIGNED8 *pucFileName, UNSIGNED16 *pusFileLen )
{
#if defined(__cplusplus)
    static ADSFINDNEXTTABLE_PTR
#else
    static ADSFINDNEXTTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDNEXTTABLE_PTR) Ace32_GetProcAddress( "AdsFindNextTable" );

    return pFunc( hConnect, lHandle, pucFileName, pusFileLen );
}

UNSIGNED32 ENTRYPOINT AdsFindFirstTable62( ADSHANDLE hConnect, UNSIGNED8 *pucFileMask, UNSIGNED8 *pucFirstDD, UNSIGNED16 *pusDDLen, UNSIGNED8 *pucFirstFile, UNSIGNED16 *pusFileLen, ADSHANDLE *plHandle )
{
#if defined(__cplusplus)
    static ADSFINDFIRSTTABLE62_PTR
#else
    static ADSFINDFIRSTTABLE62_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDFIRSTTABLE62_PTR) Ace32_GetProcAddress( "AdsFindFirstTable62" );

    return pFunc( hConnect, pucFileMask, pucFirstDD, pusDDLen, pucFirstFile, pusFileLen, plHandle );
}

UNSIGNED32 ENTRYPOINT AdsFindNextTable62( ADSHANDLE hConnect, ADSHANDLE lHandle, UNSIGNED8 *pucDDName, UNSIGNED16 *pusDDLen, UNSIGNED8 *pucFileName, UNSIGNED16 *pusFileLen )
{
#if defined(__cplusplus)
    static ADSFINDNEXTTABLE62_PTR
#else
    static ADSFINDNEXTTABLE62_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDNEXTTABLE62_PTR) Ace32_GetProcAddress( "AdsFindNextTable62" );

    return pFunc( hConnect, lHandle, pucDDName, pusDDLen, pucFileName, pusFileLen );
}

UNSIGNED32 ENTRYPOINT AdsGetAllIndexes(ADSHANDLE hTable, ADSHANDLE ahIndex[], UNSIGNED16 *pusArrayLen )
{
#if defined(__cplusplus)
    static ADSGETALLINDEXES_PTR
#else
    static ADSGETALLINDEXES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETALLINDEXES_PTR) Ace32_GetProcAddress( "AdsGetAllIndexes" );

    return pFunc( hTable, ahIndex, pusArrayLen );
}

UNSIGNED32 ENTRYPOINT AdsGetFTSIndexes(ADSHANDLE hTable, ADSHANDLE ahIndex[], UNSIGNED16 *pusArrayLen )
{
#if defined(__cplusplus)
    static ADSGETFTSINDEXES_PTR
#else
    static ADSGETFTSINDEXES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFTSINDEXES_PTR) Ace32_GetProcAddress( "AdsGetFTSIndexes" );

    return pFunc( hTable, ahIndex, pusArrayLen );
}

UNSIGNED32 ENTRYPOINT AdsGetFTSIndexInfo(ADSHANDLE hIndex, UNSIGNED8 *pucOutput, UNSIGNED32 *pulBufLen, UNSIGNED8 **ppucField, UNSIGNED32 *pulMinWordLen, UNSIGNED32 *pulMaxWordLen, UNSIGNED8 **ppucDelimiters, UNSIGNED8 **ppucNoiseWords, UNSIGNED8 **ppucDropChars, UNSIGNED8 **ppucConditionalChars, UNSIGNED8 **ppucReserved1, UNSIGNED8 **ppucReserved2, UNSIGNED32 *pulOptions )
{
#if defined(__cplusplus)
    static ADSGETFTSINDEXINFO_PTR
#else
    static ADSGETFTSINDEXINFO_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFTSINDEXINFO_PTR) Ace32_GetProcAddress( "AdsGetFTSIndexInfo" );

    return pFunc( hIndex, pucOutput, pulBufLen, ppucField, pulMinWordLen, pulMaxWordLen, ppucDelimiters, ppucNoiseWords, ppucDropChars, ppucConditionalChars, ppucReserved1, ppucReserved2, pulOptions );
}

UNSIGNED32 ENTRYPOINT AdsGetAllLocks(ADSHANDLE hTable, UNSIGNED32 aulLocks[], UNSIGNED16 *pusArrayLen )
{
#if defined(__cplusplus)
    static ADSGETALLLOCKS_PTR
#else
    static ADSGETALLLOCKS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETALLLOCKS_PTR) Ace32_GetProcAddress( "AdsGetAllLocks" );

    return pFunc( hTable, aulLocks, pusArrayLen );
}

UNSIGNED32 ENTRYPOINT AdsGetAllTables( ADSHANDLE ahTable[], UNSIGNED16 *pusArrayLen )
{
#if defined(__cplusplus)
    static ADSGETALLTABLES_PTR
#else
    static ADSGETALLTABLES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETALLTABLES_PTR) Ace32_GetProcAddress( "AdsGetAllTables" );

    return pFunc( ahTable, pusArrayLen );
}

UNSIGNED32 ENTRYPOINT AdsGetBinary(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 ulOffset, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen )
{
#if defined(__cplusplus)
    static ADSGETBINARY_PTR
#else
    static ADSGETBINARY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETBINARY_PTR) Ace32_GetProcAddress( "AdsGetBinary" );

    return pFunc( hTable, pucFldName, ulOffset, pucBuf, pulLen );
}

UNSIGNED32 ENTRYPOINT AdsGetBinaryLength(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETBINARYLENGTH_PTR
#else
    static ADSGETBINARYLENGTH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETBINARYLENGTH_PTR) Ace32_GetProcAddress( "AdsGetBinaryLength" );

    return pFunc( hTable, pucFldName, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsGetBookmark(ADSHANDLE hTable, ADSHANDLE *phBookmark )
{
#if defined(__cplusplus)
    static ADSGETBOOKMARK_PTR
#else
    static ADSGETBOOKMARK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETBOOKMARK_PTR) Ace32_GetProcAddress( "AdsGetBookmark" );

    return pFunc( hTable, phBookmark );
}

UNSIGNED32 ENTRYPOINT AdsGetBookmark60(ADSHANDLE hObj, UNSIGNED8 *pucBookmark, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETBOOKMARK60_PTR
#else
    static ADSGETBOOKMARK60_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETBOOKMARK60_PTR) Ace32_GetProcAddress( "AdsGetBookmark60" );

    return pFunc( hObj, pucBookmark, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsGetBookmarkLength(ADSHANDLE hObj, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETBOOKMARKLENGTH_PTR
#else
    static ADSGETBOOKMARKLENGTH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETBOOKMARKLENGTH_PTR) Ace32_GetProcAddress( "AdsGetBookmarkLength" );

    return pFunc( hObj, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsCompareBookmarks(UNSIGNED8 *pucBookmark1, UNSIGNED8 *pucBookmark2, SIGNED32 *plResult )
{
#if defined(__cplusplus)
    static ADSCOMPAREBOOKMARKS_PTR
#else
    static ADSCOMPAREBOOKMARKS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCOMPAREBOOKMARKS_PTR) Ace32_GetProcAddress( "AdsCompareBookmarks" );

    return pFunc( pucBookmark1, pucBookmark2, plResult );
}

UNSIGNED32 ENTRYPOINT AdsGetCollationLang(UNSIGNED8 *pucLang, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETCOLLATIONLANG_PTR
#else
    static ADSGETCOLLATIONLANG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETCOLLATIONLANG_PTR) Ace32_GetProcAddress( "AdsGetCollationLang" );

    return pFunc( pucLang, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetCollation(ADSHANDLE hConnect, UNSIGNED8 *pucCollation, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETCOLLATION_PTR
#else
    static ADSGETCOLLATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETCOLLATION_PTR) Ace32_GetProcAddress( "AdsGetCollation" );

    return pFunc( hConnect, pucCollation, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetIntProperty(ADSHANDLE hObj, UNSIGNED32 ulPropertyID, UNSIGNED32 *pulProperty )
{
#if defined(__cplusplus)
    static ADSGETINTPROPERTY_PTR
#else
    static ADSGETINTPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINTPROPERTY_PTR) Ace32_GetProcAddress( "AdsGetIntProperty" );

    return pFunc( hObj, ulPropertyID, pulProperty );
}

UNSIGNED32 ENTRYPOINT AdsGetConnectionType(ADSHANDLE hConnect, UNSIGNED16 *pusConnectType )
{
#if defined(__cplusplus)
    static ADSGETCONNECTIONTYPE_PTR
#else
    static ADSGETCONNECTIONTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETCONNECTIONTYPE_PTR) Ace32_GetProcAddress( "AdsGetConnectionType" );

    return pFunc( hConnect, pusConnectType );
}

UNSIGNED32 ENTRYPOINT AdsGetTransactionCount(ADSHANDLE hConnect, UNSIGNED32 *pulTransactionCount )
{
#if defined(__cplusplus)
    static ADSGETTRANSACTIONCOUNT_PTR
#else
    static ADSGETTRANSACTIONCOUNT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTRANSACTIONCOUNT_PTR) Ace32_GetProcAddress( "AdsGetTransactionCount" );

    return pFunc( hConnect, pulTransactionCount );
}

UNSIGNED32 ENTRYPOINT AdsGetConnectionPath(ADSHANDLE hConnect, UNSIGNED8 *pucConnectionPath, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETCONNECTIONPATH_PTR
#else
    static ADSGETCONNECTIONPATH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETCONNECTIONPATH_PTR) Ace32_GetProcAddress( "AdsGetConnectionPath" );

    return pFunc( hConnect, pucConnectionPath, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetConnectionProperty(ADSHANDLE hConnect, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED32 *pulPropertyLen )
{
#if defined(__cplusplus)
    static ADSGETCONNECTIONPROPERTY_PTR
#else
    static ADSGETCONNECTIONPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETCONNECTIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsGetConnectionProperty" );

    return pFunc( hConnect, usPropertyID, pvProperty, pulPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsGetDate(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETDATE_PTR
#else
    static ADSGETDATE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDATE_PTR) Ace32_GetProcAddress( "AdsGetDate" );

    return pFunc( hTable, pucFldName, pucBuf, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetDateFormat( UNSIGNED8 *pucFormat, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETDATEFORMAT_PTR
#else
    static ADSGETDATEFORMAT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDATEFORMAT_PTR) Ace32_GetProcAddress( "AdsGetDateFormat" );

    return pFunc( pucFormat, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetDateFormat60( ADSHANDLE hConnect, UNSIGNED8 *pucFormat, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETDATEFORMAT60_PTR
#else
    static ADSGETDATEFORMAT60_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDATEFORMAT60_PTR) Ace32_GetProcAddress( "AdsGetDateFormat60" );

    return pFunc( hConnect, pucFormat, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetDecimals( UNSIGNED16 *pusDecimals )
{
#if defined(__cplusplus)
    static ADSGETDECIMALS_PTR
#else
    static ADSGETDECIMALS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDECIMALS_PTR) Ace32_GetProcAddress( "AdsGetDecimals" );

    return pFunc( pusDecimals );
}

UNSIGNED32 ENTRYPOINT AdsGetDefault(UNSIGNED8 *pucDefault, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETDEFAULT_PTR
#else
    static ADSGETDEFAULT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDEFAULT_PTR) Ace32_GetProcAddress( "AdsGetDefault" );

    return pFunc( pucDefault, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetDeleted( UNSIGNED16 *pbUseDeleted )
{
#if defined(__cplusplus)
    static ADSGETDELETED_PTR
#else
    static ADSGETDELETED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDELETED_PTR) Ace32_GetProcAddress( "AdsGetDeleted" );

    return pFunc( pbUseDeleted );
}

UNSIGNED32 ENTRYPOINT AdsGetDouble(ADSHANDLE hTable, UNSIGNED8 *pucFldName, DOUBLE *pdValue )
{
#if defined(__cplusplus)
    static ADSGETDOUBLE_PTR
#else
    static ADSGETDOUBLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDOUBLE_PTR) Ace32_GetProcAddress( "AdsGetDouble" );

    return pFunc( hTable, pucFldName, pdValue );
}

UNSIGNED32 ENTRYPOINT AdsGetEpoch( UNSIGNED16 *pusCentury )
{
#if defined(__cplusplus)
    static ADSGETEPOCH_PTR
#else
    static ADSGETEPOCH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETEPOCH_PTR) Ace32_GetProcAddress( "AdsGetEpoch" );

    return pFunc( pusCentury );
}

UNSIGNED32 ENTRYPOINT AdsGetErrorString(UNSIGNED32 ulErrCode, UNSIGNED8 *pucBuf, UNSIGNED16 *pusBufLen )
{
#if defined(__cplusplus)
    static ADSGETERRORSTRING_PTR
#else
    static ADSGETERRORSTRING_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETERRORSTRING_PTR) Ace32_GetProcAddress( "AdsGetErrorString" );

    return pFunc( ulErrCode, pucBuf, pusBufLen );
}

UNSIGNED32 ENTRYPOINT AdsGetExact( UNSIGNED16 *pbExact )
{
#if defined(__cplusplus)
    static ADSGETEXACT_PTR
#else
    static ADSGETEXACT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETEXACT_PTR) Ace32_GetProcAddress( "AdsGetExact" );

    return pFunc( pbExact );
}

UNSIGNED32 ENTRYPOINT AdsGetExact22(ADSHANDLE hObj, UNSIGNED16 *pbExact )
{
#if defined(__cplusplus)
    static ADSGETEXACT22_PTR
#else
    static ADSGETEXACT22_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETEXACT22_PTR) Ace32_GetProcAddress( "AdsGetExact22" );

    return pFunc( hObj, pbExact );
}

UNSIGNED32 ENTRYPOINT AdsGetField(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen, UNSIGNED16 usOption )
{
#if defined(__cplusplus)
    static ADSGETFIELD_PTR
#else
    static ADSGETFIELD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELD_PTR) Ace32_GetProcAddress( "AdsGetField" );

    return pFunc( hTable, pucFldName, pucBuf, pulLen, usOption );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldW( ADSHANDLE hObj, UNSIGNED8 *pucFldName, WCHAR *pwcBuf, UNSIGNED32 *pulLen, UNSIGNED16 usOption )
{
#if defined(__cplusplus)
    static ADSGETFIELDW_PTR
#else
    static ADSGETFIELDW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDW_PTR) Ace32_GetProcAddress( "AdsGetFieldW" );

    return pFunc( hObj, pucFldName, pwcBuf, pulLen, usOption );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldDecimals(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusDecimals )
{
#if defined(__cplusplus)
    static ADSGETFIELDDECIMALS_PTR
#else
    static ADSGETFIELDDECIMALS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDDECIMALS_PTR) Ace32_GetProcAddress( "AdsGetFieldDecimals" );

    return pFunc( hTable, pucFldName, pusDecimals );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldLength(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETFIELDLENGTH_PTR
#else
    static ADSGETFIELDLENGTH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDLENGTH_PTR) Ace32_GetProcAddress( "AdsGetFieldLength" );

    return pFunc( hTable, pucFldName, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldName(ADSHANDLE hTable, UNSIGNED16 usFld, UNSIGNED8 *pucName, UNSIGNED16 *pusBufLen )
{
#if defined(__cplusplus)
    static ADSGETFIELDNAME_PTR
#else
    static ADSGETFIELDNAME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDNAME_PTR) Ace32_GetProcAddress( "AdsGetFieldName" );

    return pFunc( hTable, usFld, pucName, pusBufLen );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldNum(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusNum )
{
#if defined(__cplusplus)
    static ADSGETFIELDNUM_PTR
#else
    static ADSGETFIELDNUM_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDNUM_PTR) Ace32_GetProcAddress( "AdsGetFieldNum" );

    return pFunc( hTable, pucFldName, pusNum );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldOffset(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulOffset )
{
#if defined(__cplusplus)
    static ADSGETFIELDOFFSET_PTR
#else
    static ADSGETFIELDOFFSET_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDOFFSET_PTR) Ace32_GetProcAddress( "AdsGetFieldOffset" );

    return pFunc( hTable, pucFldName, pulOffset );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldType(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusType )
{
#if defined(__cplusplus)
    static ADSGETFIELDTYPE_PTR
#else
    static ADSGETFIELDTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDTYPE_PTR) Ace32_GetProcAddress( "AdsGetFieldType" );

    return pFunc( hTable, pucFldName, pusType );
}

UNSIGNED32 ENTRYPOINT AdsGetFilter(ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETFILTER_PTR
#else
    static ADSGETFILTER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFILTER_PTR) Ace32_GetProcAddress( "AdsGetFilter" );

    return pFunc( hTable, pucFilter, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetHandleLong(ADSHANDLE hObj, UNSIGNED32 *pulVal )
{
#if defined(__cplusplus)
    static ADSGETHANDLELONG_PTR
#else
    static ADSGETHANDLELONG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETHANDLELONG_PTR) Ace32_GetProcAddress( "AdsGetHandleLong" );

    return pFunc( hObj, pulVal );
}

UNSIGNED32 ENTRYPOINT AdsGetHandleType(ADSHANDLE hObj, UNSIGNED16 *pusType )
{
#if defined(__cplusplus)
    static ADSGETHANDLETYPE_PTR
#else
    static ADSGETHANDLETYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETHANDLETYPE_PTR) Ace32_GetProcAddress( "AdsGetHandleType" );

    return pFunc( hObj, pusType );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexCondition(ADSHANDLE hIndex, UNSIGNED8 *pucExpr, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETINDEXCONDITION_PTR
#else
    static ADSGETINDEXCONDITION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXCONDITION_PTR) Ace32_GetProcAddress( "AdsGetIndexCondition" );

    return pFunc( hIndex, pucExpr, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexExpr(ADSHANDLE hIndex, UNSIGNED8 *pucExpr, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETINDEXEXPR_PTR
#else
    static ADSGETINDEXEXPR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXEXPR_PTR) Ace32_GetProcAddress( "AdsGetIndexExpr" );

    return pFunc( hIndex, pucExpr, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexFilename(ADSHANDLE hIndex, UNSIGNED16 usOption, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETINDEXFILENAME_PTR
#else
    static ADSGETINDEXFILENAME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXFILENAME_PTR) Ace32_GetProcAddress( "AdsGetIndexFilename" );

    return pFunc( hIndex, usOption, pucName, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexHandle(ADSHANDLE hTable, UNSIGNED8 *pucIndexOrder, ADSHANDLE *phIndex )
{
#if defined(__cplusplus)
    static ADSGETINDEXHANDLE_PTR
#else
    static ADSGETINDEXHANDLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXHANDLE_PTR) Ace32_GetProcAddress( "AdsGetIndexHandle" );

    return pFunc( hTable, pucIndexOrder, phIndex );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByOrder(ADSHANDLE hTable, UNSIGNED16 usOrderNum, ADSHANDLE *phIndex )
{
#if defined(__cplusplus)
    static ADSGETINDEXHANDLEBYORDER_PTR
#else
    static ADSGETINDEXHANDLEBYORDER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXHANDLEBYORDER_PTR) Ace32_GetProcAddress( "AdsGetIndexHandleByOrder" );

    return pFunc( hTable, usOrderNum, phIndex );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByExpr(ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED32 ulDescending, ADSHANDLE *phIndex )
{
#if defined(__cplusplus)
    static ADSGETINDEXHANDLEBYEXPR_PTR
#else
    static ADSGETINDEXHANDLEBYEXPR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXHANDLEBYEXPR_PTR) Ace32_GetProcAddress( "AdsGetIndexHandleByExpr" );

    return pFunc( hTable, pucExpr, ulDescending, phIndex );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexName(ADSHANDLE hIndex, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETINDEXNAME_PTR
#else
    static ADSGETINDEXNAME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXNAME_PTR) Ace32_GetProcAddress( "AdsGetIndexName" );

    return pFunc( hIndex, pucName, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexOrderByHandle( ADSHANDLE hIndex, UNSIGNED16 *pusIndexOrder )
{
#if defined(__cplusplus)
    static ADSGETINDEXORDERBYHANDLE_PTR
#else
    static ADSGETINDEXORDERBYHANDLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXORDERBYHANDLE_PTR) Ace32_GetProcAddress( "AdsGetIndexOrderByHandle" );

    return pFunc( hIndex, pusIndexOrder );
}

UNSIGNED32 ENTRYPOINT AdsGetJulian(ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED32 *plDate )
{
#if defined(__cplusplus)
    static ADSGETJULIAN_PTR
#else
    static ADSGETJULIAN_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETJULIAN_PTR) Ace32_GetProcAddress( "AdsGetJulian" );

    return pFunc( hTable, pucFldName, plDate );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyCount(ADSHANDLE hIndex, UNSIGNED16 usFilterOption, UNSIGNED32 *pulCount )
{
#if defined(__cplusplus)
    static ADSGETKEYCOUNT_PTR
#else
    static ADSGETKEYCOUNT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETKEYCOUNT_PTR) Ace32_GetProcAddress( "AdsGetKeyCount" );

    return pFunc( hIndex, usFilterOption, pulCount );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyNum(ADSHANDLE hIndex, UNSIGNED16 usFilterOption, UNSIGNED32 *pulKey )
{
#if defined(__cplusplus)
    static ADSGETKEYNUM_PTR
#else
    static ADSGETKEYNUM_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETKEYNUM_PTR) Ace32_GetProcAddress( "AdsGetKeyNum" );

    return pFunc( hIndex, usFilterOption, pulKey );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyLength(ADSHANDLE hIndex, UNSIGNED16 *pusKeyLength )
{
#if defined(__cplusplus)
    static ADSGETKEYLENGTH_PTR
#else
    static ADSGETKEYLENGTH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETKEYLENGTH_PTR) Ace32_GetProcAddress( "AdsGetKeyLength" );

    return pFunc( hIndex, pusKeyLength );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyType(ADSHANDLE hIndex, UNSIGNED16 *usKeyType )
{
#if defined(__cplusplus)
    static ADSGETKEYTYPE_PTR
#else
    static ADSGETKEYTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETKEYTYPE_PTR) Ace32_GetProcAddress( "AdsGetKeyType" );

    return pFunc( hIndex, usKeyType );
}

UNSIGNED32 ENTRYPOINT AdsGetLastError(UNSIGNED32 *pulErrCode, UNSIGNED8 *pucBuf, UNSIGNED16 *pusBufLen )
{
#if defined(__cplusplus)
    static ADSGETLASTERROR_PTR
#else
    static ADSGETLASTERROR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETLASTERROR_PTR) Ace32_GetProcAddress( "AdsGetLastError" );

    return pFunc( pulErrCode, pucBuf, pusBufLen );
}

UNSIGNED32 ENTRYPOINT AdsGetLastTableUpdate(ADSHANDLE hTable, UNSIGNED8 *pucDate, UNSIGNED16 *pusDateLen )
{
#if defined(__cplusplus)
    static ADSGETLASTTABLEUPDATE_PTR
#else
    static ADSGETLASTTABLEUPDATE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETLASTTABLEUPDATE_PTR) Ace32_GetProcAddress( "AdsGetLastTableUpdate" );

    return pFunc( hTable, pucDate, pusDateLen );
}

UNSIGNED32 ENTRYPOINT AdsGetLogical(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbValue )
{
#if defined(__cplusplus)
    static ADSGETLOGICAL_PTR
#else
    static ADSGETLOGICAL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETLOGICAL_PTR) Ace32_GetProcAddress( "AdsGetLogical" );

    return pFunc( hTable, pucFldName, pbValue );
}

UNSIGNED32 ENTRYPOINT AdsGetLong(ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED32 *plValue )
{
#if defined(__cplusplus)
    static ADSGETLONG_PTR
#else
    static ADSGETLONG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETLONG_PTR) Ace32_GetProcAddress( "AdsGetLong" );

    return pFunc( hTable, pucFldName, plValue );
}

UNSIGNED32 ENTRYPOINT AdsGetLongLong(ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED64 *pqValue )
{
#if defined(__cplusplus)
    static ADSGETLONGLONG_PTR
#else
    static ADSGETLONGLONG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETLONGLONG_PTR) Ace32_GetProcAddress( "AdsGetLongLong" );

    return pFunc( hTable, pucFldName, pqValue );
}

UNSIGNED32 ENTRYPOINT AdsGetMemoBlockSize(ADSHANDLE hTable, UNSIGNED16 *pusBlockSize )
{
#if defined(__cplusplus)
    static ADSGETMEMOBLOCKSIZE_PTR
#else
    static ADSGETMEMOBLOCKSIZE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETMEMOBLOCKSIZE_PTR) Ace32_GetProcAddress( "AdsGetMemoBlockSize" );

    return pFunc( hTable, pusBlockSize );
}

UNSIGNED32 ENTRYPOINT AdsGetMemoLength(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETMEMOLENGTH_PTR
#else
    static ADSGETMEMOLENGTH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETMEMOLENGTH_PTR) Ace32_GetProcAddress( "AdsGetMemoLength" );

    return pFunc( hTable, pucFldName, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsGetMemoDataType(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusType )
{
#if defined(__cplusplus)
    static ADSGETMEMODATATYPE_PTR
#else
    static ADSGETMEMODATATYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETMEMODATATYPE_PTR) Ace32_GetProcAddress( "AdsGetMemoDataType" );

    return pFunc( hTable, pucFldName, pusType );
}

UNSIGNED32 ENTRYPOINT AdsGetMilliseconds(ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED32 *plTime )
{
#if defined(__cplusplus)
    static ADSGETMILLISECONDS_PTR
#else
    static ADSGETMILLISECONDS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETMILLISECONDS_PTR) Ace32_GetProcAddress( "AdsGetMilliseconds" );

    return pFunc( hTable, pucFldName, plTime );
}

UNSIGNED32 ENTRYPOINT AdsGetMoney(ADSHANDLE hTbl, UNSIGNED8 *pucFldName, SIGNED64 *pqValue )
{
#if defined(__cplusplus)
    static ADSGETMONEY_PTR
#else
    static ADSGETMONEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETMONEY_PTR) Ace32_GetProcAddress( "AdsGetMoney" );

    return pFunc( hTbl, pucFldName, pqValue );
}

UNSIGNED32 ENTRYPOINT AdsGetActiveLinkInfo(ADSHANDLE hDBConn, UNSIGNED16 usLinkNum, UNSIGNED8 *pucLinkInfo, UNSIGNED16 *pusBufferLen )
{
#if defined(__cplusplus)
    static ADSGETACTIVELINKINFO_PTR
#else
    static ADSGETACTIVELINKINFO_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETACTIVELINKINFO_PTR) Ace32_GetProcAddress( "AdsGetActiveLinkInfo" );

    return pFunc( hDBConn, usLinkNum, pucLinkInfo, pusBufferLen );
}

UNSIGNED32 ENTRYPOINT AdsGetNumActiveLinks(ADSHANDLE hDBConn, UNSIGNED16 *pusNumLinks )
{
#if defined(__cplusplus)
    static ADSGETNUMACTIVELINKS_PTR
#else
    static ADSGETNUMACTIVELINKS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETNUMACTIVELINKS_PTR) Ace32_GetProcAddress( "AdsGetNumActiveLinks" );

    return pFunc( hDBConn, pusNumLinks );
}

UNSIGNED32 ENTRYPOINT AdsGetNumFields(ADSHANDLE hTable, UNSIGNED16 *pusCount )
{
#if defined(__cplusplus)
    static ADSGETNUMFIELDS_PTR
#else
    static ADSGETNUMFIELDS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETNUMFIELDS_PTR) Ace32_GetProcAddress( "AdsGetNumFields" );

    return pFunc( hTable, pusCount );
}

UNSIGNED32 ENTRYPOINT AdsGetNumIndexes(ADSHANDLE hTable, UNSIGNED16 *pusNum )
{
#if defined(__cplusplus)
    static ADSGETNUMINDEXES_PTR
#else
    static ADSGETNUMINDEXES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETNUMINDEXES_PTR) Ace32_GetProcAddress( "AdsGetNumIndexes" );

    return pFunc( hTable, pusNum );
}

UNSIGNED32 ENTRYPOINT AdsGetNumFTSIndexes(ADSHANDLE hTable, UNSIGNED16 *pusNum )
{
#if defined(__cplusplus)
    static ADSGETNUMFTSINDEXES_PTR
#else
    static ADSGETNUMFTSINDEXES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETNUMFTSINDEXES_PTR) Ace32_GetProcAddress( "AdsGetNumFTSIndexes" );

    return pFunc( hTable, pusNum );
}

UNSIGNED32 ENTRYPOINT AdsGetNumLocks(ADSHANDLE hTable, UNSIGNED16 *pusNum )
{
#if defined(__cplusplus)
    static ADSGETNUMLOCKS_PTR
#else
    static ADSGETNUMLOCKS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETNUMLOCKS_PTR) Ace32_GetProcAddress( "AdsGetNumLocks" );

    return pFunc( hTable, pusNum );
}

UNSIGNED32 ENTRYPOINT AdsGetNumOpenTables( UNSIGNED16 *pusNum )
{
#if defined(__cplusplus)
    static ADSGETNUMOPENTABLES_PTR
#else
    static ADSGETNUMOPENTABLES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETNUMOPENTABLES_PTR) Ace32_GetProcAddress( "AdsGetNumOpenTables" );

    return pFunc( pusNum );
}

UNSIGNED32 ENTRYPOINT AdsGetRecord(ADSHANDLE hTable, UNSIGNED8 *pucRec, UNSIGNED32 *pulLen )
{
#if defined(__cplusplus)
    static ADSGETRECORD_PTR
#else
    static ADSGETRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETRECORD_PTR) Ace32_GetProcAddress( "AdsGetRecord" );

    return pFunc( hTable, pucRec, pulLen );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordCount(ADSHANDLE hTable, UNSIGNED16 usFilterOption, UNSIGNED32 *pulCount )
{
#if defined(__cplusplus)
    static ADSGETRECORDCOUNT_PTR
#else
    static ADSGETRECORDCOUNT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETRECORDCOUNT_PTR) Ace32_GetProcAddress( "AdsGetRecordCount" );

    return pFunc( hTable, usFilterOption, pulCount );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordNum(ADSHANDLE hTable, UNSIGNED16 usFilterOption, UNSIGNED32 *pulRec )
{
#if defined(__cplusplus)
    static ADSGETRECORDNUM_PTR
#else
    static ADSGETRECORDNUM_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETRECORDNUM_PTR) Ace32_GetProcAddress( "AdsGetRecordNum" );

    return pFunc( hTable, usFilterOption, pulRec );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordLength(ADSHANDLE hTable, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETRECORDLENGTH_PTR
#else
    static ADSGETRECORDLENGTH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETRECORDLENGTH_PTR) Ace32_GetProcAddress( "AdsGetRecordLength" );

    return pFunc( hTable, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordCRC(ADSHANDLE hTable, UNSIGNED32 *pulCRC, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSGETRECORDCRC_PTR
#else
    static ADSGETRECORDCRC_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETRECORDCRC_PTR) Ace32_GetProcAddress( "AdsGetRecordCRC" );

    return pFunc( hTable, pulCRC, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsGetRelKeyPos(ADSHANDLE hIndex, DOUBLE *pdPos )
{
#if defined(__cplusplus)
    static ADSGETRELKEYPOS_PTR
#else
    static ADSGETRELKEYPOS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETRELKEYPOS_PTR) Ace32_GetProcAddress( "AdsGetRelKeyPos" );

    return pFunc( hIndex, pdPos );
}

UNSIGNED32 ENTRYPOINT AdsGetScope(ADSHANDLE hIndex, UNSIGNED16 usScopeOption, UNSIGNED8 *pucScope, UNSIGNED16 *pusBufLen )
{
#if defined(__cplusplus)
    static ADSGETSCOPE_PTR
#else
    static ADSGETSCOPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSCOPE_PTR) Ace32_GetProcAddress( "AdsGetScope" );

    return pFunc( hIndex, usScopeOption, pucScope, pusBufLen );
}

UNSIGNED32 ENTRYPOINT AdsGetSearchPath(UNSIGNED8 *pucPath, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETSEARCHPATH_PTR
#else
    static ADSGETSEARCHPATH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSEARCHPATH_PTR) Ace32_GetProcAddress( "AdsGetSearchPath" );

    return pFunc( pucPath, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetServerName(ADSHANDLE hConnect, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETSERVERNAME_PTR
#else
    static ADSGETSERVERNAME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSERVERNAME_PTR) Ace32_GetProcAddress( "AdsGetServerName" );

    return pFunc( hConnect, pucName, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetServerTime(ADSHANDLE hConnect, UNSIGNED8 *pucDateBuf, UNSIGNED16 *pusDateBufLen, SIGNED32 *plTime, UNSIGNED8 *pucTimeBuf, UNSIGNED16 *pusTimeBufLen )
{
#if defined(__cplusplus)
    static ADSGETSERVERTIME_PTR
#else
    static ADSGETSERVERTIME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSERVERTIME_PTR) Ace32_GetProcAddress( "AdsGetServerTime" );

    return pFunc( hConnect, pucDateBuf, pusDateBufLen, plTime, pucTimeBuf, pusTimeBufLen );
}

UNSIGNED32 ENTRYPOINT AdsGetShort(ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED16 *psValue )
{
#if defined(__cplusplus)
    static ADSGETSHORT_PTR
#else
    static ADSGETSHORT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSHORT_PTR) Ace32_GetProcAddress( "AdsGetShort" );

    return pFunc( hTable, pucFldName, psValue );
}

UNSIGNED32 ENTRYPOINT AdsGetString(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen, UNSIGNED16 usOption )
{
#if defined(__cplusplus)
    static ADSGETSTRING_PTR
#else
    static ADSGETSTRING_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSTRING_PTR) Ace32_GetProcAddress( "AdsGetString" );

    return pFunc( hTable, pucFldName, pucBuf, pulLen, usOption );
}

UNSIGNED32 ENTRYPOINT AdsGetStringW( ADSHANDLE hObj, UNSIGNED8 *pucFldName, WCHAR *pwcBuf, UNSIGNED32 *pulLen, UNSIGNED16 usOption )
{
#if defined(__cplusplus)
    static ADSGETSTRINGW_PTR
#else
    static ADSGETSTRINGW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSTRINGW_PTR) Ace32_GetProcAddress( "AdsGetStringW" );

    return pFunc( hObj, pucFldName, pwcBuf, pulLen, usOption );
}

UNSIGNED32 ENTRYPOINT AdsGetTableAlias(ADSHANDLE hTable, UNSIGNED8 *pucAlias, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETTABLEALIAS_PTR
#else
    static ADSGETTABLEALIAS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLEALIAS_PTR) Ace32_GetProcAddress( "AdsGetTableAlias" );

    return pFunc( hTable, pucAlias, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetTableCharType(ADSHANDLE hTable, UNSIGNED16 *pusCharType )
{
#if defined(__cplusplus)
    static ADSGETTABLECHARTYPE_PTR
#else
    static ADSGETTABLECHARTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLECHARTYPE_PTR) Ace32_GetProcAddress( "AdsGetTableCharType" );

    return pFunc( hTable, pusCharType );
}

UNSIGNED32 ENTRYPOINT AdsGetTableConnection(ADSHANDLE hTable, ADSHANDLE *phConnect )
{
#if defined(__cplusplus)
    static ADSGETTABLECONNECTION_PTR
#else
    static ADSGETTABLECONNECTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLECONNECTION_PTR) Ace32_GetProcAddress( "AdsGetTableConnection" );

    return pFunc( hTable, phConnect );
}

UNSIGNED32 ENTRYPOINT AdsGetTableFilename(ADSHANDLE hTable, UNSIGNED16 usOption, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETTABLEFILENAME_PTR
#else
    static ADSGETTABLEFILENAME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLEFILENAME_PTR) Ace32_GetProcAddress( "AdsGetTableFilename" );

    return pFunc( hTable, usOption, pucName, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetTableHandle(UNSIGNED8 *pucName, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSGETTABLEHANDLE_PTR
#else
    static ADSGETTABLEHANDLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLEHANDLE_PTR) Ace32_GetProcAddress( "AdsGetTableHandle" );

    return pFunc( pucName, phTable );
}

UNSIGNED32 ENTRYPOINT AdsGetTableHandle25(ADSHANDLE hConnect, UNSIGNED8 *pucName, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSGETTABLEHANDLE25_PTR
#else
    static ADSGETTABLEHANDLE25_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLEHANDLE25_PTR) Ace32_GetProcAddress( "AdsGetTableHandle25" );

    return pFunc( hConnect, pucName, phTable );
}

UNSIGNED32 ENTRYPOINT AdsGetTableLockType(ADSHANDLE hTable, UNSIGNED16 *pusLockType )
{
#if defined(__cplusplus)
    static ADSGETTABLELOCKTYPE_PTR
#else
    static ADSGETTABLELOCKTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLELOCKTYPE_PTR) Ace32_GetProcAddress( "AdsGetTableLockType" );

    return pFunc( hTable, pusLockType );
}

UNSIGNED32 ENTRYPOINT AdsGetTableMemoSize(ADSHANDLE hTable, UNSIGNED16 *pusMemoSize )
{
#if defined(__cplusplus)
    static ADSGETTABLEMEMOSIZE_PTR
#else
    static ADSGETTABLEMEMOSIZE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLEMEMOSIZE_PTR) Ace32_GetProcAddress( "AdsGetTableMemoSize" );

    return pFunc( hTable, pusMemoSize );
}

UNSIGNED32 ENTRYPOINT AdsGetTableOpenOptions(ADSHANDLE hTable, UNSIGNED32 *pulOptions )
{
#if defined(__cplusplus)
    static ADSGETTABLEOPENOPTIONS_PTR
#else
    static ADSGETTABLEOPENOPTIONS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLEOPENOPTIONS_PTR) Ace32_GetProcAddress( "AdsGetTableOpenOptions" );

    return pFunc( hTable, pulOptions );
}

UNSIGNED32 ENTRYPOINT AdsGetTableRights(ADSHANDLE hTable, UNSIGNED16 *pusRights )
{
#if defined(__cplusplus)
    static ADSGETTABLERIGHTS_PTR
#else
    static ADSGETTABLERIGHTS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLERIGHTS_PTR) Ace32_GetProcAddress( "AdsGetTableRights" );

    return pFunc( hTable, pusRights );
}

UNSIGNED32 ENTRYPOINT AdsGetTableType(ADSHANDLE hTable, UNSIGNED16 *pusType )
{
#if defined(__cplusplus)
    static ADSGETTABLETYPE_PTR
#else
    static ADSGETTABLETYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLETYPE_PTR) Ace32_GetProcAddress( "AdsGetTableType" );

    return pFunc( hTable, pusType );
}

UNSIGNED32 ENTRYPOINT AdsGetTime(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETTIME_PTR
#else
    static ADSGETTIME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTIME_PTR) Ace32_GetProcAddress( "AdsGetTime" );

    return pFunc( hTable, pucFldName, pucBuf, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetVersion(UNSIGNED32 *pulMajor, UNSIGNED32 *pulMinor, UNSIGNED8 *pucLetter, UNSIGNED8 *pucDesc, UNSIGNED16 *pusDescLen )
{
#if defined(__cplusplus)
    static ADSGETVERSION_PTR
#else
    static ADSGETVERSION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETVERSION_PTR) Ace32_GetProcAddress( "AdsGetVersion" );

    return pFunc( pulMajor, pulMinor, pucLetter, pucDesc, pusDescLen );
}

UNSIGNED32 ENTRYPOINT AdsGotoBookmark(ADSHANDLE hTable, ADSHANDLE hBookmark )
{
#if defined(__cplusplus)
    static ADSGOTOBOOKMARK_PTR
#else
    static ADSGOTOBOOKMARK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGOTOBOOKMARK_PTR) Ace32_GetProcAddress( "AdsGotoBookmark" );

    return pFunc( hTable, hBookmark );
}

UNSIGNED32 ENTRYPOINT AdsGotoBookmark60(ADSHANDLE hObj, UNSIGNED8 *pucBookmark )
{
#if defined(__cplusplus)
    static ADSGOTOBOOKMARK60_PTR
#else
    static ADSGOTOBOOKMARK60_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGOTOBOOKMARK60_PTR) Ace32_GetProcAddress( "AdsGotoBookmark60" );

    return pFunc( hObj, pucBookmark );
}

UNSIGNED32 ENTRYPOINT AdsGotoBottom( ADSHANDLE hObj )
{
#if defined(__cplusplus)
    static ADSGOTOBOTTOM_PTR
#else
    static ADSGOTOBOTTOM_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGOTOBOTTOM_PTR) Ace32_GetProcAddress( "AdsGotoBottom" );

    return pFunc( hObj );
}

UNSIGNED32 ENTRYPOINT AdsGotoRecord(ADSHANDLE hTable, UNSIGNED32 ulRec )
{
#if defined(__cplusplus)
    static ADSGOTORECORD_PTR
#else
    static ADSGOTORECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGOTORECORD_PTR) Ace32_GetProcAddress( "AdsGotoRecord" );

    return pFunc( hTable, ulRec );
}

UNSIGNED32 ENTRYPOINT AdsGotoTop( ADSHANDLE hObj )
{
#if defined(__cplusplus)
    static ADSGOTOTOP_PTR
#else
    static ADSGOTOTOP_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGOTOTOP_PTR) Ace32_GetProcAddress( "AdsGotoTop" );

    return pFunc( hObj );
}

UNSIGNED32 ENTRYPOINT AdsImageToClipboard( ADSHANDLE hTable, UNSIGNED8 *pucFldName )
{
#if defined(__cplusplus)
    static ADSIMAGETOCLIPBOARD_PTR
#else
    static ADSIMAGETOCLIPBOARD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSIMAGETOCLIPBOARD_PTR) Ace32_GetProcAddress( "AdsImageToClipboard" );

    return pFunc( hTable, pucFldName );
}

UNSIGNED32 ENTRYPOINT AdsInTransaction(ADSHANDLE hConnect, UNSIGNED16 *pbInTrans )
{
#if defined(__cplusplus)
    static ADSINTRANSACTION_PTR
#else
    static ADSINTRANSACTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSINTRANSACTION_PTR) Ace32_GetProcAddress( "AdsInTransaction" );

    return pFunc( hConnect, pbInTrans );
}

UNSIGNED32 ENTRYPOINT AdsIsEmpty(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbEmpty )
{
#if defined(__cplusplus)
    static ADSISEMPTY_PTR
#else
    static ADSISEMPTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISEMPTY_PTR) Ace32_GetProcAddress( "AdsIsEmpty" );

    return pFunc( hTable, pucFldName, pbEmpty );
}

UNSIGNED32 ENTRYPOINT AdsIsExprValid(ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 *pbValid )
{
#if defined(__cplusplus)
    static ADSISEXPRVALID_PTR
#else
    static ADSISEXPRVALID_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISEXPRVALID_PTR) Ace32_GetProcAddress( "AdsIsExprValid" );

    return pFunc( hTable, pucExpr, pbValid );
}

UNSIGNED32 ENTRYPOINT AdsIsFound(ADSHANDLE hObj, UNSIGNED16 *pbFound )
{
#if defined(__cplusplus)
    static ADSISFOUND_PTR
#else
    static ADSISFOUND_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISFOUND_PTR) Ace32_GetProcAddress( "AdsIsFound" );

    return pFunc( hObj, pbFound );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexCompound(ADSHANDLE hIndex, UNSIGNED16 *pbCompound )
{
#if defined(__cplusplus)
    static ADSISINDEXCOMPOUND_PTR
#else
    static ADSISINDEXCOMPOUND_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXCOMPOUND_PTR) Ace32_GetProcAddress( "AdsIsIndexCompound" );

    return pFunc( hIndex, pbCompound );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexCandidate(ADSHANDLE hIndex, UNSIGNED16 *pbCandidate )
{
#if defined(__cplusplus)
    static ADSISINDEXCANDIDATE_PTR
#else
    static ADSISINDEXCANDIDATE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXCANDIDATE_PTR) Ace32_GetProcAddress( "AdsIsIndexCandidate" );

    return pFunc( hIndex, pbCandidate );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexNullable(ADSHANDLE hIndex, UNSIGNED16 *pbNullable )
{
#if defined(__cplusplus)
    static ADSISINDEXNULLABLE_PTR
#else
    static ADSISINDEXNULLABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXNULLABLE_PTR) Ace32_GetProcAddress( "AdsIsIndexNullable" );

    return pFunc( hIndex, pbNullable );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexCustom(ADSHANDLE hIndex, UNSIGNED16 *pbCustom )
{
#if defined(__cplusplus)
    static ADSISINDEXCUSTOM_PTR
#else
    static ADSISINDEXCUSTOM_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXCUSTOM_PTR) Ace32_GetProcAddress( "AdsIsIndexCustom" );

    return pFunc( hIndex, pbCustom );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexDescending(ADSHANDLE hIndex, UNSIGNED16 *pbDescending )
{
#if defined(__cplusplus)
    static ADSISINDEXDESCENDING_PTR
#else
    static ADSISINDEXDESCENDING_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXDESCENDING_PTR) Ace32_GetProcAddress( "AdsIsIndexDescending" );

    return pFunc( hIndex, pbDescending );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexPrimaryKey(ADSHANDLE hIndex, UNSIGNED16 *pbPrimaryKey )
{
#if defined(__cplusplus)
    static ADSISINDEXPRIMARYKEY_PTR
#else
    static ADSISINDEXPRIMARYKEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXPRIMARYKEY_PTR) Ace32_GetProcAddress( "AdsIsIndexPrimaryKey" );

    return pFunc( hIndex, pbPrimaryKey );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexFTS(ADSHANDLE hIndex, UNSIGNED16 *pbFTS )
{
#if defined(__cplusplus)
    static ADSISINDEXFTS_PTR
#else
    static ADSISINDEXFTS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXFTS_PTR) Ace32_GetProcAddress( "AdsIsIndexFTS" );

    return pFunc( hIndex, pbFTS );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexUnique(ADSHANDLE hIndex, UNSIGNED16 *pbUnique )
{
#if defined(__cplusplus)
    static ADSISINDEXUNIQUE_PTR
#else
    static ADSISINDEXUNIQUE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXUNIQUE_PTR) Ace32_GetProcAddress( "AdsIsIndexUnique" );

    return pFunc( hIndex, pbUnique );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordDeleted(ADSHANDLE hTable, UNSIGNED16 *pbDeleted )
{
#if defined(__cplusplus)
    static ADSISRECORDDELETED_PTR
#else
    static ADSISRECORDDELETED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISRECORDDELETED_PTR) Ace32_GetProcAddress( "AdsIsRecordDeleted" );

    return pFunc( hTable, pbDeleted );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordEncrypted(ADSHANDLE hTable, UNSIGNED16 *pbEncrypted )
{
#if defined(__cplusplus)
    static ADSISRECORDENCRYPTED_PTR
#else
    static ADSISRECORDENCRYPTED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISRECORDENCRYPTED_PTR) Ace32_GetProcAddress( "AdsIsRecordEncrypted" );

    return pFunc( hTable, pbEncrypted );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordLocked(ADSHANDLE hTable, UNSIGNED32 ulRec, UNSIGNED16 *pbLocked )
{
#if defined(__cplusplus)
    static ADSISRECORDLOCKED_PTR
#else
    static ADSISRECORDLOCKED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISRECORDLOCKED_PTR) Ace32_GetProcAddress( "AdsIsRecordLocked" );

    return pFunc( hTable, ulRec, pbLocked );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordVisible(ADSHANDLE hObj, UNSIGNED16 *pbVisible )
{
#if defined(__cplusplus)
    static ADSISRECORDVISIBLE_PTR
#else
    static ADSISRECORDVISIBLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISRECORDVISIBLE_PTR) Ace32_GetProcAddress( "AdsIsRecordVisible" );

    return pFunc( hObj, pbVisible );
}

UNSIGNED32 ENTRYPOINT AdsIsServerLoaded(UNSIGNED8 *pucServer, UNSIGNED16 *pbLoaded )
{
#if defined(__cplusplus)
    static ADSISSERVERLOADED_PTR
#else
    static ADSISSERVERLOADED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISSERVERLOADED_PTR) Ace32_GetProcAddress( "AdsIsServerLoaded" );

    return pFunc( pucServer, pbLoaded );
}

UNSIGNED32 ENTRYPOINT AdsIsTableEncrypted(ADSHANDLE hTable, UNSIGNED16 *pbEncrypted )
{
#if defined(__cplusplus)
    static ADSISTABLEENCRYPTED_PTR
#else
    static ADSISTABLEENCRYPTED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISTABLEENCRYPTED_PTR) Ace32_GetProcAddress( "AdsIsTableEncrypted" );

    return pFunc( hTable, pbEncrypted );
}

UNSIGNED32 ENTRYPOINT AdsIsTableLocked(ADSHANDLE hTable, UNSIGNED16 *pbLocked )
{
#if defined(__cplusplus)
    static ADSISTABLELOCKED_PTR
#else
    static ADSISTABLELOCKED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISTABLELOCKED_PTR) Ace32_GetProcAddress( "AdsIsTableLocked" );

    return pFunc( hTable, pbLocked );
}

UNSIGNED32 ENTRYPOINT AdsLocate(ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 bForward, UNSIGNED16 *pbFound )
{
#if defined(__cplusplus)
    static ADSLOCATE_PTR
#else
    static ADSLOCATE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSLOCATE_PTR) Ace32_GetProcAddress( "AdsLocate" );

    return pFunc( hTable, pucExpr, bForward, pbFound );
}

UNSIGNED32 ENTRYPOINT AdsLockRecord(ADSHANDLE hTable, UNSIGNED32 ulRec )
{
#if defined(__cplusplus)
    static ADSLOCKRECORD_PTR
#else
    static ADSLOCKRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSLOCKRECORD_PTR) Ace32_GetProcAddress( "AdsLockRecord" );

    return pFunc( hTable, ulRec );
}

UNSIGNED32 ENTRYPOINT AdsLockTable( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSLOCKTABLE_PTR
#else
    static ADSLOCKTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSLOCKTABLE_PTR) Ace32_GetProcAddress( "AdsLockTable" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsLookupKey( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 usKeyLen, UNSIGNED16 usDataType, UNSIGNED16 *pbFound )
{
#if defined(__cplusplus)
    static ADSLOOKUPKEY_PTR
#else
    static ADSLOOKUPKEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSLOOKUPKEY_PTR) Ace32_GetProcAddress( "AdsLookupKey" );

    return pFunc( hIndex, pucKey, usKeyLen, usDataType, pbFound );
}

UNSIGNED32 ENTRYPOINT AdsMgConnect( UNSIGNED8 *pucServerName, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, ADSHANDLE *phMgmtHandle )
{
#if defined(__cplusplus)
    static ADSMGCONNECT_PTR
#else
    static ADSMGCONNECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGCONNECT_PTR) Ace32_GetProcAddress( "AdsMgConnect" );

    return pFunc( pucServerName, pucUserName, pucPassword, phMgmtHandle );
}

UNSIGNED32 ENTRYPOINT AdsMgDisconnect( ADSHANDLE hMgmtHandle )
{
#if defined(__cplusplus)
    static ADSMGDISCONNECT_PTR
#else
    static ADSMGDISCONNECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGDISCONNECT_PTR) Ace32_GetProcAddress( "AdsMgDisconnect" );

    return pFunc( hMgmtHandle );
}

UNSIGNED32 ENTRYPOINT AdsMgGetCommStats( ADSHANDLE hMgmtHandle, ADS_MGMT_COMM_STATS *pstCommStats, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETCOMMSTATS_PTR
#else
    static ADSMGGETCOMMSTATS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETCOMMSTATS_PTR) Ace32_GetProcAddress( "AdsMgGetCommStats" );

    return pFunc( hMgmtHandle, pstCommStats, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgResetCommStats( ADSHANDLE hMgmtHandle )
{
#if defined(__cplusplus)
    static ADSMGRESETCOMMSTATS_PTR
#else
    static ADSMGRESETCOMMSTATS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGRESETCOMMSTATS_PTR) Ace32_GetProcAddress( "AdsMgResetCommStats" );

    return pFunc( hMgmtHandle );
}

UNSIGNED32 ENTRYPOINT AdsMgDumpInternalTables( ADSHANDLE hMgmtHandle )
{
#if defined(__cplusplus)
    static ADSMGDUMPINTERNALTABLES_PTR
#else
    static ADSMGDUMPINTERNALTABLES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGDUMPINTERNALTABLES_PTR) Ace32_GetProcAddress( "AdsMgDumpInternalTables" );

    return pFunc( hMgmtHandle );
}

UNSIGNED32 ENTRYPOINT AdsMgGetConfigInfo(ADSHANDLE hMgmtHandle, ADS_MGMT_CONFIG_PARAMS *pstConfigValues, UNSIGNED16 *pusConfigValuesStructSize, ADS_MGMT_CONFIG_MEMORY *pstConfigMemory, UNSIGNED16 *pusConfigMemoryStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETCONFIGINFO_PTR
#else
    static ADSMGGETCONFIGINFO_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETCONFIGINFO_PTR) Ace32_GetProcAddress( "AdsMgGetConfigInfo" );

    return pFunc( hMgmtHandle, pstConfigValues, pusConfigValuesStructSize, pstConfigMemory, pusConfigMemoryStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetInstallInfo(ADSHANDLE hMgmtHandle, ADS_MGMT_INSTALL_INFO *pstInstallInfo, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETINSTALLINFO_PTR
#else
    static ADSMGGETINSTALLINFO_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETINSTALLINFO_PTR) Ace32_GetProcAddress( "AdsMgGetInstallInfo" );

    return pFunc( hMgmtHandle, pstInstallInfo, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetActivityInfo(ADSHANDLE hMgmtHandle, ADS_MGMT_ACTIVITY_INFO *pstActivityInfo, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETACTIVITYINFO_PTR
#else
    static ADSMGGETACTIVITYINFO_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETACTIVITYINFO_PTR) Ace32_GetProcAddress( "AdsMgGetActivityInfo" );

    return pFunc( hMgmtHandle, pstActivityInfo, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetUserNames( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucFileName, ADS_MGMT_USER_INFO astUserInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETUSERNAMES_PTR
#else
    static ADSMGGETUSERNAMES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETUSERNAMES_PTR) Ace32_GetProcAddress( "AdsMgGetUserNames" );

    return pFunc( hMgmtHandle, pucFileName, astUserInfo, pusArrayLen, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetOpenTables(ADSHANDLE hMgmtHandle, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber, ADS_MGMT_TABLE_INFO astOpenTableInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETOPENTABLES_PTR
#else
    static ADSMGGETOPENTABLES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETOPENTABLES_PTR) Ace32_GetProcAddress( "AdsMgGetOpenTables" );

    return pFunc( hMgmtHandle, pucUserName, usConnNumber, astOpenTableInfo, pusArrayLen, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetOpenIndexes(ADSHANDLE hMgmtHandle, UNSIGNED8 *pucTableName, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber, ADS_MGMT_INDEX_INFO astOpenIndexInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETOPENINDEXES_PTR
#else
    static ADSMGGETOPENINDEXES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETOPENINDEXES_PTR) Ace32_GetProcAddress( "AdsMgGetOpenIndexes" );

    return pFunc( hMgmtHandle, pucTableName, pucUserName, usConnNumber, astOpenIndexInfo, pusArrayLen, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetLocks( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucTableName, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber, ADS_MGMT_RECORD_INFO astRecordInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETLOCKS_PTR
#else
    static ADSMGGETLOCKS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETLOCKS_PTR) Ace32_GetProcAddress( "AdsMgGetLocks" );

    return pFunc( hMgmtHandle, pucTableName, pucUserName, usConnNumber, astRecordInfo, pusArrayLen, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetServerType( ADSHANDLE hMgmtHandle, UNSIGNED16 *pusServerType )
{
#if defined(__cplusplus)
    static ADSMGGETSERVERTYPE_PTR
#else
    static ADSMGGETSERVERTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETSERVERTYPE_PTR) Ace32_GetProcAddress( "AdsMgGetServerType" );

    return pFunc( hMgmtHandle, pusServerType );
}

UNSIGNED32 ENTRYPOINT AdsMgKillUser( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber )
{
#if defined(__cplusplus)
    static ADSMGKILLUSER_PTR
#else
    static ADSMGKILLUSER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGKILLUSER_PTR) Ace32_GetProcAddress( "AdsMgKillUser" );

    return pFunc( hMgmtHandle, pucUserName, usConnNumber );
}

UNSIGNED32 ENTRYPOINT AdsMgGetWorkerThreadActivity(ADSHANDLE hMgmtHandle, ADS_MGMT_THREAD_ACTIVITY astWorkerThreadActivity[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
#if defined(__cplusplus)
    static ADSMGGETWORKERTHREADACTIVITY_PTR
#else
    static ADSMGGETWORKERTHREADACTIVITY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETWORKERTHREADACTIVITY_PTR) Ace32_GetProcAddress( "AdsMgGetWorkerThreadActivity" );

    return pFunc( hMgmtHandle, astWorkerThreadActivity, pusArrayLen, pusStructSize );
}

UNSIGNED32 ENTRYPOINT AdsMgGetLockOwner( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucTableName, UNSIGNED32 ulRecordNumber, ADS_MGMT_USER_INFO *pstUserInfo, UNSIGNED16 *pusStructSize, UNSIGNED16 *pusLockType )
{
#if defined(__cplusplus)
    static ADSMGGETLOCKOWNER_PTR
#else
    static ADSMGGETLOCKOWNER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGGETLOCKOWNER_PTR) Ace32_GetProcAddress( "AdsMgGetLockOwner" );

    return pFunc( hMgmtHandle, pucTableName, ulRecordNumber, pstUserInfo, pusStructSize, pusLockType );
}

UNSIGNED32 ENTRYPOINT AdsNullTerminateStrings( UNSIGNED16 bNullTerminate )
{
#if defined(__cplusplus)
    static ADSNULLTERMINATESTRINGS_PTR
#else
    static ADSNULLTERMINATESTRINGS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSNULLTERMINATESTRINGS_PTR) Ace32_GetProcAddress( "AdsNullTerminateStrings" );

    return pFunc( bNullTerminate );
}

UNSIGNED32 ENTRYPOINT AdsOpenIndex(ADSHANDLE hTable, UNSIGNED8 *pucName, ADSHANDLE ahIndex[], UNSIGNED16 *pusArrayLen )
{
#if defined(__cplusplus)
    static ADSOPENINDEX_PTR
#else
    static ADSOPENINDEX_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSOPENINDEX_PTR) Ace32_GetProcAddress( "AdsOpenIndex" );

    return pFunc( hTable, pucName, ahIndex, pusArrayLen );
}

UNSIGNED32 ENTRYPOINT AdsOpenTable(ADSHANDLE hConnect, UNSIGNED8 *pucName, UNSIGNED8 *pucAlias, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED32 ulOptions, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSOPENTABLE_PTR
#else
    static ADSOPENTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSOPENTABLE_PTR) Ace32_GetProcAddress( "AdsOpenTable" );

    return pFunc( hConnect, pucName, pucAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, phTable );
}

UNSIGNED32 ENTRYPOINT AdsOpenTable90(ADSHANDLE hConnect, UNSIGNED8 *pucName, UNSIGNED8 *pucAlias, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED32 ulOptions, UNSIGNED8 *pucCollation, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSOPENTABLE90_PTR
#else
    static ADSOPENTABLE90_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSOPENTABLE90_PTR) Ace32_GetProcAddress( "AdsOpenTable90" );

    return pFunc( hConnect, pucName, pucAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, pucCollation, phTable );
}

UNSIGNED32 ENTRYPOINT AdsPackTable( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSPACKTABLE_PTR
#else
    static ADSPACKTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSPACKTABLE_PTR) Ace32_GetProcAddress( "AdsPackTable" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsRecallRecord( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSRECALLRECORD_PTR
#else
    static ADSRECALLRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSRECALLRECORD_PTR) Ace32_GetProcAddress( "AdsRecallRecord" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsRecallAllRecords( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSRECALLALLRECORDS_PTR
#else
    static ADSRECALLALLRECORDS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSRECALLALLRECORDS_PTR) Ace32_GetProcAddress( "AdsRecallAllRecords" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsRefreshRecord( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSREFRESHRECORD_PTR
#else
    static ADSREFRESHRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREFRESHRECORD_PTR) Ace32_GetProcAddress( "AdsRefreshRecord" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsClearProgressCallback( void )
{
#if defined(__cplusplus)
    static ADSCLEARPROGRESSCALLBACK_PTR
#else
    static ADSCLEARPROGRESSCALLBACK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARPROGRESSCALLBACK_PTR) Ace32_GetProcAddress( "AdsClearProgressCallback" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsRegisterProgressCallback(UNSIGNED32 (WINAPI *lpfnCallback)( UNSIGNED16 usPercent ) )
{
#if defined(__cplusplus)
    static ADSREGISTERPROGRESSCALLBACK_PTR
#else
    static ADSREGISTERPROGRESSCALLBACK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREGISTERPROGRESSCALLBACK_PTR) Ace32_GetProcAddress( "AdsRegisterProgressCallback" );

    return pFunc( lpfnCallback );
}

UNSIGNED32 ENTRYPOINT AdsRegisterCallbackFunction(UNSIGNED32 (WINAPI *lpfnCallback)( UNSIGNED16 usPercent,  UNSIGNED32 ulCallbackID ), UNSIGNED32 ulCallbackID )
{
#if defined(__cplusplus)
    static ADSREGISTERCALLBACKFUNCTION_PTR
#else
    static ADSREGISTERCALLBACKFUNCTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREGISTERCALLBACKFUNCTION_PTR) Ace32_GetProcAddress( "AdsRegisterCallbackFunction" );

    return pFunc( lpfnCallback, ulCallbackID );
}

UNSIGNED32 ENTRYPOINT AdsClearCallbackFunction( void )
{
#if defined(__cplusplus)
    static ADSCLEARCALLBACKFUNCTION_PTR
#else
    static ADSCLEARCALLBACKFUNCTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARCALLBACKFUNCTION_PTR) Ace32_GetProcAddress( "AdsClearCallbackFunction" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsSetSQLTimeout( ADSHANDLE hObj, UNSIGNED32 ulTimeout )
{
#if defined(__cplusplus)
    static ADSSETSQLTIMEOUT_PTR
#else
    static ADSSETSQLTIMEOUT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSQLTIMEOUT_PTR) Ace32_GetProcAddress( "AdsSetSQLTimeout" );

    return pFunc( hObj, ulTimeout );
}

UNSIGNED32 ENTRYPOINT AdsReindex( ADSHANDLE hObject )
{
#if defined(__cplusplus)
    static ADSREINDEX_PTR
#else
    static ADSREINDEX_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREINDEX_PTR) Ace32_GetProcAddress( "AdsReindex" );

    return pFunc( hObject );
}

UNSIGNED32 ENTRYPOINT AdsReindex61( ADSHANDLE hObject, UNSIGNED32 ulPageSize )
{
#if defined(__cplusplus)
    static ADSREINDEX61_PTR
#else
    static ADSREINDEX61_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREINDEX61_PTR) Ace32_GetProcAddress( "AdsReindex61" );

    return pFunc( hObject, ulPageSize );
}

UNSIGNED32 ENTRYPOINT AdsReindexFTS( ADSHANDLE hObject, UNSIGNED32 ulPageSize )
{
#if defined(__cplusplus)
    static ADSREINDEXFTS_PTR
#else
    static ADSREINDEXFTS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREINDEXFTS_PTR) Ace32_GetProcAddress( "AdsReindexFTS" );

    return pFunc( hObject, ulPageSize );
}

UNSIGNED32 ENTRYPOINT AdsResetConnection( ADSHANDLE hConnect )
{
#if defined(__cplusplus)
    static ADSRESETCONNECTION_PTR
#else
    static ADSRESETCONNECTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSRESETCONNECTION_PTR) Ace32_GetProcAddress( "AdsResetConnection" );

    return pFunc( hConnect );
}

UNSIGNED32 ENTRYPOINT AdsRollbackTransaction( ADSHANDLE hConnect )
{
#if defined(__cplusplus)
    static ADSROLLBACKTRANSACTION_PTR
#else
    static ADSROLLBACKTRANSACTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSROLLBACKTRANSACTION_PTR) Ace32_GetProcAddress( "AdsRollbackTransaction" );

    return pFunc( hConnect );
}

UNSIGNED32 ENTRYPOINT AdsSeek(ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 usKeyLen, UNSIGNED16 usDataType, UNSIGNED16 usSeekType, UNSIGNED16 *pbFound )
{
#if defined(__cplusplus)
    static ADSSEEK_PTR
#else
    static ADSSEEK_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSEEK_PTR) Ace32_GetProcAddress( "AdsSeek" );

    return pFunc( hIndex, pucKey, usKeyLen, usDataType, usSeekType, pbFound );
}

UNSIGNED32 ENTRYPOINT AdsSeekLast(ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 usKeyLen, UNSIGNED16 usDataType, UNSIGNED16 *pbFound )
{
#if defined(__cplusplus)
    static ADSSEEKLAST_PTR
#else
    static ADSSEEKLAST_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSEEKLAST_PTR) Ace32_GetProcAddress( "AdsSeekLast" );

    return pFunc( hIndex, pucKey, usKeyLen, usDataType, pbFound );
}

UNSIGNED32 ENTRYPOINT AdsSetBinary(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 usBinaryType, UNSIGNED32 ulTotalLength, UNSIGNED32 ulOffset, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
    static ADSSETBINARY_PTR
#else
    static ADSSETBINARY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETBINARY_PTR) Ace32_GetProcAddress( "AdsSetBinary" );

    return pFunc( hTable, pucFldName, usBinaryType, ulTotalLength, ulOffset, pucBuf, ulLen );
}

UNSIGNED32 ENTRYPOINT AdsSetCollationLang( UNSIGNED8 *pucLang )
{
#if defined(__cplusplus)
    static ADSSETCOLLATIONLANG_PTR
#else
    static ADSSETCOLLATIONLANG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETCOLLATIONLANG_PTR) Ace32_GetProcAddress( "AdsSetCollationLang" );

    return pFunc( pucLang );
}

UNSIGNED32 ENTRYPOINT AdsSetCollation(ADSHANDLE hConnect, UNSIGNED8 *pucCollation )
{
#if defined(__cplusplus)
    static ADSSETCOLLATION_PTR
#else
    static ADSSETCOLLATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETCOLLATION_PTR) Ace32_GetProcAddress( "AdsSetCollation" );

    return pFunc( hConnect, pucCollation );
}

UNSIGNED32 ENTRYPOINT AdsSetDate(ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucValue, UNSIGNED16 usLen )
{
#if defined(__cplusplus)
    static ADSSETDATE_PTR
#else
    static ADSSETDATE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETDATE_PTR) Ace32_GetProcAddress( "AdsSetDate" );

    return pFunc( hObj, pucFldName, pucValue, usLen );
}

UNSIGNED32 ENTRYPOINT AdsSetDateFormat( UNSIGNED8 *pucFormat )
{
#if defined(__cplusplus)
    static ADSSETDATEFORMAT_PTR
#else
    static ADSSETDATEFORMAT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETDATEFORMAT_PTR) Ace32_GetProcAddress( "AdsSetDateFormat" );

    return pFunc( pucFormat );
}

UNSIGNED32 ENTRYPOINT AdsSetDateFormat60( ADSHANDLE hConnect, UNSIGNED8 *pucFormat )
{
#if defined(__cplusplus)
    static ADSSETDATEFORMAT60_PTR
#else
    static ADSSETDATEFORMAT60_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETDATEFORMAT60_PTR) Ace32_GetProcAddress( "AdsSetDateFormat60" );

    return pFunc( hConnect, pucFormat );
}

UNSIGNED32 ENTRYPOINT AdsSetDecimals( UNSIGNED16 usDecimals )
{
#if defined(__cplusplus)
    static ADSSETDECIMALS_PTR
#else
    static ADSSETDECIMALS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETDECIMALS_PTR) Ace32_GetProcAddress( "AdsSetDecimals" );

    return pFunc( usDecimals );
}

UNSIGNED32 ENTRYPOINT AdsSetDefault( UNSIGNED8 *pucDefault )
{
#if defined(__cplusplus)
    static ADSSETDEFAULT_PTR
#else
    static ADSSETDEFAULT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETDEFAULT_PTR) Ace32_GetProcAddress( "AdsSetDefault" );

    return pFunc( pucDefault );
}

UNSIGNED32 ENTRYPOINT AdsShowDeleted( UNSIGNED16 bShowDeleted )
{
#if defined(__cplusplus)
    static ADSSHOWDELETED_PTR
#else
    static ADSSHOWDELETED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSHOWDELETED_PTR) Ace32_GetProcAddress( "AdsShowDeleted" );

    return pFunc( bShowDeleted );
}

UNSIGNED32 ENTRYPOINT AdsSetDouble(ADSHANDLE hObj, UNSIGNED8 *pucFldName, DOUBLE dValue )
{
#if defined(__cplusplus)
    static ADSSETDOUBLE_PTR
#else
    static ADSSETDOUBLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETDOUBLE_PTR) Ace32_GetProcAddress( "AdsSetDouble" );

    return pFunc( hObj, pucFldName, dValue );
}

UNSIGNED32 ENTRYPOINT AdsSetEmpty(ADSHANDLE hObj, UNSIGNED8 *pucFldName )
{
#if defined(__cplusplus)
    static ADSSETEMPTY_PTR
#else
    static ADSSETEMPTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETEMPTY_PTR) Ace32_GetProcAddress( "AdsSetEmpty" );

    return pFunc( hObj, pucFldName );
}

UNSIGNED32 ENTRYPOINT AdsSetEpoch( UNSIGNED16 usCentury )
{
#if defined(__cplusplus)
    static ADSSETEPOCH_PTR
#else
    static ADSSETEPOCH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETEPOCH_PTR) Ace32_GetProcAddress( "AdsSetEpoch" );

    return pFunc( usCentury );
}

UNSIGNED32 ENTRYPOINT AdsSetExact( UNSIGNED16 bExact )
{
#if defined(__cplusplus)
    static ADSSETEXACT_PTR
#else
    static ADSSETEXACT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETEXACT_PTR) Ace32_GetProcAddress( "AdsSetExact" );

    return pFunc( bExact );
}

UNSIGNED32 ENTRYPOINT AdsSetExact22(ADSHANDLE hObj, UNSIGNED16 bExact )
{
#if defined(__cplusplus)
    static ADSSETEXACT22_PTR
#else
    static ADSSETEXACT22_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETEXACT22_PTR) Ace32_GetProcAddress( "AdsSetExact22" );

    return pFunc( hObj, bExact );
}

UNSIGNED32 ENTRYPOINT AdsSetField(ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
    static ADSSETFIELD_PTR
#else
    static ADSSETFIELD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETFIELD_PTR) Ace32_GetProcAddress( "AdsSetField" );

    return pFunc( hObj, pucFldName, pucBuf, ulLen );
}

UNSIGNED32 ENTRYPOINT AdsSetFieldW( ADSHANDLE hObj, UNSIGNED8 *pucFldName, WCHAR *pwcBuf, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
    static ADSSETFIELDW_PTR
#else
    static ADSSETFIELDW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETFIELDW_PTR) Ace32_GetProcAddress( "AdsSetFieldW" );

    return pFunc( hObj, pucFldName, pwcBuf, ulLen );
}

UNSIGNED32 ENTRYPOINT AdsSetFilter(ADSHANDLE hTable, UNSIGNED8 *pucFilter )
{
#if defined(__cplusplus)
    static ADSSETFILTER_PTR
#else
    static ADSSETFILTER_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETFILTER_PTR) Ace32_GetProcAddress( "AdsSetFilter" );

    return pFunc( hTable, pucFilter );
}

UNSIGNED32 ENTRYPOINT AdsSetFilter100( ADSHANDLE hTable, VOID *pvFilter, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSSETFILTER100_PTR
#else
    static ADSSETFILTER100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETFILTER100_PTR) Ace32_GetProcAddress( "AdsSetFilter100" );

    return pFunc( hTable, pvFilter, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsSetHandleLong(ADSHANDLE hObj, UNSIGNED32 ulVal )
{
#if defined(__cplusplus)
    static ADSSETHANDLELONG_PTR
#else
    static ADSSETHANDLELONG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETHANDLELONG_PTR) Ace32_GetProcAddress( "AdsSetHandleLong" );

    return pFunc( hObj, ulVal );
}

UNSIGNED32 ENTRYPOINT AdsSetJulian(ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED32 lDate )
{
#if defined(__cplusplus)
    static ADSSETJULIAN_PTR
#else
    static ADSSETJULIAN_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETJULIAN_PTR) Ace32_GetProcAddress( "AdsSetJulian" );

    return pFunc( hObj, pucFldName, lDate );
}

UNSIGNED32 ENTRYPOINT AdsSetLogical(ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED16 bValue )
{
#if defined(__cplusplus)
    static ADSSETLOGICAL_PTR
#else
    static ADSSETLOGICAL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETLOGICAL_PTR) Ace32_GetProcAddress( "AdsSetLogical" );

    return pFunc( hObj, pucFldName, bValue );
}

UNSIGNED32 ENTRYPOINT AdsSetLong(ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED32 lValue )
{
#if defined(__cplusplus)
    static ADSSETLONG_PTR
#else
    static ADSSETLONG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETLONG_PTR) Ace32_GetProcAddress( "AdsSetLong" );

    return pFunc( hObj, pucFldName, lValue );
}

UNSIGNED32 ENTRYPOINT AdsSetLongLong(ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED64 qValue )
{
#if defined(__cplusplus)
    static ADSSETLONGLONG_PTR
#else
    static ADSSETLONGLONG_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETLONGLONG_PTR) Ace32_GetProcAddress( "AdsSetLongLong" );

    return pFunc( hObj, pucFldName, qValue );
}

UNSIGNED32 ENTRYPOINT AdsSetMilliseconds(ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED32 lTime )
{
#if defined(__cplusplus)
    static ADSSETMILLISECONDS_PTR
#else
    static ADSSETMILLISECONDS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETMILLISECONDS_PTR) Ace32_GetProcAddress( "AdsSetMilliseconds" );

    return pFunc( hObj, pucFldName, lTime );
}

UNSIGNED32 ENTRYPOINT AdsSetMoney(ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED64 qValue )
{
#if defined(__cplusplus)
    static ADSSETMONEY_PTR
#else
    static ADSSETMONEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETMONEY_PTR) Ace32_GetProcAddress( "AdsSetMoney" );

    return pFunc( hObj, pucFldName, qValue );
}

UNSIGNED32 ENTRYPOINT AdsSetRecord(ADSHANDLE hObj, UNSIGNED8 *pucRec, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
    static ADSSETRECORD_PTR
#else
    static ADSSETRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETRECORD_PTR) Ace32_GetProcAddress( "AdsSetRecord" );

    return pFunc( hObj, pucRec, ulLen );
}

UNSIGNED32 ENTRYPOINT AdsSetRelation(ADSHANDLE hTableParent, ADSHANDLE hIndexChild, UNSIGNED8 *pucExpr )
{
#if defined(__cplusplus)
    static ADSSETRELATION_PTR
#else
    static ADSSETRELATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETRELATION_PTR) Ace32_GetProcAddress( "AdsSetRelation" );

    return pFunc( hTableParent, hIndexChild, pucExpr );
}

UNSIGNED32 ENTRYPOINT AdsSetRelKeyPos(ADSHANDLE hIndex, DOUBLE dPos )
{
#if defined(__cplusplus)
    static ADSSETRELKEYPOS_PTR
#else
    static ADSSETRELKEYPOS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETRELKEYPOS_PTR) Ace32_GetProcAddress( "AdsSetRelKeyPos" );

    return pFunc( hIndex, dPos );
}

UNSIGNED32 ENTRYPOINT AdsSetScope(ADSHANDLE hIndex, UNSIGNED16 usScopeOption, UNSIGNED8 *pucScope, UNSIGNED16 usScopeLen, UNSIGNED16 usDataType )
{
#if defined(__cplusplus)
    static ADSSETSCOPE_PTR
#else
    static ADSSETSCOPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSCOPE_PTR) Ace32_GetProcAddress( "AdsSetScope" );

    return pFunc( hIndex, usScopeOption, pucScope, usScopeLen, usDataType );
}

UNSIGNED32 ENTRYPOINT AdsSetScopedRelation(ADSHANDLE hTableParent, ADSHANDLE hIndexChild, UNSIGNED8 *pucExpr )
{
#if defined(__cplusplus)
    static ADSSETSCOPEDRELATION_PTR
#else
    static ADSSETSCOPEDRELATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSCOPEDRELATION_PTR) Ace32_GetProcAddress( "AdsSetScopedRelation" );

    return pFunc( hTableParent, hIndexChild, pucExpr );
}

UNSIGNED32 ENTRYPOINT AdsSetSearchPath( UNSIGNED8 *pucPath )
{
#if defined(__cplusplus)
    static ADSSETSEARCHPATH_PTR
#else
    static ADSSETSEARCHPATH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSEARCHPATH_PTR) Ace32_GetProcAddress( "AdsSetSearchPath" );

    return pFunc( pucPath );
}

UNSIGNED32 ENTRYPOINT AdsSetServerType( UNSIGNED16 usServerOptions )
{
#if defined(__cplusplus)
    static ADSSETSERVERTYPE_PTR
#else
    static ADSSETSERVERTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSERVERTYPE_PTR) Ace32_GetProcAddress( "AdsSetServerType" );

    return pFunc( usServerOptions );
}

UNSIGNED32 ENTRYPOINT AdsSetShort(ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED16 sValue )
{
#if defined(__cplusplus)
    static ADSSETSHORT_PTR
#else
    static ADSSETSHORT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSHORT_PTR) Ace32_GetProcAddress( "AdsSetShort" );

    return pFunc( hObj, pucFldName, sValue );
}

UNSIGNED32 ENTRYPOINT AdsSetString( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
    static ADSSETSTRING_PTR
#else
    static ADSSETSTRING_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSTRING_PTR) Ace32_GetProcAddress( "AdsSetString" );

    return pFunc( hObj, pucFldName, pucBuf, ulLen );
}

UNSIGNED32 ENTRYPOINT AdsSetStringW( ADSHANDLE hObj, UNSIGNED8 *pucFldName, WCHAR *pwcBuf, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
    static ADSSETSTRINGW_PTR
#else
    static ADSSETSTRINGW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETSTRINGW_PTR) Ace32_GetProcAddress( "AdsSetStringW" );

    return pFunc( hObj, pucFldName, pwcBuf, ulLen );
}

UNSIGNED32 ENTRYPOINT AdsSetTime( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucValue, UNSIGNED16 usLen )
{
#if defined(__cplusplus)
    static ADSSETTIME_PTR
#else
    static ADSSETTIME_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETTIME_PTR) Ace32_GetProcAddress( "AdsSetTime" );

    return pFunc( hObj, pucFldName, pucValue, usLen );
}

UNSIGNED32 ENTRYPOINT AdsShowError( UNSIGNED8 *pucTitle )
{
#if defined(__cplusplus)
    static ADSSHOWERROR_PTR
#else
    static ADSSHOWERROR_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSHOWERROR_PTR) Ace32_GetProcAddress( "AdsShowError" );

    return pFunc( pucTitle );
}

UNSIGNED32 ENTRYPOINT AdsSkip( ADSHANDLE hObj, SIGNED32 lRecs )
{
#if defined(__cplusplus)
    static ADSSKIP_PTR
#else
    static ADSSKIP_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSKIP_PTR) Ace32_GetProcAddress( "AdsSkip" );

    return pFunc( hObj, lRecs );
}

UNSIGNED32 ENTRYPOINT AdsSkipUnique( ADSHANDLE hIndex, SIGNED32 lRecs )
{
#if defined(__cplusplus)
    static ADSSKIPUNIQUE_PTR
#else
    static ADSSKIPUNIQUE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSKIPUNIQUE_PTR) Ace32_GetProcAddress( "AdsSkipUnique" );

    return pFunc( hIndex, lRecs );
}

UNSIGNED32 ENTRYPOINT AdsThreadExit( void )
{
#if defined(__cplusplus)
    static ADSTHREADEXIT_PTR
#else
    static ADSTHREADEXIT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSTHREADEXIT_PTR) Ace32_GetProcAddress( "AdsThreadExit" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsUnlockRecord( ADSHANDLE hTable, UNSIGNED32 ulRec )
{
#if defined(__cplusplus)
    static ADSUNLOCKRECORD_PTR
#else
    static ADSUNLOCKRECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSUNLOCKRECORD_PTR) Ace32_GetProcAddress( "AdsUnlockRecord" );

    return pFunc( hTable, ulRec );
}

UNSIGNED32 ENTRYPOINT AdsUnlockTable( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSUNLOCKTABLE_PTR
#else
    static ADSUNLOCKTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSUNLOCKTABLE_PTR) Ace32_GetProcAddress( "AdsUnlockTable" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsVerifyPassword( ADSHANDLE hTable, UNSIGNED16 *pusEnabled )
{
#if defined(__cplusplus)
    static ADSVERIFYPASSWORD_PTR
#else
    static ADSVERIFYPASSWORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSVERIFYPASSWORD_PTR) Ace32_GetProcAddress( "AdsVerifyPassword" );

    return pFunc( hTable, pusEnabled );
}

UNSIGNED32 ENTRYPOINT AdsIsEncryptionEnabled( ADSHANDLE hTable, UNSIGNED16 *pusEnabled )
{
#if defined(__cplusplus)
    static ADSISENCRYPTIONENABLED_PTR
#else
    static ADSISENCRYPTIONENABLED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISENCRYPTIONENABLED_PTR) Ace32_GetProcAddress( "AdsIsEncryptionEnabled" );

    return pFunc( hTable, pusEnabled );
}

UNSIGNED32 ENTRYPOINT AdsWriteAllRecords( void )
{
#if defined(__cplusplus)
    static ADSWRITEALLRECORDS_PTR
#else
    static ADSWRITEALLRECORDS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSWRITEALLRECORDS_PTR) Ace32_GetProcAddress( "AdsWriteAllRecords" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsWriteRecord( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSWRITERECORD_PTR
#else
    static ADSWRITERECORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSWRITERECORD_PTR) Ace32_GetProcAddress( "AdsWriteRecord" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsZapTable( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSZAPTABLE_PTR
#else
    static ADSZAPTABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSZAPTABLE_PTR) Ace32_GetProcAddress( "AdsZapTable" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsSetAOF( ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 usOptions )
{
#if defined(__cplusplus)
    static ADSSETAOF_PTR
#else
    static ADSSETAOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETAOF_PTR) Ace32_GetProcAddress( "AdsSetAOF" );

    return pFunc( hTable, pucFilter, usOptions );
}

UNSIGNED32 ENTRYPOINT AdsSetAOF100( ADSHANDLE hTable, VOID *pvFilter, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSSETAOF100_PTR
#else
    static ADSSETAOF100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETAOF100_PTR) Ace32_GetProcAddress( "AdsSetAOF100" );

    return pFunc( hTable, pvFilter, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsEvalAOF( ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 *pusOptLevel )
{
#if defined(__cplusplus)
    static ADSEVALAOF_PTR
#else
    static ADSEVALAOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEVALAOF_PTR) Ace32_GetProcAddress( "AdsEvalAOF" );

    return pFunc( hTable, pucFilter, pusOptLevel );
}

UNSIGNED32 ENTRYPOINT AdsEvalAOF100( ADSHANDLE hTable, VOID *pvFilter, UNSIGNED32 ulOptions, UNSIGNED16 *pusOptLevel )
{
#if defined(__cplusplus)
    static ADSEVALAOF100_PTR
#else
    static ADSEVALAOF100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEVALAOF100_PTR) Ace32_GetProcAddress( "AdsEvalAOF100" );

    return pFunc( hTable, pvFilter, ulOptions, pusOptLevel );
}

UNSIGNED32 ENTRYPOINT AdsClearAOF( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSCLEARAOF_PTR
#else
    static ADSCLEARAOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARAOF_PTR) Ace32_GetProcAddress( "AdsClearAOF" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsRefreshAOF( ADSHANDLE hTable )
{
#if defined(__cplusplus)
    static ADSREFRESHAOF_PTR
#else
    static ADSREFRESHAOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREFRESHAOF_PTR) Ace32_GetProcAddress( "AdsRefreshAOF" );

    return pFunc( hTable );
}

UNSIGNED32 ENTRYPOINT AdsGetAOF( ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETAOF_PTR
#else
    static ADSGETAOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETAOF_PTR) Ace32_GetProcAddress( "AdsGetAOF" );

    return pFunc( hTable, pucFilter, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetAOF100( ADSHANDLE hTable, UNSIGNED32 ulOptions, VOID *pvFilter, UNSIGNED32 *pulLen )
{
#if defined(__cplusplus)
    static ADSGETAOF100_PTR
#else
    static ADSGETAOF100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETAOF100_PTR) Ace32_GetProcAddress( "AdsGetAOF100" );

    return pFunc( hTable, ulOptions, pvFilter, pulLen );
}

UNSIGNED32 ENTRYPOINT AdsGetAOFOptLevel( ADSHANDLE hTable, UNSIGNED16 *pusOptLevel, UNSIGNED8 *pucNonOpt, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETAOFOPTLEVEL_PTR
#else
    static ADSGETAOFOPTLEVEL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETAOFOPTLEVEL_PTR) Ace32_GetProcAddress( "AdsGetAOFOptLevel" );

    return pFunc( hTable, pusOptLevel, pucNonOpt, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetAOFOptLevel100( ADSHANDLE hTable, UNSIGNED16 *pusOptLevel, VOID *pvNonOpt, UNSIGNED32 *pulExprLen, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSGETAOFOPTLEVEL100_PTR
#else
    static ADSGETAOFOPTLEVEL100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETAOFOPTLEVEL100_PTR) Ace32_GetProcAddress( "AdsGetAOFOptLevel100" );

    return pFunc( hTable, pusOptLevel, pvNonOpt, pulExprLen, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordInAOF( ADSHANDLE hTable, UNSIGNED32 ulRecordNum, UNSIGNED16 *pusIsInAOF )
{
#if defined(__cplusplus)
    static ADSISRECORDINAOF_PTR
#else
    static ADSISRECORDINAOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISRECORDINAOF_PTR) Ace32_GetProcAddress( "AdsIsRecordInAOF" );

    return pFunc( hTable, ulRecordNum, pusIsInAOF );
}

UNSIGNED32 ENTRYPOINT AdsCustomizeAOF( ADSHANDLE hTable, UNSIGNED32 ulNumRecords, UNSIGNED32 *pulRecords, UNSIGNED16 usOption )
{
#if defined(__cplusplus)
    static ADSCUSTOMIZEAOF_PTR
#else
    static ADSCUSTOMIZEAOF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCUSTOMIZEAOF_PTR) Ace32_GetProcAddress( "AdsCustomizeAOF" );

    return pFunc( hTable, ulNumRecords, pulRecords, usOption );
}

UNSIGNED32 ENTRYPOINT AdsInitRawKey( ADSHANDLE hIndex )
{
#if defined(__cplusplus)
    static ADSINITRAWKEY_PTR
#else
    static ADSINITRAWKEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSINITRAWKEY_PTR) Ace32_GetProcAddress( "AdsInitRawKey" );

    return pFunc( hIndex );
}

UNSIGNED32 ENTRYPOINT AdsBuildRawKey( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 *pusKeyLen )
{
#if defined(__cplusplus)
    static ADSBUILDRAWKEY_PTR
#else
    static ADSBUILDRAWKEY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSBUILDRAWKEY_PTR) Ace32_GetProcAddress( "AdsBuildRawKey" );

    return pFunc( hIndex, pucKey, pusKeyLen );
}

UNSIGNED32 ENTRYPOINT AdsBuildRawKey100( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 *pusKeyLen, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSBUILDRAWKEY100_PTR
#else
    static ADSBUILDRAWKEY100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSBUILDRAWKEY100_PTR) Ace32_GetProcAddress( "AdsBuildRawKey100" );

    return pFunc( hIndex, pucKey, pusKeyLen, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsCreateSQLStatement( ADSHANDLE hConnect, ADSHANDLE *phStatement )
{
#if defined(__cplusplus)
    static ADSCREATESQLSTATEMENT_PTR
#else
    static ADSCREATESQLSTATEMENT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATESQLSTATEMENT_PTR) Ace32_GetProcAddress( "AdsCreateSQLStatement" );

    return pFunc( hConnect, phStatement );
}

UNSIGNED32 ENTRYPOINT AdsPrepareSQL( ADSHANDLE hStatement, UNSIGNED8 *pucSQL )
{
#if defined(__cplusplus)
    static ADSPREPARESQL_PTR
#else
    static ADSPREPARESQL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSPREPARESQL_PTR) Ace32_GetProcAddress( "AdsPrepareSQL" );

    return pFunc( hStatement, pucSQL );
}

UNSIGNED32 ENTRYPOINT AdsPrepareSQLW( ADSHANDLE hStatement, WCHAR *pwcSQL )
{
#if defined(__cplusplus)
    static ADSPREPARESQLW_PTR
#else
    static ADSPREPARESQLW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSPREPARESQLW_PTR) Ace32_GetProcAddress( "AdsPrepareSQLW" );

    return pFunc( hStatement, pwcSQL );
}

UNSIGNED32 ENTRYPOINT AdsExecuteSQL( ADSHANDLE hStatement, ADSHANDLE *phCursor )
{
#if defined(__cplusplus)
    static ADSEXECUTESQL_PTR
#else
    static ADSEXECUTESQL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEXECUTESQL_PTR) Ace32_GetProcAddress( "AdsExecuteSQL" );

    return pFunc( hStatement, phCursor );
}

UNSIGNED32 ENTRYPOINT AdsExecuteSQLDirect( ADSHANDLE hStatement, UNSIGNED8 *pucSQL, ADSHANDLE *phCursor )
{
#if defined(__cplusplus)
    static ADSEXECUTESQLDIRECT_PTR
#else
    static ADSEXECUTESQLDIRECT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEXECUTESQLDIRECT_PTR) Ace32_GetProcAddress( "AdsExecuteSQLDirect" );

    return pFunc( hStatement, pucSQL, phCursor );
}

UNSIGNED32 ENTRYPOINT AdsExecuteSQLDirectW( ADSHANDLE hStatement, WCHAR *pwcSQL, ADSHANDLE *phCursor )
{
#if defined(__cplusplus)
    static ADSEXECUTESQLDIRECTW_PTR
#else
    static ADSEXECUTESQLDIRECTW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSEXECUTESQLDIRECTW_PTR) Ace32_GetProcAddress( "AdsExecuteSQLDirectW" );

    return pFunc( hStatement, pwcSQL, phCursor );
}

UNSIGNED32 ENTRYPOINT AdsCloseSQLStatement( ADSHANDLE hStatement )
{
#if defined(__cplusplus)
    static ADSCLOSESQLSTATEMENT_PTR
#else
    static ADSCLOSESQLSTATEMENT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLOSESQLSTATEMENT_PTR) Ace32_GetProcAddress( "AdsCloseSQLStatement" );

    return pFunc( hStatement );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableRights( ADSHANDLE hStatement, UNSIGNED16 usCheckRights )
{
#if defined(__cplusplus)
    static ADSSTMTSETTABLERIGHTS_PTR
#else
    static ADSSTMTSETTABLERIGHTS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTSETTABLERIGHTS_PTR) Ace32_GetProcAddress( "AdsStmtSetTableRights" );

    return pFunc( hStatement, usCheckRights );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableReadOnly( ADSHANDLE hStatement, UNSIGNED16 usReadOnly )
{
#if defined(__cplusplus)
    static ADSSTMTSETTABLEREADONLY_PTR
#else
    static ADSSTMTSETTABLEREADONLY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTSETTABLEREADONLY_PTR) Ace32_GetProcAddress( "AdsStmtSetTableReadOnly" );

    return pFunc( hStatement, usReadOnly );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableLockType( ADSHANDLE hStatement, UNSIGNED16 usLockType )
{
#if defined(__cplusplus)
    static ADSSTMTSETTABLELOCKTYPE_PTR
#else
    static ADSSTMTSETTABLELOCKTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTSETTABLELOCKTYPE_PTR) Ace32_GetProcAddress( "AdsStmtSetTableLockType" );

    return pFunc( hStatement, usLockType );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableCharType( ADSHANDLE hStatement, UNSIGNED16 usCharType )
{
#if defined(__cplusplus)
    static ADSSTMTSETTABLECHARTYPE_PTR
#else
    static ADSSTMTSETTABLECHARTYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTSETTABLECHARTYPE_PTR) Ace32_GetProcAddress( "AdsStmtSetTableCharType" );

    return pFunc( hStatement, usCharType );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableType( ADSHANDLE hStatement, UNSIGNED16 usTableType )
{
#if defined(__cplusplus)
    static ADSSTMTSETTABLETYPE_PTR
#else
    static ADSSTMTSETTABLETYPE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTSETTABLETYPE_PTR) Ace32_GetProcAddress( "AdsStmtSetTableType" );

    return pFunc( hStatement, usTableType );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableCollation( ADSHANDLE hStatement, UNSIGNED8 *pucCollation )
{
#if defined(__cplusplus)
    static ADSSTMTSETTABLECOLLATION_PTR
#else
    static ADSSTMTSETTABLECOLLATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTSETTABLECOLLATION_PTR) Ace32_GetProcAddress( "AdsStmtSetTableCollation" );

    return pFunc( hStatement, pucCollation );
}

UNSIGNED32 ENTRYPOINT AdsStmtConstrainUpdates( ADSHANDLE hStatement, UNSIGNED16 usConstrain )
{
#if defined(__cplusplus)
    static ADSSTMTCONSTRAINUPDATES_PTR
#else
    static ADSSTMTCONSTRAINUPDATES_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTCONSTRAINUPDATES_PTR) Ace32_GetProcAddress( "AdsStmtConstrainUpdates" );

    return pFunc( hStatement, usConstrain );
}

UNSIGNED32 ENTRYPOINT AdsStmtEnableEncryption( ADSHANDLE hStatement, UNSIGNED8 *pucPassword )
{
#if defined(__cplusplus)
    static ADSSTMTENABLEENCRYPTION_PTR
#else
    static ADSSTMTENABLEENCRYPTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTENABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsStmtEnableEncryption" );

    return pFunc( hStatement, pucPassword );
}

UNSIGNED32 ENTRYPOINT AdsStmtDisableEncryption( ADSHANDLE hStatement )
{
#if defined(__cplusplus)
    static ADSSTMTDISABLEENCRYPTION_PTR
#else
    static ADSSTMTDISABLEENCRYPTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTDISABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsStmtDisableEncryption" );

    return pFunc( hStatement );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTablePassword( ADSHANDLE hStatement, UNSIGNED8 *pucTableName, UNSIGNED8 *pucPassword )
{
#if defined(__cplusplus)
    static ADSSTMTSETTABLEPASSWORD_PTR
#else
    static ADSSTMTSETTABLEPASSWORD_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTSETTABLEPASSWORD_PTR) Ace32_GetProcAddress( "AdsStmtSetTablePassword" );

    return pFunc( hStatement, pucTableName, pucPassword );
}

UNSIGNED32 ENTRYPOINT AdsStmtClearTablePasswords( ADSHANDLE hStatement )
{
#if defined(__cplusplus)
    static ADSSTMTCLEARTABLEPASSWORDS_PTR
#else
    static ADSSTMTCLEARTABLEPASSWORDS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTCLEARTABLEPASSWORDS_PTR) Ace32_GetProcAddress( "AdsStmtClearTablePasswords" );

    return pFunc( hStatement );
}

UNSIGNED32 ENTRYPOINT AdsStmtReadAllColumns( ADSHANDLE hStatement, UNSIGNED16 usReadColumns )
{
#if defined(__cplusplus)
    static ADSSTMTREADALLCOLUMNS_PTR
#else
    static ADSSTMTREADALLCOLUMNS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSTMTREADALLCOLUMNS_PTR) Ace32_GetProcAddress( "AdsStmtReadAllColumns" );

    return pFunc( hStatement, usReadColumns );
}

UNSIGNED32 ENTRYPOINT AdsClearSQLParams( ADSHANDLE hStatement )
{
#if defined(__cplusplus)
    static ADSCLEARSQLPARAMS_PTR
#else
    static ADSCLEARSQLPARAMS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARSQLPARAMS_PTR) Ace32_GetProcAddress( "AdsClearSQLParams" );

    return pFunc( hStatement );
}

UNSIGNED32 ENTRYPOINT AdsSetTimeStamp( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
#if defined(__cplusplus)
    static ADSSETTIMESTAMP_PTR
#else
    static ADSSETTIMESTAMP_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETTIMESTAMP_PTR) Ace32_GetProcAddress( "AdsSetTimeStamp" );

    return pFunc( hObj, pucFldName, pucBuf, ulLen );
}

UNSIGNED32 ENTRYPOINT AdsClearSQLAbortFunc( void )
{
#if defined(__cplusplus)
    static ADSCLEARSQLABORTFUNC_PTR
#else
    static ADSCLEARSQLABORTFUNC_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCLEARSQLABORTFUNC_PTR) Ace32_GetProcAddress( "AdsClearSQLAbortFunc" );

    return pFunc();
}

UNSIGNED32 ENTRYPOINT AdsRegisterSQLAbortFunc( UNSIGNED32 (WINAPI *lpfnCallback)(void) )
{
#if defined(__cplusplus)
    static ADSREGISTERSQLABORTFUNC_PTR
#else
    static ADSREGISTERSQLABORTFUNC_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREGISTERSQLABORTFUNC_PTR) Ace32_GetProcAddress( "AdsRegisterSQLAbortFunc" );

    return pFunc( lpfnCallback );
}

UNSIGNED32 ENTRYPOINT AdsRegisterUDF( ADSHANDLE hObj, UNSIGNED16 usType, UNSIGNED32 (WINAPI *lpfnUDF)(void) )
{
#if defined(__cplusplus)
    static ADSREGISTERUDF_PTR
#else
    static ADSREGISTERUDF_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSREGISTERUDF_PTR) Ace32_GetProcAddress( "AdsRegisterUDF" );

    return pFunc( hObj, usType, lpfnUDF );
}

UNSIGNED32 ENTRYPOINT AdsGetNumParams( ADSHANDLE hStatement,  UNSIGNED16 *pusNumParams )
{
#if defined(__cplusplus)
    static ADSGETNUMPARAMS_PTR
#else
    static ADSGETNUMPARAMS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETNUMPARAMS_PTR) Ace32_GetProcAddress( "AdsGetNumParams" );

    return pFunc( hStatement, pusNumParams );
}

UNSIGNED32 ENTRYPOINT AdsGetLastAutoinc( ADSHANDLE hObj,  UNSIGNED32 *pulAutoIncVal )
{
#if defined(__cplusplus)
    static ADSGETLASTAUTOINC_PTR
#else
    static ADSGETLASTAUTOINC_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETLASTAUTOINC_PTR) Ace32_GetProcAddress( "AdsGetLastAutoinc" );

    return pFunc( hObj, pulAutoIncVal );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexUserDefined( ADSHANDLE hIndex, UNSIGNED16 *pbUserDefined )
{
#if defined(__cplusplus)
    static ADSISINDEXUSERDEFINED_PTR
#else
    static ADSISINDEXUSERDEFINED_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISINDEXUSERDEFINED_PTR) Ace32_GetProcAddress( "AdsIsIndexUserDefined" );

    return pFunc( hIndex, pbUserDefined );
}

UNSIGNED32 ENTRYPOINT AdsRestructureTable( ADSHANDLE hObj, UNSIGNED8 *pucName, UNSIGNED8 *pucPassword, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED8 *pucAddFields, UNSIGNED8 *pucDeleteFields, UNSIGNED8 *pucChangeFields )
{
#if defined(__cplusplus)
    static ADSRESTRUCTURETABLE_PTR
#else
    static ADSRESTRUCTURETABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSRESTRUCTURETABLE_PTR) Ace32_GetProcAddress( "AdsRestructureTable" );

    return pFunc( hObj, pucName, pucPassword, usTableType, usCharType, usLockType, usCheckRights, pucAddFields, pucDeleteFields, pucChangeFields );
}

UNSIGNED32 ENTRYPOINT AdsRestructureTable90( ADSHANDLE hObj, UNSIGNED8 *pucName, UNSIGNED8 *pucPassword, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED8 *pucAddFields, UNSIGNED8 *pucDeleteFields, UNSIGNED8 *pucChangeFields, UNSIGNED8 *pucCollation )
{
#if defined(__cplusplus)
    static ADSRESTRUCTURETABLE90_PTR
#else
    static ADSRESTRUCTURETABLE90_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSRESTRUCTURETABLE90_PTR) Ace32_GetProcAddress( "AdsRestructureTable90" );

    return pFunc( hObj, pucName, pucPassword, usTableType, usCharType, usLockType, usCheckRights, pucAddFields, pucDeleteFields, pucChangeFields, pucCollation );
}

UNSIGNED32 ENTRYPOINT AdsGetSQLStatementHandle( ADSHANDLE hCursor, ADSHANDLE *phStmt )
{
#if defined(__cplusplus)
    static ADSGETSQLSTATEMENTHANDLE_PTR
#else
    static ADSGETSQLSTATEMENTHANDLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSQLSTATEMENTHANDLE_PTR) Ace32_GetProcAddress( "AdsGetSQLStatementHandle" );

    return pFunc( hCursor, phStmt );
}

UNSIGNED32 ENTRYPOINT AdsGetSQLStatement( ADSHANDLE hStmt, UNSIGNED8 *pucSQL, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETSQLSTATEMENT_PTR
#else
    static ADSGETSQLSTATEMENT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETSQLSTATEMENT_PTR) Ace32_GetProcAddress( "AdsGetSQLStatement" );

    return pFunc( hStmt, pucSQL, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsFlushFileBuffers( ADSHANDLE hTable )
{
    UNSIGNED32 u_ADS_LIB_VERSION = hb_AdsGetVersionN();
#if defined(__cplusplus)
    static ADSFLUSHFILEBUFFERS_PTR
       pFunc = ( u_ADS_LIB_VERSION > 600 ) ? (ADSFLUSHFILEBUFFERS_PTR) Ace32_GetProcAddress( "AdsFlushFileBuffers" ) : 0;
#else
    static ADSFLUSHFILEBUFFERS_PTR pFunc = NULL;

    if ( ( u_ADS_LIB_VERSION > 600 ) && !pFunc )
       pFunc = (ADSFLUSHFILEBUFFERS_PTR) Ace32_GetProcAddress( "AdsFlushFileBuffers" );
#endif

    return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeployDatabase( UNSIGNED8 *pucDestination, UNSIGNED8 *pucDestinationPassword, UNSIGNED8 *pucSource, UNSIGNED8 *pucSourcePassword, UNSIGNED16 usServerTypes, UNSIGNED16 usValidateOption, UNSIGNED16 usBackupFiles, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSDDDEPLOYDATABASE_PTR
#else
    static ADSDDDEPLOYDATABASE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDDEPLOYDATABASE_PTR) Ace32_GetProcAddress( "AdsDDDeployDatabase" );

    return pFunc( pucDestination, pucDestinationPassword, pucSource, pucSourcePassword, usServerTypes, usValidateOption, usBackupFiles, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsVerifySQL( ADSHANDLE hStatement, UNSIGNED8 *pucSQL )
{
#if defined(__cplusplus)
    static ADSVERIFYSQL_PTR
#else
    static ADSVERIFYSQL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSVERIFYSQL_PTR) Ace32_GetProcAddress( "AdsVerifySQL" );

    return pFunc( hStatement, pucSQL );
}

UNSIGNED32 ENTRYPOINT AdsVerifySQLW( ADSHANDLE hStatement, WCHAR *pwcSQL )
{
#if defined(__cplusplus)
    static ADSVERIFYSQLW_PTR
#else
    static ADSVERIFYSQLW_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSVERIFYSQLW_PTR) Ace32_GetProcAddress( "AdsVerifySQLW" );

    return pFunc( hStatement, pwcSQL );
}

UNSIGNED32 ENTRYPOINT AdsDisableUniqueEnforcement( ADSHANDLE hConnection )
{
#if defined(__cplusplus)
    static ADSDISABLEUNIQUEENFORCEMENT_PTR
#else
    static ADSDISABLEUNIQUEENFORCEMENT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDISABLEUNIQUEENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsDisableUniqueEnforcement" );

    return pFunc( hConnection );
}

UNSIGNED32 ENTRYPOINT AdsEnableUniqueEnforcement( ADSHANDLE hConnection )
{
#if defined(__cplusplus)
    static ADSENABLEUNIQUEENFORCEMENT_PTR
#else
    static ADSENABLEUNIQUEENFORCEMENT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSENABLEUNIQUEENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsEnableUniqueEnforcement" );

    return pFunc( hConnection );
}

UNSIGNED32 ENTRYPOINT AdsDisableRI( ADSHANDLE hConnection )
{
#if defined(__cplusplus)
    static ADSDISABLERI_PTR
#else
    static ADSDISABLERI_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDISABLERI_PTR) Ace32_GetProcAddress( "AdsDisableRI" );

    return pFunc( hConnection );
}

UNSIGNED32 ENTRYPOINT AdsEnableRI( ADSHANDLE hConnection )
{
#if defined(__cplusplus)
    static ADSENABLERI_PTR
#else
    static ADSENABLERI_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSENABLERI_PTR) Ace32_GetProcAddress( "AdsEnableRI" );

    return pFunc( hConnection );
}

UNSIGNED32 ENTRYPOINT AdsDisableAutoIncEnforcement( ADSHANDLE hConnection )
{
#if defined(__cplusplus)
    static ADSDISABLEAUTOINCENFORCEMENT_PTR
#else
    static ADSDISABLEAUTOINCENFORCEMENT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDISABLEAUTOINCENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsDisableAutoIncEnforcement" );

    return pFunc( hConnection );
}

UNSIGNED32 ENTRYPOINT AdsEnableAutoIncEnforcement( ADSHANDLE hConnection )
{
#if defined(__cplusplus)
    static ADSENABLEAUTOINCENFORCEMENT_PTR
#else
    static ADSENABLEAUTOINCENFORCEMENT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSENABLEAUTOINCENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsEnableAutoIncEnforcement" );

    return pFunc( hConnection );
}

UNSIGNED32 ENTRYPOINT AdsRollbackTransaction80( ADSHANDLE hConnect, UNSIGNED8 *pucSavepoint, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSROLLBACKTRANSACTION80_PTR
#else
    static ADSROLLBACKTRANSACTION80_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSROLLBACKTRANSACTION80_PTR) Ace32_GetProcAddress( "AdsRollbackTransaction80" );

    return pFunc( hConnect, pucSavepoint, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsCreateSavepoint( ADSHANDLE hConnect, UNSIGNED8 *pucSavepoint, UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSCREATESAVEPOINT_PTR
#else
    static ADSCREATESAVEPOINT_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSCREATESAVEPOINT_PTR) Ace32_GetProcAddress( "AdsCreateSavepoint" );

    return pFunc( hConnect, pucSavepoint, ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsDDFreeTable( UNSIGNED8 *pucTableName,  UNSIGNED8 *pucPassword )
{
#if defined(__cplusplus)
    static ADSDDFREETABLE_PTR
#else
    static ADSDDFREETABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDFREETABLE_PTR) Ace32_GetProcAddress( "AdsDDFreeTable" );

    return pFunc( pucTableName, pucPassword );
}

UNSIGNED32 ENTRYPOINT AdsDDSetIndexProperty( ADSHANDLE hAdminConn, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSDDSETINDEXPROPERTY_PTR
#else
    static ADSDDSETINDEXPROPERTY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSDDSETINDEXPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetIndexProperty" );

    return pFunc( hAdminConn, pucTableName, pucIndexName, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsIsFieldBinary( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbBinary )
{
#if defined(__cplusplus)
    static ADSISFIELDBINARY_PTR
#else
    static ADSISFIELDBINARY_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISFIELDBINARY_PTR) Ace32_GetProcAddress( "AdsIsFieldBinary" );

    return pFunc( hTable, pucFldName, pbBinary );
}

UNSIGNED32 ENTRYPOINT AdsIsNull( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbNull )
{
#if defined(__cplusplus)
    static ADSISNULL_PTR
#else
    static ADSISNULL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISNULL_PTR) Ace32_GetProcAddress( "AdsIsNull" );

    return pFunc( hTable, pucFldName, pbNull );
}

UNSIGNED32 ENTRYPOINT AdsIsNullable( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbNullable )
{
#if defined(__cplusplus)
    static ADSISNULLABLE_PTR
#else
    static ADSISNULLABLE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISNULLABLE_PTR) Ace32_GetProcAddress( "AdsIsNullable" );

    return pFunc( hTable, pucFldName, pbNullable );
}

UNSIGNED32 ENTRYPOINT AdsSetNull( ADSHANDLE hTable, UNSIGNED8 *pucFldName )
{
#if defined(__cplusplus)
    static ADSSETNULL_PTR
#else
    static ADSSETNULL_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETNULL_PTR) Ace32_GetProcAddress( "AdsSetNull" );

    return pFunc( hTable, pucFldName );
}

UNSIGNED32 ENTRYPOINT AdsGetTableCollation( ADSHANDLE hTbl, UNSIGNED8 *pucCollation, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETTABLECOLLATION_PTR
#else
    static ADSGETTABLECOLLATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETTABLECOLLATION_PTR) Ace32_GetProcAddress( "AdsGetTableCollation" );

    return pFunc( hTbl, pucCollation, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexCollation( ADSHANDLE hIndex, UNSIGNED8 *pucCollation, UNSIGNED16 *pusLen )
{
#if defined(__cplusplus)
    static ADSGETINDEXCOLLATION_PTR
#else
    static ADSGETINDEXCOLLATION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETINDEXCOLLATION_PTR) Ace32_GetProcAddress( "AdsGetIndexCollation" );

    return pFunc( hIndex, pucCollation, pusLen );
}

UNSIGNED32 ENTRYPOINT AdsGetDataLength(ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 ulOptions, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETDATALENGTH_PTR
#else
    static ADSGETDATALENGTH_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETDATALENGTH_PTR) Ace32_GetProcAddress( "AdsGetDataLength" );

    return pFunc( hTable, pucFldName, ulOptions, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsSetIndexDirection( ADSHANDLE hIndex, UNSIGNED16 usReverseDirection )
{
#if defined(__cplusplus)
    static ADSSETINDEXDIRECTION_PTR
#else
    static ADSSETINDEXDIRECTION_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETINDEXDIRECTION_PTR) Ace32_GetProcAddress( "AdsSetIndexDirection" );

    return pFunc( hIndex, usReverseDirection );
}

UNSIGNED32 ENTRYPOINT AdsMgKillUser90( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
#if defined(__cplusplus)
    static ADSMGKILLUSER90_PTR
#else
    static ADSMGKILLUSER90_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSMGKILLUSER90_PTR) Ace32_GetProcAddress( "AdsMgKillUser90" );

    return pFunc( hMgmtHandle, pucUserName, usConnNumber, usPropertyID, pvProperty, usPropertyLen );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldLength100( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 ulOptions, UNSIGNED32 *pulLength )
{
#if defined(__cplusplus)
    static ADSGETFIELDLENGTH100_PTR
#else
    static ADSGETFIELDLENGTH100_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSGETFIELDLENGTH100_PTR) Ace32_GetProcAddress( "AdsGetFieldLength100" );

    return pFunc( hTable, pucFldName, ulOptions, pulLength );
}

UNSIGNED32 ENTRYPOINT AdsSetRightsChecking( UNSIGNED32 ulOptions )
{
#if defined(__cplusplus)
    static ADSSETRIGHTSCHECKING_PTR
#else
    static ADSSETRIGHTSCHECKING_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETRIGHTSCHECKING_PTR) Ace32_GetProcAddress( "AdsSetRightsChecking" );

    return pFunc( ulOptions );
}

UNSIGNED32 ENTRYPOINT AdsSetTableTransactionFree( ADSHANDLE hTable, UNSIGNED16 usTransFree )
{
#if defined(__cplusplus)
    static ADSSETTABLETRANSACTIONFREE_PTR
#else
    static ADSSETTABLETRANSACTIONFREE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSSETTABLETRANSACTIONFREE_PTR) Ace32_GetProcAddress( "AdsSetTableTransactionFree" );

    return pFunc( hTable, usTransFree );
}

UNSIGNED32 ENTRYPOINT AdsIsTableTransactionFree( ADSHANDLE hTable, UNSIGNED16 *pusTransFree )
{
#if defined(__cplusplus)
    static ADSISTABLETRANSACTIONFREE_PTR
#else
    static ADSISTABLETRANSACTIONFREE_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSISTABLETRANSACTIONFREE_PTR) Ace32_GetProcAddress( "AdsIsTableTransactionFree" );

    return pFunc( hTable, pusTransFree );
}

UNSIGNED32 ENTRYPOINT AdsFindServers( UNSIGNED32 ulOptions, ADSHANDLE *phTable )
{
#if defined(__cplusplus)
    static ADSFINDSERVERS_PTR
#else
    static ADSFINDSERVERS_PTR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (ADSFINDSERVERS_PTR) Ace32_GetProcAddress( "AdsFindServers" );

    return pFunc( ulOptions, phTable );
}
