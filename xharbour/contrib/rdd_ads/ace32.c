/*
 * $Id: ace32.c,v 1.3 2009/01/24 00:33:08 likewolf Exp $
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

#ifndef HB_OS_WIN_USED
#define HB_OS_WIN_USED
#endif

#include "rddads.h"

static HMODULE hModule = NULL;

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
         hb_snprintf( __szError, sizeof( __szError ), "Cannot find function address: %s", szFuncName );
         MessageBox( NULL, __szError, szFuncName, MB_ICONSTOP );
         return NULL;
      }
   }
   else
   {
      char __szError[256];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot load ace32.dll" );
      MessageBox( NULL, __szError, "Error Loading DLL", MB_ICONSTOP );
      return NULL;
   }
}

UNSIGNED32 ENTRYPOINT AdsSetFieldRaw( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
   static ADSSETFIELDRAW_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETFIELDRAW_PTR) Ace32_GetProcAddress( "AdsSetFieldRaw" );

   return ( pFunc ? pFunc( hObj, pucFldName, pucBuf, ulLen  ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDeleteFile( ADSHANDLE hConnection, UNSIGNED8* pucFileName )
{
   static ADSDELETEFILE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDELETEFILE_PTR) Ace32_GetProcAddress( "AdsDeleteFile" );

   return ( pFunc ? pFunc( hConnection, pucFileName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldRaw( ADSHANDLE hTbl, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen )
{
   static ADSGETFIELDRAW_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELDRAW_PTR) Ace32_GetProcAddress( "AdsGetFieldRaw" );

   return ( pFunc ? pFunc( hTbl, pucFldName, pucBuf, pulLen  ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsAddCustomKey( ADSHANDLE hIndex )
{
   static ADSADDCUSTOMKEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSADDCUSTOMKEY_PTR) Ace32_GetProcAddress( "AdsAddCustomKey" );

   return ( pFunc ? pFunc( hIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsAppendRecord( ADSHANDLE hTable )
{
   static ADSAPPENDRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSAPPENDRECORD_PTR) Ace32_GetProcAddress( "AdsAppendRecord" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

#ifdef __BORLANDC__
   #pragma option push -w-pro
#endif
UNSIGNED32 ENTRYPOINT AdsApplicationExit()
{
   static ADSAPPLICATIONEXIT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSAPPLICATIONEXIT_PTR) Ace32_GetProcAddress( "AdsApplicationExit" );

   return ( pFunc ? pFunc() : 0 );
}
#ifdef __BORLANDC__
   #pragma option pop
#endif

UNSIGNED32 ENTRYPOINT AdsAtBOF( ADSHANDLE hTable, UNSIGNED16 *pbBof )
{
   static ADSATBOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSATBOF_PTR) Ace32_GetProcAddress( "AdsAtBOF" );

   return ( pFunc ? pFunc( hTable, pbBof ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsAtEOF( ADSHANDLE hTable, UNSIGNED16 *pbEof )
{
   static ADSATEOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSATEOF_PTR) Ace32_GetProcAddress( "AdsAtEOF" );

   return ( pFunc ? pFunc( hTable, pbEof ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsBeginTransaction( ADSHANDLE hConnect )
{
   static ADSBEGINTRANSACTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSBEGINTRANSACTION_PTR) Ace32_GetProcAddress( "AdsBeginTransaction" );

   return ( pFunc ? pFunc( hConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsBinaryToFile( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucFileName )
{
   static ADSBINARYTOFILE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSBINARYTOFILE_PTR) Ace32_GetProcAddress( "AdsBinaryToFile" );

   return ( pFunc ? pFunc( hTable, pucFldName, pucFileName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCacheOpenCursors( UNSIGNED16 usOpen )
{
   static ADSCACHEOPENCURSORS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCACHEOPENCURSORS_PTR) Ace32_GetProcAddress( "AdsCacheOpenCursors" );

   return ( pFunc ? pFunc( usOpen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCacheOpenTables( UNSIGNED16 usOpen )
{
   static ADSCACHEOPENTABLES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCACHEOPENTABLES_PTR) Ace32_GetProcAddress( "AdsCacheOpenTables" );

   return ( pFunc ? pFunc( usOpen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCacheRecords( ADSHANDLE hTable, UNSIGNED16 usNumRecords )
{
   static ADSCACHERECORDS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCACHERECORDS_PTR) Ace32_GetProcAddress( "AdsCacheRecords" );

   return ( pFunc ? pFunc( hTable, usNumRecords ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCancelUpdate( ADSHANDLE hTable )
{
   static ADSCANCELUPDATE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCANCELUPDATE_PTR) Ace32_GetProcAddress( "AdsCancelUpdate" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCheckExistence( ADSHANDLE hConnect, UNSIGNED8 *pucFileName, UNSIGNED16 *pusOnDisk )
{
   static ADSCHECKEXISTENCE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCHECKEXISTENCE_PTR) Ace32_GetProcAddress( "AdsCheckExistence" );

   return ( pFunc ? pFunc( hConnect, pucFileName, pusOnDisk ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearAllScopes( ADSHANDLE hTable )
{
   static ADSCLEARALLSCOPES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARALLSCOPES_PTR) Ace32_GetProcAddress( "AdsClearAllScopes" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearDefault( void )
{
   static ADSCLEARDEFAULT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARDEFAULT_PTR) Ace32_GetProcAddress( "AdsClearDefault" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearFilter( ADSHANDLE hTable )
{
   static ADSCLEARFILTER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARFILTER_PTR) Ace32_GetProcAddress( "AdsClearFilter" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearRelation( ADSHANDLE hTableParent )
{
   static ADSCLEARRELATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARRELATION_PTR) Ace32_GetProcAddress( "AdsClearRelation" );

   return ( pFunc ? pFunc( hTableParent ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearScope( ADSHANDLE hIndex, UNSIGNED16 usScopeOption )
{
   static ADSCLEARSCOPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARSCOPE_PTR) Ace32_GetProcAddress( "AdsClearScope" );

   return ( pFunc ? pFunc( hIndex, usScopeOption ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCloneTable( ADSHANDLE hTable, ADSHANDLE *phClone )
{
   static ADSCLONETABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLONETABLE_PTR) Ace32_GetProcAddress( "AdsCloneTable" );

   return ( pFunc ? pFunc( hTable, phClone ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCloseAllIndexes( ADSHANDLE hTable )
{
   static ADSCLOSEALLINDEXES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLOSEALLINDEXES_PTR) Ace32_GetProcAddress( "AdsCloseAllIndexes" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCloseAllTables( void )
{
   static ADSCLOSEALLTABLES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLOSEALLTABLES_PTR) Ace32_GetProcAddress( "AdsCloseAllTables" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCloseIndex( ADSHANDLE hIndex )
{
   static ADSCLOSEINDEX_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLOSEINDEX_PTR) Ace32_GetProcAddress( "AdsCloseIndex" );

   return ( pFunc ? pFunc( hIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCloseTable( ADSHANDLE hTable )
{
   static ADSCLOSETABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLOSETABLE_PTR) Ace32_GetProcAddress( "AdsCloseTable" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCloseCachedTables( ADSHANDLE hConnection )
{
   static ADSCLOSECACHEDTABLES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLOSECACHEDTABLES_PTR) Ace32_GetProcAddress( "AdsCloseCachedTables" );

   return ( pFunc ? pFunc( hConnection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCommitTransaction( ADSHANDLE hConnect )
{
   static ADSCOMMITTRANSACTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCOMMITTRANSACTION_PTR) Ace32_GetProcAddress( "AdsCommitTransaction" );

   return ( pFunc ? pFunc( hConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsConnect( UNSIGNED8 *pucServerName, ADSHANDLE *phConnect )
{
   static ADSCONNECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCONNECT_PTR) Ace32_GetProcAddress( "AdsConnect" );

   return ( pFunc ? pFunc( pucServerName, phConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsConnect26( UNSIGNED8 *pucServerName, UNSIGNED16 usServerTypes, ADSHANDLE *phConnect )
{
   static ADSCONNECT26_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCONNECT26_PTR) Ace32_GetProcAddress( "AdsConnect26" );

   return ( pFunc ? pFunc( pucServerName, usServerTypes, phConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsConnect60( UNSIGNED8 *pucServerPath, UNSIGNED16 usServerTypes, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED32 ulOptions, ADSHANDLE *phConnect )
{
   static ADSCONNECT60_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCONNECT60_PTR) Ace32_GetProcAddress( "AdsConnect60" );

   return ( pFunc ? pFunc( pucServerPath, usServerTypes, pucUserName, pucPassword, ulOptions, phConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsConnectionAlive( ADSHANDLE hConnect, UNSIGNED16 *pbConnectionIsAlive )
{
   static ADSISCONNECTIONALIVE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISCONNECTIONALIVE_PTR) Ace32_GetProcAddress( "AdsIsConnectionAlive" );

   return ( pFunc ? pFunc( hConnect, pbConnectionIsAlive ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsContinue( ADSHANDLE hTable, UNSIGNED16 *pbFound )
{
   static ADSCONTINUE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCONTINUE_PTR) Ace32_GetProcAddress( "AdsContinue" );

   return ( pFunc ? pFunc( hTable, pbFound ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsConvertTable( ADSHANDLE hObj, UNSIGNED16 usFilterOption, UNSIGNED8 *pucFile, UNSIGNED16 usTableType )
{
   static ADSCONVERTTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCONVERTTABLE_PTR) Ace32_GetProcAddress( "AdsConvertTable" );

   return ( pFunc ? pFunc( hObj, usFilterOption, pucFile, usTableType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCopyTable( ADSHANDLE hObj, UNSIGNED16 usFilterOption, UNSIGNED8 *pucFile )
{
   static ADSCOPYTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCOPYTABLE_PTR) Ace32_GetProcAddress( "AdsCopyTable" );

   return ( pFunc ? pFunc( hObj, usFilterOption, pucFile ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCopyTableContents( ADSHANDLE hObjFrom, ADSHANDLE hTableTo, UNSIGNED16 usFilterOption )
{
   static ADSCOPYTABLECONTENTS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCOPYTABLECONTENTS_PTR) Ace32_GetProcAddress( "AdsCopyTableContents" );

   return ( pFunc ? pFunc( hObjFrom, hTableTo, usFilterOption ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCopyTableStructure( ADSHANDLE hTable, UNSIGNED8 *pucFile )
{
   static ADSCOPYTABLESTRUCTURE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCOPYTABLESTRUCTURE_PTR) Ace32_GetProcAddress( "AdsCopyTableStructure" );

   return ( pFunc ? pFunc( hTable, pucFile ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateIndex( ADSHANDLE hObj, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucExpr, UNSIGNED8 *pucCondition, UNSIGNED8 *pucWhile, UNSIGNED32 ulOptions, ADSHANDLE *phIndex )
{
   static ADSCREATEINDEX_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATEINDEX_PTR) Ace32_GetProcAddress( "AdsCreateIndex" );

   return ( pFunc ? pFunc( hObj, pucFileName, pucTag, pucExpr, pucCondition, pucWhile, ulOptions, phIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateIndex61( ADSHANDLE hObj, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucExpr, UNSIGNED8 *pucCondition, UNSIGNED8 *pucWhile, UNSIGNED32 ulOptions, UNSIGNED32 ulPageSize, ADSHANDLE *phIndex )
{
   static ADSCREATEINDEX61_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATEINDEX61_PTR) Ace32_GetProcAddress( "AdsCreateIndex61" );

   return ( pFunc ? pFunc( hObj, pucFileName, pucTag, pucExpr, pucCondition, pucWhile, ulOptions, ulPageSize, phIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateFTSIndex( ADSHANDLE hTable, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucField, UNSIGNED32 ulPageSize, UNSIGNED32 ulMinWordLen, UNSIGNED32 ulMaxWordLen, UNSIGNED16 usUseDefaultDelim, UNSIGNED8 *pucDelimiters, UNSIGNED16 usUseDefaultNoise, UNSIGNED8 *pucNoiseWords, UNSIGNED16 usUseDefaultDrop, UNSIGNED8 *pucDropChars, UNSIGNED16 usUseDefaultConditionals, UNSIGNED8 *pucConditionalChars, UNSIGNED8 *pucCollation, UNSIGNED8 *pucReserved1, UNSIGNED32 ulOptions )
{
   static ADSCREATEFTSINDEX_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATEFTSINDEX_PTR) Ace32_GetProcAddress( "AdsCreateFTSIndex" );

   return ( pFunc ? pFunc( hTable, pucFileName, pucTag, pucField, ulPageSize, ulMinWordLen, ulMaxWordLen, usUseDefaultDelim, pucDelimiters, usUseDefaultNoise, pucNoiseWords, usUseDefaultDrop, pucDropChars, usUseDefaultConditionals, pucConditionalChars, pucCollation, pucReserved1, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateTable( ADSHANDLE hConnection, UNSIGNED8 *pucName, UNSIGNED8 *pucAlias, UNSIGNED16 usTableType,UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED16 usMemoSize, UNSIGNED8 *pucFields, ADSHANDLE *phTable )
{
   static ADSCREATETABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATETABLE_PTR) Ace32_GetProcAddress( "AdsCreateTable" );

   return ( pFunc ? pFunc( hConnection, pucName, pucAlias, usTableType,usCharType, usLockType, usCheckRights, usMemoSize, pucFields, phTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateTable71( ADSHANDLE hConnection, UNSIGNED8 *pucName, UNSIGNED8 *pucDBObjName, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED16 usMemoSize, UNSIGNED8 *pucFields, UNSIGNED32 ulOptions, ADSHANDLE *phTable )
{
   static ADSCREATETABLE71_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATETABLE71_PTR) Ace32_GetProcAddress( "AdsCreateTable71" );

   return ( pFunc ? pFunc( hConnection, pucName, pucDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, pucFields, ulOptions, phTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreate( UNSIGNED8 *pucDictionaryPath, UNSIGNED16 usEncrypt, UNSIGNED8 *pucDescription, ADSHANDLE *phDictionary )
{
   static ADSDDCREATE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATE_PTR) Ace32_GetProcAddress( "AdsDDCreate" );

   return ( pFunc ? pFunc( pucDictionaryPath, usEncrypt, pucDescription, phDictionary ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateRefIntegrity( ADSHANDLE hDictionary, UNSIGNED8 *pucRIName, UNSIGNED8 *pucFailTable,UNSIGNED8 *pucParentTableName, UNSIGNED8 *pucParentTagName, UNSIGNED8 *pucChildTableName, UNSIGNED8 *pucChildTagName, UNSIGNED16 usUpdateRule, UNSIGNED16 usDeleteRule )
{
   static ADSDDCREATEREFINTEGRITY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATEREFINTEGRITY_PTR) Ace32_GetProcAddress( "AdsDDCreateRefIntegrity" );

   return ( pFunc ? pFunc( hDictionary, pucRIName, pucFailTable,pucParentTableName, pucParentTagName, pucChildTableName, pucChildTagName, usUpdateRule, usDeleteRule ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateRefIntegrity62( ADSHANDLE hDictionary, UNSIGNED8 *pucRIName, UNSIGNED8 *pucFailTable, UNSIGNED8 *pucParentTableName, UNSIGNED8 *pucParentTagName, UNSIGNED8 *pucChildTableName, UNSIGNED8 *pucChildTagName, UNSIGNED16 usUpdateRule, UNSIGNED16 usDeleteRule, UNSIGNED8 *pucNoPrimaryError, UNSIGNED8 *pucCascadeError )
{
   static ADSDDCREATEREFINTEGRITY62_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATEREFINTEGRITY62_PTR) Ace32_GetProcAddress( "AdsDDCreateRefIntegrity62" );

   return ( pFunc ? pFunc( hDictionary, pucRIName, pucFailTable, pucParentTableName, pucParentTagName, pucChildTableName, pucChildTagName, usUpdateRule, usDeleteRule, pucNoPrimaryError, pucCascadeError ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveRefIntegrity( ADSHANDLE hDictionary, UNSIGNED8 *pucRIName )
{
   static ADSDDREMOVEREFINTEGRITY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREMOVEREFINTEGRITY_PTR) Ace32_GetProcAddress( "AdsDDRemoveRefIntegrity" );

   return ( pFunc ? pFunc( hDictionary, pucRIName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetDatabaseProperty( ADSHANDLE hObject, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETDATABASEPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETDATABASEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetDatabaseProperty" );

   return ( pFunc ? pFunc( hObject, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetFieldProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED8 *pucFieldName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETFIELDPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETFIELDPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetFieldProperty" );

   return ( pFunc ? pFunc( hObject, pucTableName, pucFieldName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetIndexFileProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexFileName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETINDEXFILEPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETINDEXFILEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetIndexFileProperty" );

   return ( pFunc ? pFunc( hObject, pucTableName, pucIndexFileName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetIndexProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETINDEXPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETINDEXPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetIndexProperty" );

   return ( pFunc ? pFunc( hObject, pucTableName, pucIndexName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetLinkProperty( ADSHANDLE hConnect, UNSIGNED8 *pucLinkName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETLINKPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETLINKPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetLinkProperty" );

   return ( pFunc ? pFunc( hConnect, pucLinkName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetTableProperty( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETTABLEPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETTABLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetTableProperty" );

   return ( pFunc ? pFunc( hObject, pucTableName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetUserGroupProperty( ADSHANDLE hObject, UNSIGNED8 *pucUserGroupName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETUSERGROUPPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETUSERGROUPPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetUserGroupProperty" );

   return ( pFunc ? pFunc( hObject, pucUserGroupName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetUserProperty( ADSHANDLE hObject, UNSIGNED8 *pucUserName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETUSERPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETUSERPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetUserProperty" );

   return ( pFunc ? pFunc( hObject, pucUserName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetViewProperty( ADSHANDLE hObject, UNSIGNED8 *pucViewName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETVIEWPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETVIEWPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetViewProperty" );

   return ( pFunc ? pFunc( hObject, pucViewName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetTriggerProperty( ADSHANDLE hObject, UNSIGNED8 *pucTriggerName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETTRIGGERPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETTRIGGERPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetTriggerProperty" );

   return ( pFunc ? pFunc( hObject, pucTriggerName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetProcedureProperty( ADSHANDLE hObject, UNSIGNED8 *pucProcName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETPROCEDUREPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETPROCEDUREPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetProcedureProperty" );

   return ( pFunc ? pFunc( hObject, pucProcName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetRefIntegrityProperty( ADSHANDLE hObject, UNSIGNED8 *pucRIName, UNSIGNED16 usPropertyID, UNSIGNED8 *pucProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETREFINTEGRITYPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETREFINTEGRITYPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetRefIntegrityProperty" );

   return ( pFunc ? pFunc( hObject, pucRIName, usPropertyID, pucProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetPermissions( ADSHANDLE hDBConn, UNSIGNED8 *pucGrantee, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucParentName, UNSIGNED16 usGetInherited, UNSIGNED32 *pulPermissions )
{
   static ADSDDGETPERMISSIONS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETPERMISSIONS_PTR) Ace32_GetProcAddress( "AdsDDGetPermissions" );

   return ( pFunc ? pFunc( hDBConn, pucGrantee, usObjectType, pucObjectName, pucParentName, usGetInherited, pulPermissions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGrantPermission( ADSHANDLE hAdminConn, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucParentName, UNSIGNED8 *pucGrantee, UNSIGNED32 ulPermissions )
{
   static ADSDDGRANTPERMISSION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGRANTPERMISSION_PTR) Ace32_GetProcAddress( "AdsDDGrantPermission" );

   return ( pFunc ? pFunc( hAdminConn, usObjectType, pucObjectName, pucParentName, pucGrantee, ulPermissions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRevokePermission( ADSHANDLE hAdminConn, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucParentName, UNSIGNED8 *pucGrantee, UNSIGNED32 ulPermissions )
{
   static ADSDDREVOKEPERMISSION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREVOKEPERMISSION_PTR) Ace32_GetProcAddress( "AdsDDRevokePermission" );

   return ( pFunc ? pFunc( hAdminConn, usObjectType, pucObjectName, pucParentName, pucGrantee, ulPermissions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetDatabaseProperty( ADSHANDLE hDictionary, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETDATABASEPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETDATABASEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetDatabaseProperty" );

   return ( pFunc ? pFunc( hDictionary, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetFieldProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucFieldName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen, UNSIGNED16 usValidateOption, UNSIGNED8 *pucFailTable )
{
   static ADSDDSETFIELDPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETFIELDPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetFieldProperty" );

   return ( pFunc ? pFunc( hDictionary, pucTableName, pucFieldName, usPropertyID, pvProperty, usPropertyLen, usValidateOption, pucFailTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetProcedureProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucProcedureName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETPROCEDUREPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETPROCEDUREPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetProcedureProperty" );

   return ( pFunc ? pFunc( hDictionary, pucProcedureName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetTableProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen, UNSIGNED16 usValidateOption, UNSIGNED8 *pucFailTable )
{
   static ADSDDSETTABLEPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETTABLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetTableProperty" );

   return ( pFunc ? pFunc( hDictionary, pucTableName, usPropertyID, pvProperty, usPropertyLen, usValidateOption, pucFailTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetUserGroupProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucUserGroupName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETUSERGROUPPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETUSERGROUPPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetUserGroupProperty" );

   return ( pFunc ? pFunc( hDictionary, pucUserGroupName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetUserProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucUserName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETUSERPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETUSERPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetUserProperty" );

   return ( pFunc ? pFunc( hDictionary, pucUserName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetViewProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucViewName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETVIEWPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETVIEWPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetViewProperty" );

   return ( pFunc ? pFunc( hDictionary, pucViewName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetObjectAccessRights( ADSHANDLE hDictionary, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucAccessorName, UNSIGNED8 *pucAllowedAccess )
{
   static ADSDDSETOBJECTACCESSRIGHTS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETOBJECTACCESSRIGHTS_PTR) Ace32_GetProcAddress( "AdsDDSetObjectAccessRights" );

   return ( pFunc ? pFunc( hDictionary, pucObjectName, pucAccessorName, pucAllowedAccess ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDAddProcedure( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucContainer, UNSIGNED8 *pucProcName, UNSIGNED32 ulInvokeOption, UNSIGNED8 *pucInParams, UNSIGNED8 *pucOutParams, UNSIGNED8 *pucComments )
{
   static ADSDDADDPROCEDURE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDADDPROCEDURE_PTR) Ace32_GetProcAddress( "AdsDDAddProcedure" );

   return ( pFunc ? pFunc( hDictionary, pucName, pucContainer, pucProcName, ulInvokeOption, pucInParams, pucOutParams, pucComments ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDAddTable( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucTablePath, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED8 *pucIndexFiles, UNSIGNED8 *pucComments )
{
   static ADSDDADDTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDADDTABLE_PTR) Ace32_GetProcAddress( "AdsDDAddTable" );

   return ( pFunc ? pFunc( hDictionary, pucTableName, pucTablePath, usTableType, usCharType, pucIndexFiles, pucComments ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDAddView( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucComments, UNSIGNED8 *pucSQL )
{
   static ADSDDADDVIEW_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDADDVIEW_PTR) Ace32_GetProcAddress( "AdsDDAddView" );

   return ( pFunc ? pFunc( hDictionary, pucName, pucComments, pucSQL ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateTrigger( ADSHANDLE hDictionary, UNSIGNED8 *pucName, UNSIGNED8 *pucTableName, UNSIGNED32 ulTriggerType, UNSIGNED32 ulEventTypes, UNSIGNED32 ulContainerType, UNSIGNED8 *pucContainer, UNSIGNED8 *pucFunctionName, UNSIGNED32 ulPriority, UNSIGNED8 *pucComments, UNSIGNED32 ulOptions )
{
   static ADSDDCREATETRIGGER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATETRIGGER_PTR) Ace32_GetProcAddress( "AdsDDCreateTrigger" );

   return ( pFunc ? pFunc( hDictionary, pucName, pucTableName, ulTriggerType, ulEventTypes, ulContainerType, pucContainer, pucFunctionName, ulPriority, pucComments, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveTrigger( ADSHANDLE hDictionary, UNSIGNED8 *pucName )
{
   static ADSDDREMOVETRIGGER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREMOVETRIGGER_PTR) Ace32_GetProcAddress( "AdsDDRemoveTrigger" );

   return ( pFunc ? pFunc( hDictionary, pucName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDAddIndexFile( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexFilePath, UNSIGNED8 *pucComment )
{
   static ADSDDADDINDEXFILE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDADDINDEXFILE_PTR) Ace32_GetProcAddress( "AdsDDAddIndexFile" );

   return ( pFunc ? pFunc( hDictionary, pucTableName, pucIndexFilePath, pucComment ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateUser( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED8 *pucDescription )
{
   static ADSDDCREATEUSER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATEUSER_PTR) Ace32_GetProcAddress( "AdsDDCreateUser" );

   return ( pFunc ? pFunc( hDictionary, pucGroupName, pucUserName, pucPassword, pucDescription ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDAddUserToGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucUserName )
{
   static ADSDDADDUSERTOGROUP_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDADDUSERTOGROUP_PTR) Ace32_GetProcAddress( "AdsDDAddUserToGroup" );

   return ( pFunc ? pFunc( hDictionary, pucGroupName, pucUserName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveUserFromGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucUserName )
{
   static ADSDDREMOVEUSERFROMGROUP_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREMOVEUSERFROMGROUP_PTR) Ace32_GetProcAddress( "AdsDDRemoveUserFromGroup" );

   return ( pFunc ? pFunc( hDictionary, pucGroupName, pucUserName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteUser( ADSHANDLE hDictionary, UNSIGNED8 *pucUserName )
{
   static ADSDDDELETEUSER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDELETEUSER_PTR) Ace32_GetProcAddress( "AdsDDDeleteUser" );

   return ( pFunc ? pFunc( hDictionary, pucUserName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateUserGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName, UNSIGNED8 *pucDescription )
{
   static ADSDDCREATEUSERGROUP_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATEUSERGROUP_PTR) Ace32_GetProcAddress( "AdsDDCreateUserGroup" );

   return ( pFunc ? pFunc( hDictionary, pucGroupName, pucDescription ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteUserGroup( ADSHANDLE hDictionary, UNSIGNED8 *pucGroupName )
{
   static ADSDDDELETEUSERGROUP_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDELETEUSERGROUP_PTR) Ace32_GetProcAddress( "AdsDDDeleteUserGroup" );

   return ( pFunc ? pFunc( hDictionary, pucGroupName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteIndex( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexName )
{
   static ADSDDDELETEINDEX_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDELETEINDEX_PTR) Ace32_GetProcAddress( "AdsDDDeleteIndex" );

   return ( pFunc ? pFunc( hDictionary, pucTableName, pucIndexName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveIndexFile( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexFileName, UNSIGNED16 usDeleteFile )
{
   static ADSDDREMOVEINDEXFILE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREMOVEINDEXFILE_PTR) Ace32_GetProcAddress( "AdsDDRemoveIndexFile" );

   return ( pFunc ? pFunc( hDictionary, pucTableName, pucIndexFileName, usDeleteFile ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveProcedure( ADSHANDLE hDictionary, UNSIGNED8 *pucName )
{
   static ADSDDREMOVEPROCEDURE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREMOVEPROCEDURE_PTR) Ace32_GetProcAddress( "AdsDDRemoveProcedure" );

   return ( pFunc ? pFunc( hDictionary, pucName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveTable( ADSHANDLE hObject, UNSIGNED8 *pucTableName, UNSIGNED16 usDeleteFiles )
{
   static ADSDDREMOVETABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREMOVETABLE_PTR) Ace32_GetProcAddress( "AdsDDRemoveTable" );

   return ( pFunc ? pFunc( hObject, pucTableName, usDeleteFiles ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRemoveView( ADSHANDLE hDictionary, UNSIGNED8 *pucName )
{
   static ADSDDREMOVEVIEW_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDREMOVEVIEW_PTR) Ace32_GetProcAddress( "AdsDDRemoveView" );

   return ( pFunc ? pFunc( hDictionary, pucName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDRenameObject( ADSHANDLE hDictionary, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucNewObjectName, UNSIGNED16 usObjectType, UNSIGNED32 ulOptions )
{
   static ADSDDRENAMEOBJECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDRENAMEOBJECT_PTR) Ace32_GetProcAddress( "AdsDDRenameObject" );

   return ( pFunc ? pFunc( hDictionary, pucObjectName, pucNewObjectName, usObjectType, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDMoveObjectFile( ADSHANDLE hDictionary, UNSIGNED16 usObjectType, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucNewPath, UNSIGNED8 *pucIndexFiles, UNSIGNED8 *pucParent, UNSIGNED32 ulOptions )
{
   static ADSDDMOVEOBJECTFILE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDMOVEOBJECTFILE_PTR) Ace32_GetProcAddress( "AdsDDMoveObjectFile" );

   return ( pFunc ? pFunc( hDictionary, usObjectType, pucObjectName, pucNewPath, pucIndexFiles, pucParent, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDFindFirstObject( ADSHANDLE hObject, UNSIGNED16 usFindObjectType, UNSIGNED8 *pucParentName, UNSIGNED8 *pucObjectName, UNSIGNED16 *pusObjectNameLen, ADSHANDLE *phFindHandle )
{
   static ADSDDFINDFIRSTOBJECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDFINDFIRSTOBJECT_PTR) Ace32_GetProcAddress( "AdsDDFindFirstObject" );

   return ( pFunc ? pFunc( hObject, usFindObjectType, pucParentName, pucObjectName, pusObjectNameLen, phFindHandle ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDFindNextObject( ADSHANDLE hObject, ADSHANDLE hFindHandle, UNSIGNED8 *pucObjectName, UNSIGNED16 *pusObjectNameLen )
{
   static ADSDDFINDNEXTOBJECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDFINDNEXTOBJECT_PTR) Ace32_GetProcAddress( "AdsDDFindNextObject" );

   return ( pFunc ? pFunc( hObject, hFindHandle, pucObjectName, pusObjectNameLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDFindClose( ADSHANDLE hObject, ADSHANDLE hFindHandle )
{
   static ADSDDFINDCLOSE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDFINDCLOSE_PTR) Ace32_GetProcAddress( "AdsDDFindClose" );

   return ( pFunc ? pFunc( hObject, hFindHandle ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateLink( ADSHANDLE hDBConn, UNSIGNED8 *pucLinkAlias, UNSIGNED8 *pucLinkedDDPath, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED32 ulOptions )
{
   static ADSDDCREATELINK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATELINK_PTR) Ace32_GetProcAddress( "AdsDDCreateLink" );

   return ( pFunc ? pFunc( hDBConn, pucLinkAlias, pucLinkedDDPath, pucUserName, pucPassword, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDModifyLink( ADSHANDLE hDBConn, UNSIGNED8 *pucLinkAlias, UNSIGNED8 *pucLinkedDDPath, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, UNSIGNED32 ulOptions )
{
   static ADSDDMODIFYLINK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDMODIFYLINK_PTR) Ace32_GetProcAddress( "AdsDDModifyLink" );

   return ( pFunc ? pFunc( hDBConn, pucLinkAlias, pucLinkedDDPath, pucUserName, pucPassword, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDropLink( ADSHANDLE hDBConn, UNSIGNED8 *pucLinkedDD, UNSIGNED16 usDropGlobal )
{
   static ADSDDDROPLINK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDROPLINK_PTR) Ace32_GetProcAddress( "AdsDDDropLink" );

   return ( pFunc ? pFunc( hDBConn, pucLinkedDD, usDropGlobal ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreatePublication( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucComments, UNSIGNED32 ulOptions )
{
   static ADSDDCREATEPUBLICATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATEPUBLICATION_PTR) Ace32_GetProcAddress( "AdsDDCreatePublication" );

   return ( pFunc ? pFunc( hDictionary, pucPublicationName, pucComments, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetPublicationProperty( ADSHANDLE hObject, UNSIGNED8 *pucPublicationName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETPUBLICATIONPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETPUBLICATIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetPublicationProperty" );

   return ( pFunc ? pFunc( hObject, pucPublicationName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetPublicationProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETPUBLICATIONPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETPUBLICATIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetPublicationProperty" );

   return ( pFunc ? pFunc( hDictionary, pucPublicationName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeletePublication( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName )
{
   static ADSDDDELETEPUBLICATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDELETEPUBLICATION_PTR) Ace32_GetProcAddress( "AdsDDDeletePublication" );

   return ( pFunc ? pFunc( hDictionary, pucPublicationName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateArticle( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName, UNSIGNED8 *pucRowIdentColumns, UNSIGNED8 *pucFilter, UNSIGNED32 ulOptions )
{
   static ADSDDCREATEARTICLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATEARTICLE_PTR) Ace32_GetProcAddress( "AdsDDCreateArticle" );

   return ( pFunc ? pFunc( hDictionary, pucPublicationName, pucObjectName, pucRowIdentColumns, pucFilter, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetArticleProperty( ADSHANDLE hObject, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETARTICLEPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETARTICLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetArticleProperty" );

   return ( pFunc ? pFunc( hObject, pucPublicationName, pucObjectName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetArticleProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETARTICLEPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETARTICLEPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetArticleProperty" );

   return ( pFunc ? pFunc( hDictionary, pucPublicationName, pucObjectName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteArticle( ADSHANDLE hDictionary, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucObjectName )
{
   static ADSDDDELETEARTICLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDELETEARTICLE_PTR) Ace32_GetProcAddress( "AdsDDDeleteArticle" );

   return ( pFunc ? pFunc( hDictionary, pucPublicationName, pucObjectName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDCreateSubscription( ADSHANDLE hDictionary, UNSIGNED8 *pucSubscriptionName, UNSIGNED8 *pucPublicationName, UNSIGNED8 *pucTarget, UNSIGNED8 *pucUser, UNSIGNED8 *pucPassword, UNSIGNED8 *pucReplicationQueue, UNSIGNED16 usForward, UNSIGNED8 *pucComments, UNSIGNED32 ulOptions )
{
   static ADSDDCREATESUBSCRIPTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDCREATESUBSCRIPTION_PTR) Ace32_GetProcAddress( "AdsDDCreateSubscription" );

   return ( pFunc ? pFunc( hDictionary, pucSubscriptionName, pucPublicationName, pucTarget, pucUser, pucPassword, pucReplicationQueue, usForward, pucComments, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDGetSubscriptionProperty( ADSHANDLE hObject, UNSIGNED8 *pucSubscriptionName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 *pusPropertyLen )
{
   static ADSDDGETSUBSCRIPTIONPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDGETSUBSCRIPTIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDGetSubscriptionProperty" );

   return ( pFunc ? pFunc( hObject, pucSubscriptionName, usPropertyID, pvProperty, pusPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetSubscriptionProperty( ADSHANDLE hDictionary, UNSIGNED8 *pucSubscriptionName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETSUBSCRIPTIONPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETSUBSCRIPTIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetSubscriptionProperty" );

   return ( pFunc ? pFunc( hDictionary, pucSubscriptionName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeleteSubscription( ADSHANDLE hDictionary, UNSIGNED8 *pucSubscriptionName )
{
   static ADSDDDELETESUBSCRIPTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDELETESUBSCRIPTION_PTR) Ace32_GetProcAddress( "AdsDDDeleteSubscription" );

   return ( pFunc ? pFunc( hDictionary, pucSubscriptionName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDecryptRecord( ADSHANDLE hTable )
{
   static ADSDECRYPTRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDECRYPTRECORD_PTR) Ace32_GetProcAddress( "AdsDecryptRecord" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDecryptTable( ADSHANDLE hTable )
{
   static ADSDECRYPTTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDECRYPTTABLE_PTR) Ace32_GetProcAddress( "AdsDecryptTable" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDeleteCustomKey( ADSHANDLE hIndex )
{
   static ADSDELETECUSTOMKEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDELETECUSTOMKEY_PTR) Ace32_GetProcAddress( "AdsDeleteCustomKey" );

   return ( pFunc ? pFunc( hIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDeleteIndex( ADSHANDLE hIndex )
{
   static ADSDELETEINDEX_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDELETEINDEX_PTR) Ace32_GetProcAddress( "AdsDeleteIndex" );

   return ( pFunc ? pFunc( hIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDeleteRecord( ADSHANDLE hTable )
{
   static ADSDELETERECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDELETERECORD_PTR) Ace32_GetProcAddress( "AdsDeleteRecord" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyColumn( ADSHANDLE hCursor, UNSIGNED8 *pucKeyColumn, UNSIGNED16 *pusLen )
{
   static ADSGETKEYCOLUMN_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETKEYCOLUMN_PTR) Ace32_GetProcAddress( "AdsGetKeyColumn" );

   return ( pFunc ? pFunc( hCursor, pucKeyColumn, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDisableEncryption( ADSHANDLE hTable )
{
   static ADSDISABLEENCRYPTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDISABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsDisableEncryption" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDisableLocalConnections( void )
{
   static ADSDISABLELOCALCONNECTIONS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDISABLELOCALCONNECTIONS_PTR) Ace32_GetProcAddress( "AdsDisableLocalConnections" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDisconnect( ADSHANDLE hConnect )
{
   static ADSDISCONNECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDISCONNECT_PTR) Ace32_GetProcAddress( "AdsDisconnect" );

   return ( pFunc ? pFunc( hConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEnableEncryption( ADSHANDLE hTable, UNSIGNED8 *pucPassword )
{
   static ADSENABLEENCRYPTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSENABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsEnableEncryption" );

   return ( pFunc ? pFunc( hTable, pucPassword ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEncryptRecord( ADSHANDLE hTable )
{
   static ADSENCRYPTRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSENCRYPTRECORD_PTR) Ace32_GetProcAddress( "AdsEncryptRecord" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEncryptTable( ADSHANDLE hTable )
{
   static ADSENCRYPTTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSENCRYPTTABLE_PTR) Ace32_GetProcAddress( "AdsEncryptTable" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEvalLogicalExpr( ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 *pbResult )
{
   static ADSEVALLOGICALEXPR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEVALLOGICALEXPR_PTR) Ace32_GetProcAddress( "AdsEvalLogicalExpr" );

   return ( pFunc ? pFunc( hTable, pucExpr, pbResult ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEvalNumericExpr( ADSHANDLE hTable, UNSIGNED8 *pucExpr, DOUBLE *pdResult )
{
   static ADSEVALNUMERICEXPR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEVALNUMERICEXPR_PTR) Ace32_GetProcAddress( "AdsEvalNumericExpr" );

   return ( pFunc ? pFunc( hTable, pucExpr, pdResult ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEvalStringExpr( ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED8 *pucResult, UNSIGNED16 *pusLen )
{
   static ADSEVALSTRINGEXPR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEVALSTRINGEXPR_PTR) Ace32_GetProcAddress( "AdsEvalStringExpr" );

   return ( pFunc ? pFunc( hTable, pucExpr, pucResult, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEvalTestExpr( ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 *pusType )
{
   static ADSEVALTESTEXPR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEVALTESTEXPR_PTR) Ace32_GetProcAddress( "AdsEvalTestExpr" );

   return ( pFunc ? pFunc( hTable, pucExpr, pusType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsExtractKey( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 *pusLen )
{
   static ADSEXTRACTKEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEXTRACTKEY_PTR) Ace32_GetProcAddress( "AdsExtractKey" );

   return ( pFunc ? pFunc( hIndex, pucKey, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsFailedTransactionRecovery( UNSIGNED8 *pucServer )
{
   static ADSFAILEDTRANSACTIONRECOVERY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFAILEDTRANSACTIONRECOVERY_PTR) Ace32_GetProcAddress( "AdsFailedTransactionRecovery" );

   return ( pFunc ? pFunc( pucServer ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsFileToBinary( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 usBinaryType, UNSIGNED8 *pucFileName )
{
   static ADSFILETOBINARY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFILETOBINARY_PTR) Ace32_GetProcAddress( "AdsFileToBinary" );

   return ( pFunc ? pFunc( hTable, pucFldName, usBinaryType, pucFileName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsFindConnection( UNSIGNED8 *pucServerName, ADSHANDLE *phConnect )
{
   static ADSFINDCONNECTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFINDCONNECTION_PTR) Ace32_GetProcAddress( "AdsFindConnection" );

   return ( pFunc ? pFunc( pucServerName, phConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsFindConnection25( UNSIGNED8 *pucFullPath, ADSHANDLE *phConnect )
{
   static ADSFINDCONNECTION25_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFINDCONNECTION25_PTR) Ace32_GetProcAddress( "AdsFindConnection25" );

   return ( pFunc ? pFunc( pucFullPath, phConnect ) : 0 );
}

#if ADS_LIB_VERSION >= 900
UNSIGNED32 ENTRYPOINT AdsFindClose( ADSHANDLE hConnect, ADSHANDLE lHandle )
#else
UNSIGNED32 ENTRYPOINT AdsFindClose( ADSHANDLE hConnect, SIGNED32 lHandle )
#endif
{
   static ADSFINDCLOSE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFINDCLOSE_PTR) Ace32_GetProcAddress( "AdsFindClose" );

   return ( pFunc ? pFunc( hConnect, lHandle ) : 0 );
}

#if ADS_LIB_VERSION >= 900
UNSIGNED32 ENTRYPOINT AdsFindFirstTable( ADSHANDLE hConnect, UNSIGNED8  *pucFileMask, UNSIGNED8  *pucFirstFile, UNSIGNED16 *pusFileLen, ADSHANDLE *plHandle )
#else
UNSIGNED32 ENTRYPOINT AdsFindFirstTable( ADSHANDLE hConnect, UNSIGNED8  *pucFileMask, UNSIGNED8  *pucFirstFile, UNSIGNED16 *pusFileLen, SIGNED32 *plHandle )
#endif
{
   static ADSFINDFIRSTTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFINDFIRSTTABLE_PTR) Ace32_GetProcAddress( "AdsFindFirstTable" );

   return ( pFunc ? pFunc( hConnect, pucFileMask, pucFirstFile, pusFileLen, plHandle ) : 0 );
}

#if ADS_LIB_VERSION >= 900
UNSIGNED32 ENTRYPOINT AdsFindNextTable( ADSHANDLE hConnect, ADSHANDLE lHandle, UNSIGNED8 *pucFileName, UNSIGNED16 *pusFileLen )
#else
UNSIGNED32 ENTRYPOINT AdsFindNextTable( ADSHANDLE hConnect, SIGNED32 lHandle, UNSIGNED8 *pucFileName, UNSIGNED16 *pusFileLen )
#endif
{
   static ADSFINDNEXTTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFINDNEXTTABLE_PTR) Ace32_GetProcAddress( "AdsFindNextTable" );

   return ( pFunc ? pFunc( hConnect, lHandle, pucFileName, pusFileLen ) : 0 );
}

#if ADS_LIB_VERSION >= 900
UNSIGNED32 ENTRYPOINT AdsFindFirstTable62( ADSHANDLE hConnect, UNSIGNED8 *pucFileMask, UNSIGNED8 *pucFirstDD, UNSIGNED16 *pusDDLen, UNSIGNED8 *pucFirstFile, UNSIGNED16 *pusFileLen, ADSHANDLE *plHandle )
#else
UNSIGNED32 ENTRYPOINT AdsFindFirstTable62( ADSHANDLE hConnect, UNSIGNED8 *pucFileMask, UNSIGNED8 *pucFirstDD, UNSIGNED16 *pusDDLen, UNSIGNED8 *pucFirstFile, UNSIGNED16 *pusFileLen, SIGNED32 *plHandle )
#endif
{
   static ADSFINDFIRSTTABLE62_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFINDFIRSTTABLE62_PTR) Ace32_GetProcAddress( "AdsFindFirstTable62" );

   return ( pFunc ? pFunc( hConnect, pucFileMask, pucFirstDD, pusDDLen, pucFirstFile, pusFileLen, plHandle ) : 0 );
}

#if ADS_LIB_VERSION >= 900
UNSIGNED32 ENTRYPOINT AdsFindNextTable62( ADSHANDLE hConnect, ADSHANDLE lHandle, UNSIGNED8 *pucDDName, UNSIGNED16 *pusDDLen, UNSIGNED8 *pucFileName, UNSIGNED16 *pusFileLen )
#else
UNSIGNED32 ENTRYPOINT AdsFindNextTable62( ADSHANDLE hConnect, SIGNED32 lHandle, UNSIGNED8 *pucDDName, UNSIGNED16 *pusDDLen, UNSIGNED8 *pucFileName, UNSIGNED16 *pusFileLen )
#endif
{
   static ADSFINDNEXTTABLE62_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFINDNEXTTABLE62_PTR) Ace32_GetProcAddress( "AdsFindNextTable62" );

   return ( pFunc ? pFunc( hConnect, lHandle, pucDDName, pusDDLen, pucFileName, pusFileLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetAllIndexes( ADSHANDLE hTable, ADSHANDLE ahIndex[], UNSIGNED16 *pusArrayLen )
{
   static ADSGETALLINDEXES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETALLINDEXES_PTR) Ace32_GetProcAddress( "AdsGetAllIndexes" );

   return ( pFunc ? pFunc( hTable, ahIndex, pusArrayLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFTSIndexes( ADSHANDLE hTable, ADSHANDLE ahIndex[], UNSIGNED16 *pusArrayLen )
{
   static ADSGETFTSINDEXES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFTSINDEXES_PTR) Ace32_GetProcAddress( "AdsGetFTSIndexes" );

   return ( pFunc ? pFunc( hTable, ahIndex, pusArrayLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFTSIndexInfo( ADSHANDLE hIndex, UNSIGNED8 *pucOutput, UNSIGNED32 *pulBufLen, UNSIGNED8 **ppucField, UNSIGNED32 *pulMinWordLen, UNSIGNED32 *pulMaxWordLen, UNSIGNED8 **ppucDelimiters, UNSIGNED8 **ppucNoiseWords, UNSIGNED8 **ppucDropChars, UNSIGNED8 **ppucConditionalChars, UNSIGNED8 **ppucReserved1, UNSIGNED8 **ppucReserved2, UNSIGNED32 *pulOptions )
{
   static ADSGETFTSINDEXINFO_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFTSINDEXINFO_PTR) Ace32_GetProcAddress( "AdsGetFTSIndexInfo" );

   return ( pFunc ? pFunc( hIndex, pucOutput, pulBufLen, ppucField, pulMinWordLen, pulMaxWordLen, ppucDelimiters, ppucNoiseWords, ppucDropChars, ppucConditionalChars, ppucReserved1, ppucReserved2, pulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetAllLocks( ADSHANDLE hTable, UNSIGNED32 aulLocks[], UNSIGNED16 *pusArrayLen )
{
   static ADSGETALLLOCKS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETALLLOCKS_PTR) Ace32_GetProcAddress( "AdsGetAllLocks" );

   return ( pFunc ? pFunc( hTable, aulLocks, pusArrayLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetAllTables( ADSHANDLE ahTable[], UNSIGNED16 *pusArrayLen )
{
   static ADSGETALLTABLES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETALLTABLES_PTR) Ace32_GetProcAddress( "AdsGetAllTables" );

   return ( pFunc ? pFunc( ahTable, pusArrayLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetBinary( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 ulOffset, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen )
{
   static ADSGETBINARY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETBINARY_PTR) Ace32_GetProcAddress( "AdsGetBinary" );

   return ( pFunc ? pFunc( hTable, pucFldName, ulOffset, pucBuf, pulLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetBinaryLength( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulLength )
{
   static ADSGETBINARYLENGTH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETBINARYLENGTH_PTR) Ace32_GetProcAddress( "AdsGetBinaryLength" );

   return ( pFunc ? pFunc( hTable, pucFldName, pulLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetBookmark( ADSHANDLE hTable, ADSHANDLE *phBookmark )
{
   static ADSGETBOOKMARK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETBOOKMARK_PTR) Ace32_GetProcAddress( "AdsGetBookmark" );

   return ( pFunc ? pFunc( hTable, phBookmark ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetBookmark60( ADSHANDLE hObj, UNSIGNED8 *pucBookmark, UNSIGNED32 *pulLength )
{
   static ADSGETBOOKMARK60_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETBOOKMARK60_PTR) Ace32_GetProcAddress( "AdsGetBookmark60" );

   return ( pFunc ? pFunc( hObj, pucBookmark, pulLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetBookmarkLength( ADSHANDLE hObj, UNSIGNED32 *pulLength )
{
   static ADSGETBOOKMARKLENGTH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETBOOKMARKLENGTH_PTR) Ace32_GetProcAddress( "AdsGetBookmarkLength" );

   return ( pFunc ? pFunc( hObj, pulLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCompareBookmarks( UNSIGNED8 *pucBookmark1, UNSIGNED8 *pucBookmark2, SIGNED32 *plResult )
{
   static ADSCOMPAREBOOKMARKS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCOMPAREBOOKMARKS_PTR) Ace32_GetProcAddress( "AdsCompareBookmarks" );

   return ( pFunc ? pFunc( pucBookmark1, pucBookmark2, plResult ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetCollationLang( UNSIGNED8 *pucLang, UNSIGNED16 *pusLen )
{
   static ADSGETCOLLATIONLANG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETCOLLATIONLANG_PTR) Ace32_GetProcAddress( "AdsGetCollationLang" );

   return ( pFunc ? pFunc( pucLang, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetConnectionType( ADSHANDLE hConnect, UNSIGNED16 *pusConnectType )
{
   static ADSGETCONNECTIONTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETCONNECTIONTYPE_PTR) Ace32_GetProcAddress( "AdsGetConnectionType" );

   return ( pFunc ? pFunc( hConnect, pusConnectType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetConnectionPath( ADSHANDLE hConnect, UNSIGNED8 *pucConnectionPath, UNSIGNED16 *pusLen )
{
   static ADSGETCONNECTIONPATH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETCONNECTIONPATH_PTR) Ace32_GetProcAddress( "AdsGetConnectionPath" );

   return ( pFunc ? pFunc( hConnect, pucConnectionPath, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetConnectionProperty( ADSHANDLE hConnect, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED32 *pulPropertyLen )
{
   static ADSGETCONNECTIONPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETCONNECTIONPROPERTY_PTR) Ace32_GetProcAddress( "AdsGetConnectionProperty" );

   return ( pFunc ? pFunc( hConnect, usPropertyID, pvProperty, pulPropertyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDate( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED16 *pusLen )
{
   static ADSGETDATE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDATE_PTR) Ace32_GetProcAddress( "AdsGetDate" );

   return ( pFunc ? pFunc( hTable, pucFldName, pucBuf, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDateFormat( UNSIGNED8 *pucFormat, UNSIGNED16 *pusLen )
{
   static ADSGETDATEFORMAT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDATEFORMAT_PTR) Ace32_GetProcAddress( "AdsGetDateFormat" );

   return ( pFunc ? pFunc( pucFormat, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDateFormat60( ADSHANDLE hConnect, UNSIGNED8 *pucFormat, UNSIGNED16 *pusLen )
{
   static ADSGETDATEFORMAT60_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDATEFORMAT60_PTR) Ace32_GetProcAddress( "AdsGetDateFormat60" );

   return ( pFunc ? pFunc( hConnect, pucFormat, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDecimals( UNSIGNED16 *pusDecimals )
{
   static ADSGETDECIMALS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDECIMALS_PTR) Ace32_GetProcAddress( "AdsGetDecimals" );

   return ( pFunc ? pFunc( pusDecimals ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDefault( UNSIGNED8 *pucDefault, UNSIGNED16 *pusLen )
{
   static ADSGETDEFAULT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDEFAULT_PTR) Ace32_GetProcAddress( "AdsGetDefault" );

   return ( pFunc ? pFunc( pucDefault, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDeleted( UNSIGNED16 *pbUseDeleted )
{
   static ADSGETDELETED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDELETED_PTR) Ace32_GetProcAddress( "AdsGetDeleted" );

   return ( pFunc ? pFunc( pbUseDeleted ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDouble( ADSHANDLE hTable, UNSIGNED8 *pucFldName, DOUBLE *pdValue )
{
   static ADSGETDOUBLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDOUBLE_PTR) Ace32_GetProcAddress( "AdsGetDouble" );

   return ( pFunc ? pFunc( hTable, pucFldName, pdValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetEpoch( UNSIGNED16 *pusCentury )
{
   static ADSGETEPOCH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETEPOCH_PTR) Ace32_GetProcAddress( "AdsGetEpoch" );

   return ( pFunc ? pFunc( pusCentury ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetErrorString( UNSIGNED32 ulErrCode, UNSIGNED8 *pucBuf, UNSIGNED16 *pusBufLen )
{
   static ADSGETERRORSTRING_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETERRORSTRING_PTR) Ace32_GetProcAddress( "AdsGetErrorString" );

   return ( pFunc ? pFunc( ulErrCode, pucBuf, pusBufLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetExact( UNSIGNED16 *pbExact )
{
   static ADSGETEXACT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETEXACT_PTR) Ace32_GetProcAddress( "AdsGetExact" );

   return ( pFunc ? pFunc( pbExact ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetExact22( ADSHANDLE hObj, UNSIGNED16 *pbExact )
{
   static ADSGETEXACT22_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETEXACT22_PTR) Ace32_GetProcAddress( "AdsGetExact22" );

   return ( pFunc ? pFunc( hObj, pbExact ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetField( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen, UNSIGNED16 usOption )
{
   static ADSGETFIELD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELD_PTR) Ace32_GetProcAddress( "AdsGetField" );

   return ( pFunc ? pFunc( hTable, pucFldName, pucBuf, pulLen, usOption ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldDecimals( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusDecimals )
{
   static ADSGETFIELDDECIMALS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELDDECIMALS_PTR) Ace32_GetProcAddress( "AdsGetFieldDecimals" );

   return ( pFunc ? pFunc( hTable, pucFldName, pusDecimals ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldLength( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulLength )
{
   static ADSGETFIELDLENGTH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELDLENGTH_PTR) Ace32_GetProcAddress( "AdsGetFieldLength" );

   return ( pFunc ? pFunc( hTable, pucFldName, pulLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldName( ADSHANDLE hTable, UNSIGNED16 usFld, UNSIGNED8 *pucName, UNSIGNED16 *pusBufLen )
{
   static ADSGETFIELDNAME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELDNAME_PTR) Ace32_GetProcAddress( "AdsGetFieldName" );

   return ( pFunc ? pFunc( hTable, usFld, pucName, pusBufLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldNum( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusNum )
{
   static ADSGETFIELDNUM_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELDNUM_PTR) Ace32_GetProcAddress( "AdsGetFieldNum" );

   return ( pFunc ? pFunc( hTable, pucFldName, pusNum ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldOffset( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulOffset )
{
   static ADSGETFIELDOFFSET_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELDOFFSET_PTR) Ace32_GetProcAddress( "AdsGetFieldOffset" );

   return ( pFunc ? pFunc( hTable, pucFldName, pulOffset ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFieldType( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusType )
{
   static ADSGETFIELDTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFIELDTYPE_PTR) Ace32_GetProcAddress( "AdsGetFieldType" );

   return ( pFunc ? pFunc( hTable, pucFldName, pusType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetFilter( ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 *pusLen )
{
   static ADSGETFILTER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETFILTER_PTR) Ace32_GetProcAddress( "AdsGetFilter" );

   return ( pFunc ? pFunc( hTable, pucFilter, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetHandleLong( ADSHANDLE hObj, UNSIGNED32 *pulVal )
{
   static ADSGETHANDLELONG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETHANDLELONG_PTR) Ace32_GetProcAddress( "AdsGetHandleLong" );

   return ( pFunc ? pFunc( hObj, pulVal ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetHandleType( ADSHANDLE hObj, UNSIGNED16 *pusType )
{
   static ADSGETHANDLETYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETHANDLETYPE_PTR) Ace32_GetProcAddress( "AdsGetHandleType" );

   return ( pFunc ? pFunc( hObj, pusType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexCondition( ADSHANDLE hIndex, UNSIGNED8 *pucExpr, UNSIGNED16 *pusLen )
{
   static ADSGETINDEXCONDITION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXCONDITION_PTR) Ace32_GetProcAddress( "AdsGetIndexCondition" );

   return ( pFunc ? pFunc( hIndex, pucExpr, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexExpr( ADSHANDLE hIndex, UNSIGNED8 *pucExpr, UNSIGNED16 *pusLen )
{
   static ADSGETINDEXEXPR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXEXPR_PTR) Ace32_GetProcAddress( "AdsGetIndexExpr" );

   return ( pFunc ? pFunc( hIndex, pucExpr, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexFilename( ADSHANDLE hIndex, UNSIGNED16 usOption, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
   static ADSGETINDEXFILENAME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXFILENAME_PTR) Ace32_GetProcAddress( "AdsGetIndexFilename" );

   return ( pFunc ? pFunc( hIndex, usOption, pucName, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexHandle( ADSHANDLE hTable, UNSIGNED8 *pucIndexOrder, ADSHANDLE *phIndex )
{
   static ADSGETINDEXHANDLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXHANDLE_PTR) Ace32_GetProcAddress( "AdsGetIndexHandle" );

   return ( pFunc ? pFunc( hTable, pucIndexOrder, phIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByOrder( ADSHANDLE hTable, UNSIGNED16 usOrderNum, ADSHANDLE *phIndex )
{
   static ADSGETINDEXHANDLEBYORDER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXHANDLEBYORDER_PTR) Ace32_GetProcAddress( "AdsGetIndexHandleByOrder" );

   return ( pFunc ? pFunc( hTable, usOrderNum, phIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByExpr( ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED32 ulDescending, ADSHANDLE *phIndex )
{
   static ADSGETINDEXHANDLEBYEXPR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXHANDLEBYEXPR_PTR) Ace32_GetProcAddress( "AdsGetIndexHandleByExpr" );

   return ( pFunc ? pFunc( hTable, pucExpr, ulDescending, phIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexName( ADSHANDLE hIndex, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
   static ADSGETINDEXNAME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXNAME_PTR) Ace32_GetProcAddress( "AdsGetIndexName" );

   return ( pFunc ? pFunc( hIndex, pucName, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexOrderByHandle( ADSHANDLE hIndex, UNSIGNED16 *pusIndexOrder )
{
   static ADSGETINDEXORDERBYHANDLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXORDERBYHANDLE_PTR) Ace32_GetProcAddress( "AdsGetIndexOrderByHandle" );

   return ( pFunc ? pFunc( hIndex, pusIndexOrder ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetJulian( ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED32 *plDate )
{
   static ADSGETJULIAN_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETJULIAN_PTR) Ace32_GetProcAddress( "AdsGetJulian" );

   return ( pFunc ? pFunc( hTable, pucFldName, plDate ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyCount( ADSHANDLE hIndex, UNSIGNED16 usFilterOption, UNSIGNED32 *pulCount )
{
   static ADSGETKEYCOUNT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETKEYCOUNT_PTR) Ace32_GetProcAddress( "AdsGetKeyCount" );

   return ( pFunc ? pFunc( hIndex, usFilterOption, pulCount ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyNum( ADSHANDLE hIndex, UNSIGNED16 usFilterOption, UNSIGNED32 *pulKey )
{
   static ADSGETKEYNUM_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETKEYNUM_PTR) Ace32_GetProcAddress( "AdsGetKeyNum" );

   return ( pFunc ? pFunc( hIndex, usFilterOption, pulKey ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyLength( ADSHANDLE hIndex, UNSIGNED16 *pusKeyLength )
{
   static ADSGETKEYLENGTH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETKEYLENGTH_PTR) Ace32_GetProcAddress( "AdsGetKeyLength" );

   return ( pFunc ? pFunc( hIndex, pusKeyLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetKeyType( ADSHANDLE hIndex, UNSIGNED16 *usKeyType )
{
   static ADSGETKEYTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETKEYTYPE_PTR) Ace32_GetProcAddress( "AdsGetKeyType" );

   return ( pFunc ? pFunc( hIndex, usKeyType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetLastError( UNSIGNED32 *pulErrCode, UNSIGNED8 *pucBuf, UNSIGNED16 *pusBufLen )
{
   static ADSGETLASTERROR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETLASTERROR_PTR) Ace32_GetProcAddress( "AdsGetLastError" );

   return ( pFunc ? pFunc( pulErrCode, pucBuf, pusBufLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetLastTableUpdate( ADSHANDLE hTable, UNSIGNED8 *pucDate, UNSIGNED16 *pusDateLen )
{
   static ADSGETLASTTABLEUPDATE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETLASTTABLEUPDATE_PTR) Ace32_GetProcAddress( "AdsGetLastTableUpdate" );

   return ( pFunc ? pFunc( hTable, pucDate, pusDateLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetLogical( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbValue )
{
   static ADSGETLOGICAL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETLOGICAL_PTR) Ace32_GetProcAddress( "AdsGetLogical" );

   return ( pFunc ? pFunc( hTable, pucFldName, pbValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetLong( ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED32 *plValue )
{
   static ADSGETLONG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETLONG_PTR) Ace32_GetProcAddress( "AdsGetLong" );

   return ( pFunc ? pFunc( hTable, pucFldName, plValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetLongLong( ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED64 *pqValue )
{
   static ADSGETLONGLONG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETLONGLONG_PTR) Ace32_GetProcAddress( "AdsGetLongLong" );

   return ( pFunc ? pFunc( hTable, pucFldName, pqValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetMemoBlockSize( ADSHANDLE hTable, UNSIGNED16 *pusBlockSize )
{
   static ADSGETMEMOBLOCKSIZE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETMEMOBLOCKSIZE_PTR) Ace32_GetProcAddress( "AdsGetMemoBlockSize" );

   return ( pFunc ? pFunc( hTable, pusBlockSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetMemoLength( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 *pulLength )
{
   static ADSGETMEMOLENGTH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETMEMOLENGTH_PTR) Ace32_GetProcAddress( "AdsGetMemoLength" );

   return ( pFunc ? pFunc( hTable, pucFldName, pulLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetMemoDataType( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pusType )
{
   static ADSGETMEMODATATYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETMEMODATATYPE_PTR) Ace32_GetProcAddress( "AdsGetMemoDataType" );

   return ( pFunc ? pFunc( hTable, pucFldName, pusType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetMilliseconds( ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED32 *plTime )
{
   static ADSGETMILLISECONDS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETMILLISECONDS_PTR) Ace32_GetProcAddress( "AdsGetMilliseconds" );

   return ( pFunc ? pFunc( hTable, pucFldName, plTime ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetMoney( ADSHANDLE hTbl, UNSIGNED8 *pucFldName, SIGNED64 *pqValue )
{
   static ADSGETMONEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETMONEY_PTR) Ace32_GetProcAddress( "AdsGetMoney" );

   return ( pFunc ? pFunc( hTbl, pucFldName, pqValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetActiveLinkInfo( ADSHANDLE hDBConn, UNSIGNED16 usLinkNum, UNSIGNED8 *pucLinkInfo, UNSIGNED16 *pusBufferLen )
{
   static ADSGETACTIVELINKINFO_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETACTIVELINKINFO_PTR) Ace32_GetProcAddress( "AdsGetActiveLinkInfo" );

   return ( pFunc ? pFunc( hDBConn, usLinkNum, pucLinkInfo, pusBufferLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetNumActiveLinks( ADSHANDLE hDBConn, UNSIGNED16 *pusNumLinks )
{
   static ADSGETNUMACTIVELINKS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETNUMACTIVELINKS_PTR) Ace32_GetProcAddress( "AdsGetNumActiveLinks" );

   return ( pFunc ? pFunc( hDBConn, pusNumLinks ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetNumFields( ADSHANDLE hTable, UNSIGNED16 *pusCount )
{
   static ADSGETNUMFIELDS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETNUMFIELDS_PTR) Ace32_GetProcAddress( "AdsGetNumFields" );

   return ( pFunc ? pFunc( hTable, pusCount ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetNumIndexes( ADSHANDLE hTable, UNSIGNED16 *pusNum )
{
   static ADSGETNUMINDEXES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETNUMINDEXES_PTR) Ace32_GetProcAddress( "AdsGetNumIndexes" );

   return ( pFunc ? pFunc( hTable, pusNum ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetNumFTSIndexes( ADSHANDLE hTable, UNSIGNED16 *pusNum )
{
   static ADSGETNUMFTSINDEXES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETNUMFTSINDEXES_PTR) Ace32_GetProcAddress( "AdsGetNumFTSIndexes" );

   return ( pFunc ? pFunc( hTable, pusNum ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetNumLocks( ADSHANDLE hTable, UNSIGNED16 *pusNum )
{
   static ADSGETNUMLOCKS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETNUMLOCKS_PTR) Ace32_GetProcAddress( "AdsGetNumLocks" );

   return ( pFunc ? pFunc( hTable, pusNum ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetNumOpenTables( UNSIGNED16 *pusNum )
{
   static ADSGETNUMOPENTABLES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETNUMOPENTABLES_PTR) Ace32_GetProcAddress( "AdsGetNumOpenTables" );

   return ( pFunc ? pFunc( pusNum ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetRecord( ADSHANDLE hTable, UNSIGNED8 *pucRec, UNSIGNED32 *pulLen )
{
   static ADSGETRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETRECORD_PTR) Ace32_GetProcAddress( "AdsGetRecord" );

   return ( pFunc ? pFunc( hTable, pucRec, pulLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordCount( ADSHANDLE hTable, UNSIGNED16 usFilterOption, UNSIGNED32 *pulCount )
{
   static ADSGETRECORDCOUNT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETRECORDCOUNT_PTR) Ace32_GetProcAddress( "AdsGetRecordCount" );

   return ( pFunc ? pFunc( hTable, usFilterOption, pulCount ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordNum( ADSHANDLE hTable, UNSIGNED16 usFilterOption, UNSIGNED32 *pulRec )
{
   static ADSGETRECORDNUM_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETRECORDNUM_PTR) Ace32_GetProcAddress( "AdsGetRecordNum" );

   return ( pFunc ? pFunc( hTable, usFilterOption, pulRec ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordLength( ADSHANDLE hTable, UNSIGNED32 *pulLength )
{
   static ADSGETRECORDLENGTH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETRECORDLENGTH_PTR) Ace32_GetProcAddress( "AdsGetRecordLength" );

   return ( pFunc ? pFunc( hTable, pulLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetRecordCRC( ADSHANDLE hTable, UNSIGNED32 *pulCRC, UNSIGNED32 ulOptions )
{
   static ADSGETRECORDCRC_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETRECORDCRC_PTR) Ace32_GetProcAddress( "AdsGetRecordCRC" );

   return ( pFunc ? pFunc( hTable, pulCRC, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetRelKeyPos( ADSHANDLE hIndex, DOUBLE *pdPos )
{
   static ADSGETRELKEYPOS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETRELKEYPOS_PTR) Ace32_GetProcAddress( "AdsGetRelKeyPos" );

   return ( pFunc ? pFunc( hIndex, pdPos ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetScope( ADSHANDLE hIndex, UNSIGNED16 usScopeOption, UNSIGNED8 *pucScope, UNSIGNED16 *pusBufLen )
{
   static ADSGETSCOPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSCOPE_PTR) Ace32_GetProcAddress( "AdsGetScope" );

   return ( pFunc ? pFunc( hIndex, usScopeOption, pucScope, pusBufLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetSearchPath( UNSIGNED8 *pucPath, UNSIGNED16 *pusLen )
{
   static ADSGETSEARCHPATH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSEARCHPATH_PTR) Ace32_GetProcAddress( "AdsGetSearchPath" );

   return ( pFunc ? pFunc( pucPath, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetServerName( ADSHANDLE hConnect, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
   static ADSGETSERVERNAME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSERVERNAME_PTR) Ace32_GetProcAddress( "AdsGetServerName" );

   return ( pFunc ? pFunc( hConnect, pucName, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetServerTime( ADSHANDLE hConnect, UNSIGNED8 *pucDateBuf, UNSIGNED16 *pusDateBufLen, SIGNED32 *plTime, UNSIGNED8 *pucTimeBuf, UNSIGNED16 *pusTimeBufLen )
{
   static ADSGETSERVERTIME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSERVERTIME_PTR) Ace32_GetProcAddress( "AdsGetServerTime" );

   return ( pFunc ? pFunc( hConnect, pucDateBuf, pusDateBufLen, plTime, pucTimeBuf, pusTimeBufLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetShort( ADSHANDLE hTable, UNSIGNED8 *pucFldName, SIGNED16 *psValue )
{
   static ADSGETSHORT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSHORT_PTR) Ace32_GetProcAddress( "AdsGetShort" );

   return ( pFunc ? pFunc( hTable, pucFldName, psValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetString( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 *pulLen, UNSIGNED16 usOption )
{
   static ADSGETSTRING_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSTRING_PTR) Ace32_GetProcAddress( "AdsGetString" );

   return ( pFunc ? pFunc( hTable, pucFldName, pucBuf, pulLen, usOption ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableAlias( ADSHANDLE hTable, UNSIGNED8 *pucAlias, UNSIGNED16 *pusLen )
{
   static ADSGETTABLEALIAS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLEALIAS_PTR) Ace32_GetProcAddress( "AdsGetTableAlias" );

   return ( pFunc ? pFunc( hTable, pucAlias, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableCharType( ADSHANDLE hTable, UNSIGNED16 *pusCharType )
{
   static ADSGETTABLECHARTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLECHARTYPE_PTR) Ace32_GetProcAddress( "AdsGetTableCharType" );

   return ( pFunc ? pFunc( hTable, pusCharType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableConnection( ADSHANDLE hTable, ADSHANDLE *phConnect )
{
   static ADSGETTABLECONNECTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLECONNECTION_PTR) Ace32_GetProcAddress( "AdsGetTableConnection" );

   return ( pFunc ? pFunc( hTable, phConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableFilename( ADSHANDLE hTable, UNSIGNED16 usOption, UNSIGNED8 *pucName, UNSIGNED16 *pusLen )
{
   static ADSGETTABLEFILENAME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLEFILENAME_PTR) Ace32_GetProcAddress( "AdsGetTableFilename" );

   return ( pFunc ? pFunc( hTable, usOption, pucName, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableHandle( UNSIGNED8 *pucName, ADSHANDLE *phTable )
{
   static ADSGETTABLEHANDLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLEHANDLE_PTR) Ace32_GetProcAddress( "AdsGetTableHandle" );

   return ( pFunc ? pFunc( pucName, phTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableHandle25( ADSHANDLE hConnect, UNSIGNED8 *pucName, ADSHANDLE *phTable )
{
   static ADSGETTABLEHANDLE25_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLEHANDLE25_PTR) Ace32_GetProcAddress( "AdsGetTableHandle25" );

   return ( pFunc ? pFunc( hConnect, pucName, phTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableLockType( ADSHANDLE hTable, UNSIGNED16 *pusLockType )
{
   static ADSGETTABLELOCKTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLELOCKTYPE_PTR) Ace32_GetProcAddress( "AdsGetTableLockType" );

   return ( pFunc ? pFunc( hTable, pusLockType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableMemoSize( ADSHANDLE hTable, UNSIGNED16 *pusMemoSize )
{
   static ADSGETTABLEMEMOSIZE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLEMEMOSIZE_PTR) Ace32_GetProcAddress( "AdsGetTableMemoSize" );

   return ( pFunc ? pFunc( hTable, pusMemoSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableOpenOptions( ADSHANDLE hTable, UNSIGNED32 *pulOptions )
{
   static ADSGETTABLEOPENOPTIONS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLEOPENOPTIONS_PTR) Ace32_GetProcAddress( "AdsGetTableOpenOptions" );

   return ( pFunc ? pFunc( hTable, pulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableRights( ADSHANDLE hTable, UNSIGNED16 *pusRights )
{
   static ADSGETTABLERIGHTS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLERIGHTS_PTR) Ace32_GetProcAddress( "AdsGetTableRights" );

   return ( pFunc ? pFunc( hTable, pusRights ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableType( ADSHANDLE hTable, UNSIGNED16 *pusType )
{
   static ADSGETTABLETYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLETYPE_PTR) Ace32_GetProcAddress( "AdsGetTableType" );

   return ( pFunc ? pFunc( hTable, pusType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTime( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED16 *pusLen )
{
   static ADSGETTIME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTIME_PTR) Ace32_GetProcAddress( "AdsGetTime" );

   return ( pFunc ? pFunc( hTable, pucFldName, pucBuf, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetVersion( UNSIGNED32 *pulMajor, UNSIGNED32 *pulMinor, UNSIGNED8 *pucLetter, UNSIGNED8 *pucDesc, UNSIGNED16 *pusDescLen )
{
   static ADSGETVERSION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETVERSION_PTR) Ace32_GetProcAddress( "AdsGetVersion" );

   return ( pFunc ? pFunc( pulMajor, pulMinor, pucLetter, pucDesc, pusDescLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGotoBookmark( ADSHANDLE hTable, ADSHANDLE hBookmark )
{
   static ADSGOTOBOOKMARK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGOTOBOOKMARK_PTR) Ace32_GetProcAddress( "AdsGotoBookmark" );

   return ( pFunc ? pFunc( hTable, hBookmark ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGotoBookmark60( ADSHANDLE hObj, UNSIGNED8 *pucBookmark )
{
   static ADSGOTOBOOKMARK60_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGOTOBOOKMARK60_PTR) Ace32_GetProcAddress( "AdsGotoBookmark60" );

   return ( pFunc ? pFunc( hObj, pucBookmark ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGotoBottom( ADSHANDLE hObj )
{
   static ADSGOTOBOTTOM_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGOTOBOTTOM_PTR) Ace32_GetProcAddress( "AdsGotoBottom" );

   return ( pFunc ? pFunc( hObj ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGotoRecord( ADSHANDLE hTable, UNSIGNED32 ulRec )
{
   static ADSGOTORECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGOTORECORD_PTR) Ace32_GetProcAddress( "AdsGotoRecord" );

   return ( pFunc ? pFunc( hTable, ulRec ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGotoTop( ADSHANDLE hObj )
{
   static ADSGOTOTOP_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGOTOTOP_PTR) Ace32_GetProcAddress( "AdsGotoTop" );

   return ( pFunc ? pFunc( hObj ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsImageToClipboard( ADSHANDLE hTable, UNSIGNED8 *pucFldName )
{
   static ADSIMAGETOCLIPBOARD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSIMAGETOCLIPBOARD_PTR) Ace32_GetProcAddress( "AdsImageToClipboard" );

   return ( pFunc ? pFunc( hTable, pucFldName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsInTransaction( ADSHANDLE hConnect, UNSIGNED16 *pbInTrans )
{
   static ADSINTRANSACTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSINTRANSACTION_PTR) Ace32_GetProcAddress( "AdsInTransaction" );

   return ( pFunc ? pFunc( hConnect, pbInTrans ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsEmpty( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbEmpty )
{
   static ADSISEMPTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISEMPTY_PTR) Ace32_GetProcAddress( "AdsIsEmpty" );

   return ( pFunc ? pFunc( hTable, pucFldName, pbEmpty ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsExprValid( ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 *pbValid )
{
   static ADSISEXPRVALID_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISEXPRVALID_PTR) Ace32_GetProcAddress( "AdsIsExprValid" );

   return ( pFunc ? pFunc( hTable, pucExpr, pbValid ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsFound( ADSHANDLE hObj, UNSIGNED16 *pbFound )
{
   static ADSISFOUND_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISFOUND_PTR) Ace32_GetProcAddress( "AdsIsFound" );

   return ( pFunc ? pFunc( hObj, pbFound ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexCompound( ADSHANDLE hIndex, UNSIGNED16 *pbCompound )
{
   static ADSISINDEXCOMPOUND_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXCOMPOUND_PTR) Ace32_GetProcAddress( "AdsIsIndexCompound" );

   return ( pFunc ? pFunc( hIndex, pbCompound ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexCustom( ADSHANDLE hIndex, UNSIGNED16 *pbCustom )
{
   static ADSISINDEXCUSTOM_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXCUSTOM_PTR) Ace32_GetProcAddress( "AdsIsIndexCustom" );

   return ( pFunc ? pFunc( hIndex, pbCustom ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexDescending( ADSHANDLE hIndex, UNSIGNED16 *pbDescending )
{
   static ADSISINDEXDESCENDING_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXDESCENDING_PTR) Ace32_GetProcAddress( "AdsIsIndexDescending" );

   return ( pFunc ? pFunc( hIndex, pbDescending ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexPrimaryKey( ADSHANDLE hIndex, UNSIGNED16 *pbPrimaryKey )
{
   static ADSISINDEXPRIMARYKEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXPRIMARYKEY_PTR) Ace32_GetProcAddress( "AdsIsIndexPrimaryKey" );

   return ( pFunc ? pFunc( hIndex, pbPrimaryKey ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexFTS( ADSHANDLE hIndex, UNSIGNED16 *pbFTS )
{
   static ADSISINDEXFTS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXFTS_PTR) Ace32_GetProcAddress( "AdsIsIndexFTS" );

   return ( pFunc ? pFunc( hIndex, pbFTS ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexUnique( ADSHANDLE hIndex, UNSIGNED16 *pbUnique )
{
   static ADSISINDEXUNIQUE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXUNIQUE_PTR) Ace32_GetProcAddress( "AdsIsIndexUnique" );

   return ( pFunc ? pFunc( hIndex, pbUnique ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordDeleted( ADSHANDLE hTable, UNSIGNED16 *pbDeleted )
{
   static ADSISRECORDDELETED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISRECORDDELETED_PTR) Ace32_GetProcAddress( "AdsIsRecordDeleted" );

   return ( pFunc ? pFunc( hTable, pbDeleted ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordEncrypted( ADSHANDLE hTable, UNSIGNED16 *pbEncrypted )
{
   static ADSISRECORDENCRYPTED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISRECORDENCRYPTED_PTR) Ace32_GetProcAddress( "AdsIsRecordEncrypted" );

   return ( pFunc ? pFunc( hTable, pbEncrypted ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordLocked( ADSHANDLE hTable, UNSIGNED32 ulRec, UNSIGNED16 *pbLocked )
{
   static ADSISRECORDLOCKED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISRECORDLOCKED_PTR) Ace32_GetProcAddress( "AdsIsRecordLocked" );

   return ( pFunc ? pFunc( hTable, ulRec, pbLocked ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordVisible( ADSHANDLE hObj, UNSIGNED16 *pbVisible )
{
   static ADSISRECORDVISIBLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISRECORDVISIBLE_PTR) Ace32_GetProcAddress( "AdsIsRecordVisible" );

   return ( pFunc ? pFunc( hObj, pbVisible ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsServerLoaded( UNSIGNED8 *pucServer, UNSIGNED16 *pbLoaded )
{
   static ADSISSERVERLOADED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISSERVERLOADED_PTR) Ace32_GetProcAddress( "AdsIsServerLoaded" );

   return ( pFunc ? pFunc( pucServer, pbLoaded ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsTableEncrypted( ADSHANDLE hTable, UNSIGNED16 *pbEncrypted )
{
   static ADSISTABLEENCRYPTED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISTABLEENCRYPTED_PTR) Ace32_GetProcAddress( "AdsIsTableEncrypted" );

   return ( pFunc ? pFunc( hTable, pbEncrypted ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsTableLocked( ADSHANDLE hTable, UNSIGNED16 *pbLocked )
{
   static ADSISTABLELOCKED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISTABLELOCKED_PTR) Ace32_GetProcAddress( "AdsIsTableLocked" );

   return ( pFunc ? pFunc( hTable, pbLocked ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsLocate( ADSHANDLE hTable, UNSIGNED8 *pucExpr, UNSIGNED16 bForward, UNSIGNED16 *pbFound )
{
   static ADSLOCATE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSLOCATE_PTR) Ace32_GetProcAddress( "AdsLocate" );

   return ( pFunc ? pFunc( hTable, pucExpr, bForward, pbFound ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsLockRecord( ADSHANDLE hTable, UNSIGNED32 ulRec )
{
   static ADSLOCKRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSLOCKRECORD_PTR) Ace32_GetProcAddress( "AdsLockRecord" );

   return ( pFunc ? pFunc( hTable, ulRec ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsLockTable( ADSHANDLE hTable )
{
   static ADSLOCKTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSLOCKTABLE_PTR) Ace32_GetProcAddress( "AdsLockTable" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsLookupKey( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 usKeyLen, UNSIGNED16 usDataType, UNSIGNED16 *pbFound )
{
   static ADSLOOKUPKEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSLOOKUPKEY_PTR) Ace32_GetProcAddress( "AdsLookupKey" );

   return ( pFunc ? pFunc( hIndex, pucKey, usKeyLen, usDataType, pbFound ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgConnect( UNSIGNED8 *pucServerName, UNSIGNED8 *pucUserName, UNSIGNED8 *pucPassword, ADSHANDLE *phMgmtHandle )
{
   static ADSMGCONNECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGCONNECT_PTR) Ace32_GetProcAddress( "AdsMgConnect" );

   return ( pFunc ? pFunc( pucServerName, pucUserName, pucPassword, phMgmtHandle ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgDisconnect( ADSHANDLE hMgmtHandle )
{
   static ADSMGDISCONNECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGDISCONNECT_PTR) Ace32_GetProcAddress( "AdsMgDisconnect" );

   return ( pFunc ? pFunc( hMgmtHandle ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetCommStats( ADSHANDLE hMgmtHandle, ADS_MGMT_COMM_STATS *pstCommStats, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETCOMMSTATS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETCOMMSTATS_PTR) Ace32_GetProcAddress( "AdsMgGetCommStats" );

   return ( pFunc ? pFunc( hMgmtHandle, pstCommStats, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgResetCommStats( ADSHANDLE hMgmtHandle )
{
   static ADSMGRESETCOMMSTATS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGRESETCOMMSTATS_PTR) Ace32_GetProcAddress( "AdsMgResetCommStats" );

   return ( pFunc ? pFunc( hMgmtHandle ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgDumpInternalTables( ADSHANDLE hMgmtHandle )
{
   static ADSMGDUMPINTERNALTABLES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGDUMPINTERNALTABLES_PTR) Ace32_GetProcAddress( "AdsMgDumpInternalTables" );

   return ( pFunc ? pFunc( hMgmtHandle ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetConfigInfo( ADSHANDLE hMgmtHandle, ADS_MGMT_CONFIG_PARAMS *pstConfigValues, UNSIGNED16 *pusConfigValuesStructSize, ADS_MGMT_CONFIG_MEMORY *pstConfigMemory, UNSIGNED16 *pusConfigMemoryStructSize )
{
   static ADSMGGETCONFIGINFO_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETCONFIGINFO_PTR) Ace32_GetProcAddress( "AdsMgGetConfigInfo" );

   return ( pFunc ? pFunc( hMgmtHandle, pstConfigValues, pusConfigValuesStructSize, pstConfigMemory, pusConfigMemoryStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetInstallInfo( ADSHANDLE hMgmtHandle, ADS_MGMT_INSTALL_INFO *pstInstallInfo, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETINSTALLINFO_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETINSTALLINFO_PTR) Ace32_GetProcAddress( "AdsMgGetInstallInfo" );

   return ( pFunc ? pFunc( hMgmtHandle, pstInstallInfo, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetActivityInfo( ADSHANDLE hMgmtHandle, ADS_MGMT_ACTIVITY_INFO *pstActivityInfo, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETACTIVITYINFO_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETACTIVITYINFO_PTR) Ace32_GetProcAddress( "AdsMgGetActivityInfo" );

   return ( pFunc ? pFunc( hMgmtHandle, pstActivityInfo, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetUserNames( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucFileName, ADS_MGMT_USER_INFO astUserInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETUSERNAMES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETUSERNAMES_PTR) Ace32_GetProcAddress( "AdsMgGetUserNames" );

   return ( pFunc ? pFunc( hMgmtHandle, pucFileName, astUserInfo, pusArrayLen, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetOpenTables( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber, ADS_MGMT_TABLE_INFO astOpenTableInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETOPENTABLES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETOPENTABLES_PTR) Ace32_GetProcAddress( "AdsMgGetOpenTables" );

   return ( pFunc ? pFunc( hMgmtHandle, pucUserName, usConnNumber, astOpenTableInfo, pusArrayLen, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetOpenIndexes( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucTableName, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber, ADS_MGMT_INDEX_INFO astOpenIndexInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETOPENINDEXES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETOPENINDEXES_PTR) Ace32_GetProcAddress( "AdsMgGetOpenIndexes" );

   return ( pFunc ? pFunc( hMgmtHandle, pucTableName, pucUserName, usConnNumber, astOpenIndexInfo, pusArrayLen, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetLocks( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucTableName, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber, ADS_MGMT_RECORD_INFO astRecordInfo[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETLOCKS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETLOCKS_PTR) Ace32_GetProcAddress( "AdsMgGetLocks" );

   return ( pFunc ? pFunc( hMgmtHandle, pucTableName, pucUserName, usConnNumber, astRecordInfo, pusArrayLen, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetServerType( ADSHANDLE hMgmtHandle, UNSIGNED16 *pusServerType )
{
   static ADSMGGETSERVERTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETSERVERTYPE_PTR) Ace32_GetProcAddress( "AdsMgGetServerType" );

   return ( pFunc ? pFunc( hMgmtHandle, pusServerType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgKillUser( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucUserName, UNSIGNED16 usConnNumber )
{
   static ADSMGKILLUSER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGKILLUSER_PTR) Ace32_GetProcAddress( "AdsMgKillUser" );

   return ( pFunc ? pFunc( hMgmtHandle, pucUserName, usConnNumber ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetWorkerThreadActivity( ADSHANDLE hMgmtHandle, ADS_MGMT_THREAD_ACTIVITY astWorkerThreadActivity[], UNSIGNED16 *pusArrayLen, UNSIGNED16 *pusStructSize )
{
   static ADSMGGETWORKERTHREADACTIVITY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETWORKERTHREADACTIVITY_PTR) Ace32_GetProcAddress( "AdsMgGetWorkerThreadActivity" );

   return ( pFunc ? pFunc( hMgmtHandle, astWorkerThreadActivity, pusArrayLen, pusStructSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsMgGetLockOwner( ADSHANDLE hMgmtHandle, UNSIGNED8 *pucTableName, UNSIGNED32 ulRecordNumber, ADS_MGMT_USER_INFO *pstUserInfo, UNSIGNED16 *pusStructSize, UNSIGNED16 *pusLockType )
{
   static ADSMGGETLOCKOWNER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSMGGETLOCKOWNER_PTR) Ace32_GetProcAddress( "AdsMgGetLockOwner" );

   return ( pFunc ? pFunc( hMgmtHandle, pucTableName, ulRecordNumber, pstUserInfo, pusStructSize, pusLockType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsNullTerminateStrings( UNSIGNED16 bNullTerminate )
{
   static ADSNULLTERMINATESTRINGS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSNULLTERMINATESTRINGS_PTR) Ace32_GetProcAddress( "AdsNullTerminateStrings" );

   return ( pFunc ? pFunc( bNullTerminate ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsOpenIndex( ADSHANDLE hTable, UNSIGNED8 *pucName, ADSHANDLE ahIndex[], UNSIGNED16 *pusArrayLen )
{
   static ADSOPENINDEX_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSOPENINDEX_PTR) Ace32_GetProcAddress( "AdsOpenIndex" );

   return ( pFunc ? pFunc( hTable, pucName, ahIndex, pusArrayLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsOpenTable( ADSHANDLE hConnect, UNSIGNED8 *pucName, UNSIGNED8 *pucAlias, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED32 ulOptions, ADSHANDLE *phTable )
{
   static ADSOPENTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSOPENTABLE_PTR) Ace32_GetProcAddress( "AdsOpenTable" );

   return ( pFunc ? pFunc( hConnect, pucName, pucAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, phTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsPackTable( ADSHANDLE hTable )
{
   static ADSPACKTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSPACKTABLE_PTR) Ace32_GetProcAddress( "AdsPackTable" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRecallRecord( ADSHANDLE hTable )
{
   static ADSRECALLRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSRECALLRECORD_PTR) Ace32_GetProcAddress( "AdsRecallRecord" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRecallAllRecords( ADSHANDLE hTable )
{
   static ADSRECALLALLRECORDS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSRECALLALLRECORDS_PTR) Ace32_GetProcAddress( "AdsRecallAllRecords" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRefreshRecord( ADSHANDLE hTable )
{
   static ADSREFRESHRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREFRESHRECORD_PTR) Ace32_GetProcAddress( "AdsRefreshRecord" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearProgressCallback( void )
{
   static ADSCLEARPROGRESSCALLBACK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARPROGRESSCALLBACK_PTR) Ace32_GetProcAddress( "AdsClearProgressCallback" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRegisterProgressCallback( UNSIGNED32 (WINAPI *lpfnCallback)( UNSIGNED16 usPercent ) )
{
   static ADSREGISTERPROGRESSCALLBACK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREGISTERPROGRESSCALLBACK_PTR) Ace32_GetProcAddress( "AdsRegisterProgressCallback" );

   return ( pFunc ? pFunc( lpfnCallback ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRegisterCallbackFunction( UNSIGNED32 (WINAPI *lpfnCallback)( UNSIGNED16 usPercent, UNSIGNED32 ulCallbackID ), UNSIGNED32 ulCallbackID )
{
   static ADSREGISTERCALLBACKFUNCTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREGISTERCALLBACKFUNCTION_PTR) Ace32_GetProcAddress( "AdsRegisterCallbackFunction" );

   return ( pFunc ? pFunc( lpfnCallback, ulCallbackID ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearCallbackFunction( void )
{
   static ADSCLEARCALLBACKFUNCTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARCALLBACKFUNCTION_PTR) Ace32_GetProcAddress( "AdsClearCallbackFunction" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsReindex( ADSHANDLE hObject )
{
   static ADSREINDEX_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREINDEX_PTR) Ace32_GetProcAddress( "AdsReindex" );

   return ( pFunc ? pFunc( hObject ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsReindex61( ADSHANDLE hObject, UNSIGNED32 ulPageSize )
{
   static ADSREINDEX61_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREINDEX61_PTR) Ace32_GetProcAddress( "AdsReindex61" );

   return ( pFunc ? pFunc( hObject, ulPageSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsReindexFTS( ADSHANDLE hObject, UNSIGNED32 ulPageSize )
{
   static ADSREINDEXFTS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREINDEXFTS_PTR) Ace32_GetProcAddress( "AdsReindexFTS" );

   return ( pFunc ? pFunc( hObject, ulPageSize ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsResetConnection( ADSHANDLE hConnect )
{
   static ADSRESETCONNECTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSRESETCONNECTION_PTR) Ace32_GetProcAddress( "AdsResetConnection" );

   return ( pFunc ? pFunc( hConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRollbackTransaction( ADSHANDLE hConnect )
{
   static ADSROLLBACKTRANSACTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSROLLBACKTRANSACTION_PTR) Ace32_GetProcAddress( "AdsRollbackTransaction" );

   return ( pFunc ? pFunc( hConnect ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSeek( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 usKeyLen, UNSIGNED16 usDataType, UNSIGNED16 usSeekType, UNSIGNED16 *pbFound )
{
   static ADSSEEK_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSEEK_PTR) Ace32_GetProcAddress( "AdsSeek" );

   return ( pFunc ? pFunc( hIndex, pucKey, usKeyLen, usDataType, usSeekType, pbFound ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSeekLast( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 usKeyLen, UNSIGNED16 usDataType, UNSIGNED16 *pbFound )
{
   static ADSSEEKLAST_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSEEKLAST_PTR) Ace32_GetProcAddress( "AdsSeekLast" );

   return ( pFunc ? pFunc( hIndex, pucKey, usKeyLen, usDataType, pbFound ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetBinary( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 usBinaryType, UNSIGNED32 ulTotalLength, UNSIGNED32 ulOffset, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
   static ADSSETBINARY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETBINARY_PTR) Ace32_GetProcAddress( "AdsSetBinary" );

   return ( pFunc ? pFunc( hTable, pucFldName, usBinaryType, ulTotalLength, ulOffset, pucBuf, ulLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetCollationLang( UNSIGNED8 *pucLang )
{
   static ADSSETCOLLATIONLANG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETCOLLATIONLANG_PTR) Ace32_GetProcAddress( "AdsSetCollationLang" );

   return ( pFunc ? pFunc( pucLang ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetDate( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucValue, UNSIGNED16 usLen )
{
   static ADSSETDATE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETDATE_PTR) Ace32_GetProcAddress( "AdsSetDate" );

   return ( pFunc ? pFunc( hObj, pucFldName, pucValue, usLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetDateFormat( UNSIGNED8 *pucFormat )
{
   static ADSSETDATEFORMAT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETDATEFORMAT_PTR) Ace32_GetProcAddress( "AdsSetDateFormat" );

   return ( pFunc ? pFunc( pucFormat ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetDateFormat60( ADSHANDLE hConnect, UNSIGNED8 *pucFormat )
{
   static ADSSETDATEFORMAT60_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETDATEFORMAT60_PTR) Ace32_GetProcAddress( "AdsSetDateFormat60" );

   return ( pFunc ? pFunc( hConnect, pucFormat ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetDecimals( UNSIGNED16 usDecimals )
{
   static ADSSETDECIMALS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETDECIMALS_PTR) Ace32_GetProcAddress( "AdsSetDecimals" );

   return ( pFunc ? pFunc( usDecimals ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetDefault( UNSIGNED8 *pucDefault )
{
   static ADSSETDEFAULT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETDEFAULT_PTR) Ace32_GetProcAddress( "AdsSetDefault" );

   return ( pFunc ? pFunc( pucDefault ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsShowDeleted( UNSIGNED16 bShowDeleted )
{
   static ADSSHOWDELETED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSHOWDELETED_PTR) Ace32_GetProcAddress( "AdsShowDeleted" );

   return ( pFunc ? pFunc( bShowDeleted ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetDouble( ADSHANDLE hObj, UNSIGNED8 *pucFldName, DOUBLE dValue )
{
   static ADSSETDOUBLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETDOUBLE_PTR) Ace32_GetProcAddress( "AdsSetDouble" );

   return ( pFunc ? pFunc( hObj, pucFldName, dValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetEmpty( ADSHANDLE hObj, UNSIGNED8 *pucFldName )
{
   static ADSSETEMPTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETEMPTY_PTR) Ace32_GetProcAddress( "AdsSetEmpty" );

   return ( pFunc ? pFunc( hObj, pucFldName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetEpoch( UNSIGNED16 usCentury )
{
   static ADSSETEPOCH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETEPOCH_PTR) Ace32_GetProcAddress( "AdsSetEpoch" );

   return ( pFunc ? pFunc( usCentury ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetExact( UNSIGNED16 bExact )
{
   static ADSSETEXACT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETEXACT_PTR) Ace32_GetProcAddress( "AdsSetExact" );

   return ( pFunc ? pFunc( bExact ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetExact22( ADSHANDLE hObj, UNSIGNED16 bExact )
{
   static ADSSETEXACT22_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETEXACT22_PTR) Ace32_GetProcAddress( "AdsSetExact22" );

   return ( pFunc ? pFunc( hObj, bExact ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetField( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
   static ADSSETFIELD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETFIELD_PTR) Ace32_GetProcAddress( "AdsSetField" );

   return ( pFunc ? pFunc( hObj, pucFldName, pucBuf, ulLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetFilter( ADSHANDLE hTable, UNSIGNED8 *pucFilter )
{
   static ADSSETFILTER_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETFILTER_PTR) Ace32_GetProcAddress( "AdsSetFilter" );

   return ( pFunc ? pFunc( hTable, pucFilter ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetHandleLong( ADSHANDLE hObj, UNSIGNED32 ulVal )
{
   static ADSSETHANDLELONG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETHANDLELONG_PTR) Ace32_GetProcAddress( "AdsSetHandleLong" );

   return ( pFunc ? pFunc( hObj, ulVal ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetJulian( ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED32 lDate )
{
   static ADSSETJULIAN_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETJULIAN_PTR) Ace32_GetProcAddress( "AdsSetJulian" );

   return ( pFunc ? pFunc( hObj, pucFldName, lDate ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetLogical( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED16 bValue )
{
   static ADSSETLOGICAL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETLOGICAL_PTR) Ace32_GetProcAddress( "AdsSetLogical" );

   return ( pFunc ? pFunc( hObj, pucFldName, bValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetLong( ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED32 lValue )
{
   static ADSSETLONG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETLONG_PTR) Ace32_GetProcAddress( "AdsSetLong" );

   return ( pFunc ? pFunc( hObj, pucFldName, lValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetLongLong( ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED64 qValue )
{
   static ADSSETLONGLONG_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETLONGLONG_PTR) Ace32_GetProcAddress( "AdsSetLongLong" );

   return ( pFunc ? pFunc( hObj, pucFldName, qValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetMilliseconds( ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED32 lTime )
{
   static ADSSETMILLISECONDS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETMILLISECONDS_PTR) Ace32_GetProcAddress( "AdsSetMilliseconds" );

   return ( pFunc ? pFunc( hObj, pucFldName, lTime ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetMoney( ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED64 qValue )
{
   static ADSSETMONEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETMONEY_PTR) Ace32_GetProcAddress( "AdsSetMoney" );

   return ( pFunc ? pFunc( hObj, pucFldName, qValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetRecord( ADSHANDLE hObj, UNSIGNED8 *pucRec, UNSIGNED32 ulLen )
{
   static ADSSETRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETRECORD_PTR) Ace32_GetProcAddress( "AdsSetRecord" );

   return ( pFunc ? pFunc( hObj, pucRec, ulLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetRelation( ADSHANDLE hTableParent, ADSHANDLE hIndexChild, UNSIGNED8 *pucExpr )
{
   static ADSSETRELATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETRELATION_PTR) Ace32_GetProcAddress( "AdsSetRelation" );

   return ( pFunc ? pFunc( hTableParent, hIndexChild, pucExpr ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetRelKeyPos( ADSHANDLE hIndex, DOUBLE dPos )
{
   static ADSSETRELKEYPOS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETRELKEYPOS_PTR) Ace32_GetProcAddress( "AdsSetRelKeyPos" );

   return ( pFunc ? pFunc( hIndex, dPos ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetScope( ADSHANDLE hIndex, UNSIGNED16 usScopeOption, UNSIGNED8 *pucScope, UNSIGNED16 usScopeLen, UNSIGNED16 usDataType )
{
   static ADSSETSCOPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETSCOPE_PTR) Ace32_GetProcAddress( "AdsSetScope" );

   return ( pFunc ? pFunc( hIndex, usScopeOption, pucScope, usScopeLen, usDataType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetScopedRelation( ADSHANDLE hTableParent, ADSHANDLE hIndexChild, UNSIGNED8 *pucExpr )
{
   static ADSSETSCOPEDRELATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETSCOPEDRELATION_PTR) Ace32_GetProcAddress( "AdsSetScopedRelation" );

   return ( pFunc ? pFunc( hTableParent, hIndexChild, pucExpr ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetSearchPath( UNSIGNED8 *pucPath )
{
   static ADSSETSEARCHPATH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETSEARCHPATH_PTR) Ace32_GetProcAddress( "AdsSetSearchPath" );

   return ( pFunc ? pFunc( pucPath ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetServerType( UNSIGNED16 usServerOptions )
{
   static ADSSETSERVERTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETSERVERTYPE_PTR) Ace32_GetProcAddress( "AdsSetServerType" );

   return ( pFunc ? pFunc( usServerOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetShort( ADSHANDLE hObj, UNSIGNED8 *pucFldName, SIGNED16 sValue )
{
   static ADSSETSHORT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETSHORT_PTR) Ace32_GetProcAddress( "AdsSetShort" );

   return ( pFunc ? pFunc( hObj, pucFldName, sValue ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetString( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
   static ADSSETSTRING_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETSTRING_PTR) Ace32_GetProcAddress( "AdsSetString" );

   return ( pFunc ? pFunc( hObj, pucFldName, pucBuf, ulLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetTime( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucValue, UNSIGNED16 usLen )
{
   static ADSSETTIME_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETTIME_PTR) Ace32_GetProcAddress( "AdsSetTime" );

   return ( pFunc ? pFunc( hObj, pucFldName, pucValue, usLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsShowError( UNSIGNED8 *pucTitle )
{
   static ADSSHOWERROR_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSHOWERROR_PTR) Ace32_GetProcAddress( "AdsShowError" );

   return ( pFunc ? pFunc( pucTitle ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSkip( ADSHANDLE hObj, SIGNED32 lRecs )
{
   static ADSSKIP_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSKIP_PTR) Ace32_GetProcAddress( "AdsSkip" );

   return ( pFunc ? pFunc( hObj, lRecs ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsThreadExit( void )
{
   static ADSTHREADEXIT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSTHREADEXIT_PTR) Ace32_GetProcAddress( "AdsThreadExit" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsUnlockRecord( ADSHANDLE hTable, UNSIGNED32 ulRec )
{
   static ADSUNLOCKRECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSUNLOCKRECORD_PTR) Ace32_GetProcAddress( "AdsUnlockRecord" );

   return ( pFunc ? pFunc( hTable, ulRec ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsUnlockTable( ADSHANDLE hTable )
{
   static ADSUNLOCKTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSUNLOCKTABLE_PTR) Ace32_GetProcAddress( "AdsUnlockTable" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsVerifyPassword( ADSHANDLE hTable, UNSIGNED16 *pusEnabled )
{
   static ADSVERIFYPASSWORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSVERIFYPASSWORD_PTR) Ace32_GetProcAddress( "AdsVerifyPassword" );

   return ( pFunc ? pFunc( hTable, pusEnabled ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsEncryptionEnabled( ADSHANDLE hTable, UNSIGNED16 *pusEnabled )
{
   static ADSISENCRYPTIONENABLED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISENCRYPTIONENABLED_PTR) Ace32_GetProcAddress( "AdsIsEncryptionEnabled" );

   return ( pFunc ? pFunc( hTable, pusEnabled ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsWriteAllRecords( void )
{
   static ADSWRITEALLRECORDS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSWRITEALLRECORDS_PTR) Ace32_GetProcAddress( "AdsWriteAllRecords" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsWriteRecord( ADSHANDLE hTable )
{
   static ADSWRITERECORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSWRITERECORD_PTR) Ace32_GetProcAddress( "AdsWriteRecord" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsZapTable( ADSHANDLE hTable )
{
   static ADSZAPTABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSZAPTABLE_PTR) Ace32_GetProcAddress( "AdsZapTable" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetAOF( ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 usOptions )
{
   static ADSSETAOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETAOF_PTR) Ace32_GetProcAddress( "AdsSetAOF" );

   return ( pFunc ? pFunc( hTable, pucFilter, usOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEvalAOF( ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 *pusOptLevel )
{
   static ADSEVALAOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEVALAOF_PTR) Ace32_GetProcAddress( "AdsEvalAOF" );

   return ( pFunc ? pFunc( hTable, pucFilter, pusOptLevel ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearAOF( ADSHANDLE hTable )
{
   static ADSCLEARAOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARAOF_PTR) Ace32_GetProcAddress( "AdsClearAOF" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRefreshAOF( ADSHANDLE hTable )
{
   static ADSREFRESHAOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREFRESHAOF_PTR) Ace32_GetProcAddress( "AdsRefreshAOF" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetAOF( ADSHANDLE hTable, UNSIGNED8 *pucFilter, UNSIGNED16 *pusLen )
{
   static ADSGETAOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETAOF_PTR) Ace32_GetProcAddress( "AdsGetAOF" );

   return ( pFunc ? pFunc( hTable, pucFilter, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetAOFOptLevel( ADSHANDLE hTable, UNSIGNED16 *pusOptLevel, UNSIGNED8 *pucNonOpt, UNSIGNED16 *pusLen )
{
   static ADSGETAOFOPTLEVEL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETAOFOPTLEVEL_PTR) Ace32_GetProcAddress( "AdsGetAOFOptLevel" );

   return ( pFunc ? pFunc( hTable, pusOptLevel, pucNonOpt, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsRecordInAOF( ADSHANDLE hTable, UNSIGNED32 ulRecordNum, UNSIGNED16 *pusIsInAOF )
{
   static ADSISRECORDINAOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISRECORDINAOF_PTR) Ace32_GetProcAddress( "AdsIsRecordInAOF" );

   return ( pFunc ? pFunc( hTable, ulRecordNum, pusIsInAOF ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCustomizeAOF( ADSHANDLE hTable, UNSIGNED32 ulNumRecords, UNSIGNED32 *pulRecords, UNSIGNED16 usOption )
{
   static ADSCUSTOMIZEAOF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCUSTOMIZEAOF_PTR) Ace32_GetProcAddress( "AdsCustomizeAOF" );

   return ( pFunc ? pFunc( hTable, ulNumRecords, pulRecords, usOption ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsInitRawKey( ADSHANDLE hIndex )
{
   static ADSINITRAWKEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSINITRAWKEY_PTR) Ace32_GetProcAddress( "AdsInitRawKey" );

   return ( pFunc ? pFunc( hIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsBuildRawKey( ADSHANDLE hIndex, UNSIGNED8 *pucKey, UNSIGNED16 *pusKeyLen )
{
   static ADSBUILDRAWKEY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSBUILDRAWKEY_PTR) Ace32_GetProcAddress( "AdsBuildRawKey" );

   return ( pFunc ? pFunc( hIndex, pucKey, pusKeyLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateSQLStatement( ADSHANDLE hConnect, ADSHANDLE *phStatement )
{
   static ADSCREATESQLSTATEMENT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATESQLSTATEMENT_PTR) Ace32_GetProcAddress( "AdsCreateSQLStatement" );

   return ( pFunc ? pFunc( hConnect, phStatement ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsPrepareSQL( ADSHANDLE hStatement, UNSIGNED8 *pucSQL )
{
   static ADSPREPARESQL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSPREPARESQL_PTR) Ace32_GetProcAddress( "AdsPrepareSQL" );

   return ( pFunc ? pFunc( hStatement, pucSQL ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsExecuteSQL( ADSHANDLE hStatement, ADSHANDLE *phCursor )
{
   static ADSEXECUTESQL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEXECUTESQL_PTR) Ace32_GetProcAddress( "AdsExecuteSQL" );

   return ( pFunc ? pFunc( hStatement, phCursor ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsExecuteSQLDirect( ADSHANDLE hStatement, UNSIGNED8 *pucSQL, ADSHANDLE *phCursor )
{
   static ADSEXECUTESQLDIRECT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSEXECUTESQLDIRECT_PTR) Ace32_GetProcAddress( "AdsExecuteSQLDirect" );

   return ( pFunc ? pFunc( hStatement, pucSQL, phCursor ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCloseSQLStatement( ADSHANDLE hStatement )
{
   static ADSCLOSESQLSTATEMENT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLOSESQLSTATEMENT_PTR) Ace32_GetProcAddress( "AdsCloseSQLStatement" );

   return ( pFunc ? pFunc( hStatement ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableRights( ADSHANDLE hStatement, UNSIGNED16 usCheckRights )
{
   static ADSSTMTSETTABLERIGHTS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTSETTABLERIGHTS_PTR) Ace32_GetProcAddress( "AdsStmtSetTableRights" );

   return ( pFunc ? pFunc( hStatement, usCheckRights ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableReadOnly( ADSHANDLE hStatement, UNSIGNED16 usReadOnly )
{
   static ADSSTMTSETTABLEREADONLY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTSETTABLEREADONLY_PTR) Ace32_GetProcAddress( "AdsStmtSetTableReadOnly" );

   return ( pFunc ? pFunc( hStatement, usReadOnly ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableLockType( ADSHANDLE hStatement, UNSIGNED16 usLockType )
{
   static ADSSTMTSETTABLELOCKTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTSETTABLELOCKTYPE_PTR) Ace32_GetProcAddress( "AdsStmtSetTableLockType" );

   return ( pFunc ? pFunc( hStatement, usLockType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableCharType( ADSHANDLE hStatement, UNSIGNED16 usCharType )
{
   static ADSSTMTSETTABLECHARTYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTSETTABLECHARTYPE_PTR) Ace32_GetProcAddress( "AdsStmtSetTableCharType" );

   return ( pFunc ? pFunc( hStatement, usCharType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableType( ADSHANDLE hStatement, UNSIGNED16 usTableType )
{
   static ADSSTMTSETTABLETYPE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTSETTABLETYPE_PTR) Ace32_GetProcAddress( "AdsStmtSetTableType" );

   return ( pFunc ? pFunc( hStatement, usTableType ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtConstrainUpdates( ADSHANDLE hStatement, UNSIGNED16 usConstrain )
{
   static ADSSTMTCONSTRAINUPDATES_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTCONSTRAINUPDATES_PTR) Ace32_GetProcAddress( "AdsStmtConstrainUpdates" );

   return ( pFunc ? pFunc( hStatement, usConstrain ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtEnableEncryption( ADSHANDLE hStatement, UNSIGNED8 *pucPassword )
{
   static ADSSTMTENABLEENCRYPTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTENABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsStmtEnableEncryption" );

   return ( pFunc ? pFunc( hStatement, pucPassword ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtDisableEncryption( ADSHANDLE hStatement )
{
   static ADSSTMTDISABLEENCRYPTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTDISABLEENCRYPTION_PTR) Ace32_GetProcAddress( "AdsStmtDisableEncryption" );

   return ( pFunc ? pFunc( hStatement ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTablePassword( ADSHANDLE hStatement, UNSIGNED8 *pucTableName, UNSIGNED8 *pucPassword )
{
   static ADSSTMTSETTABLEPASSWORD_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTSETTABLEPASSWORD_PTR) Ace32_GetProcAddress( "AdsStmtSetTablePassword" );

   return ( pFunc ? pFunc( hStatement, pucTableName, pucPassword ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtClearTablePasswords( ADSHANDLE hStatement )
{
   static ADSSTMTCLEARTABLEPASSWORDS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTCLEARTABLEPASSWORDS_PTR) Ace32_GetProcAddress( "AdsStmtClearTablePasswords" );

   return ( pFunc ? pFunc( hStatement ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtReadAllColumns( ADSHANDLE hStatement, UNSIGNED16 usReadColumns )
{
   static ADSSTMTREADALLCOLUMNS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTREADALLCOLUMNS_PTR) Ace32_GetProcAddress( "AdsStmtReadAllColumns" );

   return ( pFunc ? pFunc( hStatement, usReadColumns ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearSQLParams( ADSHANDLE hStatement )
{
   static ADSCLEARSQLPARAMS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARSQLPARAMS_PTR) Ace32_GetProcAddress( "AdsClearSQLParams" );

   return ( pFunc ? pFunc( hStatement ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetTimeStamp( ADSHANDLE hObj, UNSIGNED8 *pucFldName, UNSIGNED8 *pucBuf, UNSIGNED32 ulLen )
{
   static ADSSETTIMESTAMP_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETTIMESTAMP_PTR) Ace32_GetProcAddress( "AdsSetTimeStamp" );

   return ( pFunc ? pFunc( hObj, pucFldName, pucBuf, ulLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsClearSQLAbortFunc( void )
{
   static ADSCLEARSQLABORTFUNC_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCLEARSQLABORTFUNC_PTR) Ace32_GetProcAddress( "AdsClearSQLAbortFunc" );

   return ( pFunc ? pFunc() : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRegisterSQLAbortFunc( UNSIGNED32 (WINAPI *lpfnCallback)(void) )
{
   static ADSREGISTERSQLABORTFUNC_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREGISTERSQLABORTFUNC_PTR) Ace32_GetProcAddress( "AdsRegisterSQLAbortFunc" );

   return ( pFunc ? pFunc( lpfnCallback ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetNumParams( ADSHANDLE hStatement, UNSIGNED16 *pusNumParams )
{
   static ADSGETNUMPARAMS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETNUMPARAMS_PTR) Ace32_GetProcAddress( "AdsGetNumParams" );

   return ( pFunc ? pFunc( hStatement, pusNumParams ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetLastAutoinc( ADSHANDLE hObj, UNSIGNED32 *pulAutoIncVal )
{
   static ADSGETLASTAUTOINC_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETLASTAUTOINC_PTR) Ace32_GetProcAddress( "AdsGetLastAutoinc" );

   return ( pFunc ? pFunc( hObj, pulAutoIncVal ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexUserDefined( ADSHANDLE hIndex, UNSIGNED16 *pbUserDefined )
{
   static ADSISINDEXUSERDEFINED_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXUSERDEFINED_PTR) Ace32_GetProcAddress( "AdsIsIndexUserDefined" );

   return ( pFunc ? pFunc( hIndex, pbUserDefined ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRestructureTable( ADSHANDLE hObj, UNSIGNED8 *pucName, UNSIGNED8 *pucPassword, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED8 *pucAddFields, UNSIGNED8 *pucDeleteFields, UNSIGNED8 *pucChangeFields )
{
   static ADSRESTRUCTURETABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSRESTRUCTURETABLE_PTR) Ace32_GetProcAddress( "AdsRestructureTable" );

   return ( pFunc ? pFunc( hObj, pucName, pucPassword, usTableType, usCharType, usLockType, usCheckRights, pucAddFields, pucDeleteFields, pucChangeFields ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetSQLStatementHandle( ADSHANDLE hCursor, ADSHANDLE *phStmt )
{
   static ADSGETSQLSTATEMENTHANDLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSQLSTATEMENTHANDLE_PTR) Ace32_GetProcAddress( "AdsGetSQLStatementHandle" );

   return ( pFunc ? pFunc( hCursor, phStmt ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetSQLStatement( ADSHANDLE hStmt, UNSIGNED8 *pucSQL, UNSIGNED16 *pusLen )
{
   static ADSGETSQLSTATEMENT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETSQLSTATEMENT_PTR) Ace32_GetProcAddress( "AdsGetSQLStatement" );

   return ( pFunc ? pFunc( hStmt, pucSQL, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsFlushFileBuffers( ADSHANDLE hTable )
{
   static ADSFLUSHFILEBUFFERS_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSFLUSHFILEBUFFERS_PTR) Ace32_GetProcAddress( "AdsFlushFileBuffers" );

   return ( pFunc ? pFunc( hTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDDeployDatabase( UNSIGNED8 *pucDestination, UNSIGNED8 *pucDestinationPassword, UNSIGNED8 *pucSource, UNSIGNED8 *pucSourcePassword, UNSIGNED16 usServerTypes, UNSIGNED16 usValidateOption, UNSIGNED16 usBackupFiles, UNSIGNED32 ulOptions )
{
   static ADSDDDEPLOYDATABASE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDDEPLOYDATABASE_PTR) Ace32_GetProcAddress( "AdsDDDeployDatabase" );

   return ( pFunc ? pFunc( pucDestination, pucDestinationPassword, pucSource, pucSourcePassword, usServerTypes, usValidateOption, usBackupFiles, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsVerifySQL( ADSHANDLE hStatement, UNSIGNED8 *pucSQL )
{
   static ADSVERIFYSQL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSVERIFYSQL_PTR) Ace32_GetProcAddress( "AdsVerifySQL" );

   return ( pFunc ? pFunc( hStatement, pucSQL ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDisableUniqueEnforcement( ADSHANDLE hConnection )
{
   static ADSDISABLEUNIQUEENFORCEMENT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDISABLEUNIQUEENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsDisableUniqueEnforcement" );

   return ( pFunc ? pFunc( hConnection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEnableUniqueEnforcement( ADSHANDLE hConnection )
{
   static ADSENABLEUNIQUEENFORCEMENT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSENABLEUNIQUEENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsEnableUniqueEnforcement" );

   return ( pFunc ? pFunc( hConnection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDisableRI( ADSHANDLE hConnection )
{
   static ADSDISABLERI_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDISABLERI_PTR) Ace32_GetProcAddress( "AdsDisableRI" );

   return ( pFunc ? pFunc( hConnection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEnableRI( ADSHANDLE hConnection )
{
   static ADSENABLERI_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSENABLERI_PTR) Ace32_GetProcAddress( "AdsEnableRI" );

   return ( pFunc ? pFunc( hConnection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDisableAutoIncEnforcement( ADSHANDLE hConnection )
{
   static ADSDISABLEAUTOINCENFORCEMENT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDISABLEAUTOINCENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsDisableAutoIncEnforcement" );

   return ( pFunc ? pFunc( hConnection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsEnableAutoIncEnforcement( ADSHANDLE hConnection )
{
   static ADSENABLEAUTOINCENFORCEMENT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSENABLEAUTOINCENFORCEMENT_PTR) Ace32_GetProcAddress( "AdsEnableAutoIncEnforcement" );

   return ( pFunc ? pFunc( hConnection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRollbackTransaction80( ADSHANDLE hConnect, UNSIGNED8 *pucSavepoint, UNSIGNED32 ulOptions )
{
   static ADSROLLBACKTRANSACTION80_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSROLLBACKTRANSACTION80_PTR) Ace32_GetProcAddress( "AdsRollbackTransaction80" );

   return ( pFunc ? pFunc( hConnect, pucSavepoint, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateSavepoint( ADSHANDLE hConnect, UNSIGNED8 *pucSavepoint, UNSIGNED32 ulOptions )
{
   static ADSCREATESAVEPOINT_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATESAVEPOINT_PTR) Ace32_GetProcAddress( "AdsCreateSavepoint" );

   return ( pFunc ? pFunc( hConnect, pucSavepoint, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDFreeTable( UNSIGNED8 *pucTableName, UNSIGNED8 *pucPassword )
{
   static ADSDDFREETABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDFREETABLE_PTR) Ace32_GetProcAddress( "AdsDDFreeTable" );

   return ( pFunc ? pFunc( pucTableName, pucPassword ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDSetIndexProperty( ADSHANDLE hAdminConn, UNSIGNED8 *pucTableName, UNSIGNED8 *pucIndexName, UNSIGNED16 usPropertyID, VOID *pvProperty, UNSIGNED16 usPropertyLen )
{
   static ADSDDSETINDEXPROPERTY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDSETINDEXPROPERTY_PTR) Ace32_GetProcAddress( "AdsDDSetIndexProperty" );

   return ( pFunc ? pFunc( hAdminConn, pucTableName, pucIndexName, usPropertyID, pvProperty, usPropertyLen ) : 0 );
}


#if ADS_LIB_VERSION >= 900

UNSIGNED32 ENTRYPOINT AdsSetIndexDirection( ADSHANDLE hIndex, UNSIGNED16 usReverseDirection )
{
   static ADSSETINDEXDIRECTION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETINDEXDIRECTION_PTR) Ace32_GetProcAddress( "AdsSetIndexDirection" );

   return ( pFunc ? pFunc( hIndex, usReverseDirection ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetDataLength( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED32 ulOptions, UNSIGNED32 *pulLength )
{
   static ADSGETDATALENGTH_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETDATALENGTH_PTR) Ace32_GetProcAddress( "AdsGetDataLength" );

   return ( pFunc ? pFunc( hTable, pucFldName, ulOptions, pulLength ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetIndexCollation( ADSHANDLE hIndex, UNSIGNED8 *pucCollation, UNSIGNED16 *pusLen )
{
   static ADSGETINDEXCOLLATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETINDEXCOLLATION_PTR) Ace32_GetProcAddress( "AdsGetIndexCollation" );

   return ( pFunc ? pFunc( hIndex, pucCollation, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetTableCollation( ADSHANDLE hTbl, UNSIGNED8 *pucCollation, UNSIGNED16 *pusLen )
{
   static ADSGETTABLECOLLATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETTABLECOLLATION_PTR) Ace32_GetProcAddress( "AdsGetTableCollation" );

   return ( pFunc ? pFunc( hTbl, pucCollation, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsNullable( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbNullable )
{
   static ADSISNULLABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISNULLABLE_PTR) Ace32_GetProcAddress( "AdsIsNullable" );

   return ( pFunc ? pFunc( hTable, pucFldName, pbNullable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexNullable( ADSHANDLE hIndex, UNSIGNED16 *pbNullable )
{
   static ADSISINDEXNULLABLE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXNULLABLE_PTR) Ace32_GetProcAddress( "AdsIsIndexNullable" );

   return ( pFunc ? pFunc( hIndex, pbNullable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetNull( ADSHANDLE hTable, UNSIGNED8 *pucFldName )
{
   static ADSSETNULL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETNULL_PTR) Ace32_GetProcAddress( "AdsSetNull" );

   return ( pFunc ? pFunc( hTable, pucFldName ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsNull( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbNull )
{
   static ADSISNULL_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISNULL_PTR) Ace32_GetProcAddress( "AdsIsNull" );

   return ( pFunc ? pFunc( hTable, pucFldName, pbNull ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsFieldBinary( ADSHANDLE hTable, UNSIGNED8 *pucFldName, UNSIGNED16 *pbBinary )
{
   static ADSISFIELDBINARY_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISFIELDBINARY_PTR) Ace32_GetProcAddress( "AdsIsFieldBinary" );

   return ( pFunc ? pFunc( hTable, pucFldName, pbBinary ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRegisterUDF( ADSHANDLE hObj, UNSIGNED16 usType, UNSIGNED32 (WINAPI *lpfnUDF)(void) )
{
   static ADSREGISTERUDF_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSREGISTERUDF_PTR) Ace32_GetProcAddress( "AdsRegisterUDF" );

   return ( pFunc ? pFunc( hObj, usType, lpfnUDF ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsStmtSetTableCollation( ADSHANDLE hStatement, UNSIGNED8 *pucCollation )
{
   static ADSSTMTSETTABLECOLLATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSTMTSETTABLECOLLATION_PTR) Ace32_GetProcAddress( "AdsStmtSetTableCollation" );

   return ( pFunc ? pFunc( hStatement, pucCollation ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSkipUnique( ADSHANDLE hIndex, SIGNED32 lRecs )
{
   static ADSSKIPUNIQUE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSKIPUNIQUE_PTR) Ace32_GetProcAddress( "AdsSkipUnique" );

   return ( pFunc ? pFunc( hIndex, lRecs ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsSetCollation( ADSHANDLE hConnect, UNSIGNED8 *pucCollation )
{
   static ADSSETCOLLATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSSETCOLLATION_PTR) Ace32_GetProcAddress( "AdsSetCollation" );

   return ( pFunc ? pFunc( hConnect, pucCollation ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsIsIndexCandidate( ADSHANDLE hIndex, UNSIGNED16 *pbCandidate )
{
   static ADSISINDEXCANDIDATE_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSISINDEXCANDIDATE_PTR) Ace32_GetProcAddress( "AdsIsIndexCandidate" );

   return ( pFunc ? pFunc( hIndex, pbCandidate ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsGetCollation( ADSHANDLE hConnect, UNSIGNED8 *pucCollation, UNSIGNED16 *pusLen )
{
   static ADSGETCOLLATION_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSGETCOLLATION_PTR) Ace32_GetProcAddress( "AdsGetCollation" );

   return ( pFunc ? pFunc( hConnect, pucCollation, pusLen ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCancelUpdate90( ADSHANDLE hTable, UNSIGNED32 ulOptions )
{
   static ADSCANCELUPDATE90_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCANCELUPDATE90_PTR) Ace32_GetProcAddress( "AdsCancelUpdate90" );

   return ( pFunc ? pFunc( hTable, ulOptions ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateIndex90( ADSHANDLE hObj, UNSIGNED8 *pucFileName, UNSIGNED8 *pucTag, UNSIGNED8 *pucExpr, UNSIGNED8 *pucCondition, UNSIGNED8 *pucWhile, UNSIGNED32 ulOptions, UNSIGNED32 ulPageSize, UNSIGNED8 *pucCollation, ADSHANDLE *phIndex )
{
   static ADSCREATEINDEX90_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATEINDEX90_PTR) Ace32_GetProcAddress( "AdsCreateIndex90" );

   return ( pFunc ? pFunc( hObj, pucFileName, pucTag, pucExpr, pucCondition, pucWhile, ulOptions, ulPageSize, pucCollation, phIndex ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsCreateTable90( ADSHANDLE hConnection, UNSIGNED8 *pucName, UNSIGNED8 *pucDBObjName, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED16 usMemoSize, UNSIGNED8 *pucFields, UNSIGNED32 ulOptions, UNSIGNED8 *pucCollation, ADSHANDLE *phTable )
{
   static ADSCREATETABLE90_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSCREATETABLE90_PTR) Ace32_GetProcAddress( "AdsCreateTable90" );

   return ( pFunc ? pFunc( hConnection, pucName, pucDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, pucFields, ulOptions, pucCollation, phTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsDDAddTable90( ADSHANDLE hDictionary, UNSIGNED8 *pucTableName, UNSIGNED8 *pucTablePath, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED8 *pucIndexFiles, UNSIGNED8 *pucComments, UNSIGNED8 *pucCollation )
{
   static ADSDDADDTABLE90_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSDDADDTABLE90_PTR) Ace32_GetProcAddress( "AdsDDAddTable90" );

   return ( pFunc ? pFunc( hDictionary, pucTableName, pucTablePath, usTableType, usCharType, pucIndexFiles, pucComments, pucCollation ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsOpenTable90( ADSHANDLE hConnect, UNSIGNED8 *pucName, UNSIGNED8 *pucAlias, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED32 ulOptions, UNSIGNED8 *pucCollation, ADSHANDLE *phTable )
{
   static ADSOPENTABLE90_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSOPENTABLE90_PTR) Ace32_GetProcAddress( "AdsOpenTable90" );

   return ( pFunc ? pFunc( hConnect, pucName, pucAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, pucCollation, phTable ) : 0 );
}

UNSIGNED32 ENTRYPOINT AdsRestructureTable90( ADSHANDLE hObj, UNSIGNED8 *pucName, UNSIGNED8 *pucPassword, UNSIGNED16 usTableType, UNSIGNED16 usCharType, UNSIGNED16 usLockType, UNSIGNED16 usCheckRights, UNSIGNED8 *pucAddFields, UNSIGNED8 *pucDeleteFields, UNSIGNED8 *pucChangeFields, UNSIGNED8 *pucCollation )
{
   static ADSRESTRUCTURETABLE90_PTR pFunc = NULL;

   if ( !pFunc )
      pFunc = (ADSRESTRUCTURETABLE90_PTR) Ace32_GetProcAddress( "AdsRestructureTable90" );

   return ( pFunc ? pFunc( hObj, pucName, pucPassword, usTableType, usCharType, usLockType, usCheckRights, pucAddFields, pucDeleteFields, pucChangeFields, pucCollation ) : 0 );
}

#endif
