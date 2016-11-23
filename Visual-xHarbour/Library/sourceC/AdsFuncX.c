/*
 * $Id: adsfunc.c 17685 2012-06-17 12:50:22Z vszakats $
 */

/*
 * Harbour Project source code:
 * Advantage Database Server RDD (additional functions)
 *
 * Copyright 2008 Viktor Szakats (harbour syenar.net) (cleanups)
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
 * www - http://harbour-project.org
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


#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbstack.h"
#include "hbdate.h"

#include "rddsys.ch"
#include "rddads.h"

#define HARBOUR_MAX_RDD_FILTER_LENGTH     256
#define MAX_STR_LEN                       255
#define ADS_MAX_PARAMDEF_LEN              2048

extern ADSHANDLE hb_ads_hConnect;

HB_FUNC( ADSCONNECT101 )
{
#if ADS_LIB_VERSION >= 1010
   UNSIGNED32 uRes;
   ADSHANDLE hConnect = 0;
   hb_stornint( uRes, -1);

   uRes = AdsConnect101( ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucConnectString */,
                         ( ADSHANDLE * ) hb_parni( 2 ) /* phConnectOptions */,
                         &hConnect );
   if( uRes == AE_SUCCESS )
   {
      hb_ads_hConnect = hConnect;           /* set new default */

      hb_stornint( hConnect, 3 );

      hb_retl( HB_TRUE );
   }
   else
   {
      hb_stornint( uRes, 4 );
      hb_retl( HB_FALSE );
   }
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDCREATE101 )
{
#if ADS_LIB_VERSION >= 1010
   ADSHANDLE hConnect = 0;

   if( AdsDDCreate101( ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucConnectString */,
                       ( ADSHANDLE * ) hb_parni( 2 ) /* phConnectOptions */,
                       &hConnect ) == AE_SUCCESS )
   {
      hb_ads_hConnect = hConnect;           /* set new default */

      hb_stornint( hConnect, 3 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
#else
   #error "needs newer ads files, please update ace.h"
   hb_retl( HB_FALSE );
#endif
}

// to recover a table bound to an .add that no longer exists
HB_FUNC( ADSDDFREETABLE )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDFreeTable( ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucTableName */,
                            ( HB_ISCHAR( 2 ) ? ( UNSIGNED8 * ) hb_parcx( 2 ) : NULL ) /* pucPassword */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDEXECUTESQLDIRECT )
{
   HB_BOOL fResult = HB_FALSE;
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 2 );

   if( hConnect && HB_ISCHAR( 1 ) )
   {
      ADSHANDLE adsStatementHandle = 0;
      ADSHANDLE hCursor = -1;

      if( AdsCreateSQLStatement( hConnect, &adsStatementHandle ) == AE_SUCCESS )
      {
         // nothing works here with with sp_SetDDEncryptionType(); AdsExecuteSQLDirect() always fails, yet it does change encryption to AES256, but disables "Encrypt Data Dictionary Files"
         // seems to work with other SP like sp_SetApplicationID()
         if( AdsExecuteSQLDirect( adsStatementHandle           /* hStatement */,
                                  ( UNSIGNED8 * ) hb_parc( 1 ) /* pucSQL */,
                                  &hCursor                     /* phCursor */ ) == AE_SUCCESS )
         {
            fResult = HB_TRUE;
         }
         else
         {
            HB_TRACE(HB_TR_DEBUG, ("AdsDDExecuteSQLDirect() error"));
         }
         hb_stornint( hCursor, 4 );
      }
      else
      {
         HB_TRACE(HB_TR_DEBUG, ("AdsDDCreateSQLStatement error"));
      }
      hb_stornint( adsStatementHandle, 3 );
      AdsCloseSQLStatement( adsStatementHandle );
   }
   hb_retl( fResult );
}

HB_FUNC( ADSDDGETDATABASEPROPERTY )
{
   UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 1 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 2 );

   switch( ulProperty )
   {
      /* String properties */
      case ADS_DD_COMMENT:
      case ADS_DD_DEFAULT_TABLE_PATH:
      case ADS_DD_USER_DEFINED_PROP:
      case ADS_DD_TEMP_TABLE_PATH:
      case ADS_DD_VERSION:
      case ADS_DD_ENCRYPT_TABLE_PASSWORD:
#if ADS_LIB_VERSION >= 710
      case ADS_DD_FTS_DELIMITERS:
      case ADS_DD_FTS_NOISE:
      case ADS_DD_FTS_DROP_CHARS:
      case ADS_DD_FTS_CONDITIONAL_CHARS:
      case ADS_DD_LOGINS_DISABLED_ERRSTR:
#endif
      {
         char sBuffer[ ADS_MAX_PARAMDEF_LEN ];
         UNSIGNED16 ulLength = sizeof( sBuffer );

         if( AdsDDGetDatabaseProperty( hConnect,
                                       ulProperty,
                                       &sBuffer,
                                       &ulLength ) != AE_SUCCESS )
         {
            /* TODO: Better error handling. */
            sBuffer[ 0 ] = '\0';
            ulLength = 0;
         }
         hb_retclen( sBuffer, ulLength );
         break;
      }
      /* Boolean properties */
      case ADS_DD_LOG_IN_REQUIRED:
      case ADS_DD_VERIFY_ACCESS_RIGHTS:
      case ADS_DD_ENCRYPT_NEW_TABLE:
#if ADS_LIB_VERSION >= 710
      case ADS_DD_ENCRYPTED:
      case ADS_DD_LOGINS_DISABLED:
#endif
#if ADS_LIB_VERSION >= 800
      case ADS_DD_ENCRYPT_INDEXES:
      case ADS_DD_ENCRYPT_COMMUNICATION:
#endif
      {
         UNSIGNED16 ulBuffer;
         UNSIGNED16 ulLength = sizeof( ulBuffer );

         AdsDDGetDatabaseProperty( hConnect,
                                   ulProperty,
                                   &ulBuffer,
                                   &ulLength );
         hb_retl( ulBuffer != 0 );
         break;
      }
      /* Integer properties */
#if ADS_LIB_VERSION >= 620
      case ADS_DD_VERSION_MAJOR:
      case ADS_DD_VERSION_MINOR:
      {
         UNSIGNED16 ulBuffer;
         UNSIGNED16 ulLength = sizeof( ulBuffer );

         AdsDDGetDatabaseProperty( hConnect,
                                   ulProperty,
                                   &ulBuffer,
                                   &ulLength );
         hb_retni( ulBuffer );
         break;
      }
#endif
      /* 4-byte properties */
#if ADS_LIB_VERSION >= 1010
      case ADS_DD_DATA_ENCRYPTION_TYPE:       // returns ADS_ENCRYPTION_RC4 (3), ADS_ENCRYPTION_AES128 (5), or ADS_ENCRYPTION_AES256 (6)
      case ADS_DD_QUERY_VIA_ROOT:             // return ADS_DD_QVR_OPT_QUERY (0x01) or ADS_DD_QVR_OPT_PROCEDURE (0x02)
      {
         UNSIGNED32 ulBuffer;
         UNSIGNED16 ulLength = sizeof( ulBuffer );

         AdsDDGetDatabaseProperty( hConnect,
                                   ulProperty,
                                   &ulBuffer,
                                   &ulLength );
         hb_retni( ulBuffer );
         break;
      }
#endif
   }
}

HB_FUNC( ADSDDSETDATABASEPROPERTY )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED16 ulBuffer;
   UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 1 );
   PHB_ITEM pParam = hb_param( 2, HB_IT_ANY );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 3 );

   switch( ulProperty )
   {
      /* String properties (NULL accepted) */
      case ADS_DD_COMMENT:
      case ADS_DD_DEFAULT_TABLE_PATH:
      case ADS_DD_USER_DEFINED_PROP:
      case ADS_DD_TEMP_TABLE_PATH:
      case ADS_DD_ADMIN_PASSWORD:
      case ADS_DD_ENCRYPT_TABLE_PASSWORD:
      {
         ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                              ulProperty,
                                              HB_IS_STRING( pParam ) ? hb_itemGetCPtr( pParam ) : NULL,
                                              ( UNSIGNED16 ) hb_itemGetCLen( pParam ) + 1 );
         break;
      }
      /* String properties (NULL not accepted) */
#if ADS_LIB_VERSION >= 710
      case ADS_DD_FTS_DELIMITERS:
      case ADS_DD_FTS_NOISE:
      case ADS_DD_FTS_DROP_CHARS:
      case ADS_DD_FTS_CONDITIONAL_CHARS:
      case ADS_DD_LOGINS_DISABLED_ERRSTR:
      {
         ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                              ulProperty,
                                              hb_itemGetCPtr( pParam ),
                                              ( UNSIGNED16 ) hb_itemGetCLen( pParam ) + 1 );
         break;
      }
#endif
      /* Boolean properties */
      case ADS_DD_LOG_IN_REQUIRED:
      case ADS_DD_VERIFY_ACCESS_RIGHTS:
      case ADS_DD_ENCRYPT_NEW_TABLE:
      case ADS_DD_ENABLE_INTERNET:
#if ADS_LIB_VERSION >= 710
      case ADS_DD_LOGINS_DISABLED:
#endif
#if ADS_LIB_VERSION >= 800
      case ADS_DD_DISABLE_DLL_CACHING:
      case ADS_DD_ENCRYPT_INDEXES:
      case ADS_DD_ENCRYPT_COMMUNICATION:
#endif
      {
         ulBuffer = ( UNSIGNED16 ) hb_itemGetL( pParam );
         ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                              ulProperty,
                                              &ulBuffer,
                                              sizeof( ulBuffer ) );
         break;
      }
      /* Integer properties */
      case ADS_DD_MAX_FAILED_ATTEMPTS:
      case ADS_DD_INTERNET_SECURITY_LEVEL:
#if ADS_LIB_VERSION >= 620
      case ADS_DD_VERSION_MAJOR:
      case ADS_DD_VERSION_MINOR:
#endif
      {
         if( HB_IS_NUMERIC( pParam ) )
         {
            ulBuffer = ( UNSIGNED16 ) hb_itemGetNI( pParam );
            ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                                 ulProperty,
                                                 &ulBuffer,
                                                 sizeof( ulBuffer ) );
         }
         else
         {
            ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                                 ulProperty,
                                                 NULL,
                                                 0 );
         }
         break;
      }
      /* 4-byte properties */
#if ADS_LIB_VERSION >= 1010
      // ADS_DD_DATA_ENCRYPTION_TYPE: can only be changed via sp_SetDDEncryptionType stored procedure
      case ADS_DD_QUERY_VIA_ROOT:
      {
         if( HB_IS_NUMERIC( pParam ) )
         {
            ulBuffer = ( UNSIGNED32 ) hb_itemGetNI( pParam );
            ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                                 ulProperty,
                                                 &ulBuffer,
                                                 sizeof( ulBuffer ) );
         }
         else
         {
            ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                                 ulProperty,
                                                 NULL,
                                                 0 );
         }
         break;
      }
#endif
      default:
      {
         ulRetVal = ( UNSIGNED32 ) ~AE_SUCCESS;
         break;
      }
   }

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDFINDOBJECT )
{
#if ADS_LIB_VERSION >= 600
   UNSIGNED32 ulRetVal;
   UNSIGNED16 ulObjectType = ( UNSIGNED16 ) hb_parni( 1 );
   UNSIGNED8  ucParentName = ( UNSIGNED8 ) hb_parcx( 2 );
   UNSIGNED8  ucObjectName[ ADS_MAX_OBJECT_NAME ];
   UNSIGNED16 usObjectNameLen = sizeof( ucObjectName );
#if ADS_LIB_VERSION >= 900
   ADSHANDLE  sHandle = 0;
#else
   SIGNED32   sHandle = 0;
#endif
   PHB_ITEM   pitmDir;
   ADSHANDLE  hConnect = HB_ADS_PARCONNECTION( 3 );

   pitmDir = hb_itemArrayNew( 0 );

   ulRetVal = AdsDDFindFirstObject( hConnect,
                                    ulObjectType,
                                    ( HB_ISCHAR( 2 ) ? ( UNSIGNED8 * ) hb_parc(2) : NULL ),
                                    ( UNSIGNED8 * ) ucObjectName,
                                    &usObjectNameLen,
                                    &sHandle );

   if( ulRetVal == AE_SUCCESS )
   {
      while( ulRetVal == AE_SUCCESS )
      {
         PHB_ITEM pitmFileName = hb_itemPutCL( NULL, ( char * ) ucObjectName, usObjectNameLen - 1 );
         hb_arrayAddForward( pitmDir, pitmFileName );

         usObjectNameLen = sizeof( ucObjectName );

         ulRetVal = AdsDDFindNextObject( hConnect,
                                         sHandle,
                                         ucObjectName,
                                         &usObjectNameLen );
      }

      AdsDDFindClose( hConnect, sHandle );
   }

   hb_itemReturnRelease( pitmDir );
#else
   hb_reta( 0 );
#endif
}

HB_FUNC( ADSDDGRANTPERMISSION )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDGrantPermission( HB_ADS_PARCONNECTION( 6 )     /* hConnect */,
                                  ( UNSIGNED16 )  hb_parni( 1 ) /* usObjectType */,
                                  ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucObjectName */,
                                  ( UNSIGNED8 * ) hb_parcx( 3 ) /* pucParentName */,
                                  ( UNSIGNED8 * ) hb_parcx( 4 ) /* pucGrantee */,
                                  ( UNSIGNED32 )  hb_parnl( 5 ) /* ulPermissions */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDREVOKEPERMISSION )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDRevokePermission( HB_ADS_PARCONNECTION( 6 )     /* hConnect */,
                                   ( UNSIGNED16 )  hb_parni( 1 ) /* usObjectType */,
                                   ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucObjectName */,
                                   ( UNSIGNED8 * ) hb_parcx( 3 ) /* pucParentName */,
                                   ( UNSIGNED8 * ) hb_parcx( 4 ) /* pucGrantee */,
                                   ( UNSIGNED32 )  hb_parnl( 5 ) /* ulPermissions */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDGETINDEXFILEPROPERTY )
{
#if ADS_LIB_VERSION >= 600
   UNSIGNED8  *pTableName = ( UNSIGNED8 * )  hb_parcx( 1 );
   UNSIGNED8  *pIndexName = ( UNSIGNED8 * )  hb_parcx( 2 );
   UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 3 );

   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 4 );

   char sBuffer[ 260 ];
   UNSIGNED16 ulLength;

   if( AdsDDGetIndexFileProperty( hConnect, pTableName, pIndexName, ulProperty, &sBuffer, &ulLength ) != AE_SUCCESS )
   {
      sBuffer[ 0 ] = '\0';
      ulLength = 0;
   }
   hb_retclen( sBuffer, ulLength );
#endif
}

//-----------------------------------------------------------------------------
HB_FUNC( ADSDDDELETEINDEX )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDDeleteIndex( HB_ADS_PARCONNECTION( 3 ) /* hConnect */,
                            ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                            ( UNSIGNED8 * ) hb_parcx( 2 ) /* pIndexName */ ) == AE_SUCCESS );
#else
   hb_retl( FALSE );
#endif
}

HB_FUNC( ADSDDREMOVEINDEXFILE )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDRemoveIndexFile( HB_ADS_PARCONNECTION( 4 ) /* hConnect */,
                                  ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                                  ( UNSIGNED8 * ) hb_parcx( 2 ) /* pIndexName */,
                                  ( UNSIGNED16 ) ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_parldef( 3, 0 ) ) /* usDeleteFiles */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDSETTABLEPROPERTY )       /* cTableName, nProperty, xNewValue, [nValidateOption], [cFailTable], [nConnection] */
{
   UNSIGNED16 ulBuffer;
   UNSIGNED32 ulRetVal;
   UNSIGNED16 ulLength;
   UNSIGNED8 *pTableName       = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED16 ulPropertyID     = ( UNSIGNED16 )  hb_parni( 2 );
   PHB_ITEM pPropValue         = hb_param( 3, HB_IT_ANY );
   UNSIGNED16 ulValidateOption = ( UNSIGNED16 )  hb_parni( 4 );
   UNSIGNED8 *pFailTable       = ( UNSIGNED8 * ) hb_parcx( 5 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 6 );

   switch( ulPropertyID )
   {
      case ADS_DD_TABLE_AUTO_CREATE:
      {
            /* ADS_DD_TABLE_AUTO_CREATE       Changes whether or not the
                  specified table and/or any associated indexes are automatically created.
                  If the value is True and the table and/or associated indexes do not exist
                  when the table is opened, the missing files will be automatically
                  created. This property is a Boolean value and by default is set to False
                  (turned off).
            */
         ulBuffer = ( UNSIGNED16 ) hb_itemGetL( pPropValue );
         ulLength = sizeof( ulBuffer );
         ulRetVal = AdsDDSetTableProperty( hConnect                        /* hConnect */,
                                           ( UNSIGNED8 * ) pTableName      /* pucTableName */,
                                           ( UNSIGNED16 ) ulPropertyID     /* usPropertyID */,
                                           ( VOID * ) &ulBuffer            /* pvProperty */,
                                           ( UNSIGNED16 ) &ulLength        /* usPropertyLen */,
                                           ( UNSIGNED16 ) ulValidateOption /* usValidateOption */,
                                           ( UNSIGNED8 * ) pFailTable      /* pucFailTable */ );
         break;
      }
      case ADS_DD_TABLE_ENCRYPTION:     // Encrypt/decrypt the database table.
      {
         ulBuffer = ( UNSIGNED16 ) hb_itemGetL( pPropValue );
         ulLength = sizeof( ulBuffer );
         ulRetVal = AdsDDSetTableProperty( hConnect                        /* hConnect */,
                                           ( UNSIGNED8 * ) pTableName      /* pucTableName */,
                                           ( UNSIGNED16 ) ulPropertyID     /* usPropertyID */,
                                           ( VOID * ) &ulBuffer            /* pvProperty */,
                                           ( UNSIGNED16 ) &ulLength        /* usPropertyLen */,
                                           ( UNSIGNED16 ) ulValidateOption /* usValidateOption */,
                                           ( UNSIGNED8 * ) pFailTable      /* pucFailTable */ );
         break;
      }
// for other types:
//         ulRetVal = AdsDDSetTableProperty( hConnect, pTableName, ulProperty, hb_itemGetCPtr( pParam ), ( UNSIGNED16 ) hb_itemGetCLen( pParam ), NULL, NULL );
//         ulBuffer = hb_itemGetNI( pParam );
//         ulRetVal = AdsDDSetTableProperty( hConnect, pTableName, ulProperty, &ulBuffer, 2, NULL, NULL );
      default:
      {
         ulRetVal = ~AE_SUCCESS;
         break;
      }
   }
   hb_retl( ulRetVal == AE_SUCCESS );
}
