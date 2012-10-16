/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifdef __XHARBOUR__
/* JC1: optimizing stack access under MT */
// #define HB_THREAD_OPTIMIZE_STACK
#endif
#ifndef _HB_API_INTERNAL_
   #define _HB_API_INTERNAL_
#endif
#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbset.h"

HB_EXTERN_BEGIN
extern void hb_rdd_wacore_Lock( PHB_STACKRDD pRddInfo );
extern void hb_rdd_wacore_Unlock( PHB_STACKRDD pRddInfo );
extern void hb_rdd_wacore_LockInit( PHB_STACKRDD pRddInfo );
extern void hb_rdd_wacore_LockDestroy( PHB_STACKRDD pRddInfo );
extern void hb_rdd_wacore_rddWaShutDown( PHB_STACKRDD pRddInfo );
extern BOOL hb_rdd_wacore_rddChangeSetWorkareasShared( BOOL bPrev, BOOL bSet, PHB_STACKRDD pRddInfo );
extern void hb_rdd_wacore_hb_atomic_inc( PHB_STACKRDD pRddInfo );
extern void hb_rdd_wacore_rddWaInit( PHB_STACKRDD pRddInfo );
HB_EXTERN_END

#define HB_SET_WA( n ) do \
   { \
      pRddTls->uiCurrArea  = n; \
      pRddTls->pCurrArea   = ( ( pRddTls->uiCurrArea < pRddInfo->uiWaNumMax ) ? \
                               pRddInfo->waList[ pRddInfo->waNums[ pRddTls->uiCurrArea ] ] : \
                               NULL ); \
   } while( 0 )


static PHB_STACKRDD s_pRddInfo = NULL;

/*
 * Insert new WorkArea node at current WA position
 */
static void hb_waNodeInsert( PHB_STACKRDD pRddInfo, PHB_STACKRDD_TLS pRddTls, AREAP pArea )
{
   USHORT uiWaPos;

   if( pRddTls->uiCurrArea >= pRddInfo->uiWaNumMax )
   {
      int iSize = ( ( ( int ) pRddTls->uiCurrArea + 256 ) >> 8 ) << 8;

      if( iSize > HB_RDD_MAX_AREA_NUM )
         iSize = HB_RDD_MAX_AREA_NUM;

      if( pRddInfo->uiWaNumMax == 0 )
      {
         pRddInfo->waNums = ( USHORT * ) hb_xgrab( iSize * sizeof( USHORT ) );
      }
      else
      {
         pRddInfo->waNums = ( USHORT * ) hb_xrealloc( pRddInfo->waNums, iSize * sizeof( USHORT ) );
      }
      memset( &pRddInfo->waNums[ pRddInfo->uiWaNumMax ], 0, ( iSize - pRddInfo->uiWaNumMax ) * sizeof( USHORT ) );
      pRddInfo->uiWaNumMax = ( USHORT ) iSize;
   }

   if( pRddInfo->uiWaSpace == 0 )
   {
      pRddInfo->uiWaSpace     = 256;
      pRddInfo->waList        = ( void ** ) hb_xgrab( pRddInfo->uiWaSpace * sizeof( void * ) );
      memset( &pRddInfo->waList[ 0 ], 0, pRddInfo->uiWaSpace * sizeof( void * ) );
      pRddInfo->waList[ 0 ]   = NULL;
      uiWaPos                 = 1;
      pRddInfo->uiWaMax       = 2;
   }
   else
   {
      uiWaPos = pRddInfo->uiWaMax++;
      if( pRddInfo->uiWaMax > pRddInfo->uiWaSpace )
      {
         pRddInfo->uiWaSpace  = ( ( pRddInfo->uiWaMax + 256 ) >> 8 ) << 8;
         pRddInfo->waList     = ( void ** ) hb_xrealloc( pRddInfo->waList, pRddInfo->uiWaSpace * sizeof( void * ) );
         memset( &pRddInfo->waList[ pRddInfo->uiWaMax ], 0, ( pRddInfo->uiWaSpace - pRddInfo->uiWaMax ) * sizeof( void * ) );
      }
      while( uiWaPos > 1 )
      {
         if( ( ( AREAP ) pRddInfo->waList[ uiWaPos - 1 ] )->uiArea < pRddTls->uiCurrArea )
            break;
         pRddInfo->waList[ uiWaPos ]                                             = pRddInfo->waList[ uiWaPos - 1 ];
         pRddInfo->waNums[ ( ( AREAP ) pRddInfo->waList[ uiWaPos ] )->uiArea ]   = uiWaPos;
         uiWaPos--;
      }
   }
   pRddInfo->waNums[ pRddTls->uiCurrArea ]   = uiWaPos;
   pRddTls->pCurrArea                        = pRddInfo->waList[ uiWaPos ] = pArea;
   pArea->uiArea                             = pRddTls->uiCurrArea;
}

/*
 * Remove current WorkArea node
 */
static void hb_waNodeDelete( PHB_STACKRDD pRddInfo, PHB_STACKRDD_TLS pRddTls )
{
   USHORT uiWaPos;

   uiWaPos                                   = pRddInfo->waNums[ pRddTls->uiCurrArea ];
   pRddInfo->waNums[ pRddTls->uiCurrArea ]   = 0;
   pRddInfo->uiWaMax--;
   if( pRddInfo->uiWaMax <= 1 )
   {
      pRddInfo->uiWaSpace  = pRddInfo->uiWaMax = pRddInfo->uiWaNumMax = 0;
      hb_xfree( pRddInfo->waList );
      hb_xfree( pRddInfo->waNums );
      pRddInfo->waList     = NULL;
      pRddInfo->waNums     = NULL;
   }
   else
   {
      while( uiWaPos < pRddInfo->uiWaMax )
      {
         pRddInfo->waList[ uiWaPos ]                                             = pRddInfo->waList[ uiWaPos + 1 ];
         pRddInfo->waNums[ ( ( AREAP ) pRddInfo->waList[ uiWaPos ] )->uiArea ]   = uiWaPos;
         uiWaPos++;
      }
      pRddInfo->waList[ pRddInfo->uiWaMax ] = NULL;
      if( pRddInfo->uiWaSpace - pRddInfo->uiWaMax >= 256 )
      {
         pRddInfo->uiWaSpace  = ( ( pRddInfo->uiWaMax + 256 ) >> 8 ) << 8;
         pRddInfo->waList     = ( void ** ) hb_xrealloc( pRddInfo->waList, pRddInfo->uiWaSpace * sizeof( void * ) );
      }
   }
   pRddTls->pCurrArea = NULL;
}

/*
 * Return the next free WorkArea for later use.
 */
HB_ERRCODE hb_rddSelectFirstAvailable( void )
{
   PHB_STACKRDD      pRddInfo;
   PHB_STACKRDD_TLS  pRddTls;
   USHORT            uiArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddSelectFirstAvailable()" ) );

   pRddInfo = hb_stackRDD();

   hb_rdd_wacore_Lock( pRddInfo );

   uiArea = 1;
   while( uiArea < pRddInfo->uiWaNumMax )
   {
      if( pRddInfo->waNums[ uiArea ] == 0 )
         break;
      uiArea++;
   }
   if( uiArea >= HB_RDD_MAX_AREA_NUM )
   {
      hb_rdd_wacore_Unlock( pRddInfo );

      return HB_FAILURE;
   }
   pRddTls = hb_stackRDDTLS();
   HB_SET_WA( uiArea );

   hb_rdd_wacore_Unlock( pRddInfo );

   return HB_SUCCESS;
}

/*
 * Create and insert the new WorkArea node
 */
USHORT hb_rddInsertAreaNode( const char * szDriver )
{
   PHB_STACKRDD      pRddInfo;
   PHB_STACKRDD_TLS  pRddTls;
   LPRDDNODE         pRddNode;
   USHORT            uiRddID;
   AREAP             pArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddInsertAreaNode(%s)", szDriver ) );

   pRddTls = hb_stackRDDTLS();
   if( pRddTls->uiCurrArea && pRddTls->pCurrArea )
      return 0;

   pRddNode = hb_rddFindNode( szDriver, &uiRddID );
   if( ! pRddNode )
      return 0;

   pArea = ( AREAP ) hb_rddNewAreaNode( pRddNode, uiRddID );
   if( ! pArea )
      return 0;

   pRddInfo = hb_stackRDD();
   if( pRddInfo->uiWaNumMax == 0 && hb_setGetWorkareasShared() )
   {
      hb_rdd_wacore_LockInit( pRddInfo );
   }

   hb_rdd_wacore_Lock( pRddInfo );

   if( pRddTls->uiCurrArea == 0 )
   {
      if( hb_rddSelectFirstAvailable() != HB_SUCCESS )
         return 0;
   }

   hb_waNodeInsert( pRddInfo, pRddTls, pArea );

   hb_rdd_wacore_Unlock( pRddInfo );

   return pRddTls->uiCurrArea;
}

/*
 * Closes and releases the current WorkArea preparing it
 * to be used with a new database.
 */
void hb_rddReleaseCurrentArea( void )
{
   PHB_STACKRDD      pRddInfo;
   PHB_STACKRDD_TLS  pRddTls;
   AREAP             pArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddReleaseCurrentArea()" ) );

   pRddTls  = hb_stackRDDTLS();
   pArea    = ( AREAP ) pRddTls->pCurrArea;
   if( ! pArea )
      return;

   pRddInfo = hb_stackRDD();

   hb_rdd_wacore_Lock( pRddInfo );

   if( SELF_CLOSE( pArea ) == HB_FAILURE )
   {
      hb_rdd_wacore_Unlock( pRddInfo );

      return;
   }

   SELF_RELEASE( pArea );

   hb_waNodeDelete( pRddInfo, pRddTls );

   hb_rdd_wacore_Unlock( pRddInfo );
}

PHB_STACKRDD hb_rddWaInit( void )
{
   PHB_STACKRDD pRddInfo;

   if( ! hb_setGetWorkareasShared() || s_pRddInfo == NULL )
   {
      pRddInfo                = ( PHB_STACKRDD ) hb_xgrab( sizeof( HB_STACKRDD ) );

      pRddInfo->szDefaultRDD  = NULL;
      pRddInfo->waList        = NULL;
      pRddInfo->waNums        = NULL;
      pRddInfo->uiWaMax       = 0;
      pRddInfo->uiWaSpace     = 0;
      pRddInfo->uiWaNumMax    = 0;

      hb_rdd_wacore_rddWaInit( pRddInfo );

      if( s_pRddInfo == NULL )
      {
         s_pRddInfo = pRddInfo;
      }
   }
   else
   {
      pRddInfo = s_pRddInfo;
      hb_rdd_wacore_hb_atomic_inc( pRddInfo );
   }

   return pRddInfo;
}

BOOL hb_rddChangeSetWorkareasShared( BOOL bPrev, BOOL bSet )
{
   BOOL bOk = hb_rdd_wacore_rddChangeSetWorkareasShared( bPrev, bSet, s_pRddInfo );

   return bOk;
}

/* Destroy the workarea mutex */
void hb_rddWaShutDown( PHB_STACKRDD pRddInfo )
{
   hb_rdd_wacore_rddWaShutDown( pRddInfo );
}

/*
 * Closes all WorkAreas.
 */
void hb_rddCloseAll( void )
{
   PHB_STACKRDD pRddInfo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddCloseAll()" ) );

   pRddInfo = hb_stackRDD();
   if( pRddInfo->uiWaMax > 0 )
   {
      BOOL              isParents, isFinish = FALSE;
      AREAP             pArea;
      USHORT            uiIndex;
      PHB_STACKRDD_TLS  pRddTls = hb_stackRDDTLS();

      hb_rdd_wacore_Lock( pRddInfo );

      do
      {
         isParents = FALSE;
         for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; uiIndex++ )
         {
            pArea = ( AREAP ) pRddInfo->waList[ uiIndex ];
            HB_SET_WA( pArea->uiArea );
            if( isFinish )
            {
               SELF_RELEASE( pArea );
               pRddInfo->waNums[ pRddTls->uiCurrArea ]   = 0;
               pRddTls->pCurrArea                        = NULL;
            }
            else if( pArea->uiParents )
            {
               isParents = TRUE;
            }
            else
            {
               SELF_CLOSE( pArea );
            }
         }
         if( ! isParents && ! isFinish )
         {
            isParents = isFinish = TRUE;
         }
      }
      while( isParents );

      pRddInfo->uiWaSpace  = pRddInfo->uiWaMax = pRddInfo->uiWaNumMax = 0;
      hb_xfree( pRddInfo->waList );
      hb_xfree( pRddInfo->waNums );
      pRddInfo->waList     = NULL;
      pRddInfo->waNums     = NULL;
      HB_SET_WA( 1 );

      hb_rdd_wacore_Unlock( pRddInfo );
   }
}

void hb_rddFlushAll( void )
{
   PHB_STACKRDD      pRddInfo = hb_stackRDD();
   PHB_STACKRDD_TLS  pRddTls  = hb_stackRDDTLS();
   USHORT            uiArea   = ( USHORT ) hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   hb_rdd_wacore_Lock( pRddInfo );

   for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( ( ( AREAP ) pRddInfo->waList[ uiIndex ] )->uiArea );
      SELF_FLUSH( ( AREAP ) pRddTls->pCurrArea );
   }

   hb_rdd_wacore_Unlock( pRddInfo );

   hb_rddSelectWorkAreaNumber( uiArea );
}

void hb_rddUnLockAll( void )
{
   PHB_STACKRDD      pRddInfo = hb_stackRDD();
   PHB_STACKRDD_TLS  pRddTls  = hb_stackRDDTLS();
   USHORT            uiArea   = ( USHORT ) hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   hb_rdd_wacore_Lock( pRddInfo );

   for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( ( ( AREAP ) pRddInfo->waList[ uiIndex ] )->uiArea );
      SELF_UNLOCK( ( AREAP ) pRddTls->pCurrArea, NULL );
   }

   hb_rdd_wacore_Unlock( pRddInfo );

   hb_rddSelectWorkAreaNumber( uiArea );
}

/*
 * call a pCallBack function with all open workareas ###
 */
HB_ERRCODE hb_rddIterateWorkAreas( WACALLBACK pCallBack, void * cargo )
{
   PHB_STACKRDD   pRddInfo;
   HB_ERRCODE     errCode = HB_SUCCESS;
   USHORT         uiIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddIterateWorkAreas(%p,%p)", pCallBack, cargo ) );

   pRddInfo = hb_stackRDD();

   hb_rdd_wacore_Lock( pRddInfo );

   for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; uiIndex++ )
   {
      errCode = pCallBack( ( AREAP ) pRddInfo->waList[ uiIndex ], cargo );
      if( errCode != HB_SUCCESS )
         break;
   }

   hb_rdd_wacore_Unlock( pRddInfo );

   return errCode;
}

BOOL hb_rddGetNetErr( void )
{
   return hb_stackRDDTLS()->fNetError;
}

void hb_rddSetNetErr( BOOL fNetErr )
{
   hb_stackRDDTLS()->fNetError = fNetErr;
}

/*
 * Get (/set) default RDD driver
 */
const char * hb_rddDefaultDrv( const char * szDriver )
{
   PHB_STACKRDD pRddInfo = hb_stackRDD();

   if( szDriver && *szDriver )
   {
      char     szNewDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
      USHORT   uiRddID;

      hb_strncpyUpper( szNewDriver, szDriver, sizeof( szNewDriver ) - 1 );
      if( ! hb_rddFindNode( szNewDriver, &uiRddID ) )
         return NULL;

      pRddInfo->szDefaultRDD = hb_rddGetNode( uiRddID )->szName;
   }
   else if( ! pRddInfo->szDefaultRDD && hb_rddGetNode( 0 ) )
   {
      const char *   szDrvTable[] = { "DBFNTX", "DBFCDX", "DBFFPT", "DBF", NULL };
      int            i;

      pRddInfo->szDefaultRDD = "";
      for( i = 0; szDrvTable[ i ]; ++i )
      {
         if( hb_rddFindNode( szDrvTable[ i ], NULL ) )
         {
            pRddInfo->szDefaultRDD = szDrvTable[ i ];
            break;
         }
      }
   }

   return pRddInfo->szDefaultRDD;
}

/*
 * Function for getting given workarea pointer
 */
void * hb_rddGetWorkAreaPointer( int iArea )
{
   PHB_STACKRDD pRddInfo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddGetWorkAreaPointer(%d)", iArea ) );

   pRddInfo = hb_stackRDD();

   if( iArea == 0 )
      return hb_stackRDDTLS()->pCurrArea;
   else if( iArea >= 1 && ( UINT ) iArea < ( UINT ) pRddInfo->uiWaNumMax )
      return pRddInfo->waList[ pRddInfo->waNums[ iArea ] ];
   else
      return NULL;
}

/*
 * Function for getting current workarea pointer
 */
void * hb_rddGetCurrentWorkAreaPointer( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddGetCurrentWorkAreaPointer()" ) );

   return hb_stackRDDTLS()->pCurrArea;
}

/*
 * Return the current WorkArea number.
 */
int hb_rddGetCurrentWorkAreaNumber( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddGetCurrentWorkAreaNumber()" ) );

   return hb_stackRDDTLS()->uiCurrArea;
}

/*
 * Select a WorkArea by the number.
 */
HB_ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
   PHB_STACKRDD      pRddInfo;
   PHB_STACKRDD_TLS  pRddTls;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddSelectWorkAreaNumber(%d)", iArea ) );

   pRddInfo = hb_stackRDD();
   pRddTls  = hb_stackRDDTLS();

   hb_rdd_wacore_Lock( pRddInfo );

   if( iArea < 1 || iArea > HB_RDD_MAX_AREA_NUM )
      HB_SET_WA( 0 );
   else
      HB_SET_WA( ( USHORT ) iArea );

   hb_rdd_wacore_Unlock( pRddInfo );

   return ( pRddTls->pCurrArea == NULL ) ? HB_FAILURE : HB_SUCCESS;
}
