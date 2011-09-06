/*
 * $Id$
 */

/*
 * DBRMAP (Record Map filters) for [x]Harbour:
 *    Record Map and ClipMore/COMIX compatible function set
 *
 * Copyright 2004-2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * All rights reserved.
 *
 */

#include "hbrddrm.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbstack.h"

typedef struct
{
   int      rlOptLevel; /* RM_OPT_NONE */
   int      rlError;    /* M6ERR_OK */
   HB_ULONG rlRecords;  /* 0 */
} HB_RMDATA, * PHB_RMDATA;

#ifdef __XHARBOUR__

static HB_RMDATA  s_rm_data;
#define HB_RM_DATA_PTR  (&s_rm_data)

#define HB_RM_LOCK
#define HB_RM_UNLOCK

#else

#include "hbthread.h"
static HB_TSD_NEW( s_rmData, sizeof( HB_RMDATA ), NULL, NULL );
#define HB_RM_DATA_PTR  ( ( PHB_RMDATA ) hb_stackGetTSD( &s_rmData ) )

static HB_CRITICAL_NEW( s_rmMtx );
#define HB_RM_LOCK      hb_threadEnterCriticalSection( &s_rmMtx );
#define HB_RM_UNLOCK    hb_threadLeaveCriticalSection( &s_rmMtx );

#endif


/* list of RM FIlters, protected by s_rmMtx */
static PHB_RMBAG s_RM_bag = NULL;

static HB_USHORT s_uiRdds[ RM_RDD_MAX ];
static int s_iRddCount = 0;

/* table with number of bits in HB_BYTE to speed up hb_rmCountRecords() */
static const HB_BYTE s_bitCount[ 256 ] =
   {
      0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
   };

static PHB_RMITEM hb_rmItemNew( void )
{
   PHB_RMITEM pRMItem = ( PHB_RMITEM ) hb_xgrab( sizeof( HB_RMITEM ) );

   pRMItem->ulTmpBlock = HB_RMITEM_DUMMY;
   pRMItem->pRecMap = ( HB_BYTE * ) hb_xgrab( HB_RMITEM_SIZE );
   memset( pRMItem->pRecMap, 0x00, HB_RMITEM_SIZE );
   return pRMItem;
}

static void hb_rmItemFree( PHB_RMITEM pRMItem )
{
   if( pRMItem->pRecMap )
      hb_xfree( pRMItem->pRecMap );
   hb_xfree( pRMItem );
}

static HB_BYTE * hb_rmItemBuf( PHB_RMFILTER pRM, int iItem )
{
   /*
    * TODO: for less memory system implement disk cache for RMITEM
    * The current API functions don't need more then four recently used
    * RecMap to be in memory so it will be very easy to implement
    * We can create simple pool with one common disk file for all
    * RecMaps in all RM filters
    */
   return pRM->pRMItems[ iItem ]->pRecMap;
}

static HB_BYTE * hb_rmItemBufMB( PHB_RMFILTER pRM, int iItem )
{
   /*
    * TODO: read note above in hb_rmItemBuf()
    */
   return pRM->pRMmaybe[ iItem ]->pRecMap;
}

static PHB_RMITEM * hb_rmItemLstNew( int iItems )
{
   PHB_RMITEM * pRMItems;
   int i;

   if( iItems == 0 )
      return NULL;

   pRMItems = ( PHB_RMITEM * ) hb_xgrab( sizeof( PHB_RMITEM ) * iItems );
   for( i = 0; i < iItems; i++ )
   {
      pRMItems[ i ] = hb_rmItemNew();
   }
   return pRMItems;
}

static void hb_rmItemLstFree( PHB_RMITEM *pRMItems, int iItems )
{
   int i;
   if( pRMItems )
   {
      for( i = 0; i < iItems; i++ )
      {
         hb_rmItemFree( pRMItems[ i ] );
      }
      hb_xfree( pRMItems );
   }
}

static PHB_RMITEM * hb_rmItemLstReSize( PHB_RMITEM *pRMItems,
                                        int iOldItems, int iNewItems )
{
   if( iOldItems != iNewItems )
   {
      if( pRMItems )
      {
         if( iNewItems == 0 )
         {
            hb_xfree( pRMItems );
            pRMItems = NULL;
         }
         else
         {
            int i;
            if( iOldItems > iNewItems )
            {
               for( i = iNewItems; i < iOldItems; i++ )
               {
                  hb_rmItemFree( pRMItems[ i ] );
               }
               pRMItems = ( PHB_RMITEM * ) hb_xrealloc( pRMItems,
                                          sizeof( PHB_RMITEM ) * iNewItems );
            }
            else
            {
               pRMItems = ( PHB_RMITEM * ) hb_xrealloc( pRMItems,
                                          sizeof( PHB_RMITEM ) * iNewItems );
               for( i = iOldItems; i < iNewItems; i++ )
               {
                  pRMItems[ i ] = hb_rmItemNew();
               }
            }
         }
      }
      else
      {
         pRMItems = hb_rmItemLstNew( iNewItems );
      }
   }
   return pRMItems;
}

/* clear the rest of record map buffer */
static void hb_rmClearRest( PHB_RMFILTER pRM, HB_BOOL fMB )
{
   if( pRM->ulRecords && ( !fMB || pRM->pRMmaybe ) )
   {
      HB_ULONG ulRec = pRM->ulRecords - 1;
      int i = HB_RM_ITEM_POS( ulRec );
      HB_BYTE * pRecMap = fMB ? hb_rmItemBufMB( pRM, HB_RM_ITEM_NO( ulRec ) ) :
                                hb_rmItemBuf( pRM, HB_RM_ITEM_NO( ulRec ) );

      pRecMap[ i++ ] &= ( HB_BYTE ) ( ( ( int ) 1 << ( ( ulRec & 0x07 ) + 1 ) ) - 1 );
      if( i < HB_RMITEM_SIZE )
         memset( &pRecMap[ i ], 0x00, HB_RMITEM_SIZE - i );
   }
}

static void hb_rmCheckMB( PHB_RMFILTER pRM )
{
   if( hb_rmNextRecordMB( pRM, 0 ) == 0 )
   {
      hb_rmClearMB( pRM );
      pRM->iOptLvl = RM_OPT_FULL;
   }
}

/*
HB_GARBAGE_FUNC( hb_rmBagDestroyGarbage )
{
   hb_rmDestroyAll();
}
*/

static void hb_rmBagInit( void )
{
   if( s_RM_bag == NULL )
   {
      s_RM_bag = ( PHB_RMBAG ) hb_xgrab( sizeof( HB_RMBAG ) );
      s_RM_bag->iSize = s_RM_bag->iCount = 0;
      s_RM_bag->pRMFilters = NULL;
   }
}

static int hb_rmNewHandle( PHB_RMFILTER pRM )
{
   int iHandle = 0, i;

   HB_RM_LOCK

   hb_rmBagInit();

   if( s_RM_bag->iSize > s_RM_bag->iCount )
   {
      for( i = 0; i < s_RM_bag->iSize; i++ )
      {
         if( s_RM_bag->pRMFilters[ i ] == NULL )
         {
            iHandle = i + 1;
            break;
         }
      }
   }
   if( iHandle == 0 )
   {
      HB_SIZE nNewSize, nOldSize;

      iHandle = s_RM_bag->iSize + 1;
      nOldSize = sizeof( PHB_RMFILTER * ) * s_RM_bag->iSize;
      s_RM_bag->iSize += HB_RMLST_ALLOC;
      nNewSize = sizeof( PHB_RMFILTER * ) * s_RM_bag->iSize;
      if( nOldSize != 0 )
         s_RM_bag->pRMFilters = ( PHB_RMFILTER * ) hb_xrealloc( s_RM_bag->pRMFilters, nNewSize );
      else
         s_RM_bag->pRMFilters = ( PHB_RMFILTER * ) hb_xgrab( nNewSize );
      memset( &(( HB_BYTE * ) s_RM_bag->pRMFilters)[ nOldSize ], 0, nNewSize - nOldSize );
   }
   s_RM_bag->iCount++;
   s_RM_bag->pRMFilters[ iHandle - 1 ] = pRM;

   HB_RM_UNLOCK

   return iHandle;
}

static void hb_rmRemoveHandle( int iHandle )
{
   HB_RM_LOCK

   if( s_RM_bag && iHandle > 0 && iHandle <= s_RM_bag->iSize &&
        s_RM_bag->pRMFilters[ iHandle - 1 ] != NULL )
   {
      s_RM_bag->pRMFilters[ iHandle - 1 ] = NULL;
      if( --s_RM_bag->iCount == 0 )
      {
         hb_xfree( s_RM_bag->pRMFilters );
         hb_xfree( s_RM_bag );
         s_RM_bag = NULL;
      }
   }

   HB_RM_UNLOCK
}

static void hb_rmXChangeMap( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 )
{
   HB_FHANDLE  hFile     = pRM1->hFile;
   int         iItems    = pRM1->iItems;
   HB_ULONG    ulRecords = pRM1->ulRecords;
   PHB_RMITEM  *pRMItems = pRM1->pRMItems;
   PHB_RMITEM  *pRMmaybe = pRM1->pRMmaybe;

   pRM1->hFile     = pRM2->hFile;
   pRM1->iItems    = pRM2->iItems;
   pRM1->ulRecords = pRM2->ulRecords;
   pRM1->pRMItems  = pRM2->pRMItems;
   pRM1->pRMmaybe  = pRM2->pRMmaybe;

   pRM2->hFile     = hFile;
   pRM2->iItems    = iItems;
   pRM2->ulRecords = ulRecords;
   pRM2->pRMItems  = pRMItems;
   pRM2->pRMmaybe  = pRMmaybe;
}

void hb_rmSetRddID( HB_USHORT uiRddId )
{
   HB_RM_LOCK

   if( s_iRddCount < RM_RDD_MAX )
      s_uiRdds[ s_iRddCount++ ] = uiRddId;

   HB_RM_UNLOCK
}

void hb_rmDelRddID( HB_USHORT uiRddId )
{
   int i;

   HB_RM_LOCK

   for( i = 0; i < s_iRddCount; ++i )
   {
      if( s_uiRdds[ i ] == uiRddId )
         break;
   }
   if( i < s_iRddCount )
   {
      for( ; i < s_iRddCount - 1; ++i )
         s_uiRdds[ i ] = s_uiRdds[ i + 1 ];

      s_uiRdds[ --s_iRddCount ] = 0;
   }

   HB_RM_UNLOCK
}

void * hb_rmGetRMAreaPointer( void )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      int i;

      for( i = 0; i < s_iRddCount; ++i )
      {
         if( hb_rddIsDerivedFrom( pArea->rddID, s_uiRdds[ i ] ) )
            break;
      }
      if( i >= s_iRddCount )
      {
         hb_rmSetError( M6ERR_NOTSUPP );
         pArea = NULL;
      }
   }
   else
   {
      hb_rmSetError( M6ERR_NOTABLE );
   }
   return pArea;
}

static int hb_rmGetOptLevel( void )
{
   return HB_RM_DATA_PTR->rlOptLevel;
}

static void hb_rmSetOptLevel( int iOptLevel )
{
   HB_RM_DATA_PTR->rlOptLevel = iOptLevel;
}

int hb_rmGetError( void )
{
   return HB_RM_DATA_PTR->rlError;
}

void hb_rmSetError( int iError )
{
   HB_RM_DATA_PTR->rlError = iError;
}

HB_ULONG hb_rmGetRecords( void )
{
   return HB_RM_DATA_PTR->rlRecords;
}
void hb_rmSetRecords( HB_ULONG ulRecords )
{
   HB_RM_DATA_PTR->rlRecords = ulRecords;
}

HB_BOOL hb_rmIsFilter( int iHandle )
{
   HB_BOOL fOK = HB_FALSE;

   HB_RM_LOCK
   if( s_RM_bag && iHandle > 0 && iHandle <= s_RM_bag->iSize )
      fOK = s_RM_bag->pRMFilters[ iHandle - 1 ] != NULL;
   HB_RM_UNLOCK

   return fOK;
}

PHB_RMFILTER hb_rmGetFilterPtr( int iHandle )
{
   PHB_RMFILTER pRM = NULL;

   HB_RM_LOCK
   if( s_RM_bag && iHandle > 0 && iHandle <= s_RM_bag->iSize )
      pRM = s_RM_bag->pRMFilters[ iHandle - 1 ];
   HB_RM_UNLOCK

   if( pRM == NULL )
      hb_rmSetError( M6ERR_BADHANDLE );

   return pRM;
}

void hb_rmDestroyAll( void )
{
   /* executed from RDD exit code, foes not need MT protection */
   int i = 0;
   while( s_RM_bag && s_RM_bag->iCount && i < s_RM_bag->iSize )
   {
      if( s_RM_bag->pRMFilters[ i ] )
         hb_rmDestroy( s_RM_bag->pRMFilters[ i ] );
      ++i;
   }
}

PHB_RMFILTER hb_rmCreate( HB_ULONG ulRecords )
{
   PHB_RMFILTER pRM;

   pRM = ( PHB_RMFILTER ) hb_xgrab( sizeof( HB_RMFILTER ) );
   pRM->hFile = FS_ERROR;
   pRM->ulRecords = ulRecords;
   pRM->ulPos = 0;
   pRM->iArea = 0;
   pRM->fLocked = HB_FALSE;
   pRM->iItems = ulRecords ? HB_RM_ITEM_NO( ulRecords - 1 ) + 1 : 0;
   pRM->iType = RM_TYPE_COMPLEX;
   pRM->iOptLvl = RM_OPT_FULL;
   pRM->pRMmaybe = NULL;
   pRM->pRMItems = hb_rmItemLstNew( pRM->iItems );
   pRM->pExpr = NULL;
   pRM->pNonExpr = NULL;
   pRM->iHandle = hb_rmNewHandle( pRM );

   return pRM;
}

PHB_RMFILTER hb_rmReSize( PHB_RMFILTER pRM, HB_ULONG ulRecords )
{
   if( pRM->ulRecords != ulRecords )
   {
      int iItems = ulRecords ? HB_RM_ITEM_NO( ulRecords - 1 ) + 1 : 0;
      HB_BOOL fGrowUp = ( ulRecords > pRM->ulRecords );

      if( pRM->iItems != iItems )
      {
         pRM->pRMItems = hb_rmItemLstReSize( pRM->pRMItems,
                                             pRM->iItems, iItems );
         pRM->iItems = iItems;
         if( pRM->pRMmaybe )
         {
            pRM->pRMmaybe = hb_rmItemLstReSize( pRM->pRMmaybe,
                                                pRM->iItems, iItems );
         }
      }
      pRM->ulRecords = ulRecords;
      if( !fGrowUp )
      {
         hb_rmClearRest( pRM, HB_FALSE );
         hb_rmClearRest( pRM, HB_TRUE );
         hb_rmCheckMB( pRM );
      }
   }
   return pRM;
}

PHB_RMFILTER hb_rmDup( PHB_RMFILTER pSrcRM )
{
   PHB_RMFILTER pRM;
   int i;

   pRM = hb_rmCreate( pSrcRM->ulRecords );
   pRM->iType = pSrcRM->iType;
   pRM->iOptLvl = pSrcRM->iOptLvl;
   if( pSrcRM->pRMmaybe )
      pRM->pRMmaybe = hb_rmItemLstNew( pRM->iItems );
   if( pSrcRM->pExpr )
      pRM->pExpr = hb_itemNew( pSrcRM->pExpr );
   if( pSrcRM->pNonExpr )
      pRM->pNonExpr = hb_itemNew( pSrcRM->pNonExpr );
   for( i = 0; i < pSrcRM->iItems; i++ )
   {
      memcpy( hb_rmItemBuf( pRM, i ), hb_rmItemBuf( pSrcRM, i ), HB_RMITEM_SIZE );
      if( pRM->pRMmaybe )
         memcpy( hb_rmItemBufMB( pRM, i ), hb_rmItemBufMB( pSrcRM, i ), HB_RMITEM_SIZE );
   }
   return pRM;
}

void hb_rmDetach( PHB_RMFILTER pRM )
{
   if( pRM->iArea != 0 )
   {
      AREAP pArea;

      int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pRM->iArea )
         hb_rddSelectWorkAreaNumber( pRM->iArea );
      else
         iCurrArea = 0;

      pArea = ( AREAP ) hb_rmGetRMAreaPointer();
      if( pArea )
      {
         PHB_ITEM pItem = hb_itemPutNI( NULL, 0 );

         SELF_INFO( pArea, DBI_RM_HANDLE, pItem );
         hb_itemRelease( pItem );
      }

      if( iCurrArea )
         hb_rddSelectWorkAreaNumber( iCurrArea );
   }
}

void hb_rmDestroy( PHB_RMFILTER pRM )
{
   if( pRM->iArea != 0 )
      hb_rmDetach( pRM );

   hb_rmRemoveHandle( pRM->iHandle );

   if( pRM->hFile != FS_ERROR )
      hb_fsClose( pRM->hFile );
   if( pRM->pRMItems )
      hb_rmItemLstFree( pRM->pRMItems, pRM->iItems );
   if( pRM->pRMmaybe )
      hb_rmItemLstFree( pRM->pRMmaybe, pRM->iItems );
   if( pRM->pExpr )
      hb_itemRelease( pRM->pExpr );
   if( pRM->pNonExpr )
      hb_itemRelease( pRM->pNonExpr );

   hb_xfree( pRM );
}

void hb_rmClear( PHB_RMFILTER pRM )
{
   int i;

   for( i = 0; i < pRM->iItems; i++ )
      memset( hb_rmItemBuf( pRM, i ), 0x00, HB_RMITEM_SIZE );
}

void hb_rmFill( PHB_RMFILTER pRM )
{
   if( pRM->ulRecords )
   {
      int i;

      for( i = 0; i < pRM->iItems; i++ )
         memset( hb_rmItemBuf( pRM, i ), 0xFF, HB_RMITEM_SIZE );
      hb_rmClearRest( pRM, HB_FALSE );
   }
}

void hb_rmFillMB( PHB_RMFILTER pRM )
{
   if( pRM->ulRecords )
   {
      int i;

      if( pRM->pRMmaybe == NULL )
         pRM->pRMmaybe = hb_rmItemLstNew( pRM->iItems );
      for( i = 0; i < pRM->iItems; i++ )
         memset( hb_rmItemBufMB( pRM, i ), 0xFF, HB_RMITEM_SIZE );
      hb_rmClearRest( pRM, HB_TRUE );
      pRM->iOptLvl = RM_OPT_NONE;
   }
}

void hb_rmClearMB( PHB_RMFILTER pRM )
{
   if( pRM->pRMmaybe != NULL )
   {
      hb_rmItemLstFree( pRM->pRMmaybe, pRM->iItems );
      pRM->pRMmaybe = NULL;
   }
   pRM->iOptLvl = RM_OPT_FULL;
}

PHB_RMFILTER hb_rmMakeMB( PHB_RMFILTER pRM )
{
   if( pRM->ulRecords )
   {
      int i, j;

      if( pRM->pRMmaybe == NULL )
         pRM->pRMmaybe = hb_rmItemLstNew( pRM->iItems );
      for( i = 0; i < pRM->iItems; i++ )
      {
         HB_BYTE * pRecMap = hb_rmItemBuf( pRM, i );
         HB_BYTE * pRecMapMB = hb_rmItemBufMB( pRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
            pRecMapMB[ j ] |= pRecMap[ j ];
         memset( pRecMap, 0x00, HB_RMITEM_SIZE );
      }
      /* hb_rmClearRest( pRM, HB_FALSE );
         hb_rmClearRest( pRM, HB_TRUE );
         hb_rmCheckMB( pDstRM ); */
      if( pRM->iOptLvl == RM_OPT_FULL )
         pRM->iOptLvl = RM_OPT_PART;
   }
   return pRM;
}

PHB_RMFILTER hb_rmOR( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 )
{
   PHB_RMFILTER pDstRM, pSrcRM;
   int i, j, iItems;

   if( pRM1->ulRecords < pRM2->ulRecords )
   {
      if( pRM2->fLocked )
      {
         pRM1 = hb_rmReSize( pRM1, pRM2->ulRecords );
      }
      else
      {
         hb_rmXChangeMap( pRM1, pRM2 );
      }
   }
   pSrcRM = pRM2;
   pDstRM = pRM1;
   iItems = pSrcRM->iItems;

   if( pSrcRM->pRMmaybe && pDstRM->pRMmaybe )
   {
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         HB_BYTE * pSrcRecMapMB = hb_rmItemBufMB( pSrcRM, i );
         HB_BYTE * pDstRecMapMB = hb_rmItemBufMB( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMapMB[ j ] &= pSrcRecMapMB[ j ] | ~pSrcRecMap[ j ];
            pDstRecMapMB[ j ] |= pSrcRecMapMB[ j ] & ~pDstRecMap[ j ];
            pDstRecMap[ j ] |= pSrcRecMap[ j ];
         }
      }
   }
   else if( pDstRM->pRMmaybe )
   {
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         HB_BYTE * pDstRecMapMB = hb_rmItemBufMB( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMapMB[ j ] &= ~pSrcRecMap[ j ];
            pDstRecMap[ j ] |= pSrcRecMap[ j ];
         }
      }
   }
   else if( pSrcRM->pRMmaybe )
   {
      if( pSrcRM->iItems == pDstRM->iItems && !pSrcRM->fLocked )
      {
         pDstRM->pRMmaybe = pSrcRM->pRMmaybe;
      }
      else
      {
         pDstRM->pRMmaybe = hb_rmItemLstNew( pDstRM->iItems );
      }
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         HB_BYTE * pSrcRecMapMB = hb_rmItemBufMB( pSrcRM, i );
         HB_BYTE * pDstRecMapMB = hb_rmItemBufMB( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMapMB[ j ] = pSrcRecMapMB[ j ] & ~pDstRecMap[ j ];
            pDstRecMap[ j ] |= pSrcRecMap[ j ];
         }
      }
      if( pDstRM->pRMmaybe == pSrcRM->pRMmaybe )
      {
         pSrcRM->pRMmaybe = NULL;
      }
   }
   else
   {
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMap[ j ] |= pSrcRecMap[ j ];
         }
      }
   }

   if( pSrcRM->iOptLvl == RM_OPT_FULL && pDstRM->iOptLvl == RM_OPT_FULL )
   {
      pDstRM->iOptLvl = RM_OPT_FULL;
   }
   else if( pSrcRM->iOptLvl == RM_OPT_NONE || pDstRM->iOptLvl == RM_OPT_NONE )
   {
      pDstRM->iOptLvl = RM_OPT_NONE;
   }
   else
   {
      pDstRM->iOptLvl = RM_OPT_PART;
   }

   if( !pSrcRM->fLocked )
   {
      hb_rmDestroy( pSrcRM );
   }
   hb_rmClearRest( pDstRM, HB_FALSE );
   hb_rmClearRest( pDstRM, HB_TRUE );
   hb_rmCheckMB( pDstRM );

   return pDstRM;
}

PHB_RMFILTER hb_rmAND( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 )
{
   PHB_RMFILTER pDstRM, pSrcRM;
   int i, j, iItems;

   if( pRM1->ulRecords > pRM2->ulRecords )
   {
      if( pRM2->fLocked )
      {
         pRM1 = hb_rmReSize( pRM1, pRM2->ulRecords );
      }
      else
      {
         hb_rmXChangeMap( pRM1, pRM2 );
      }
   }
   pSrcRM = pRM2;
   pDstRM = pRM1;
   iItems = pDstRM->iItems;

   if( pSrcRM->pRMmaybe && pDstRM->pRMmaybe )
   {
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         HB_BYTE * pSrcRecMapMB = hb_rmItemBufMB( pSrcRM, i );
         HB_BYTE * pDstRecMapMB = hb_rmItemBufMB( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMapMB[ j ] &= pSrcRecMapMB[ j ] | pSrcRecMap[ j ];
            pDstRecMapMB[ j ] |= pSrcRecMapMB[ j ] & pDstRecMap[ j ];
            pDstRecMap[ j ] &= pSrcRecMap[ j ];
         }
      }
   }
   else if( pDstRM->pRMmaybe )
   {
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         HB_BYTE * pDstRecMapMB = hb_rmItemBufMB( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMapMB[ j ] &= pSrcRecMap[ j ];
            pDstRecMap[ j ] &= pSrcRecMap[ j ];
         }
      }
   }
   else if( pSrcRM->pRMmaybe )
   {
      if( pSrcRM->iItems == pDstRM->iItems && !pSrcRM->fLocked )
      {
         pDstRM->pRMmaybe = pSrcRM->pRMmaybe;
      }
      else
      {
         pDstRM->pRMmaybe = hb_rmItemLstNew( pDstRM->iItems );
      }
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         HB_BYTE * pSrcRecMapMB = hb_rmItemBufMB( pSrcRM, i );
         HB_BYTE * pDstRecMapMB = hb_rmItemBufMB( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMapMB[ j ] = pSrcRecMapMB[ j ] & pDstRecMap[ j ];
            pDstRecMap[ j ] &= pSrcRecMap[ j ];
         }
      }
      if( pDstRM->pRMmaybe == pSrcRM->pRMmaybe )
      {
         pSrcRM->pRMmaybe = NULL;
      }
   }
   else
   {
      for( i = 0; i < iItems; i++ )
      {
         HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
         HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
         for( j = 0; j < HB_RMITEM_SIZE; j++ )
         {
            pDstRecMap[ j ] &= pSrcRecMap[ j ];
         }
      }
   }

   if( pSrcRM->iOptLvl == RM_OPT_FULL && pDstRM->iOptLvl == RM_OPT_FULL )
   {
      pDstRM->iOptLvl = RM_OPT_FULL;
   }
   else if( pSrcRM->iOptLvl == RM_OPT_NONE && pDstRM->iOptLvl == RM_OPT_NONE )
   {
      pDstRM->iOptLvl = RM_OPT_NONE;
   }
   else
   {
      pDstRM->iOptLvl = RM_OPT_PART;
   }

   if( !pSrcRM->fLocked )
   {
      hb_rmDestroy( pSrcRM );
   }
   hb_rmClearRest( pDstRM, HB_FALSE );
   hb_rmClearRest( pDstRM, HB_TRUE );
   hb_rmCheckMB( pDstRM );

   return pDstRM;
}

PHB_RMFILTER hb_rmXOR( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 )
{
   PHB_RMFILTER pDstRM, pSrcRM;
   int i, j, iItems;

   if( pRM1->ulRecords < pRM2->ulRecords )
   {
      if( pRM2->fLocked )
      {
         pRM1 = hb_rmReSize( pRM1, pRM2->ulRecords );
      }
      else
      {
         hb_rmXChangeMap( pRM1, pRM2 );
      }
   }
   pSrcRM = pRM2;
   pDstRM = pRM1;
   iItems = pSrcRM->iItems;

   for( i = 0; i < iItems; i++ )
   {
      HB_BYTE * pSrcRecMap = hb_rmItemBuf( pSrcRM, i );
      HB_BYTE * pDstRecMap = hb_rmItemBuf( pDstRM, i );
      for( j = 0; j < HB_RMITEM_SIZE; j++ )
      {
         pDstRecMap[ j ] ^= pSrcRecMap[ j ];
      }
   }
   if( pSrcRM->pRMmaybe )
   {
      if( !pDstRM->pRMmaybe && !pSrcRM->fLocked  &&
          pSrcRM->iItems == pDstRM->iItems )
      {
         pDstRM->pRMmaybe = pSrcRM->pRMmaybe;
         pSrcRM->pRMmaybe = NULL;
      }
      else
      {
         if( !pDstRM->pRMmaybe )
         {
            pDstRM->pRMmaybe = hb_rmItemLstNew( pDstRM->iItems );
         }
         for( i = 0; i < iItems; i++ )
         {
            HB_BYTE * pSrcRecMapMB = hb_rmItemBufMB( pSrcRM, i );
            HB_BYTE * pDstRecMapMB = hb_rmItemBufMB( pDstRM, i );
            for( j = 0; j < HB_RMITEM_SIZE; j++ )
            {
               pDstRecMapMB[ j ] |= pSrcRecMapMB[ j ];
            }
         }
      }
   }

   if( pSrcRM->iOptLvl == RM_OPT_FULL && pDstRM->iOptLvl == RM_OPT_FULL )
   {
      pDstRM->iOptLvl = RM_OPT_FULL;
   }
   else if( pSrcRM->iOptLvl == RM_OPT_NONE || pDstRM->iOptLvl == RM_OPT_NONE )
   {
      pDstRM->iOptLvl = RM_OPT_NONE;
   }
   else
   {
      pDstRM->iOptLvl = RM_OPT_PART;
   }

   if( !pSrcRM->fLocked )
   {
      hb_rmDestroy( pSrcRM );
   }
   hb_rmClearRest( pDstRM, HB_FALSE );
   hb_rmClearRest( pDstRM, HB_TRUE );
   hb_rmCheckMB( pDstRM );

   return pDstRM;
}

PHB_RMFILTER hb_rmNOT( PHB_RMFILTER pRM )
{
   int i, j;
   for( i = 0; i < pRM->iItems; i++ )
   {
      HB_BYTE * pRecMap = hb_rmItemBuf( pRM, i );
      for( j = 0; j < HB_RMITEM_SIZE; j++ )
      {
         pRecMap[ j ] ^= 0xff;
      }
   }
   hb_rmClearRest( pRM, HB_FALSE );
   return pRM;
}

void hb_rmSetRecord( PHB_RMFILTER pRM, HB_ULONG ulRec )
{
   if( ulRec >= pRM->ulRecords && pRM->iArea != 0 )
   {
      pRM = hb_rmReSize( pRM, ulRec );
   }

   if( --ulRec < pRM->ulRecords )
   {
      HB_BYTE * pRecMap = hb_rmItemBuf( pRM, HB_RM_ITEM_NO( ulRec ) );
      pRecMap[ HB_RM_ITEM_POS( ulRec ) ] |= 1 << ( ulRec & 0x07 );
      if( pRM->pRMmaybe )
      {
         pRecMap = hb_rmItemBufMB( pRM, HB_RM_ITEM_NO( ulRec ) );
         pRecMap[ HB_RM_ITEM_POS( ulRec ) ] &= ~( 1 << ( ulRec & 0x07 ) );
      }
   }
   else
      hb_rmSetError( M6ERR_RECRANGE );
}

void hb_rmClearRecord( PHB_RMFILTER pRM, HB_ULONG ulRec )
{
   if( --ulRec < pRM->ulRecords )
   {
      HB_BYTE * pRecMap = hb_rmItemBuf( pRM, HB_RM_ITEM_NO( ulRec ) );
      pRecMap[ HB_RM_ITEM_POS( ulRec ) ] &= ~( 1 << ( ulRec & 0x07 ) );
      if( pRM->pRMmaybe )
      {
         pRecMap = hb_rmItemBufMB( pRM, HB_RM_ITEM_NO( ulRec ) );
         pRecMap[ HB_RM_ITEM_POS( ulRec ) ] &= ~( 1 << ( ulRec & 0x07 ) );
      }
   }
   else
      hb_rmSetError( M6ERR_RECRANGE );
}

HB_BOOL hb_rmTestRecord( PHB_RMFILTER pRM, HB_ULONG ulRec )
{
   if( --ulRec < pRM->ulRecords )
   {
      HB_BYTE * pRecMap = hb_rmItemBuf( pRM, HB_RM_ITEM_NO( ulRec ) );
      if( ( pRecMap[ HB_RM_ITEM_POS( ulRec ) ] & ( 1 << ( ulRec & 0x07 ) ) ) != 0 )
         return HB_TRUE;
      if( pRM->pRMmaybe )
      {
         pRecMap = hb_rmItemBufMB( pRM, HB_RM_ITEM_NO( ulRec ) );
         return ( pRecMap[ HB_RM_ITEM_POS( ulRec ) ] & ( 1 << ( ulRec & 0x07 ) ) ) != 0;
      }
   }
   else
      hb_rmSetError( M6ERR_RECRANGE );
   return HB_FALSE;
}

HB_BOOL hb_rmHasMB( PHB_RMFILTER pRM )
{
   hb_rmCheckMB( pRM );
   return ( pRM->pRMmaybe != NULL );
}

HB_ULONG hb_rmNextRecordMB( PHB_RMFILTER pRM, HB_ULONG ulRec )
{
   HB_ULONG ulNext = 0;
   if( pRM->pRMmaybe )
   {
      int i, j, l;

      for( i = HB_RM_ITEM_NO( ulRec ); ulNext == 0 && i < pRM->iItems &&
                                       ulRec < pRM->ulRecords; i++ )
      {
         HB_BYTE * pRecMap = hb_rmItemBufMB( pRM, i );
         for( j = HB_RM_ITEM_POS( ulRec ); ulNext == 0 && j < HB_RMITEM_SIZE &&
                                           ulRec < pRM->ulRecords; j++ )
         {
            if( pRecMap[ j ] )
            {
               for( l = ulRec & 0x07; l < 8; l++ )
               {
                  if( ( pRecMap[ j ] & ( 1 << l ) ) != 0 )
                  {
                     ulNext = ulRec + 1;
                     break;
                  }
                  ulRec++;
               }
            }
            else
               ulRec += 8 - ( ulRec & 0x07 );
         }
      }
   }
   return ulNext;
}

HB_ULONG hb_rmNextRecord( PHB_RMFILTER pRM, HB_ULONG ulRec )
{
   HB_ULONG ulNext = 0;
   int i, j, l;

   for( i = HB_RM_ITEM_NO( ulRec ); ulNext == 0 && i < pRM->iItems &&
                                    ulRec < pRM->ulRecords; i++ )
   {
      HB_BYTE * pRecMap = hb_rmItemBuf( pRM, i ), * pRecMapMB, b;
      pRecMapMB = pRM->pRMmaybe ? hb_rmItemBufMB( pRM, i ) : pRecMap;
      for( j = HB_RM_ITEM_POS( ulRec ); ulNext == 0 && j < HB_RMITEM_SIZE &&
                                        ulRec < pRM->ulRecords; j++ )
      {
         b = ( pRecMap[ j ] | pRecMapMB[ j ] );
         if( b )
         {
            for( l = ulRec & 0x07; l < 8; l++ )
            {
               if( ( b & ( 1 << l ) ) != 0 )
               {
                  ulNext = ulRec + 1;
                  break;
               }
               ulRec++;
            }
         }
         else
            ulRec += 8 - ( ulRec & 0x07 );
      }
   }
   return ulNext;
}

HB_ULONG hb_rmPrevRecord( PHB_RMFILTER pRM, HB_ULONG ulRec )
{
   HB_ULONG ulPrev = 0;

   if( ulRec > pRM->ulRecords )
      ulRec = pRM->ulRecords - 1;
   else
      ulRec -= 2;

   if( ulRec < pRM->ulRecords )
   {
      int i, j, l;

      for( i = HB_RM_ITEM_NO( ulRec ); ulPrev == 0 && i >= 0 &&
                                       ulRec < pRM->ulRecords; i-- )
      {
         HB_BYTE * pRecMap = hb_rmItemBuf( pRM, i ), * pRecMapMB, b;
         pRecMapMB = pRM->pRMmaybe ? hb_rmItemBufMB( pRM, i ) : pRecMap;
         for( j = HB_RM_ITEM_POS( ulRec ); ulPrev == 0 && j >= 0 &&
                                           ulRec < pRM->ulRecords; j-- )
         {
            b = ( pRecMap[ j ] | pRecMapMB[ j ] );
            if( b )
            {
               for( l = ulRec & 0x07; l >= 0; l-- )
               {
                  if( ( b & ( 1 << l ) ) != 0 )
                  {
                     ulPrev = ulRec + 1;
                     break;
                  }
                  ulRec--;
               }
            }
            else
               ulRec -= ( ulRec & 0x07 ) + 1;
         }
      }
   }
   return ulPrev;
}

HB_ULONG hb_rmCountRecords( PHB_RMFILTER pRM )
{
   HB_ULONG ulRec = 0, ulCount = 0;
   int i, j;

   for( i = 0; i < pRM->iItems; i++ )
   {
      HB_BYTE * pRecMap = hb_rmItemBuf( pRM, i ), * pRecMapMB;
      pRecMapMB = pRM->pRMmaybe ? hb_rmItemBufMB( pRM, i ) : pRecMap;

      for( j = 0; j < HB_RMITEM_SIZE && ulRec < pRM->ulRecords; j++ )
      {
         ulCount += s_bitCount[ pRecMap[ j ] | pRecMapMB[ j ] ];
         ulRec += 8;
      }
   }
   return ulCount;
}

HB_ULONG hb_rmRecordPos( PHB_RMFILTER pRM, HB_ULONG ulRecord )
{
   HB_ULONG ulCount = 0;

   if( hb_rmTestRecord( pRM, ulRecord ) )
   {
      HB_BYTE * pRecMap, * pRecMapMB;
      int i, j, l;

      --ulRecord;
      l = ulRecord & 0x07;
      i = HB_RM_ITEM_NO( ulRecord );
      j = HB_RM_ITEM_POS( ulRecord );
      pRecMap = hb_rmItemBuf( pRM, i );
      pRecMapMB = pRM->pRMmaybe ? hb_rmItemBufMB( pRM, i ) : pRecMap;
      ulCount += s_bitCount[ ( pRecMap[ j ] | pRecMapMB[ j ] ) &
                             ( ( 1 << ( l + 1 ) ) - 1 ) ];
      ulRecord -= l;
      for( i = HB_RM_ITEM_NO( ulRecord - 1 ); i >= 0 && ulRecord; i-- )
      {
         pRecMap = hb_rmItemBuf( pRM, i );
         pRecMapMB = pRM->pRMmaybe ? hb_rmItemBufMB( pRM, i ) : pRecMap;

         for( j = HB_RM_ITEM_POS( ulRecord - 1 ); j >= 0; j-- )
         {
            ulCount += s_bitCount[ pRecMap[ j ] | pRecMapMB[ j ] ];
            ulRecord -= 8;
         }
      }
   }
   return ulCount;
}

/* ************************************************************************ */

PHB_RMFILTER hb_rmNewQuery( AREAP pArea, PHB_ITEM pFilterText )
{
   PHB_RMFILTER pRM;

   pRM = hb_rmqBuildQRM( pArea, pFilterText );
   hb_rmSetOptLevel( pRM ? pRM->iOptLvl : RM_OPT_NONE );

   return pRM;
}

PHB_RMFILTER hb_rmGetAreaFilter( void )
{
   AREAP pArea = ( AREAP ) hb_rmGetRMAreaPointer();
   PHB_RMFILTER pRM = NULL;

   if( pArea )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );

      if( SELF_INFO( pArea, DBI_RM_HANDLE, pItem ) == HB_SUCCESS )
         pRM = hb_rmGetFilterPtr( hb_itemGetNI( pItem ) );
      else
         hb_rmSetError( M6ERR_NOTSUPP );
      hb_itemRelease( pItem );
   }
   return pRM;
}

HB_BOOL hb_rmSetAreaFilter( PHB_RMFILTER pRM )
{
   AREAP pArea = ( AREAP ) hb_rmGetRMAreaPointer();
   HB_BOOL fResult = HB_FALSE;

   if( pRM && pRM->iArea != 0 )
   {
      hb_rmSetError( M6ERR_BADRMTYPE );
   }
   else if( pArea )
   {
      PHB_ITEM pItem = hb_itemPutNI( NULL, pRM ? pRM->iHandle : 0 );

      if( SELF_INFO( pArea, DBI_RM_HANDLE, pItem ) == HB_SUCCESS )
      {
         PHB_RMFILTER pRMold = hb_rmGetFilterPtr( hb_itemGetNI( pItem ) );

         if( pRMold && pRM != pRMold )
         {
            hb_rmDestroy( pRMold );
         }
         fResult = HB_TRUE;
      }
      else
      {
         hb_rmSetError( M6ERR_NOTSUPP );
      }
      hb_itemRelease( pItem );
   }
   return fResult;
}

PHB_RMFILTER hb_rmReplaceAreaFilter( PHB_RMFILTER pRM )
{
   AREAP pArea = ( AREAP ) hb_rmGetRMAreaPointer();
   PHB_RMFILTER pRMold = NULL;

   if( pRM && pRM->iArea != 0 )
   {
      hb_rmSetError( M6ERR_BADRMTYPE );
   }
   else if( pArea )
   {
      PHB_ITEM pItem = hb_itemPutNI( NULL, pRM ? pRM->iHandle : 0 );

      if( SELF_INFO( pArea, DBI_RM_HANDLE, pItem ) == HB_SUCCESS )
         pRMold = hb_rmGetFilterPtr( hb_itemGetNI( pItem ) );
      else
         hb_rmSetError( M6ERR_NOTSUPP );
      hb_itemRelease( pItem );
   }
   return pRMold;
}

void hb_rmDoLinear( AREAP pArea )
{
   if( pArea )
   {
      PHB_RMFILTER pRM;
      HB_ULONG ulRec;
      int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pArea->uiArea )
         hb_rddSelectWorkAreaNumber( pArea->uiArea );
      else
         iCurrArea = 0;

      pRM = hb_rmGetAreaFilter();
      ulRec = pRM ? hb_rmNextRecordMB( pRM, 0 ) : 0;

      if( ulRec != 0 )
      {
         HB_BOOL fResult;
         HB_ULONG ulRecNo;

         SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
         do
         {
            if( SELF_GOTO( ( AREAP ) pArea, ulRec ) == HB_FAILURE )
               break;

            if( pArea->dbfi.itmCobExpr )
            {
               PHB_ITEM pResult;
               pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
               fResult = !HB_IS_LOGICAL( pResult ) || hb_itemGetL( pResult );
            }
            else
            {
               fResult = HB_TRUE;
            }
            if( fResult && hb_setGetDeleted() )
            {
               HB_BOOL fDeleted;
               SELF_DELETED( ( AREAP ) pArea, &fDeleted );
               fResult = !fDeleted;
            }
            if( fResult )
               hb_rmSetRecord( pRM, ulRec );
            else
               hb_rmClearRecord( pRM, ulRec );
            ulRec = hb_rmNextRecordMB( pRM, ulRec );
         }
         while( ulRec != 0 );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         hb_rmClearMB( pRM );
      }
      if( iCurrArea )
         hb_rddSelectWorkAreaNumber( iCurrArea );
   }
}

static void hb_rmFilterMark( HB_ULONG ulRec, HB_BYTE * pKeyVal, HB_ULONG ulLen, void * pParam )
{
   HB_SYMBOL_UNUSED( pKeyVal );
   HB_SYMBOL_UNUSED( ulLen );

   hb_rmSetRecord( ( PHB_RMFILTER ) pParam, ulRec );
}

HB_ULONG hb_rmMaybeEval( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pCodeBlock )
{
   HB_ULONG ulCount = 0;

   if( pArea )
   {
      HB_ULONG ulRec = hb_rmNextRecordMB( pRM, 0 );
      if( ulRec != 0 )
      {
         HB_ULONG ulRecNo;
         HB_BOOL fResult;

         SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
         do
         {
            if( SELF_GOTO( ( AREAP ) pArea, ulRec ) == HB_FAILURE )
               break;
            ulCount++;
            fResult = hb_itemGetL( hb_vmEvalBlockOrMacro( pCodeBlock ) );
            if( fResult )
               hb_rmSetRecord( pRM, ulRec );
            else
               hb_rmClearRecord( pRM, ulRec );
            ulRec = hb_rmNextRecordMB( pRM, ulRec );
         }
         while( ulRec != 0 );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         hb_rmClearMB( pRM );
      }
   }
   else
   {
      hb_rmSetError( M6ERR_NOTABLE );
   }

   return ulCount;
}

HB_ULONG hb_rmDbEval( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pCodeBlock )
{
   HB_ULONG ulCount = 0;

   if( pArea )
   {
      HB_ULONG ulRec = hb_rmNextRecord( pRM, 0 );
      if( ulRec != 0 )
      {
         HB_ULONG ulRecNo;
         HB_BOOL fResult;

         SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
         do
         {
            if( SELF_GOTO( ( AREAP ) pArea, ulRec ) == HB_FAILURE )
               break;

            if( pArea->dbfi.itmCobExpr )
            {
               PHB_ITEM pResult;
               pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
               fResult = !HB_IS_LOGICAL( pResult ) || hb_itemGetL( pResult );
            }
            else
            {
               fResult = HB_TRUE;
            }
            if( fResult && hb_setGetDeleted() )
            {
               HB_BOOL fDeleted;
               SELF_DELETED( ( AREAP ) pArea, &fDeleted );
               fResult = !fDeleted;
            }
            if( fResult )
            {
               ulCount++;
               hb_vmEvalBlockOrMacro( pCodeBlock );
            }
            ulRec = hb_rmNextRecord( pRM, ulRec );
         }
         while( ulRec != 0 );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         hb_rmClearMB( pRM );
      }
   }
   else
   {
      hb_rmSetError( M6ERR_NOTABLE );
   }

   return ulCount;
}

HB_ULONG hb_rmSetLoHi( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pItmLo, PHB_ITEM pItmHi, PHB_ITEM pTag, PHB_ITEM pBag )
{
   HB_ULONG ulCount = 0;

   if( pArea )
   {
      PHB_RMFILTER pRMold;
      DBORDERINFO OrderInfo;
      union {
         HB_EVALSCOPE_FUNC func;
         void *            data;
      } u;

      u.func = hb_rmFilterMark;
      memset( &OrderInfo, 0, sizeof( DBORDERINFO ) );
      OrderInfo.itmOrder    = pTag;
      OrderInfo.atomBagName = pBag;
      OrderInfo.itmResult   = hb_itemPutNI( NULL, 0 );
      OrderInfo.itmNewVal   = hb_itemNew( NULL );
      hb_arrayNew( OrderInfo.itmNewVal, DBRMI_SIZE );
      hb_arraySetPtr( OrderInfo.itmNewVal, DBRMI_FUNCTION, u.data );
      hb_arraySetPtr( OrderInfo.itmNewVal, DBRMI_PARAM, ( void * ) pRM );
      if( pItmLo )
         hb_arraySet( OrderInfo.itmNewVal, DBRMI_LOVAL, pItmLo );
      if( pItmHi )
         hb_arraySet( OrderInfo.itmNewVal, DBRMI_HIVAL, pItmHi );
      pRMold = hb_rmReplaceAreaFilter( NULL );
      SELF_ORDINFO( pArea, DBOI_SCOPEEVAL, &OrderInfo );
      hb_rmSetAreaFilter( pRMold );
      ulCount = hb_itemGetNL( OrderInfo.itmResult );
      hb_itemRelease( OrderInfo.itmNewVal );
      hb_itemRelease( OrderInfo.itmResult );
   }
   else
   {
      hb_rmSetError( M6ERR_NOTABLE );
   }

   return ulCount;
}

HB_BOOL hb_rmSave( PHB_RMFILTER pRM, const char * szFile )
{
   HB_BOOL fResult = HB_FALSE;
   HB_FHANDLE hFile = hb_fsExtOpen( szFile, NULL, FO_READWRITE |
                                    FO_EXCLUSIVE | FXO_TRUNCATE |
                                    FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL );

   if( hFile != FS_ERROR )
   {
      HB_RMFILE rmHeader;
      HB_SIZE nExpr, nNExpr, nRMLen, nTotal, nLen;
      HB_BOOL fHasMaybe;
      int i;

      nExpr = hb_itemGetCLen( pRM->pExpr );
      nNExpr = hb_itemGetCLen( pRM->pNonExpr );
      fHasMaybe = hb_rmHasMB( pRM );
      HB_PUT_LE_UINT32( rmHeader.hdrSig, RMFILE_SIGNATURE );
      HB_PUT_LE_UINT32( rmHeader.recNum, pRM->ulRecords );
      HB_PUT_LE_UINT32( rmHeader.currPos, pRM->ulPos );
      HB_PUT_LE_UINT32( rmHeader.expSize, nExpr );
      HB_PUT_LE_UINT32( rmHeader.nExpSize, nNExpr );
      rmHeader.optLvl[ 0 ] = ( HB_BYTE ) pRM->iOptLvl;
      rmHeader.hasMB[ 0 ] = fHasMaybe ? 1 : 0;
      HB_PUT_LE_UINT16( rmHeader.filler, 0 );
      nRMLen = ( pRM->ulRecords + 7 ) >> 3;

      if( hb_fsWrite( hFile, &rmHeader, sizeof( rmHeader ) ) == sizeof( rmHeader ) )
         fResult = HB_TRUE;
      else
         hb_rmSetError( M6ERR_FWRITE );

      if( fResult && nExpr )
      {
         if( hb_fsWriteLarge( hFile, hb_itemGetCPtr( pRM->pExpr ),
                              nExpr ) != nExpr )
         {
            hb_rmSetError( M6ERR_FWRITE );
            fResult = HB_FALSE;
         }
      }

      if( fResult && nNExpr )
      {
         if( hb_fsWriteLarge( hFile, hb_itemGetCPtr( pRM->pNonExpr ),
                              nNExpr ) != nNExpr )
         {
            hb_rmSetError( M6ERR_FWRITE );
            fResult = HB_FALSE;
         }
      }

      if( fResult )
      {
         nTotal = nRMLen;
         for( i = 0; i < pRM->iItems && nTotal; i++ )
         {
            nLen = HB_MIN( nTotal, HB_RMITEM_SIZE );
            if( hb_fsWriteLarge( hFile, hb_rmItemBuf( pRM, i ), nLen ) != nLen )
            {
               hb_rmSetError( M6ERR_FWRITE );
               fResult = HB_FALSE;
               break;
            }
            nTotal -= nLen;
         }
      }
      if( fResult && fHasMaybe )
      {
         nTotal = nRMLen;
         for( i = 0; i < pRM->iItems && nTotal; i++ )
         {
            nLen = HB_MIN( nTotal, HB_RMITEM_SIZE );
            if( hb_fsWriteLarge( hFile, hb_rmItemBufMB( pRM, i ), nLen ) != nLen )
            {
               hb_rmSetError( M6ERR_FWRITE );
               fResult = HB_FALSE;
               break;
            }
            nTotal -= nLen;
         }
      }

      hb_fsClose( hFile );
   }
   else
      hb_rmSetError( M6ERR_FCREATE );

   return fResult;
}

PHB_RMFILTER hb_rmRestore( const char * szFile )
{
   PHB_RMFILTER pRM = NULL;

   if( szFile && *szFile )
   {
      HB_FHANDLE hFile = hb_fsExtOpen( szFile, NULL, FO_READ |
                                       FO_DENYNONE | FXO_DEFAULTS | FXO_SHARELOCK,
                                       NULL, NULL );

      if( hFile != FS_ERROR )
      {
         HB_RMFILE rmHeader;
         HB_SIZE nSize = hb_fsSeekLarge( hFile, 0, FS_END );

         if( hb_fsSeek( hFile, 0, FS_SET ) == 0 &&
             hb_fsRead( hFile, &rmHeader, sizeof( rmHeader ) ) == sizeof( rmHeader ) )
         {
            HB_ULONG ulSig, ulRecords, ulPos;
            HB_SIZE nExp, nNExp, nTotal, nRMLen;
            int iOptLvl, i;
            HB_BOOL fHasMaybe;

            ulSig     = HB_GET_LE_UINT32( rmHeader.hdrSig );
            ulRecords = HB_GET_LE_UINT32( rmHeader.recNum );
            ulPos     = HB_GET_LE_UINT32( rmHeader.currPos );
            nExp      = HB_GET_LE_UINT32( rmHeader.expSize );
            nNExp     = HB_GET_LE_UINT32( rmHeader.nExpSize );
            iOptLvl   = rmHeader.optLvl[ 0 ];
            fHasMaybe = rmHeader.hasMB[ 0 ] != 0;
            nRMLen = ( ulRecords + 7 ) >> 3;
            nTotal = sizeof( rmHeader ) + nExp + nNExp +
                      nRMLen + ( fHasMaybe ? nRMLen : 0 );
            if( ulSig == RMFILE_SIGNATURE && nTotal <= nSize &&
                nExp < RMFILE_MAXEXPSIZE && nNExp < RMFILE_MAXEXPSIZE )
            {
               pRM = hb_rmCreate( ulRecords );
               if( pRM )
               {
                  pRM->ulPos = ulPos;
                  pRM->iOptLvl = iOptLvl;
                  if( nExp )
                  {
                     char *pExpr = ( char * ) hb_xgrab( nExp + 1 );
                     hb_fsReadLarge( hFile, pExpr, nExp );
                     pRM->pExpr = hb_itemPutCLPtr( pRM->pExpr, pExpr, nExp );
                  }
                  if( nNExp )
                  {
                     char *pNonExpr = ( char * ) hb_xgrab( nNExp + 1 );
                     hb_fsReadLarge( hFile, pNonExpr, nNExp );
                     pRM->pNonExpr = hb_itemPutCLPtr( pRM->pNonExpr, pNonExpr, nNExp );
                  }
                  nTotal = nRMLen;
                  for( i = 0; i < pRM->iItems && nTotal; i++ )
                  {
                     nSize = HB_MIN( nTotal, HB_RMITEM_SIZE );
                     if( hb_fsReadLarge( hFile, hb_rmItemBuf( pRM, i ), nSize ) != nSize )
                     {
                        hb_rmDestroy( pRM );
                        pRM = NULL;
                        hb_rmSetError( M6ERR_FREAD );
                        break;
                     }
                     nTotal -= nSize;
                  }
                  if( pRM && fHasMaybe )
                  {
                     nTotal = nRMLen;
                     if( !pRM->pRMmaybe )
                        pRM->pRMmaybe = hb_rmItemLstNew( pRM->iItems );
                     for( i = 0; i < pRM->iItems && nTotal; i++ )
                     {
                        nSize = HB_MIN( nTotal, HB_RMITEM_SIZE );
                        if( hb_fsReadLarge( hFile, hb_rmItemBufMB( pRM, i ), nSize ) != nSize )
                        {
                           hb_rmDestroy( pRM );
                           pRM = NULL;
                           hb_rmSetError( M6ERR_FREAD );
                           break;
                        }
                        nTotal -= nSize;
                     }
                  }
                  /* ???
                  if( pRM && pRM->iOptLvl != RM_OPT_FULL )
                     pRM = hb_rmMakeMB( pRM );
                  */
               }
            }
            else
               hb_rmSetError( M6ERR_FREAD );
         }
         else
            hb_rmSetError( M6ERR_FREAD );
         hb_fsClose( hFile );
      }
      else
         hb_rmSetError( M6ERR_FOPEN );
   }
   else
   {
      hb_rmSetError( M6ERR_TYPE );
   }
   return pRM;
}

/***************************************************************************/

static PHB_RMFILTER hb_rmGetFilterParPtr( int iParam )
{
   PHB_RMFILTER pRM;

   pRM = hb_rmGetFilterPtr( hb_parni( iParam ) );
   return pRM;
}

HB_FUNC( RLNEW )
{
   PHB_RMFILTER pRM = NULL;
   PHB_ITEM pItem = hb_param( 1, HB_IT_NUMERIC );

   if( pItem )
   {
      HB_ULONG ulRecords = hb_itemGetND( pItem ) < 0 ? 0 : hb_itemGetNL( pItem );
      pRM = hb_rmCreate( ulRecords );
   }
   else
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      HB_ULONG ulRecords;

      if( !pArea )
         hb_rmSetError( M6ERR_NOTABLE );
      else if( SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords ) == HB_SUCCESS )
         pRM = hb_rmCreate( ulRecords );
      else
         hb_rmSetError( M6ERR_TYPE );
   }

   hb_retni( pRM ? pRM->iHandle : 0 );
}

HB_FUNC( RLDESTROY )
{
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
      hb_rmDestroy( pRM );
}

HB_FUNC( RLRESIZE )
{
   int iHandle = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      PHB_ITEM pItem = hb_param( 1, HB_IT_NUMERIC );
      if( pItem )
      {
         HB_ULONG ulRecords = hb_itemGetND( pItem ) < 0 ? 0 : hb_itemGetNL( pItem );
         pRM = hb_rmReSize( pRM, ulRecords );
         if( pRM )
         {
            iHandle = pRM->iHandle;
         }
      }
      else
      {
         hb_rmSetError( M6ERR_TYPE );
      }
   }
   hb_retni( iHandle );
}

HB_FUNC( RLNEWDUP )
{
   int iHandle = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      pRM = hb_rmDup( pRM );
      if( pRM )
      {
         iHandle = pRM->iHandle;
      }
   }
   hb_retni( iHandle );
}

HB_FUNC( RLNEWQUERY )
{
   int iHandle = 0;
   AREAP pArea = ( AREAP ) hb_rmGetRMAreaPointer();

   if( pArea )
   {
      PHB_RMFILTER pRM = hb_rmNewQuery( pArea, hb_param( 1, HB_IT_ANY ) );

      if( pRM )
      {
         iHandle = pRM->iHandle;
      }
   }
   hb_retni( iHandle );
}

HB_FUNC( RLGETFILTER )
{
   int iHandle = 0;
   PHB_RMFILTER pRM = hb_rmGetAreaFilter();
   if( pRM )
   {
      iHandle = pRM->iHandle;
   }
   hb_retni( iHandle );
}

HB_FUNC( RLSETFILTER )
{
   hb_rmSetAreaFilter( hb_rmGetFilterParPtr( 1 ) );
}

HB_FUNC( RLEXFILTER )
{
   int iHandle = 0;
   PHB_RMFILTER pRM = hb_rmReplaceAreaFilter( NULL );

   if( pRM )
   {
      iHandle = pRM->iHandle;
   }
   hb_retni( iHandle );
}

HB_FUNC( RLDOLINEAR )
{
   AREAP pArea = ( AREAP ) hb_rmGetRMAreaPointer();
   if( pArea )
      hb_rmDoLinear( pArea );
}

HB_FUNC( RLHASMAYBE )
{
   HB_BOOL fHas = HB_FALSE;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      fHas = hb_rmHasMB( pRM );
   }
   hb_retl( fHas );
}

HB_FUNC( RLMAYBEEVAL )
{
   HB_ULONG ulResult = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK );

   if( pArea )
   {
      if( pRM && pBlock )
      {
         ulResult = hb_rmMaybeEval( pArea, pRM, pBlock );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RLMAYBEEVAL" );
   }
   hb_retnint( ulResult );
}

HB_FUNC( RLSETLOHI )
{
   HB_ULONG ulResult = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      if( pRM )
      {
         ulResult = hb_rmSetLoHi( pArea, pRM,
                                  hb_param( 2, HB_IT_ANY ),
                                  hb_param( 3, HB_IT_ANY ),
                                  hb_param( 4, HB_IT_ANY ),
                                  hb_param( 5, HB_IT_ANY ) );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RLSETLOHI" );
   }
   hb_retnint( ulResult );
}

HB_FUNC( RLOR )
{
   int iHandle = 0;
   PHB_RMFILTER pRM1 = hb_rmGetFilterParPtr( 1 ),
                pRM2 = hb_rmGetFilterParPtr( 2 );
   if( pRM1 && pRM2 )
   {
      pRM1 = hb_rmOR( pRM1, pRM2 );
      if( pRM1 )
         iHandle = pRM1->iHandle;
   }
   hb_retni( iHandle );
}

HB_FUNC( RLAND )
{
   int iHandle = 0;
   PHB_RMFILTER pRM1 = hb_rmGetFilterParPtr( 1 ),
                pRM2 = hb_rmGetFilterParPtr( 2 );
   if( pRM1 && pRM2 )
   {
      pRM1 = hb_rmAND( pRM1, pRM2 );
      if( pRM1 )
         iHandle = pRM1->iHandle;
   }
   hb_retni( iHandle );
}

HB_FUNC( RLXOR )
{
   int iHandle = 0;
   PHB_RMFILTER pRM1 = hb_rmGetFilterParPtr( 1 ),
                pRM2 = hb_rmGetFilterParPtr( 2 );
   if( pRM1 && pRM2 )
   {
      pRM1 = hb_rmXOR( pRM1, pRM2 );
      if( pRM1 )
         iHandle = pRM1->iHandle;
   }
   hb_retni( iHandle );
}

HB_FUNC( RLNOT )
{
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );
   int iHandle = 0;

   if( pRM )
      pRM = hb_rmNOT( pRM );
   if( pRM )
      iHandle = pRM->iHandle;

   hb_retni( iHandle );
}

HB_FUNC( RLLEN )
{
   HB_ULONG ulLen = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      ulLen = pRM->ulRecords;
   }
   hb_retnint( ulLen );
}

HB_FUNC( RLCOUNT )
{
   HB_ULONG ulCount = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      ulCount = hb_rmCountRecords( pRM );
   }
   hb_retnint( ulCount );
}

HB_FUNC( RLPOSRECNO )
{
   HB_ULONG ulPos = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      ulPos = hb_rmRecordPos( pRM, hb_parnl( 2 ) );
   }
   hb_retnint( ulPos );
}

HB_FUNC( RLNEXTRECNO )
{
   HB_ULONG ulNext = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      ulNext = hb_rmNextRecord( pRM, hb_parnl( 2 ) );
   }
   hb_retnint( ulNext );
}

HB_FUNC( RLPREVRECNO )
{
   HB_ULONG ulPrev = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      ulPrev = hb_rmPrevRecord( pRM, hb_parnl( 2 ) );
   }
   hb_retnint( ulPrev );
}

HB_FUNC( RLTEST )
{
   HB_BOOL fSet = HB_FALSE;
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
   {
      fSet = hb_rmTestRecord( pRM, hb_parnl( 2 ) );
   }
   hb_retl( fSet );
}

HB_FUNC( RLSET )
{
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
      hb_rmSetRecord( pRM, hb_parnl( 2 ) );
}

HB_FUNC( RLCLEAR )
{
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );

   if( pRM )
      hb_rmClearRecord( pRM, hb_parnl( 2 ) );
}

HB_FUNC( RLOPTLEVEL )
{
   if( hb_parclen( 1 ) )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      hb_retni( pArea ? hb_rmqOptLevel( pArea, hb_param( 1, HB_IT_STRING ) ) :
                        RM_OPT_NONE );
   }
   else
   {
      PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );
      hb_retni( pRM ? pRM->iOptLvl : hb_rmGetOptLevel() );
   }
}

HB_FUNC( RLNONOPT )
{
   if( hb_parclen( 1 ) )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      if( pArea )
      {
         PHB_ITEM pItem = hb_rmqNonOptExpression( pArea, hb_param( 1, HB_IT_STRING ) );
         if( pItem )
         {
            hb_itemReturnRelease( pItem );
         }
         else
         {
            hb_retc_null();
         }
      }
      else
      {
         hb_retc( hb_parc( 1 ) );
      }
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( RLERROR )
{
   hb_retni( hb_rmGetError() );
}


HB_FUNC( RLDO )
{
   PHB_RMFILTER pRM = hb_rmGetFilterParPtr( 1 );
   int iOper = hb_parni( 2 );

   if( pRM )
   {
      switch( iOper )
      {
         case 1:
            hb_rmFill( pRM );
            break;
         case 2:
            hb_rmClear( pRM );
            break;
         case 3:
            hb_rmFillMB( pRM );
            break;
         case 4:
            hb_rmClearMB( pRM );
            break;
         case 5:
            hb_rmMakeMB( pRM );
            break;
         case 10:
            hb_retnint( pRM->ulRecords );
            break;
         case 11:
            hb_retni( pRM->iItems );
            break;
         case 12:
            hb_retni( HB_RMITEM_SIZE );
            break;
         case 13:
            hb_retni( HB_RM_ITEM_NO( hb_parnl( 3 ) ) );
            break;
         case 14:
            hb_retni( HB_RM_ITEM_POS( hb_parnl( 3 ) ) );
            break;
         default:
            break;
      }
   }
}
