/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The garbage collector for Harbour
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "error.ch"
#include "hashapi.h"

HB_EXTERN_BEGIN
extern HB_ITEM hb_vm_BreakBlock;
HB_EXTERN_END

#define HB_GC_COLLECTION_JUSTIFIED  64

/* status of memory block */
#define HB_GC_UNLOCKED              0
#define HB_GC_LOCKED                1  /* do not collect a memory block */
#define HB_GC_USED_FLAG             2  /* the bit for used/unused flag */
#define HB_GC_DELETE                4  /* item will be deleted during finalization */

/* pointer to memory block that will be checked in next step */
static PHB_GARBAGE s_pCurrBlock = NULL;
/* memory blocks are stored in linked list with a loop */

/* pointer to locked memory blocks */
static PHB_GARBAGE s_pLockedBlock = NULL;

/* #define GC_RECYCLE */

#ifdef GC_RECYCLE
/* pointer to Cached Items memory blocks */
static PHB_GARBAGE   s_pAvailableItems       = NULL;

/* pointer to Cached BaseArrays memory blocks */
static PHB_GARBAGE   s_pAvailableBaseArrays  = NULL;
#endif

/* marks if block releasing is requested during garbage collecting */
static volatile BOOL s_bCollecting     = FALSE;

/* Signify ReleaseAll Processing is taking place. */
HB_EXTERN_BEGIN
#ifndef HB_ARRAY_USE_COUNTER
BOOL                 hb_gc_bReleaseAll = FALSE;
#else
static BOOL          hb_gc_bReleaseAll = FALSE;
#endif
HB_EXTERN_END

/* flag for used/unused blocks - the meaning of the HB_GC_USED_FLAG bit
 * is reversed on every collecting attempt
 */
static USHORT  s_uUsedFlag             = HB_GC_USED_FLAG;

static HB_SIZE s_uAllocated            = 0;
static HB_SIZE s_uAllocatedCnt         = 0;

#ifdef GC_RECYCLE
   #define HB_GARBAGE_FREE( pAlloc )   ( pAlloc->pFunc == hb_gcGripRelease ? \
                                         ( hb_gcLink( &s_pAvailableItems, pAlloc ) ) \
                                         : \
                                         ( pAlloc->pFunc == hb_arrayReleaseGarbage ? \
                                           ( hb_gcLink( &s_pAvailableBaseArrays, pAlloc ) ) \
                                           : \
                                           ( hb_xfree( ( void * ) ( pAlloc ) ) ) \
                                         ) \
                                         )
#else
   #define HB_GARBAGE_NEW( ulSize )    ( PHB_GARBAGE ) hb_xgrab( ulSize )
   #define HB_GARBAGE_FREE( pAlloc )   hb_xfree( ( void * ) ( pAlloc ) )
#endif

#define HB_GC_PTR( p )                 ( ( ( PHB_GARBAGE ) p ) - 1 )

/* Forward declaration.*/
static HB_GARBAGE_FUNC( hb_gcGripRelease );

static void hb_gcLink( PHB_GARBAGE * pList, PHB_GARBAGE pAlloc )
{
   if( *pList )
   {
      /* add new block at the logical end of list */
      pAlloc->pNext        = *pList;
      pAlloc->pPrev        = ( *pList )->pPrev;
      pAlloc->pPrev->pNext = pAlloc;
      ( *pList )->pPrev    = pAlloc;
   }
   else
      *pList = pAlloc->pNext = pAlloc->pPrev = pAlloc;
}

static void hb_gcUnlink( PHB_GARBAGE * pList, PHB_GARBAGE pAlloc )
{
   pAlloc->pPrev->pNext = pAlloc->pNext;
   pAlloc->pNext->pPrev = pAlloc->pPrev;

   if( *pList == pAlloc )
   {
      *pList = pAlloc->pNext;

      if( *pList == pAlloc )
         *pList = NULL;    /* this was the last block */
   }
}

/* allocates a memory block */
void * hb_gcAlloc( HB_SIZE ulSize, PHB_GARBAGE_FUNC pCleanupFunc )
{
   PHB_GARBAGE pAlloc;

#ifdef GC_RECYCLE
   HB_CRITICAL_LOCK( hb_garbageAllocMutex );
   if( s_pAvailableBaseArrays && ulSize == sizeof( HB_BASEARRAY ) )
   {
      pAlloc = s_pAvailableBaseArrays;
      hb_gcUnlink( &s_pAvailableBaseArrays, s_pAvailableBaseArrays );
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
   }
   else
   {
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
      pAlloc = ( PHB_GARBAGE ) hb_xgrab( ulSize + sizeof( HB_GARBAGE ) );
   }
#else
   pAlloc = HB_GARBAGE_NEW( ulSize + sizeof( HB_GARBAGE ) );
#endif

   if( pAlloc )
   {
      pAlloc->pFunc     = pCleanupFunc;
      pAlloc->ulHolders = 0;
      pAlloc->locked    = 0;
      pAlloc->used      = s_uUsedFlag;

      HB_CRITICAL_LOCK( hb_garbageAllocMutex );
      s_uAllocated++;
      s_uAllocatedCnt++;
      hb_gcLink( &s_pCurrBlock, pAlloc );
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );

      HB_TRACE( HB_TR_DEBUG, ( "hb_gcAlloc %p in %p", pAlloc + 1, pAlloc ) );

      return ( void * ) ( pAlloc + 1 );   /* hide the internal data */
   }
   else
      return NULL;
}

void hb_gcIncRef( void * pBlock )
{
   PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pBlock;

   --pAlloc;

   HB_ATOMIC_INC( pAlloc->ulHolders );
}

HB_SIZE hb_gcDecRef( void * pBlock )
{
   PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pBlock;

   --pAlloc;

   if( pAlloc->ulHolders == 0 )
      hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature Pointer Release detected: '%p'", ( char * ) pBlock, NULL );

   if( HB_ATOMIC_DEC( pAlloc->ulHolders ) == 0 && ! pAlloc->locked )
   {
      /* OutputDebugString("Calling GC Cleanup function...");
       */
      if( pAlloc->pFunc )
         ( pAlloc->pFunc )( ( void * ) ( pBlock ) );

      /* OutputDebugString("Attempting to free GC mem...");
       */
      hb_gcFree( pBlock );
      /* OutputDebugString("GC mem freed...");
       */

      return 0;
   }
   else
      return pAlloc->ulHolders;
}

/* release a memory block allocated with hb_gcAlloc() */
void hb_gcFree( void * pBlock )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gcFree(%p)", pBlock ) );

   if( hb_gc_bReleaseAll )
   {
      HB_TRACE( HB_TR_DEBUG, ( "Aborted - hb_gcFree(%p)", pBlock ) );
      return;
   }

   if( pBlock )
   {
      PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pBlock;
      --pAlloc;

      if( pAlloc->locked )
      {
         HB_TRACE( HB_TR_DEBUG, ( "hb_gcFree(%p) *LOCKED* %p", pBlock, pAlloc ) );

         HB_THREAD_GUARD( hb_garbageAllocMutex, hb_gcUnlink( &s_pLockedBlock, pAlloc ) );

         HB_GARBAGE_FREE( pAlloc );
      }
      else
      {
         /* Might already be marked for deletion.
          */
         HB_CRITICAL_LOCK( hb_garbageAllocMutex );
         if( ! ( pAlloc->used & HB_GC_DELETE ) )
         {
            s_uAllocated--;
            s_uAllocatedCnt--;
            hb_gcUnlink( &s_pCurrBlock, pAlloc );
            HB_GARBAGE_FREE( pAlloc );
         }
         HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
      }
   }
   else
      hb_errInternal( HB_EI_XFREENULL, NULL, NULL, NULL );
}

/* return cleanup function pointer */
PHB_GARBAGE_FUNC hb_gcFunc( void * pBlock )
{
   return HB_GC_PTR( pBlock )->pFunc;
}

HB_GARBAGE_FUNC( hb_gcDummyMark )
{
   HB_SYMBOL_UNUSED( Cargo );
}

static HB_GARBAGE_FUNC( hb_gcGripRelease )
{
   /* Item was already released in hb_gcGripDrop() - then we have nothing
    * to do here
    */
   HB_SYMBOL_UNUSED( Cargo );
}

/** JC1:
 * Warning: THREAD UNSAFE
 * If pOrigin is given, hb_gcGripGet must be provided in a thread safe way,
 * (generally locking the data set of pOrigin)
 ****/

PHB_ITEM hb_gcGripGet( PHB_ITEM pOrigin )
{
   PHB_GARBAGE pAlloc;

#ifdef GC_RECYCLE
   HB_CRITICAL_LOCK( hb_garbageAllocMutex );
   if( s_pAvailableItems )
   {
      pAlloc = s_pAvailableItems;
      hb_gcUnlink( &s_pAvailableItems, s_pAvailableItems );
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
   }
   else
   {
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
      pAlloc = ( PHB_GARBAGE ) hb_xgrab( sizeof( HB_ITEM ) + sizeof( HB_GARBAGE ) );
   }
#else
   pAlloc = HB_GARBAGE_NEW( sizeof( HB_ITEM ) + sizeof( HB_GARBAGE ) );
#endif /* GC_RECYCLE */

   if( pAlloc )
   {
      PHB_ITEM pItem = ( PHB_ITEM ) ( pAlloc + 1 );

      pAlloc->pFunc  = hb_gcGripRelease;
      pAlloc->locked = 1;
      pAlloc->used   = s_uUsedFlag;

      pItem->type    = HB_IT_NIL;

      if( pOrigin )
         hb_itemCopy( pItem, pOrigin );

      HB_THREAD_GUARD( hb_garbageAllocMutex, hb_gcLink( &s_pLockedBlock, pAlloc ) );
      return pItem;
   }
   else
      return NULL;
}

void hb_gcGripDrop( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gcGripDrop(%p)", pItem ) );

   if( hb_gc_bReleaseAll )
   {
      HB_TRACE( HB_TR_DEBUG, ( "Aborted - hb_gcGripDrop(%p)", pItem ) );
      return;
   }

   if( pItem )
   {
      PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pItem;
      --pAlloc;

      HB_TRACE( HB_TR_INFO, ( "Drop %p %p", pItem, pAlloc ) );

      if( pAlloc->pFunc == hb_gcGripRelease )
      {
         if( HB_IS_COMPLEX( pItem ) )
            hb_itemClear( pItem );    /* clear value stored in this item */
      }

      HB_CRITICAL_LOCK( hb_garbageAllocMutex );

      hb_gcUnlink( &s_pLockedBlock, pAlloc );

      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );

      HB_GARBAGE_FREE( pAlloc );
   }
}

/* Lock a memory pointer so it will not be released if stored
   outside of harbour variables
 */
void * hb_gcLock( void * pBlock )
{
   if( pBlock )
   {
      PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pBlock;
      --pAlloc;

      if( ! pAlloc->locked )
      {
         HB_CRITICAL_LOCK( hb_garbageAllocMutex );

         hb_gcUnlink( &s_pCurrBlock, pAlloc );

         hb_gcLink( &s_pLockedBlock, pAlloc );

         pAlloc->used = s_uUsedFlag;

         HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
      }
      ++pAlloc->locked;
   }

   return pBlock;
}

/* Unlock a memory pointer so it can be released if there is no
   references inside of harbour variables
 */
void * hb_gcUnlock( void * pBlock )
{
   if( pBlock )
   {
      PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pBlock;
      --pAlloc;

      if( pAlloc->locked )
      {
         if( --pAlloc->locked == 0 )
         {
            HB_CRITICAL_LOCK( hb_garbageAllocMutex );

            hb_gcUnlink( &s_pLockedBlock, pAlloc );

            hb_gcLink( &s_pCurrBlock, pAlloc );

            pAlloc->used = s_uUsedFlag;

            HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
         }
      }
   }

   return pBlock;
}

#if ( ( defined( _MSC_VER ) && _MSC_VER <= 1200 && ! defined( __POCC__ ) ) || defined( __DMC__ ) || defined( __ICL ) )
#else
   #define SIMULATE_ITEMREF_RECURSION
#endif

#ifdef SIMULATE_ITEMREF_RECURSION

typedef struct
{
   char cResumePoint;

   union
   {
      /* ResumePoint_1 does not require any data! */

      struct
      {
         PHB_ITEM pItem;
         HB_SIZE ulSize;
      } ResumePoint_2;     /* Resume: if( HB_IS_ARRAY( pItem ) ) */

      struct
      {
         PHB_ITEM pKey;
         PHB_ITEM pValue;
         HB_SIZE ulSize;
      } ResumePoint_3;     /* Resume (after pKey): if( HB_IS_HASH( pItem ) ) */

      struct
      {
         PHB_ITEM pKey;
         PHB_ITEM pValue;
         HB_SIZE ulSize;
      } ResumePoint_4;     /* Resume (after pValue): if( HB_IS_HASH( pItem ) ) */

      struct
      {
         PHB_CODEBLOCK pCBlock;
         USHORT ui;
      } ResumePoint_5;     /* Resume: if( HB_IS_BLOCK( pItem ) ) */

   } data;

} ITEMREF_RESUMEINFO, * PITEMREF_RESUMEINFO;

   #define NESTED_ITEMREF( pNewItem, cNewResumePoint ) \
            \
   pResumeInfo[ iResumeCounter ].cResumePoint = ( cNewResumePoint ); \
            \
   switch( ( cNewResumePoint ) ) \
   { \
      case 1: \
         break; \
              \
      case 2: \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_2.pItem   = pItem; \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_2.ulSize  = ulSize; \
         break; \
               \
      case 3: \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_3.pKey    = pKey; \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_3.pValue  = pValue; \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_3.ulSize  = ulSize; \
         break; \
               \
      case 4: \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_4.pKey    = pKey; \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_4.pValue  = pValue; \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_4.ulSize  = ulSize; \
         break; \
               \
      case 5: \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_5.pCBlock = pCBlock; \
         pResumeInfo[ iResumeCounter ].data.ResumePoint_5.ui      = ui; \
         break; \
               \
      default: \
         /* Unexpected case! */ \
         assert( 0 ); \
   } \
            \
   pResumeInfo = ( PITEMREF_RESUMEINFO ) hb_xrealloc( ( void * ) pResumeInfo, ( ++iResumeCounter + 1 ) * sizeof( ITEMREF_RESUMEINFO ) ); \
            \
   pItem       = ( pNewItem ); \
   goto ItemRef_Top;

   #define RETURN_OR_RESUME_ITEMREF() \
            \
   if( iResumeCounter == 0 ) \
   { \
      hb_xfree( ( void * ) pResumeInfo ); \
      return; \
   } \
            \
   --iResumeCounter; \
            \
   switch( pResumeInfo[ iResumeCounter ].cResumePoint ) \
   { \
      case 1: \
         goto ItemRef_ResumePoint_1; \
              \
      case 2: \
         pItem    = pResumeInfo[ iResumeCounter ].data.ResumePoint_2.pItem; \
         ulSize   = pResumeInfo[ iResumeCounter ].data.ResumePoint_2.ulSize; \
         goto ItemRef_ResumePoint_2; \
               \
      case 3: \
         pKey     = pResumeInfo[ iResumeCounter ].data.ResumePoint_3.pKey; \
         pValue   = pResumeInfo[ iResumeCounter ].data.ResumePoint_3.pValue; \
         ulSize   = pResumeInfo[ iResumeCounter ].data.ResumePoint_3.ulSize; \
         goto ItemRef_ResumePoint_3; \
               \
      case 4: \
         pKey     = pResumeInfo[ iResumeCounter ].data.ResumePoint_4.pKey; \
         pValue   = pResumeInfo[ iResumeCounter ].data.ResumePoint_4.pValue; \
         ulSize   = pResumeInfo[ iResumeCounter ].data.ResumePoint_4.ulSize; \
         goto ItemRef_ResumePoint_4; \
               \
      case 5: \
         pCBlock  = pResumeInfo[ iResumeCounter ].data.ResumePoint_5.pCBlock; \
         ui       = pResumeInfo[ iResumeCounter ].data.ResumePoint_5.ui; \
         goto ItemRef_ResumePoint_5; \
               \
      default: \
         /* Unexpected case! */ \
         assert( 0 ); \
   }
#else
   #define NESTED_ITEMREF( pNewItem, nIgnore )  hb_gcItemRef( pNewItem )
   #define RETURN_OR_RESUME_ITEMREF()           return
#endif


/* Mark a passed item as used so it will be not released by the GC
 */
void hb_gcItemRef( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_SIZE              ulSize = 0;
   HB_ITEM              FakedItem;
   PHB_ITEM             pKey = NULL;
   PHB_ITEM             pValue = NULL;
   PHB_CODEBLOCK     pCBlock = NULL;
   USHORT               ui = 0;

#ifdef SIMULATE_ITEMREF_RECURSION
   PITEMREF_RESUMEINFO  pResumeInfo    = ( PITEMREF_RESUMEINFO ) hb_xgrab( sizeof( ITEMREF_RESUMEINFO ) );
   int                  iResumeCounter = 0;
#endif

   FakedItem.type = HB_IT_ARRAY;

#ifdef SIMULATE_ITEMREF_RECURSION
   ItemRef_Top:
#endif

   while( HB_IS_BYREF( pItem ) )
   {
      if( HB_IS_EXTREF( pItem ) )
      {

         pItem->item.asExtRef.func->mark( pItem->item.asExtRef.value );

         RETURN_OR_RESUME_ITEMREF();
      }

      if( HB_IS_MEMVAR( pItem ) == FALSE )
      {
         if( pItem->item.asRefer.offset == 0 )
         {
            FakedItem.item.asArray.value = pItem->item.asRefer.BasePtr.pBaseArray;

            /* hb_gcItemRef( &FakedItem );
             */
            NESTED_ITEMREF( &FakedItem, 1 );
#ifdef SIMULATE_ITEMREF_RECURSION
            ItemRef_ResumePoint_1:
#endif
            /* return;
             */
            RETURN_OR_RESUME_ITEMREF();
         }
      }
      else
      {
         if( HB_VM_STACK.pPos == HB_VM_STACK.pItems )
         {
            /* return;
             */
            RETURN_OR_RESUME_ITEMREF();
         }
      }

      pItem = hb_itemUnRefOnce( pItem );
   }

   if( HB_IS_ARRAY( pItem ) )
   {
      PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pItem->item.asArray.value;

      /* printf( "Array %p\n", pItem->item.asArray.value );
       */
      --pAlloc;

      /* Check this array only if it was not checked yet
       */
      if( pAlloc->used == s_uUsedFlag )
      {
         ulSize         = pItem->item.asArray.value->ulLen;
         /* mark this block as used so it will be no re-checked from
          * other references
          */
         pAlloc->used   ^= HB_GC_USED_FLAG;

         /* mark also all array elements
          */
         pItem          = pItem->item.asArray.value->pItems;
         /* printf( "Items %p\n", pItem );
          */

         while( ulSize )
         {
            /* printf( "Item %p\n", pItem );
             * hb_gcItemRef( pItem );
             */
            NESTED_ITEMREF( pItem, 2 );
#ifdef SIMULATE_ITEMREF_RECURSION
            ItemRef_ResumePoint_2:
#endif
            ++pItem;
            --ulSize;
         }
      }
   }
   else if( HB_IS_HASH( pItem ) )
   {
      PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pItem->item.asHash.value;
      --pAlloc;

      /* Check this hash only if it was not checked yet */
      if( pAlloc->used == s_uUsedFlag )
      {
         ulSize         = pItem->item.asHash.value->ulLen;
         pKey           = pItem->item.asHash.value->pKeys;
         pValue         = pItem->item.asHash.value->pValues;

         /* mark this block as used so it will be no re-checked from
          * other references
          */
         pAlloc->used   ^= HB_GC_USED_FLAG;

         /* mark also all hash elements */
         while( ulSize )
         {
            /* printf( "Kry %p Value: %p\n", pKey, pValue );
             * hb_gcItemRef( pKey );
             */
            NESTED_ITEMREF( pKey, 3 );
#ifdef SIMULATE_ITEMREF_RECURSION
            ItemRef_ResumePoint_3:
#endif
            /* hb_gcItemRef( pValue );
             */
            NESTED_ITEMREF( pValue, 4 );
#ifdef SIMULATE_ITEMREF_RECURSION
            ItemRef_ResumePoint_4:
#endif
            ++pKey;
            ++pValue;
            --ulSize;
         }
      }
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pItem->item.asBlock.value;
      --pAlloc;

      /* Check this block only if it was not checked yet
       */
      if( pAlloc->used == s_uUsedFlag )
      {
         pCBlock        = pItem->item.asBlock.value;
         ui             = 1;

         pAlloc->used   ^= HB_GC_USED_FLAG; /* mark this codeblock as used */

         /* mark as used all detached variables in a codeblock
          */
         while( ui <= pCBlock->uiLocals )
         {
            /* hb_gcItemRef( &pCBlock->pLocals[ ui ] );
             */
            NESTED_ITEMREF( &pCBlock->pLocals[ ui ], 5 );
#ifdef SIMULATE_ITEMREF_RECURSION
            ItemRef_ResumePoint_5:
#endif
            ++ui;
         }
      }
   }
   else if( HB_IS_POINTER( pItem ) )
   {
      /* check if this memory was allocated by a hb_gcAlloc()
       */
      if( pItem->item.asPointer.collect )
      {
         PHB_GARBAGE pAlloc = ( PHB_GARBAGE ) pItem->item.asPointer.value;
         --pAlloc;

         /* Check this memory only if it was not checked yet
          */
         if( pAlloc->used == s_uUsedFlag )
         {
            /* mark this memory as used so it will be no re-checked from
             * other references
             */
            pAlloc->used ^= HB_GC_USED_FLAG;
         }
      }
   }

   /* all other data types don't need the GC
    */
   RETURN_OR_RESUME_ITEMREF();
}

void hb_gcCollect( void )
{
   /* TODO: decrease the amount of time spend collecting
    */
   hb_gcCollectAll( FALSE );
}

/* Check all memory blocks if they can be released
 */
void hb_gcCollectAll( BOOL bForce )
{
   PHB_GARBAGE pAlloc, pDelete;

   HB_TRACE( HB_TR_INFO, ( "hb_gcCollectAll(%i), %p, %i", bForce, s_pCurrBlock, s_bCollecting ) );

   /* is anoter garbage in action? */
#ifdef HB_THREAD_SUPPORT
   HB_CRITICAL_LOCK( hb_garbageAllocMutex );
   if( s_pCurrBlock == NULL || ( bForce == FALSE && s_uAllocated < HB_GC_COLLECTION_JUSTIFIED ) )
   {
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );
      return;
   }
   HB_CRITICAL_UNLOCK( hb_garbageAllocMutex );

   /* Force this thread to be an idle inspector: only this thread can run
      past this point; depending on settings, this thread may prevents others
      to regain control or just wait for a time where no thread is active. */
   hb_threadWaitForIdle();

#else
   /* note: 1) is volatile and
    *       2) not very important if fails 1 time
    */
   if( s_bCollecting )
      return;

   /* Even if not locked, a read only non-critical variable here
    * should not be a problem
    */
   if( s_pCurrBlock == NULL || ( bForce == FALSE && s_uAllocated < HB_GC_COLLECTION_JUSTIFIED ) )
   {
      s_bCollecting = FALSE;
      return;
   }
#endif

   /* By hypotesis, only one thread will be granted the right to be here;
      so cheching for consistency of s_pCurrBlock further is useless.*/

   /* Now that we are rightful owner of the GC process, we must
    * forbid all other threads from acting into the objects that
    * are going to be (in different times):
    * - scanned,
    * - freed (in their members)
    * - modified/released (in their strucure )
    *****/

   s_bCollecting  = TRUE;
   s_uAllocated   = 0;

   /* Step 1 - mark */
   /* All blocks are already marked because we are flipping
    * the used/unused flag
    */

   HB_TRACE( HB_TR_INFO, ( "Sweep Scan" ) );

#ifdef TRACE_COLLECT
   TraceLog( NULL, "Sweep Scan\n" );
#endif

   /* Step 1 - MARK */
   /* check all known places for blocks they are referring */
#ifdef HB_THREAD_SUPPORT
   hb_threadIsLocalRef();
#else
   hb_vmIsLocalRef();
#endif

#ifdef TRACE_COLLECT
   TraceLog( NULL, "After LocalRef\n" );
#endif

   hb_vmIsStaticRef();
#ifdef TRACE_COLLECT
   TraceLog( NULL, "After StaticRef\n" );
#endif

   hb_vmIsGlobalRef();
#ifdef TRACE_COLLECT
   TraceLog( NULL, "After Globals\n" );
#endif

#ifndef HB_THREAD_SUPPORT
   /* JC1: under MT, each threadIsLocalRef does its memvar reffing */
   hb_memvarsIsMemvarRef();
#endif

#ifdef TRACE_COLLECT
   TraceLog( NULL, "After MemvarRef\n" );
#endif

   hb_clsIsClassRef();
#ifdef TRACE_COLLECT
   TraceLog( NULL, "After ClassRef\n" );
#endif

   if( HB_IS_GCITEM( &hb_vm_BreakBlock ) )
   {
      hb_gcItemRef( &hb_vm_BreakBlock );
   }
#ifdef TRACE_COLLECT
   TraceLog( NULL, "After BreakBlock\n" );
#endif

   HB_TRACE( HB_TR_INFO, ( "Locked Scan" ) );

   /* check list of locked blocks for blocks referenced from
    * locked block
    */

   if( s_pLockedBlock )
   {
      pAlloc = s_pLockedBlock;

      do
      {
         /* it is not very elegant method but it works well
          */
         if( pAlloc->pFunc == hb_gcGripRelease )
            hb_gcItemRef( ( PHB_ITEM ) ( pAlloc + 1 ) );
         else if( pAlloc->pFunc == hb_arrayReleaseGarbage )
         {
            HB_ITEM FakedItem;

            ( &FakedItem )->type                = HB_IT_ARRAY;
            ( &FakedItem )->item.asArray.value  = ( PHB_BASEARRAY ) ( pAlloc + 1 );

            hb_gcItemRef( &FakedItem );
         }
         else if( pAlloc->pFunc == hb_hashReleaseGarbage )
         {
            HB_ITEM FakedItem;

            ( &FakedItem )->type                = HB_IT_HASH;
            ( &FakedItem )->item.asHash.value   = ( PHB_BASEHASH ) ( pAlloc + 1 );

            hb_gcItemRef( &FakedItem );
         }
         else if( pAlloc->pFunc == hb_codeblockDeleteGarbage )
         {
            HB_ITEM FakedItem;

            ( &FakedItem )->type                = HB_IT_BLOCK;
            ( &FakedItem )->item.asBlock.value  = ( PHB_CODEBLOCK ) ( pAlloc + 1 );

            hb_gcItemRef( &FakedItem );
         }

         pAlloc = pAlloc->pNext;
      }
      while( s_pLockedBlock != pAlloc );
   }
#ifdef TRACE_COLLECT
   TraceLog( NULL, "After Lock scan\n" );
#endif

   HB_TRACE( HB_TR_INFO, ( "Cleanup Scan" ) );

   /* Step 3 - Call Cleanup Functions  */

   pAlloc = s_pCurrBlock;
   do
   {
      if( s_pCurrBlock->used == s_uUsedFlag )
      {
         s_pCurrBlock->used |= HB_GC_DELETE;

         /* call the cleanup function - now for NON Blosks. */
         if( s_pCurrBlock->pFunc )
         {
            HB_TRACE( HB_TR_INFO, ( "Cleanup, %p", s_pCurrBlock ) );
            ( s_pCurrBlock->pFunc )( ( void * ) ( s_pCurrBlock + 1 ) );
            HB_TRACE( HB_TR_INFO, ( "DONE Cleanup, %p", s_pCurrBlock ) );
         }
      }

      s_pCurrBlock = s_pCurrBlock->pNext;
   }
   while( s_pCurrBlock && ( s_pCurrBlock != pAlloc ) );
#ifdef TRACE_COLLECT
   TraceLog( NULL, "After Cleanup scan\n" );
#endif

   HB_TRACE( HB_TR_INFO, ( "Release Scan" ) );

   /* Step 4 - Release all blocks that are still marked as unused */
   pAlloc = s_pCurrBlock;

   do
   {
      NewTopBlock:

      if( s_pCurrBlock->used & HB_GC_DELETE )
      {
         HB_TRACE( HB_TR_INFO, ( "Delete, %p", s_pCurrBlock ) );

         pDelete = s_pCurrBlock;
         hb_gcUnlink( &s_pCurrBlock, s_pCurrBlock );

         /*
          * Releasing the top block in the list, so we must mark the new top into pAlloc
          * but we still need to process this new top. Without this goto, the while
          * condition will immediatly fail. Using extra flags, and new conditions
          * will adversly effect performance.
          */
         if( pDelete == pAlloc )
         {
            HB_TRACE( HB_TR_INFO, ( "New Top, %p", pDelete ) );

            pAlloc = s_pCurrBlock;
            HB_GARBAGE_FREE( pDelete );

            if( s_pCurrBlock )
            {
               goto NewTopBlock;
            }
         }
         else
         {
            HB_TRACE( HB_TR_INFO, ( "Free, %p", pDelete ) );
            HB_GARBAGE_FREE( pDelete );
            HB_TRACE( HB_TR_INFO, ( "DONE Free, %p", pDelete ) );
         }
      }
      else
         s_pCurrBlock = s_pCurrBlock->pNext;
   }
   while( s_pCurrBlock && ( pAlloc != s_pCurrBlock ) );
#ifdef TRACE_COLLECT
   TraceLog( NULL, "After Release scan\n" );
#endif

   s_pCurrBlock = pAlloc;

   /* Step 4 - flip flag */
   /* Reverse used/unused flag so we don't have to mark all blocks
    * during next collecting
    */
   s_uUsedFlag    ^= HB_GC_USED_FLAG;

   /* Step 5: garbage requests will be now allowed again. */
   s_bCollecting  = FALSE;

   /* Step 6: release all the locks on the scanned objects */
   /* Put itself back on machine execution count */

#if defined( HB_THREAD_SUPPORT )
   hb_threadIdleEnd();
#endif
}

/* JC1: THREAD UNSAFE
 * Should be called only at VM termination, when all threads have been terminated
 */
void hb_gcReleaseAll( void )
{
   PHB_GARBAGE pDelete, pAlloc;

   HB_TRACE( HB_TR_INFO, ( "hb_gcReleaseAll()" ) );

   hb_gc_bReleaseAll = TRUE;
   s_bCollecting     = TRUE;

   if( s_pLockedBlock )
   {
      pAlloc = s_pLockedBlock;
      do
      {
         s_pLockedBlock->used |= HB_GC_DELETE;

         /* call the cleanup function now for NON Blocks! */
         if( s_pLockedBlock->pFunc )
         {
            HB_TRACE( HB_TR_INFO, ( "Cleanup for Locked, %p", s_pLockedBlock ) );
            ( s_pLockedBlock->pFunc )( ( void * ) ( s_pLockedBlock + 1 ) );
         }

         s_pLockedBlock = s_pLockedBlock->pNext;

      }
      while( s_pLockedBlock && ( s_pLockedBlock != pAlloc ) );
#ifdef TRACE_RELEASE
      TraceLog( NULL, "After Cleanup scan\n" );
#endif

      do
      {
         HB_TRACE( HB_TR_INFO, ( "Release Locked %p", s_pLockedBlock ) );
         pDelete = s_pLockedBlock;
         hb_gcUnlink( &s_pLockedBlock, s_pLockedBlock );
         /* HB_GARBAGE_FREE( pDelete );
          */
         hb_xfree( ( void * ) ( pDelete ) );
      }
      while( s_pLockedBlock );
#ifdef TRACE_RELEASE
      TraceLog( NULL, "After Release scan\n" );
#endif
   }

   if( s_pCurrBlock )
   {
#ifdef TRACE_RELEASE
      TraceLog( NULL, "Before 2. Cleanup scan %p\n", s_pCurrBlock );
#endif

      pAlloc = s_pCurrBlock;
      do
      {
#ifdef TRACE_RELEASE
         TraceLog( NULL, "Clean: %p %p %p %p %p %p\n", s_pCurrBlock, s_pCurrBlock->pFunc, hb_gcGripRelease, hb_arrayReleaseGarbage, hb_hashReleaseGarbage, hb_codeblockDeleteGarbage );
#endif

         s_pCurrBlock->used |= HB_GC_DELETE;

         /* call the cleanup function now for NON Blocks! */
         if( s_pCurrBlock->pFunc )
         {
            HB_TRACE( HB_TR_INFO, ( "Cleanup, %p", s_pCurrBlock ) );
            ( s_pCurrBlock->pFunc )( ( void * ) ( s_pCurrBlock + 1 ) );
            HB_TRACE( HB_TR_INFO, ( "DONE Cleanup, %p", s_pCurrBlock ) );
         }
#ifdef TRACE_RELEASE
         TraceLog( NULL, "  Cleaned: %p, Next: %p\n", s_pCurrBlock, s_pCurrBlock->pNext );
#endif
         s_pCurrBlock = s_pCurrBlock->pNext;

      }
      while( s_pCurrBlock && ( s_pCurrBlock != pAlloc ) );
#ifdef TRACE_RELEASE
      TraceLog( NULL, "2. After Cleanup scan\n" );
#endif

      do
      {
         HB_TRACE( HB_TR_INFO, ( "Release %p", s_pCurrBlock ) );
         pDelete = s_pCurrBlock;
         hb_gcUnlink( &s_pCurrBlock, s_pCurrBlock );
         /* HB_GARBAGE_FREE( pDelete );
          */
         hb_xfree( ( void * ) ( pDelete ) );
      }
      while( s_pCurrBlock );
#ifdef TRACE_RELEASE
      TraceLog( NULL, "2. After Release scan\n" );
#endif
   }

#ifdef GC_RECYCLE
   while( s_pAvailableItems )
   {
      HB_TRACE( HB_TR_INFO, ( "Release %p", s_pAvailableItems ) );
      pDelete = s_pAvailableItems;
      hb_gcUnlink( &s_pAvailableItems, s_pAvailableItems );
      /* HB_GARBAGE_FREE( pDelete );
       */
      hb_xfree( ( void * ) ( pDelete ) );
   }
#ifdef TRACE_RELEASE
   TraceLog( NULL, "3. After Release scan\n" );
#endif

   while( s_pAvailableBaseArrays )
   {
      HB_TRACE( HB_TR_INFO, ( "Release %p", s_pAvailableBaseArrays ) );
      pDelete = s_pAvailableBaseArrays;
      hb_gcUnlink( &s_pAvailableBaseArrays, s_pAvailableBaseArrays );
      /* HB_GARBAGE_FREE( pDelete );
       */
      hb_xfree( ( void * ) ( pDelete ) );
   }
#ifdef TRACE_RELEASE
   TraceLog( NULL, "4. After Release scan\n" );
#endif
#endif /* GC_RECYCLE */

   s_bCollecting     = FALSE;
   hb_gc_bReleaseAll = FALSE;

   HB_TRACE( HB_TR_INFO, ( "DONE Release All" ) );
}

void hb_gcInit( void )
{
   s_pCurrBlock            = NULL;
   s_pLockedBlock          = NULL;

#ifdef GC_RECYCLE
   s_pAvailableItems       = NULL;
   s_pAvailableBaseArrays  = NULL;
#endif

   s_bCollecting           = FALSE;
   hb_gc_bReleaseAll       = FALSE;
   s_uUsedFlag             = HB_GC_USED_FLAG;
   s_uAllocated            = 0;
}

BOOL hb_gcSetCollecting( BOOL bCollecting )
{
   BOOL bPreset = s_bCollecting;

   s_bCollecting = bCollecting;

   return bPreset;
}

/* service a single garbage collector step
 * Check a single memory block if it can be released
 */
HB_FUNC( HB_GCSTEP )
{
   hb_gcCollect();
}

/* Check all memory blocks if they can be released
 */
HB_FUNC( HB_GCALL )
{
   hb_gcCollectAll( hb_parl( 1 ) );
}

HB_FUNC( HB_GCALLOCATED )
{
   HB_THREAD_STUB
   hb_retns( s_uAllocatedCnt );
}

