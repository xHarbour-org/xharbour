/*
 * $Id: hash.c,v 1.14 2003/11/24 15:15:26 lf_sfnet Exp $
 */

/*
 * Harbour Project source code:
 * The HASH API (C level)
 *
 * Copyright 2003 Giancarlo Niccolai
 * www - http://www.xharbour.org
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

#include "hbapi.h"
#include "hbfast.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbvmpub.h"
#include "hbstack.h"
#include "hashapi.h"

/******************************************************
* Utility functions
*******************************************************/

static int s_hashOrderComplex( PHB_ITEM pFirst,
            PHB_ITEM pSecond, BOOL bCase )
{
   if ( pFirst->type & HB_IT_NUMERIC )
   {
      if ( pSecond->type & HB_IT_NUMERIC )
      {
         if ( pFirst->type == HB_IT_DOUBLE || pSecond->type == HB_IT_DOUBLE )
         {
            double d1 = hb_itemGetND(pFirst);
            double d2 = hb_itemGetND(pSecond);
            if ( d1 < d2 )
            {
               return -1;
            }
            else if ( d1 > d2 )
            {
               return 1;
            }
            else
            {
               return 0;
            }
         }
         else
         {
            LONGLONG l1 = hb_itemGetNLL(pFirst);
            LONGLONG l2 = hb_itemGetNLL(pSecond);
            if ( l1 < l2 )
            {
               return -1;
            }
            else if ( l1 > l2 )
            {
               return 1;
            }
            else
            {
               return 0;
            }
         }
      }
      // Numeric have higher priority
      else if ( pSecond->type == HB_IT_DATE || pSecond->type == HB_IT_STRING )
      {
         return -1;
      }
   }
   else if ( pFirst->type == HB_IT_DATE )
   {
      if ( pSecond->type & HB_IT_NUMERIC )
      {
         return 1;
      }
      else if ( pSecond->type == HB_IT_DATE )
      {
         long d1 = pFirst->item.asDate.value;
         long d2 = pSecond->item.asDate.value;
         if ( d1 < d2 )
         {
            return -1;
         }
         else if ( d1 > d2 )
         {
            return 1;
         }
         else
         {
            return 0;
         }
      }
      else if ( pSecond->type == HB_IT_STRING )
      {
         return -1;
      }
   }
   else if ( pFirst->type == HB_IT_STRING )
   {
      if ( (pSecond->type & HB_IT_NUMERIC) || pSecond->type == HB_IT_DATE )
      {
         return 1;
      }
      else if ( pSecond->type == HB_IT_STRING )
      {
         if ( bCase )
         {
            return strcmp( pFirst->item.asString.value,
                     pSecond->item.asString.value);
         }
         else
         {
            return hb_stricmp( pFirst->item.asString.value,
                     pSecond->item.asString.value );
         }
      }
      // nothing with higher priority
   }

   /* Signal unorderable types */
   return -2;
}


static BOOL hb_hashSurfaceScan( PHB_ITEM pHash, PHB_ITEM pKey, ULONG *ulIndex )
{
   ULONG ulLower = 0, ulHigher, ulPoint;
   int iRes;
   PHB_ITEM pKeys, pCurrent;
   PHB_BASEHASH pBase;
   BOOL bCase;
   PHB_HASH_ORDER_FUNC fOrder;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashSurfaceScan(%p, %p, %p)", pHash, pKey, ulIndex));

   ulHigher = pHash->item.asHash.value->ulLen;

   if ( ulHigher == 0 )
   {
      *ulIndex = 0;
      return FALSE;
   }
   ulHigher --;

   pBase = pHash->item.asHash.value;
   fOrder = pBase->fOrder;
   pKeys = pBase->pKeys;
   bCase = pBase->bCase;

   ulPoint = ( ulLower + ulHigher ) / 2;

   while ( 1 )
   {
      // get the table row
      pCurrent = pKeys + ulPoint;

      // Todo; check function for different types
      iRes = fOrder( pKey, pCurrent, bCase );

      if ( iRes == 0 )
      {
         *ulIndex = ulPoint+1;
         return TRUE;
      }
      else {
         if ( ulLower == ulHigher )
         {
            break;
         }
         // last try. In pair distros, it can be also in the other node
         else if ( ulLower == ulHigher -1 )
         {
            // key is EVEN less than the lower one
            if ( iRes <  0 ) {
               *ulIndex = ulLower+1;
               return FALSE;
            }

            // being integer math, ulPoint is rounded by defect and has
            // already looked at the ulLower position
            pCurrent = pKeys + ulHigher;

            iRes = fOrder( pKey, pCurrent,bCase );
            if ( iRes == 0 )
            {
               *ulIndex = ulHigher+1;
               return TRUE;
            }
            break;
         }

         if ( iRes > 0 )
         {
            ulLower = ulPoint;
         }
         else
         {
            ulHigher = ulPoint;
         }
         ulPoint = ( ulLower + ulHigher ) / 2;
      }
   }

   // entry not found, but signal the best match anyway
   *ulIndex =  iRes > 0 ? ulHigher+2 : ulHigher+1;

   return FALSE;
}

static void hb_calcTotalLenght( PHB_BASEHASH pBase )
{
   ULONG ulTot = 0;
   ULONG ulPos;

   if ( pBase->uiLevel == 0 )
   {
      pBase->ulTotalLen = pBase->ulLen;
      return;
   }

   for( ulPos = 0; ulPos < pBase->ulLen; ulPos ++ )
   {
      ulTot += ( pBase->pValues + ulPos)->item.asHash.value->ulTotalLen;
   }

   pBase->ulTotalLen = ulTot;
}



/*****************************************************
* CORE api
******************************************************/

PHB_ITEM HB_EXPORT hb_hashNew( PHB_ITEM pItem ) /* creates a new hash */
{
   PHB_BASEHASH pBaseHash;
   HB_TRACE(HB_TR_DEBUG, ("hb_hashNew(%p)", pItem ));

   pBaseHash = ( PHB_BASEHASH )
      hb_gcAlloc( sizeof( HB_BASEHASH ), hb_hashReleaseGarbage );

   if ( pItem == NULL ) {
      pItem = hb_itemNew( NULL );
   }
   else if( HB_IS_COMPLEX( pItem ) )
   {
      hb_itemClear( pItem );
   }

   pBaseHash->ulLen = 0;
   pBaseHash->ulTotalLen = 0;
   pBaseHash->uiLevel = 0;
   pBaseHash->ulPageSize = 0;
   pBaseHash->ulAllocated = HB_HASH_ALLOC_BLOCK;
   pBaseHash->pValues = (PHB_ITEM) hb_xgrab( sizeof( HB_ITEM ) * HB_HASH_ALLOC_BLOCK );
   pBaseHash->pKeys = (PHB_ITEM) hb_xgrab( sizeof( HB_ITEM ) * HB_HASH_ALLOC_BLOCK );
   pBaseHash->fOrder = s_hashOrderComplex;
   pBaseHash->bCase = TRUE;
   pBaseHash->bAutoAdd = TRUE;

   // ITEM TYPE MUST BE SET FOR LAST!
   pItem->item.asHash.value = pBaseHash;
   pItem->type = HB_IT_HASH;

   return pItem;
}



BOOL HB_EXPORT hb_hashAdd( PHB_ITEM pHash, ULONG ulPos, PHB_ITEM pKey, PHB_ITEM pValue )
{
   ULONG ulLen;
   PHB_ITEM pPos, pPos1;
   PHB_BASEHASH pBaseHash;
   HB_ITEM hbSubHash;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashAdd(%p, %p, %p)", pHash, pKey, pValue));

   if( ! HB_IS_HASH( pHash ) )
   {
      return FALSE;
   }

   // if the user don't know where to put this data...
   if ( ulPos == ULONG_MAX )
   {
      // ... check if a key already exists ...
      if ( hb_hashScan( pHash, pKey, &ulPos ) )
      {
         return hb_hashSet( pHash, ulPos, pValue );
      }
      // else we must add it at the point of ulpos.
   }
   // ... else, the user must have decided to really add the data in that
   // position. Notice that this can effectively destroy the hash ordering
   // if misused. if ulPos != 0, this ulPos must always be obtained with a
   // failed scan.

   pBaseHash = pHash->item.asHash.value;

   // if we are here, we are autoadding.
   if ( ! pBaseHash->bAutoAdd )
   {
      hb_errRT_BASE( EG_BOUND, 1131, "Hash key not found and Auto Add turned off",
         hb_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
      return FALSE;
   }

   if( pBaseHash->ulTotalLen < ULONG_MAX )
   {
      // If we are partitioning,
      if ( pBaseHash->uiLevel > 0 ) {
         PHB_ITEM pPage;
         PHB_BASEHASH pPageBase;

         // Creates the first partition
         if ( pBaseHash->ulLen == 0 )
         {
            pPage = hb_hashNew( NULL );
            pPageBase = pPage->item.asHash.value;
            pPageBase->uiLevel = pBaseHash->uiLevel - 1;
            pPageBase->bCase = pBaseHash->bCase;
            pPageBase->fOrder = pBaseHash->fOrder;
            pPageBase->bAutoAdd = pBaseHash->bAutoAdd;
            pPageBase->ulPageSize = pBaseHash->ulPageSize;
            hb_hashAdd( pPage, ULONG_MAX, pKey, pValue );

            pBaseHash->ulLen = 1;
            pBaseHash->ulTotalLen = 1;
            hb_itemCopy( pBaseHash->pKeys, pKey );
            hb_itemForwardValue( pBaseHash->pValues, pPage );
            return TRUE;

         }

         // get Current Partition.
         hb_hashSurfaceScan( pHash, pKey, &ulPos );
         if ( ulPos > pBaseHash->ulLen ) {
            // stack in last partition
            ulPos = pBaseHash->ulLen;
         }
         pPage = pBaseHash->pValues + ( ulPos - 1 );
         pPageBase = pPage->item.asHash.value;


         // we must see if lower level has reached maximum partition size.
         // Notice that ulLenght may be bigger than ulPageSize by one or two elements,
         // some api function may allow this for faster operations.
         if ( pPageBase->ulLen >= pPageBase->ulPageSize )
         {
            // repartitioning
            PHB_ITEM pPivot = pPageBase->pKeys + ( pPageBase->ulPageSize / 2 -1 );
            PHB_BASEHASH pNewBase;

            hbSubHash.type = HB_IT_HASH;
            hbSubHash.item.asHash.value = ( PHB_BASEHASH )
                  hb_gcAlloc( sizeof( HB_BASEHASH ), hb_hashReleaseGarbage );

            pNewBase = hbSubHash.item.asHash.value;
            pNewBase->bCase = pPageBase->bCase;
            pNewBase->bAutoAdd = pPageBase->bAutoAdd;
            pNewBase->fOrder = pPageBase->fOrder;
            pNewBase->uiLevel = pPageBase->uiLevel;
            pNewBase->ulPageSize = pPageBase->ulPageSize;
            // WARNING: May be an odd number. Leave this line as it is.
            pNewBase->ulAllocated = pPageBase->ulLen -
                  (pPageBase->ulPageSize / 2);
            pNewBase->pKeys = (PHB_ITEM) hb_xgrab(
                  sizeof( HB_ITEM) * pNewBase->ulAllocated );
            pNewBase->pValues = (PHB_ITEM) hb_xgrab(
                  sizeof( HB_ITEM) * pNewBase->ulAllocated );

            memcpy( pNewBase->pKeys, pPivot + 1,
                     sizeof( HB_ITEM) * pNewBase->ulAllocated );
            memcpy( pNewBase->pValues, pPageBase->pValues +
                     (pBaseHash->ulPageSize / 2),
                     sizeof( HB_ITEM) * pNewBase->ulAllocated );
            pNewBase->ulLen = pNewBase->ulAllocated;

            // Resize old page
            pPageBase->ulLen = pPageBase->ulLen - pNewBase->ulLen;
            hb_xrealloc( pPageBase->pKeys, pPageBase->ulLen * sizeof( HB_ITEM ) );
            hb_xrealloc( pPageBase->pValues, pPageBase->ulLen * sizeof( HB_ITEM ) );
            pPageBase->ulAllocated = pPageBase->ulLen;

            // recalculate ulTotalLenght
            hb_calcTotalLenght( pPageBase );
            hb_calcTotalLenght( pNewBase );

            // now, add the item to the right page.
            if ( pPageBase->fOrder( pKey, pPivot, pPageBase->bCase ) <= 0 )
            {
               hb_hashAdd( pPage, ULONG_MAX, pKey, pValue );
            }
            else
            {
               hb_hashAdd( &hbSubHash, ULONG_MAX, pKey, pValue );
            }

            // and finally, add this pivotal element to ourselves
            hb_itemCopy( pBaseHash->pKeys + ( ulPos - 1 ),
               pPageBase->pKeys + ( pPageBase->ulLen -1 ) );
            pKey = pNewBase->pKeys + ( pNewBase->ulLen - 1 );
            pValue = &hbSubHash;
            // insert after the previous one
            ulPos++;

            // will fall through normal item insertion.
         }
         else
         {
            // simply add the element to the page
            if ( hb_hashAdd( pPage, ULONG_MAX, pKey, pValue ) )
            {
               // eventually change partition key:
               PHB_ITEM pPivot = pPage->item.asHash.value->pKeys +
                         ( pPage->item.asHash.value->ulLen - 1 );
               PHB_ITEM pPartKey = pBaseHash->pKeys + (ulPos - 1);

               if ( pBaseHash->fOrder( pPivot, pPartKey, pBaseHash->bCase ) != 0 )
               {
                  hb_itemCopy( pPartKey, pPivot );
               }
               // done
               pBaseHash->ulTotalLen++;
               return TRUE;
            }
            // done, unsuccesful
            return FALSE;
         }
      }

      /* ulLen is the OLD length */
      ulLen = pBaseHash->ulLen++;
      pBaseHash->ulTotalLen++;

      if ( ulLen >= pBaseHash->ulAllocated )
      {
         pBaseHash->ulAllocated += HB_HASH_ALLOC_BLOCK;
         pBaseHash->pValues = ( PHB_ITEM ) hb_xrealloc(
               pBaseHash->pValues,
               sizeof( HB_ITEM ) * pBaseHash->ulAllocated );
         pBaseHash->pKeys = ( PHB_ITEM ) hb_xrealloc(
               pBaseHash->pKeys,
               sizeof( HB_ITEM ) * pBaseHash->ulAllocated );
      }

      // find the point where I have to insert the data.
      if ( ulLen == 0 )
      {
         hb_itemCopy( pBaseHash->pValues, pValue );
         hb_itemCopy( pBaseHash->pKeys, pKey );
      }
      else {
         pPos = pBaseHash->pValues + ulLen;
         pPos1 = pBaseHash->pKeys + ulLen;

         for( ; ulPos <= ulLen; ulLen--, pPos--, pPos1-- )
         {
            memcpy( pPos, pPos-1, sizeof( HB_ITEM ) );
            memcpy( pPos1, pPos1-1, sizeof( HB_ITEM ) );
         }

         /* Insert BEFORE the given position */
         pPos = pBaseHash->pValues + (ulPos-1);
         pPos->type = HB_IT_NIL;
         hb_itemCopy( pPos, pValue );

         pPos = pBaseHash->pKeys + (ulPos-1);
         pPos->type = HB_IT_NIL;
         hb_itemCopy( pPos, pKey );
      }

      return TRUE;
   }

   return FALSE;
}

//WARNING: DOES NOT WORK WITH PAGED HASHES

BOOL HB_EXPORT hb_hashAddForward( PHB_ITEM pHash, ULONG ulPos, PHB_ITEM pKey, PHB_ITEM pValue )
{
   ULONG ulLen;
   PHB_ITEM pPos, pPos1;
   PHB_BASEHASH pBaseHash;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashAdd(%p, %p, %p)", pHash, pKey, pValue));

   if( ! HB_IS_HASH( pHash ) )
   {
      return FALSE;
   }

   // if the user don't know where to put this data...
   if ( ulPos == ULONG_MAX )
   {
      // ... check if a key already exists ...
      if ( hb_hashSurfaceScan( pHash, pKey, &ulPos ) )
      {
         // ... and if so, just set the value
         return hb_hashSet( pHash, ulPos, pValue );
      }
      // else we must add it at the point of ulpos.
   }
   // ... else, the user must have decided to really add the data in that
   // position. Notice that this can effectively destroy the hash ordering
   // if misused. if ulPos != 0, this ulPos must always be obtained with a
   // failed scan.

   pBaseHash = pHash->item.asHash.value;

   // if we are here, we are autoadding.
   if ( ! pBaseHash->bAutoAdd )
   {
      hb_errRT_BASE( EG_BOUND, 1131, "Hash key not found and Auto Add turned off",
         hb_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
      return FALSE;
   }

   if( pBaseHash->ulLen < ULONG_MAX )
   {
      /* ulLen is the OLD length */
      ulLen = pBaseHash->ulLen++;
      pBaseHash->ulTotalLen++;
      if ( ulLen >= pBaseHash->ulAllocated )
      {
         pBaseHash->ulAllocated += HB_HASH_ALLOC_BLOCK;
         pBaseHash->pValues = ( PHB_ITEM ) hb_xrealloc(
               pBaseHash->pValues,
               sizeof( HB_ITEM ) * pBaseHash->ulAllocated );
         pBaseHash->pKeys = ( PHB_ITEM ) hb_xrealloc(
               pBaseHash->pKeys,
               sizeof( HB_ITEM ) * pBaseHash->ulAllocated );
      }
      // find the point where I have to insert the data.
      if ( ulLen == 0 )
      {
         hb_itemForwardValue( pBaseHash->pValues, pValue );
         hb_itemForwardValue( pBaseHash->pKeys, pKey );
      }
      else {
         pPos = pBaseHash->pValues + ulLen;
         pPos1 = pBaseHash->pKeys + ulLen;

         for( ; ulPos <= ulLen; ulLen--, pPos--, pPos1-- )
         {
            memcpy( pPos, pPos-1, sizeof( HB_ITEM ) );
            memcpy( pPos1, pPos1-1, sizeof( HB_ITEM ) );
         }

         /* Insert AFTER the given position */
         pPos = pBaseHash->pValues + (ulPos-1);
         pPos->type = HB_IT_NIL;
         hb_itemForwardValue( pPos, pValue );

         pPos = pBaseHash->pKeys + (ulPos-1);
         pPos->type = HB_IT_NIL;
         hb_itemForwardValue( pPos, pKey );
      }

      return TRUE;
   }

   return FALSE;
}


BOOL HB_EXPORT hb_hashScan( PHB_ITEM pHash, PHB_ITEM pKey, ULONG *ulIndex )
{
   ULONG ulPos, ulTotal, ulElem;
   PHB_BASEHASH pBase;
   PHB_ITEM pPage;
   BOOL bRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashScan(%p, %p, %p)", pHash, pKey, ulIndex));

   pBase = pHash->item.asHash.value;

   if ( pBase->ulLen == 0 )
   {
      *ulIndex = 1;
      return FALSE;
   }

   bRet = hb_hashSurfaceScan( pHash, pKey, ulIndex );
   if ( pBase->uiLevel == 0 )
   {
      return bRet;
   }

   ulPos = *ulIndex;

   if ( ulPos <= pBase->ulLen )
   {
      ulTotal = 0;
      pPage = pBase->pValues;

      for( ulElem = 0; ulElem < ulPos-1; ulElem ++, pPage++ )
      {
         ulTotal += pPage->item.asHash.value->ulTotalLen;
      }

      bRet = hb_hashScan( pBase->pValues + ( ulPos - 1 ), pKey, &ulPos );

      *ulIndex = ulPos + ulTotal;
   }
   else {
      *ulIndex = pBase->ulTotalLen+1;
      bRet = FALSE;
   }

   return bRet;
}


BOOL HB_EXPORT hb_hashRemove( PHB_ITEM pHash, ULONG ulPos )
{
   ULONG ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashRemove(%p, %lo)", pHash, ulPos ));

   if( HB_IS_HASH( pHash ) )
   {
      PHB_BASEHASH pBaseHash = pHash->item.asHash.value;

      if ( pBaseHash->uiLevel > 0 )
      {
         ULONG ulElem = 1;
         ULONG ulTotal = 0;
         PHB_ITEM pPage;

         if ( ulPos < 1 || ulPos > pBaseHash->ulTotalLen ) {
            return FALSE;
         }

         pPage = pBaseHash->pValues;

         while ( ulTotal + pPage->item.asHash.value->ulTotalLen < ulPos )
         {
            ulTotal += pPage->item.asHash.value->ulTotalLen;
            ulElem ++;
            pPage++;
         }

         if ( hb_hashRemove( pPage, ulPos - ulTotal ) )
         {
            // is the page empty?
            if ( pPage->item.asHash.value->ulTotalLen == 0 )
            {
               // then falling through to removal of this item.
               ulPos = ulElem;
            }
            else
            {
               // we are done
               pBaseHash->ulTotalLen--;
               return TRUE;
            }
         }
         else {
            // done, unsuccesful
            return FALSE;
         }
      }

      /* ulLen is the OLD length */
      ulLen = pBaseHash->ulLen;

      // flat inem removal.
      if ( ulPos > 0 && ulPos <= ulLen )
      {
         // find the point where I have to insert the data.

         hb_itemClear( pBaseHash->pValues + (ulPos-1) );
         hb_itemClear( pBaseHash->pKeys + (ulPos-1) );

         if ( ulLen > 1 ) // if ulLen == 1 just set ulLen to 0.
         {
            memcpy( pBaseHash->pValues + (ulPos-1),
               pBaseHash->pValues + ulPos,
               sizeof( HB_ITEM ) * (ulLen - ulPos ) );
            memcpy( pBaseHash->pKeys + (ulPos-1),
               pBaseHash->pKeys + ulPos,
               sizeof( HB_ITEM ) * (ulLen - ulPos ) );

            /* Give elasticity: release memory but leave HB_ALLOC_BLOCK
               more blocks than needed alwas allocated. */
            if ( pBaseHash->ulAllocated ==
                     pBaseHash->ulLen + ( HB_HASH_ALLOC_BLOCK * 2 ) )
            {
               pBaseHash->ulAllocated -= HB_HASH_ALLOC_BLOCK;
               pBaseHash->pValues = ( PHB_ITEM ) hb_xrealloc(
                     pBaseHash->pValues,
                     sizeof( HB_ITEM ) * pBaseHash->ulAllocated );
               pBaseHash->pKeys = ( PHB_ITEM ) hb_xrealloc(
                     pBaseHash->pKeys,
                     sizeof( HB_ITEM ) * pBaseHash->ulAllocated );
            }
         }

         pBaseHash->ulLen--;
         pBaseHash->ulTotalLen--;
         return TRUE;
      }
   }

   return FALSE;
}


ULONG HB_EXPORT hb_hashLen( PHB_ITEM pHash )
{
   ULONG ulLen = 0;
   HB_TRACE(HB_TR_DEBUG, ("hb_hashLen(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
   {
      ulLen = pHash->item.asHash.value->ulTotalLen;
   }

   return ulLen;
}


BOOL HB_EXPORT hb_hashSet( PHB_ITEM pHash, ULONG ulIndex, PHB_ITEM pItem )
{
   PHB_ITEM pElement;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashSet(%p, %lu, %p)", pHash, ulIndex, pItem));

   if( HB_IS_HASH( pHash ) && ulIndex > 0 )
   {

      if ( pHash->item.asHash.value->uiLevel > 0 )
      {
         ULONG ulTotal = 0;

         if ( pHash->item.asHash.value->ulTotalLen < ulIndex )
         {
            return FALSE;
         }

         pElement = pHash->item.asHash.value->pValues;

         while( ulTotal + pElement->item.asHash.value->ulTotalLen < ulIndex )
         {
            ulTotal += pElement->item.asHash.value->ulTotalLen;
            pElement ++;
         }
         return hb_hashSet( pElement, ulIndex - ulTotal, pItem );
      }

      pElement = pHash->item.asHash.value->pValues + ( ulIndex - 1 );

      if  ( ulIndex <= pHash->item.asHash.value->ulLen )
      {
         pElement = pHash->item.asHash.value->pValues + ( ulIndex - 1 );

         if( HB_IS_BYREF( pElement ) )
         {
            hb_itemCopy( hb_itemUnRef( pElement ), pItem );
         }
         else
         {
            hb_itemCopy( pElement, pItem );
         }
         return TRUE;
      }
   }

   return FALSE;
}


BOOL HB_EXPORT hb_hashSetForward( PHB_ITEM pHash, ULONG ulIndex, PHB_ITEM pItem )
{
   PHB_ITEM pElement;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashSetForward(%p, %lu, %p)", pHash, ulIndex, pItem));

   if( HB_IS_HASH( pHash ) && ulIndex > 0 )
   {
      pElement = pHash->item.asHash.value->pValues + ( ulIndex - 1 );

      if ( pHash->item.asHash.value->uiLevel > 0 )
      {
         ULONG ulTotal = 0;

         if ( pHash->item.asHash.value->ulTotalLen < ulIndex )
         {
            return FALSE;
         }

         while( ulTotal + pElement->item.asHash.value->ulTotalLen < ulIndex )
         {
            ulTotal += pElement->item.asHash.value->ulTotalLen;
            pElement ++;
         }
         return hb_hashSetForward( pElement, ulIndex - ulTotal, pItem );
      }


      if  ( ulIndex <= pHash->item.asHash.value->ulLen )
      {
         pElement = pHash->item.asHash.value->pValues + ( ulIndex - 1 );

         if( HB_IS_BYREF( pElement ) )
         {
            hb_itemForwardValue( hb_itemUnRef( pElement ), pItem );
         }
         else
         {
            hb_itemForwardValue( pElement, pItem );
         }
         return TRUE;
      }
   }

   return FALSE;
}




BOOL HB_EXPORT hb_hashGet( PHB_ITEM pHash, ULONG ulIndex, PHB_ITEM pItem )
{
   PHB_ITEM pElement;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashGet(%p, %lu, %p) Base: %p Keys: %p Values: %p", pHash, ulIndex, pItem, pHash->item.asHash.value, pHash->item.asHash.value->pKeys, pHash->item.asHash.value->pValues));

   if( HB_IS_HASH( pHash ) && ulIndex > 0 )
   {
      if ( pHash->item.asHash.value->uiLevel > 0 )
      {
         ULONG ulTotal = 0;

         if ( pHash->item.asHash.value->ulTotalLen < ulIndex )
         {
            return FALSE;
         }

         pElement = pHash->item.asHash.value->pValues;

         while( ulTotal + pElement->item.asHash.value->ulTotalLen < ulIndex )
         {
            ulTotal += pElement->item.asHash.value->ulTotalLen;
            pElement ++;
         }

         return hb_hashGet( pElement, ulIndex - ulTotal, pItem );
      }

      pElement = pHash->item.asHash.value->pValues + ( ulIndex - 1 );

      if  ( ulIndex <= pHash->item.asHash.value->ulLen )
      {
         pElement = pHash->item.asHash.value->pValues + ( ulIndex - 1 );

         if( HB_IS_BYREF( pElement ) )
         {
            hb_itemCopy( pItem, hb_itemUnRef( pElement ) );
         }
         else
         {
            hb_itemCopy( pItem, pElement );
         }
         return TRUE;
      }
   }

   if( HB_IS_COMPLEX( pItem ) )
   {
      hb_itemClear( pItem );
   }
   else
   {
      pItem->type = HB_IT_NIL;
   }
   return FALSE;
}



void HB_EXPORT hb_hashPreallocate( PHB_ITEM pHash, ULONG ulNewLen )
{
   ULONG ulLen, ulAlloc;
   PHB_BASEHASH pBase;

   if(! HB_IS_HASH( pHash ) )
   {
      return;
   }


   pBase = pHash->item.asHash.value;

   ulAlloc = pBase->ulAllocated;
   ulLen = pBase->ulLen;
   if( ulLen < ulNewLen )
   {
      ulNewLen = ulLen;
   }

   if( ulNewLen < HB_HASH_ALLOC_BLOCK )
   {
      ulNewLen = HB_HASH_ALLOC_BLOCK;
   }

   if ( ulAlloc != ulNewLen )
   {
      pBase->pKeys = (PHB_ITEM) hb_xrealloc( pBase->pKeys,
         sizeof( HB_ITEM ) * ulNewLen );
      pBase->pValues = (PHB_ITEM) hb_xrealloc( pBase->pValues,
         sizeof( HB_ITEM ) * ulNewLen );
      pBase->ulAllocated = ulNewLen;
   }
}


PHB_ITEM HB_EXPORT hb_hashClone( PHB_ITEM pSrcHash )
{
   PHB_BASEHASH pSrcBase, pDestBase;
   PHB_ITEM pDest;
   PHB_ITEM pKey, pVal;

   ULONG ulLen, ulCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashClone( %p, %p)", pSrcHash ));

   if(! HB_IS_HASH( pSrcHash ) )
   {
      return hb_itemNew( NULL );
   }

   pSrcBase = pSrcHash->item.asHash.value;
   ulLen = pSrcBase->ulLen;

   pDest = hb_hashNew( NULL );
   pDestBase = pDest->item.asHash.value;
   pKey = pSrcBase->pKeys;
   pVal = pSrcBase->pValues;

   hb_hashPreallocate( pDest, pSrcBase->ulAllocated );

   for( ulCount = 0; ulCount < ulLen; ulCount++, pKey++, pVal++ )
   {
      hb_itemCopy( pDestBase->pKeys + ulCount, pKey );
      hb_itemCopy( pDestBase->pValues + ulCount, pVal );
   }
   pDestBase->ulLen = ulLen;
   pDestBase->ulTotalLen = pSrcBase->ulTotalLen;
   pDestBase->ulPageSize = pSrcBase->ulPageSize;
   pDestBase->uiLevel = pSrcBase->uiLevel;
   pDestBase->fOrder = pSrcBase->fOrder;
   pDestBase->bCase = pSrcBase->bCase;
   pDestBase->bAutoAdd = pSrcBase->bAutoAdd;

   return pDest;
}


void HB_EXPORT hb_hashMerge( PHB_ITEM pDest, PHB_ITEM pSource,
      ULONG ulStart, ULONG ulCount, PHB_ITEM pBlock )
{
   int mode = 0;
   ULONG ulElem;
   PHB_ITEM pKey, pValue;
   ULONG ulPos;

   if ( pBlock != NULL )
   {
      if ( HB_IS_NUMERIC( pBlock ) )
      {
         mode = hb_itemGetNI( pBlock );
         if ( mode < 0 || mode > 3 )
         {
            mode = 0;
         }
      }
      else if( HB_IS_BLOCK( pBlock ) )
      {
         mode = -1;
      }
   }


   if ( mode != 1 ) // and mode is different
   {
      BOOL bAdd = pDest->item.asHash.value->bAutoAdd;

      // temporarily enabling auto add
      pDest->item.asHash.value->bAutoAdd = TRUE;

      if ( pSource->item.asHash.value->uiLevel == 0 )
      {
         pKey = pSource->item.asHash.value->pKeys;
         pValue = pSource->item.asHash.value->pValues;

         for ( ulElem = ulStart - 1; ulElem < ulStart-1+ulCount;
               ulElem ++, pKey++, pValue++ )
         {
            switch( mode )
            {
               case 0: // default OR mode
                  hb_hashAdd( pDest, ULONG_MAX, pKey, pValue );
                  break;

               case 2: // XOR mode
                  if ( ! hb_hashScan( pDest, pKey, &ulPos ) )
                  {
                     hb_hashAdd( pDest, ulPos, pKey, pValue );
                  }
                  else
                  {
                     hb_hashRemove( pDest, ulPos );
                  }
                  break;

               case 3: // NOT mode
                  if ( hb_hashScan( pDest, pKey, &ulPos ) )
                  {
                     hb_hashRemove( pDest, ulPos );
                  }
                  break;

               default: // codeblock mode
                  hb_vmPushSymbol( &hb_symEval );
                  hb_vmPush( pBlock );
                  hb_vmPush( pKey );
                  hb_vmPush( pValue );
                  hb_vmPushNumber( ( double ) ( ulElem + 1 ), 0 );
                  hb_vmSend( 3 );
                  if( HB_IS_LOGICAL( &(HB_VM_STACK.Return) ) &&
                        HB_VM_STACK.Return.item.asLogical.value )
                  {
                     hb_hashAdd( pDest, ULONG_MAX, pKey, pValue );
                  }
            }
         }
      }
      else
      {
         for ( ulElem = ulStart; ulElem < ulStart+ulCount; ulElem ++ )
         {
            pKey = hb_hashGetKeyAt( pSource, ulElem);
            pValue = hb_hashGetValueAt( pSource, ulElem);

            switch( mode )
            {
               case 0: // default OR mode
                  hb_hashAdd( pDest, ULONG_MAX, pKey, pValue );
                  break;

               case 2: // XOR mode
                  if ( ! hb_hashScan( pDest, pKey, &ulPos ) )
                  {
                     hb_hashAdd( pDest, ulPos, pKey, pValue );
                  }
                  else
                  {
                     hb_hashRemove( pDest, ulPos );
                  }
                  break;

               case 3: // NOT mode
                  if ( hb_hashScan( pDest, pKey, &ulPos ) )
                  {
                     hb_hashRemove( pDest, ulPos );
                  }
                  break;

               default: // codeblock mode
                  hb_vmPushSymbol( &hb_symEval );
                  hb_vmPush( pBlock );
                  hb_vmPush( pKey );
                  hb_vmPush( pValue );
                  hb_vmPushNumber( ( double ) ( ulElem ), 0 );
                  hb_vmSend( 3 );
                  if( HB_IS_LOGICAL( &(HB_VM_STACK.Return) ) &&
                        HB_VM_STACK.Return.item.asLogical.value )
                  {
                     hb_hashAdd( pDest, ULONG_MAX, pKey, pValue );
                  }
            }
         }
      }
      // resetting default autoadd status
      pDest->item.asHash.value->bAutoAdd = bAdd;
   }
   else // AND mode; we must remove elements in PDEST that are not in pSource
   {
      ULONG ulDestLen = hb_hashLen( pDest );
      ulElem = 0;

      if ( pDest->item.asHash.value->uiLevel == 0 )
      {
         while ( ulElem < ulDestLen )
         {
            PHB_ITEM pKeyDest = pDest->item.asHash.value->pKeys + ulElem;

            if ( ! hb_hashScan( pSource, pKeyDest, &ulPos ) )
            {
               hb_hashRemove( pDest, ulElem + 1);
               ulDestLen--;
            }
            else {
               if ( ulPos < ulStart || ulPos > ulCount+ ulStart ) {
                  hb_hashRemove( pDest, ulElem +1 );
                  ulDestLen--;

               }
               else {
                  hb_hashSet( pDest, ulElem+1,
                  pSource->item.asHash.value->pValues + (ulPos-1) );
                  ulElem++;
               }
            }
         }
      }
      else
      {
         ulElem = 1;

         while ( ulElem <= ulDestLen )
         {
            PHB_ITEM pKeyDest = hb_hashGetKeyAt( pDest, ulElem );

            if ( ! hb_hashScan( pSource, pKeyDest, &ulPos ) )
            {
               hb_hashRemove( pDest, ulElem );
               ulDestLen--;
            }
            else {
               if ( ulPos < ulStart || ulPos > ulCount+ ulStart ) {
                  hb_hashRemove( pDest, ulElem );
                  ulDestLen--;

               }
               else {
                  hb_hashSet( pDest, ulElem, hb_hashGetValueAt( pSource, ulPos ) );
                  ulElem++;
               }
            }
         }
      }
   }
}

void hb_hashReleaseBase( PHB_BASEHASH pBaseHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashReleaseBase( %p )", pBaseHash ) );

   //TraceLog( NULL, "Releasing Basearray %p\n", pBaseArray );

   // Called recursively from hb_hashReleaseGarbage!
   if( pBaseHash->ulAllocated == 0)
   {
      return;
   }

   // Avoid possible recursion problem when one of the items
   // in turn points to this hash. ulAllocated is no longer
   // needed, as the only thing we can do now is to free keys
   // and values.

   pBaseHash->ulAllocated = 0;

   if( pBaseHash->ulLen > 0 )
   {
      PHB_ITEM pKey = pBaseHash->pKeys;
      PHB_ITEM pValue = pBaseHash->pValues;
      ULONG ulLen = pBaseHash->ulLen;

      while( ulLen-- )
      {
         HB_TRACE( HB_TR_INFO, ( "Hash Key %p, Value %p, type:%i",
               pKey, pValue, pValue->type ) );

         if( HB_IS_HASH( pValue ) && pValue->item.asHash.value == pBaseHash )
         {
            HB_TRACE( HB_TR_DEBUG, ("Warning! Nested Release (Cyclic) %p %p", pValue, pValue->item.asHash.value ) );
            TraceLog( NULL, "Warning! Nested Release (Cyclic) %p %p\n", pValue, pValue->item.asHash.value );
         }
         else if( HB_IS_COMPLEX( pValue ) )
         {
            hb_itemClear( pValue );
         }

         // This should always be true atm, but I may implement numeric keys.
         if( HB_IS_COMPLEX( pKey ) )
         {
            hb_itemClear( pKey );
         }

         pKey++;
         pValue++;
      }

   }

   if ( pBaseHash->pKeys ) {
      HB_TRACE( HB_TR_INFO, ( "Release pKeys %p", pBaseHash->pKeys ) );
      hb_xfree( pBaseHash->pKeys );
      pBaseHash->pKeys = NULL;

      HB_TRACE( HB_TR_INFO, ( "Release pValues %p", pBaseHash->pValues ) );
      hb_xfree( pBaseHash->pValues );
      pBaseHash->pValues = NULL;
   }

   HB_TRACE( HB_TR_INFO, ( "Release pBaseHash %p", pBaseHash ) );
   hb_gcFree( ( void * ) pBaseHash );
}


BOOL HB_EXPORT hb_hashRelease( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_hashRelease(%p) %p", pHash, pHash->item.asHash.value ) );

   if( HB_IS_HASH( pHash ) )
   {
      hb_hashReleaseBase( pHash->item.asHash.value );

      pHash->type = HB_IT_NIL;
      pHash->item.asArray.value = NULL;

      return TRUE;
   }
   else
   {
      char szProc[64], szModule[64];
      USHORT uiLine;

      hb_procinfo( 0, szProc, &uiLine, szModule  );
      TraceLog( NULL, "Warning! not an hash %p [%s->%s(%i)]\n", pHash, szModule, szProc, uiLine );
      return FALSE;
   }
}


/* This releases array when called from the garbage collector */
HB_GARBAGE_FUNC( hb_hashReleaseGarbage )
{
   PHB_BASEHASH pBaseHash = ( PHB_BASEHASH ) Cargo;

   HB_TRACE( HB_TR_INFO, ( "hb_hashReleaseGarbage( %p )", pBaseHash ) );


   if( pBaseHash->ulAllocated )
   {

      PHB_ITEM pKey = pBaseHash->pKeys;
      PHB_ITEM pValue = pBaseHash->pValues;
      ULONG ulLen = pBaseHash->ulLen;

      // Avoid possible recursion problem when one of the items
      // in turn points to this hash. ulAllocated is no longer
      // needed, as the only thing we can do now is to free keys
      // and values.

      pBaseHash->ulAllocated = 0;

      while( ulLen-- )
      {
         HB_TRACE( HB_TR_INFO, ( "Hash Key %p, Value %p, type:%i",
               pKey, pValue, pValue->type ) );

         if( HB_IS_HASH( pValue ) && pValue->item.asHash.value == pBaseHash )
         {
            HB_TRACE( HB_TR_DEBUG, ("Warning! Nested Release (Cyclic) %p %p", pValue, pValue->item.asHash.value ) );
            TraceLog( NULL, "Warning! Nested Release (Cyclic) %p %p\n", pValue, pValue->item.asHash.value );
         }
         if( HB_IS_STRING( pValue ) )
         {
            hb_itemReleaseString( pValue );
         }
         else if( HB_IS_MEMVAR( pValue ) )
         {
            hb_memvarValueDecGarbageRef( pValue->item.asMemvar.value );
         }

         // This should always be true atm, but I may implement numeric keys.
         if( HB_IS_STRING( pKey ) )
         {
            hb_itemReleaseString( pKey );
         }

         pKey++;
         pValue++;
      }

      HB_TRACE( HB_TR_INFO, ( "Release pKeys %p", pBaseHash->pKeys ) );
      hb_xfree( pBaseHash->pKeys );
      pBaseHash->pKeys = NULL;

      HB_TRACE( HB_TR_INFO, ( "Release pValues %p", pBaseHash->pValues ) );
      hb_xfree( pBaseHash->pValues );
      pBaseHash->pValues = NULL;
   }
}

PHB_ITEM HB_EXPORT hb_hashGetKeys( PHB_ITEM pHash )
{
   PHB_ITEM pKeys, pK, pArr;
   ULONG ulPos, ulLen;

   if ( ! HB_IS_HASH( pHash ) )
   {
      return NULL;
   }

   pKeys = hb_itemNew( NULL );

   if ( pHash->item.asHash.value->uiLevel == 0 )
   {
      ulLen = pHash->item.asHash.value->ulLen;
      hb_arrayNew( pKeys, ulLen );
      pK = pHash->item.asHash.value->pKeys;
      pArr = pKeys->item.asArray.value->pItems;

      for ( ulPos = 1 ; ulPos <= ulLen; ulPos ++, pK++, pArr++ )
      {
         hb_itemCopy( pArr, pK );
      }
   }
   else {
      ulLen = pHash->item.asHash.value->ulTotalLen;
      hb_arrayNew( pKeys, ulLen );
      pArr = pKeys->item.asArray.value->pItems;

      for ( ulPos = 1 ; ulPos <= ulLen; ulPos ++, pArr++ )
      {
         hb_itemCopy( pArr, hb_hashGetKeyAt( pHash, ulPos ) );
      }
   }

   return pKeys;
}


PHB_ITEM HB_EXPORT hb_hashGetValues( PHB_ITEM pHash )
{
   PHB_ITEM pVals, pV, pArr;
   ULONG ulPos, ulLen;

   if ( ! HB_IS_HASH( pHash ) )
   {
      return NULL;
   }

   pVals = hb_itemNew( NULL );

   if ( pHash->item.asHash.value->uiLevel == 0 )
   {
      ulLen = pHash->item.asHash.value->ulLen;
      hb_arrayNew( pVals, ulLen );
      pV = pHash->item.asHash.value->pKeys;
      pArr = pVals->item.asArray.value->pItems;

      for ( ulPos = 1 ; ulPos <= ulLen; ulPos ++, pV++, pArr++ )
      {
         hb_itemCopy( pArr, pV );
      }
   }
   else {
      ulLen = pHash->item.asHash.value->ulTotalLen;
      hb_arrayNew( pVals, ulLen );
      pArr = pVals->item.asArray.value->pItems;

      for ( ulPos = 1 ; ulPos <= ulLen; ulPos ++, pArr++ )
      {
         hb_itemCopy( pArr, hb_hashGetValueAt( pHash, ulPos ) );
      }
   }

   return pVals;
}


PHB_ITEM HB_EXPORT hb_hashGetKeyAt( PHB_ITEM pHash, ULONG ulPos )
{
   PHB_BASEHASH pBaseHash = pHash->item.asHash.value;
   PHB_ITEM pElement;

   if ( ulPos < 1 )
   {
      return NULL;
   }

   if ( pBaseHash->uiLevel > 0 )
   {
      ULONG ulTotal = 0;

      if ( pBaseHash->ulTotalLen < ulPos )
      {
         return NULL;
      }

      pElement = pBaseHash->pValues;

      while( ulTotal + pElement->item.asHash.value->ulTotalLen < ulPos )
      {
         ulTotal += pElement->item.asHash.value->ulTotalLen;
         pElement ++;
      }

      return hb_hashGetKeyAt( pElement, ulPos - ulTotal );
   }

   if ( ulPos > pHash->item.asHash.value->ulLen )
   {
      return NULL;
   }

   return pBaseHash->pKeys + (ulPos -1 );
}


PHB_ITEM HB_EXPORT hb_hashGetValueAt( PHB_ITEM pHash, ULONG ulPos )
{

   PHB_BASEHASH pBaseHash = pHash->item.asHash.value;
   PHB_ITEM pElement;

   if ( ulPos < 1 )
   {
      return NULL;
   }

   if ( pBaseHash->uiLevel > 0 )
   {
      ULONG ulTotal = 0;

      if ( pBaseHash->ulTotalLen < ulPos )
      {
         return NULL;
      }

      pElement = pBaseHash->pValues;

      while( ulTotal + pElement->item.asHash.value->ulTotalLen < ulPos )
      {
         ulTotal += pElement->item.asHash.value->ulTotalLen;
         pElement ++;
      }

      return hb_hashGetValueAt( pElement, ulPos - ulTotal );
   }

   if ( ulPos > pBaseHash->ulLen  )
   {
      return NULL;
   }

   return pBaseHash->pValues + (ulPos -1 );
}

/**********************************************************************
* Harbour API
**********************************************************************/

HB_FUNC( HASH )
{
   int iPCount = hb_pcount();
   PHB_ITEM pHash;

   if ( iPCount % 2 != 0 )
   {
      hb_errRT_BASE( EG_BOUND, 1131, "Hash arguments must be in pairs",
         hb_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
      return;
   }

   pHash = hb_hashNew( NULL );
   if( iPCount > 0 )
   {
      int iParam;

      for( iParam = 1; iParam <= iPCount; iParam+=2 )
      {
         /* For now only allows string keys */
         PHB_ITEM pKey = hb_param( iParam, HB_IT_STRING | HB_IT_NUMERIC | HB_IT_DATE );
         PHB_ITEM pValue = hb_param( iParam+1, HB_IT_ANY );

         if ( pKey == NULL )
         {
            hb_errRT_BASE( EG_BOUND, 1131,
               "Hash keys must be strings, numbers or dates",
               hb_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
            hb_hashRelease( pHash );
            return;
         }

         if (! hb_hashAdd( pHash, ULONG_MAX, pKey, pValue ) )
         {
            hb_errRT_BASE( EG_BOUND, 1131, "Hash value insertion failed",
               hb_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
            hb_hashRelease( pHash );
            return;
         }

      }
   }

   hb_itemForwardValue( &HB_VM_STACK.Return, pHash );
}


HB_FUNC( HGETPOS )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_STRING | HB_IT_DATE | HB_IT_NUMERIC );
   ULONG ulPos;

   if ( pHash == NULL || pKey == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HGET", 2, hb_paramError(1), hb_paramError( 2 ) );
      return;
   }

   if ( ! hb_hashScan( pHash, pKey, &ulPos ) )
   {
      hb_retnl( 0 );
   }
   else
   {
      hb_retnl( ulPos );
   }
}

HB_FUNC( HHASKEY )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_STRING | HB_IT_DATE | HB_IT_NUMERIC );
   ULONG ulPos;

   if ( pHash == NULL || pKey == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HHASKEY", 2, hb_paramError(1), hb_paramError( 2 ) );
      return;
   }

   if ( ! hb_hashScan( pHash, pKey, &ulPos ) )
   {
      hb_retl( FALSE );
   }
   else
   {
      hb_retnl( TRUE );
   }
}



HB_FUNC( HGET )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_STRING | HB_IT_DATE | HB_IT_NUMERIC );
   HB_ITEM hbRet;
   ULONG ulPos;

   if ( pHash == NULL || pKey == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HGET", 2, hb_paramError(1), hb_paramError( 2 ) );
      return;
   }

   if ( ! hb_hashScan( pHash, pKey, &ulPos  ) )
   {
      hb_errRT_BASE( EG_BOUND, 1187, "Hash key not found", "HGET", 2,
            hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hbRet.type = HB_IT_NIL;
   hb_hashGet( pHash, ulPos, &hbRet );
   hb_itemReturn( &hbRet );
}


HB_FUNC( HSET )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_STRING | HB_IT_DATE | HB_IT_NUMERIC );
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );

   if ( pHash == NULL || pKey == NULL || pValue == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HSET", 3, hb_paramError(1), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }

   hb_hashAdd( pHash, ULONG_MAX, pKey, pValue );
}



HB_FUNC( HDEL )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_STRING | HB_IT_DATE | HB_IT_NUMERIC );
   ULONG ulPos;

   if ( pHash == NULL || pKey == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HDEL", 2, hb_paramError(1), hb_paramError( 2 ) );
      return;
   }

   if (! hb_hashScan( pHash, pKey, &ulPos ) )
   {
      hb_errRT_BASE( EG_BOUND, 1187, "Hash key not found", "HDEL", 2,
            hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hb_hashRemove( pHash, ulPos );
}


/**************************************************************
* Index oriented operations
***************************************************************/

HB_FUNC( HGETKEYAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );
   ULONG ulPos;

   if ( pHash == NULL || pPos == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HGETKEYAT", 2, hb_paramError(1), hb_paramError( 2 ) );
      return;
   }

   ulPos = hb_itemGetNL( pPos );

   if ( ulPos < 1 ||  ulPos > hb_hashLen( pHash )  )
   {
      hb_errRT_BASE( EG_BOUND, 1187, NULL, "HGETKEYAT", 2,
            hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hb_itemCopy ( &HB_VM_STACK.Return, hb_hashGetKeyAt(pHash, ulPos ) );
}

HB_FUNC( HGETVALUEAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );
   ULONG ulPos;

   if ( pHash == NULL || pPos == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HGETVALUEAT", 2, hb_paramError(1), hb_paramError( 2 ) );
      return;
   }

   ulPos = hb_itemGetNL( pPos );

   if ( ulPos < 1 || ulPos > hb_hashLen( pHash )  )
   {
      hb_errRT_BASE( EG_BOUND, 1187, NULL, "HGETVALUEAT", 2,
            hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hb_itemCopy ( &HB_VM_STACK.Return, hb_hashGetValueAt(pHash, ulPos ) );
}

HB_FUNC( HSETVALUEAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );
   PHB_ITEM pItem;
   ULONG ulPos;

   if ( pHash == NULL || pPos == NULL || pValue == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HSETVALUEAT", 3, hb_paramError(1), hb_paramError(2),
         hb_paramError(3) );
      return;
   }
   ulPos = hb_itemGetNL( pPos );

   if ( ulPos < 1 || ulPos > hb_hashLen( pHash )  )
   {
      hb_errRT_BASE( EG_BOUND, 1187, NULL, "HSETVALUEAT",
          3, hb_paramError(1), hb_paramError(2),
         hb_paramError(3) );
      return;
   }

   pItem = hb_hashGetValueAt(pHash, ulPos );
   hb_itemCopy( pItem, pValue );
}

HB_FUNC( HGETPAIRAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pKey, pValue;
   PHB_ITEM pArrRet;

   PHB_BASEHASH pBaseHash;
   ULONG ulPos;

   if ( pHash == NULL || pPos == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "HGETPAIRAT",
         4, hb_paramError(1), hb_paramError( 2 ),
         hb_paramError(3), hb_paramError( 4 ) );
      return;
   }

   ulPos = hb_itemGetNL( pPos );
   pBaseHash = pHash->item.asHash.value;

   if ( ulPos < 1 || ulPos > hb_hashLen( pHash )  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "HGETPAIRAT",
         4, hb_paramError(1), hb_paramError( 2 ),
         hb_paramError(3), hb_paramError( 4 ) );
      return;
   }

   ulPos = hb_itemGetNL( pPos );
   pKey = hb_param( 3, HB_IT_BYREF );
   pValue = hb_param( 4, HB_IT_BYREF );

   if ( pKey == NULL || pValue == NULL )
   {
      pArrRet = hb_itemNew( NULL );
      hb_arrayNew( pArrRet, 2 );
      hb_itemCopy( hb_arrayGetItemPtr( pArrRet, 1 ),
         hb_hashGetKeyAt(pHash, ulPos ) );
      hb_itemCopy( hb_arrayGetItemPtr( pArrRet, 2 ),
         hb_hashGetValueAt(pHash, ulPos ) );
      hb_itemForwardValue( &HB_VM_STACK.Return, pArrRet );
   }
   else
   {
      hb_itemCopy( pKey, hb_hashGetKeyAt(pHash, ulPos ) );
      hb_itemCopy( pValue, hb_hashGetValueAt(pHash, ulPos ) );
      hb_ret();
   }
}

HB_FUNC( HDELAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_NUMERIC );
   ULONG ulPos;

   if ( pHash == NULL || pKey == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HDELAT", 2, hb_paramError(1), hb_paramError(2));
      return;
   }
   ulPos = hb_itemGetNL( pKey );

   if ( ulPos < 1 || ulPos > hb_hashLen( pHash )  )
   {
      hb_errRT_BASE( EG_BOUND, 1187, NULL, "HDELAT", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   hb_hashRemove( pHash, ulPos );
}


/**************************************************************
* Keys/values arrays
***************************************************************/
HB_FUNC( HGETKEYS )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if ( pHash == NULL  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HGETKEYS", 1, hb_paramError(1) );
      return;
   }

   hb_itemForwardValue( &HB_VM_STACK.Return, hb_hashGetKeys( pHash )  );
}


HB_FUNC( HGETVALUES )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if ( pHash == NULL  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HGETVALUES", 1, hb_paramError(1) );
      return;
   }

   hb_itemForwardValue( &HB_VM_STACK.Return, hb_hashGetValues( pHash )  );
}

/***********************************************************
* Filling, scanning and evaluating
************************************************************/

HB_FUNC( HFILL )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pVal = hb_param( 2, HB_IT_ANY );
   PHB_ITEM pV;
   ULONG ulPos, ulLen;

   if ( pHash == NULL || pVal == NULL  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL,
         "HFILL", 2, hb_paramError(1), hb_paramError(2) );
      return;
   }

   if ( pHash->item.asHash.value->uiLevel == 0 )
   {
      pV = pHash->item.asHash.value->pValues;
      ulLen = pHash->item.asHash.value->ulLen;

      for ( ulPos = 1 ; ulPos <= ulLen; ulPos ++, pV++ )
      {
         hb_itemCopy( pV, pVal );
      }
   }
   else
   {
      for ( ulPos = 1; ulPos <= hb_hashLen( pHash ); ulPos ++ )
      {
         hb_itemCopy( hb_hashGetValueAt( pHash, ulPos ), pVal );
      }
   }
}

HB_FUNC( HSCAN )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

   if( pHash && pValue )
   {
      ULONG ulStart = hb_parnl( 3 );
      ULONG ulCount = hb_parnl( 4 );
      BOOL bExact   = hb_parl( 5 );

      hb_retnl( hb_arrayScan( pHash, pValue, ISNUM( 3 ) ? &ulStart : NULL, ISNUM( 4 ) ? &ulCount : NULL, bExact ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HSCAN", 5,
      hb_paramError( 1 ), hb_paramError( 2 ),
      hb_paramError( 3 ), hb_paramError( 4 ), hb_paramError( 5 ));
   }

}

HB_FUNC( HEVAL )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK );

   if( pHash && pBlock )
   {
      ULONG ulStart = hb_parnl( 3 );
      ULONG ulCount = hb_parnl( 4 );

      hb_arrayEval( pHash,
                    pBlock,
                    ISNUM( 3 ) ? &ulStart : NULL,
                    ISNUM( 4 ) ? &ulCount : NULL );

      /* HEval() returns the array itself */
      if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
      {
         hb_itemCopy( &(HB_VM_STACK.Return), pHash );
      }
      else
      {
         hb_itemForwardValue( &(HB_VM_STACK.Return), pHash );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HEVAL", 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ) );
   }
}


/**********************************************************
* Clone and merge
***********************************************************/

HB_FUNC( HCLONE )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( ! pHash )
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HCLONE", 2,
      hb_paramError( 1 ), hb_paramError( 2 ));
   }

   hb_itemForwardValue( &HB_VM_STACK.Return, hb_hashClone( pHash ) );
}


HB_FUNC( HCOPY )
{
   PHB_ITEM pSource = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pDest = hb_param( 2, HB_IT_HASH );
   PHB_ITEM pStart = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pEnd = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pBlock = hb_param( 5, HB_IT_BLOCK | HB_IT_LOGICAL );
   ULONG ulStart, ulCount, ulLen;

   if ( pSource == NULL || pDest == NULL )
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HCOPY", 3,
      hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ));
   }

   ulLen   = hb_hashLen(pSource);
   ulStart = pStart == NULL ? 1 : hb_itemGetNL( pStart );
   ulCount = pEnd   == NULL ? ulLen - ulStart + 1 : hb_itemGetNL( pEnd );

   if ( ulStart < 1 ||  ulCount <= 0 || ulStart + ulCount > ulLen)
   {
   }

   hb_hashMerge( pDest, pSource, ulStart, ulCount, pBlock );

   /* return a reference to the hash */
   hb_itemCopy( &(HB_VM_STACK.Return), pDest );
}

HB_FUNC( HMERGE )
{
   PHB_ITEM pDest = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pSource = hb_param( 2, HB_IT_HASH );
   PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK | HB_IT_NUMERIC );

   if ( pSource == NULL || pDest == NULL )
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HMERGE", 5,
      hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }

   hb_hashMerge( pDest, pSource, 1, hb_hashLen(pSource), pBlock );

   /* return a reference to the hash */
   hb_itemCopy( &(HB_VM_STACK.Return), pDest );
}


/**********************************************************
* Setup and set options
***********************************************************/
static void hb_hashSetCaseMatch( PHB_ITEM pHash, BOOL bCase )
{
   PHB_BASEHASH pBase = pHash->item.asHash.value;

   pBase->bCase = bCase;

   if ( pBase->uiLevel > 0 )
   {
      ULONG ulCount;

      for ( ulCount = 0; ulCount < pBase->ulLen; ulCount ++ )
      {
         hb_hashSetCaseMatch( pBase->pValues + ulCount, bCase );
      }
   }
}


HB_FUNC( HSETCASEMATCH )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_LOGICAL );

   if( pHash && pValue )
   {
      hb_hashSetCaseMatch( pHash, hb_itemGetL( pValue ) );

      /* return a reference to the hash */
      hb_itemCopy( &(HB_VM_STACK.Return), pHash );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HSETCASEMATCH", 2,
      hb_paramError( 1 ), hb_paramError( 2 ));
   }
}

HB_FUNC( HGETCASEMATCH )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( ! pHash )
   {
         hb_errRT_BASE( EG_ARG, 2017, NULL, "HGETCASEMATCH", 2,
         hb_paramError( 1 ), hb_paramError( 2 ));
   }
   else {
      hb_retl( pHash->item.asHash.value->bCase);
   }
}


HB_FUNC( HSETAUTOADD )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_LOGICAL );

   if( pHash && pValue )
   {
      pHash->item.asHash.value->bAutoAdd = hb_itemGetL( pValue );

      /* return a reference to the hash */
      hb_itemCopy( &(HB_VM_STACK.Return), pHash );
   }
   else
   {
         hb_errRT_BASE( EG_ARG, 2017, NULL, "HSETAUTOADD", 1,
         hb_paramError( 1 ));
   }
}


HB_FUNC( HGETAUTOADD )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( ! pHash )
   {
         hb_errRT_BASE( EG_ARG, 2017, NULL, "HGETAUTOADD", 1,
         hb_paramError( 1 ));
   }
   else {
      hb_retl( pHash->item.asHash.value->bAutoAdd );
   }
}


HB_FUNC( HSETPARTITION )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pSize = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pLevel = hb_param( 3, HB_IT_NUMERIC );
   ULONG ulSize;
   UINT uiLevel;


   if( ! pHash )
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HSETPARTITION", 3,
            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ));
      return;
   }

   if( hb_hashLen( pHash ) > 0 )
   {
      hb_errRT_BASE( EG_ARG, 2017, "Can't change partitioning in a non-empty hash",
            "HSETPARTITION", 3,
            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ));
      return;
   }

   if ( pLevel != NULL )
   {
      uiLevel = hb_itemGetNI( pLevel );
   }
   else
   {
      uiLevel = 1;
   }

   if ( uiLevel < 1 || uiLevel > 8 )
   {
      hb_errRT_BASE( EG_ARG, 2017, "Pagination level must be between 1 and 8",
            "HSETPARTITION", 3,
            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ));
      return;
   }

   if ( pSize == NULL )
   {
      ulSize = 0;
   }
   else
   {
      ulSize = hb_itemGetNL( pSize );
   }


   if ( ulSize == 0 )
   {
      pHash->item.asHash.value->uiLevel = 0;
      pHash->item.asHash.value->ulPageSize = 0;
   }
   else
   {
      pHash->item.asHash.value->uiLevel = uiLevel;
      pHash->item.asHash.value->ulPageSize = ulSize;
   }
}

HB_FUNC( HGETPARTITION )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pSize = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pLevel = hb_param( 3, HB_IT_BYREF );
   BOOL bPaged;

   if( ! pHash )
   {
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HGETPARTITION", 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ));
      return;
   }

   bPaged = pHash->item.asHash.value->uiLevel > 0;

   if ( bPaged )
   {
      if ( pSize != NULL )
      {
         hb_itemPutNL( pSize, pHash->item.asHash.value->ulPageSize );
      }

      if ( pLevel != NULL )
      {
         hb_itemPutNI( pLevel, pHash->item.asHash.value->uiLevel );
      }
   }

   hb_retl( bPaged );

}


HB_FUNC( HALLOCATE )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_NUMERIC );

   if( pHash && pValue )
   {
      long lMem = hb_itemGetNL( pValue );
      if ( lMem > 0 )
      {
         hb_hashPreallocate( pHash, lMem );
         return;
      }
   }

   hb_errRT_BASE( EG_ARG, 2017, NULL, "HALLOCATE", 2,
   hb_paramError( 1 ), hb_paramError( 2 ));

}
