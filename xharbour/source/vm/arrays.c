/*
 * $Id: arrays.c,v 1.73 2003/09/10 19:31:22 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * The Array API (C level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_arrayIsObject()
 *    hb_arrayCopyC()
 *    hb_arrayGetC()
 *
 * Copyright 2001 Ron Pinkas <ron@ronpinks.com>
 *    hb_arrayClone()
 *    hb_arrayFromStack()
 *    hb_arrayFromParams()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbfast.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbstack.h"

extern char *hb_vm_acAscii[256];

BOOL HB_EXPORT hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ) /* creates a new array */
{
   PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) hb_gcAlloc( sizeof( HB_BASEARRAY ), hb_arrayReleaseGarbage );
   ULONG ulPos;

   //#define DEBUG_ARRAYS

   #ifdef DEBUG_ARRAYS
      char szProc[64], szModule[64];
      static int s_i = 0;

      if( s_i++ > 0 )
      {
         hb_procinfo( 0, szProc, NULL, szModule  );
         TraceLog( NULL, "New array %p of %i items (%s->%s)\n", pBaseArray, ulLen, szModule, szProc );
      }
   #endif

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayNew(%p, %lu)", pItem, ulLen));

   if( HB_IS_COMPLEX( pItem ) )
   {
      hb_itemClear( pItem );
   }

   if( ulLen > 0 )
   {
      pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * ulLen );
   }
   else
   {
      pBaseArray->pItems = NULL;
   }

   #ifdef HB_ARRAY_USE_COUNTER
      pBaseArray->uiHolders = 1;
   #else
      pBaseArray->pOwners = NULL;
      hb_arrayRegisterHolder( pBaseArray, (void *) pItem );
   #endif

   pBaseArray->ulLen = ulLen;
   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;
   pBaseArray->puiClsTree = NULL;

   for( ulPos = 0; ulPos < ulLen; ulPos++ )
   {
      ( pBaseArray->pItems + ulPos )->type = HB_IT_NIL;
   }

   pItem->item.asArray.value = pBaseArray;

   // ITEM ARRAY MUST BE SET FOR LAST!
   pItem->type = HB_IT_ARRAY;

   return TRUE;
}

BOOL HB_EXPORT hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayAdd(%p, %p)", pArray, pValue));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->ulLen < ULONG_MAX )
      {
         hb_arraySize( pArray, pBaseArray->ulLen + 1 );
         pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         hb_itemCopy( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );

         return TRUE;
      }
   }

   return FALSE;
}

BOOL HB_EXPORT hb_arrayAddForward( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayAddForward(%p, %p)", pArray, pValue));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->ulLen < ULONG_MAX )
      {
         hb_arraySize( pArray, pBaseArray->ulLen + 1 );
         pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         hb_itemForwardValue( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );

         return TRUE;
      }
   }

   return FALSE;
}

ULONG HB_EXPORT hb_arrayLen( PHB_ITEM pArray )
{
   ULONG ulLen = 0;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLen(%p)", pArray));

   if( HB_IS_ARRAY( pArray ) )
   {
      ulLen = pArray->item.asArray.value->ulLen;
   }

   return ulLen;
}

BOOL HB_EXPORT hb_arrayIsObject( PHB_ITEM pArray )
{
   BOOL bObj = FALSE;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIsObject(%p)", pArray));

   if( HB_IS_ARRAY( pArray ) )
   {
      bObj = pArray->item.asArray.value->uiClass != 0;
   }

   return bObj;
}

BOOL HB_EXPORT hb_arraySize( PHB_ITEM pArray, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySize(%p, %lu)", pArray, ulLen));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

      if( ulLen != pBaseArray->ulLen )
      {
         ULONG ulPos;

         if( pBaseArray->ulLen == 0 )
         {
            pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( ulLen * sizeof( HB_ITEM ) );

            for( ulPos = 0; ulPos < ulLen; ulPos++ )
            {
               ( pBaseArray->pItems + ulPos )->type = HB_IT_NIL;
            }
         }
         else
         {
            if( pBaseArray->ulLen < ulLen )
            {
               #ifndef HB_ARRAY_USE_COUNTER
                  PHB_ITEM pOldItems = pBaseArray->pItems;
               #endif

               pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulLen );

               #ifndef HB_ARRAY_USE_COUNTER
                  if( pBaseArray->pItems != pOldItems )
                  {
                     for( ulPos = 0; ulPos < pBaseArray->ulLen; ulPos++ )
                     {
                        if( HB_IS_ARRAY( pBaseArray->pItems + ulPos ) && ( pBaseArray->pItems + ulPos )->item.asArray.value )
                        {
                           hb_arrayResetHolder( ( pBaseArray->pItems + ulPos )->item.asArray.value, ( pOldItems + ulPos ), ( pBaseArray->pItems + ulPos ) );
                        }
                     }
                  }
               #endif

               /* set value for new items */
               for( ulPos = pBaseArray->ulLen; ulPos < ulLen; ulPos++ )
               {
                  ( pBaseArray->pItems + ulPos )->type = HB_IT_NIL;
               }
            }
            else if( pBaseArray->ulLen > ulLen )
            {
               /* release old items */
               for( ulPos = ulLen; ulPos < pBaseArray->ulLen; ulPos++ )
               {
                  if( HB_IS_COMPLEX( pBaseArray->pItems + ulPos ) )
                  {
                     hb_itemClear( pBaseArray->pItems + ulPos );
                  }
                  else
                  {
                     ( pBaseArray->pItems + ulPos )->type = HB_IT_NIL;
                  }
               }

               if( ulLen == 0 )
               {
                  hb_xfree( pBaseArray->pItems );
                  pBaseArray->pItems = NULL;
               }
               else
               {
                  #ifndef HB_ARRAY_USE_COUNTER
                     PHB_ITEM pOldItems = pBaseArray->pItems;
                  #endif

                  pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulLen );

                  #ifndef HB_ARRAY_USE_COUNTER
                     if( pBaseArray->pItems != pOldItems )
                     {
                        for( ulPos = 0; ulPos < pBaseArray->ulLen; ulPos++ )
                        {
                           if( HB_IS_ARRAY( pBaseArray->pItems + ulPos ) && ( pBaseArray->pItems + ulPos )->item.asArray.value )
                           {
                              hb_arrayResetHolder( ( pBaseArray->pItems + ulPos )->item.asArray.value, (void *) ( pOldItems + ulPos ), (void *) ( pBaseArray->pItems + ulPos ) );
                           }
                        }
                     }
                  #endif
               }
            }
         }

         pBaseArray->ulLen = ulLen;
      }

      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

BOOL HB_EXPORT hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayDel(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         for( ulIndex--; ulIndex < ulLen - 1; ulIndex++ )       /* move items */
         {
            hb_itemForwardValue( pBaseArray->pItems + ulIndex, pBaseArray->pItems + ( ulIndex + 1 ) );
         }
      }
      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

BOOL HB_EXPORT hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIns(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         for( ulLen--; ulLen >= ulIndex; ulLen-- )          /* move items */
         {
            hb_itemForwardValue( pBaseArray->pItems + ulLen, pBaseArray->pItems + ( ulLen - 1 ) );
         }

         if( HB_IS_COMPLEX( pBaseArray->pItems + ulLen ) )
         {
            hb_itemClear( pBaseArray->pItems + ulLen );
         }
         else
         {
            ( pBaseArray->pItems + ulLen )->type = HB_IT_NIL;
         }
      }
      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

BOOL HB_EXPORT hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   PHB_ITEM pElement;

   HB_TRACE(HB_TR_DEBUG, ("hb_arraySet(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

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
   else
   {
      return FALSE;
   }
}

BOOL HB_EXPORT hb_arraySetForward( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   PHB_ITEM pElement;

   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetForward(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

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
   else
   {
      return FALSE;
   }
}

BOOL HB_EXPORT hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGet(%p, %lu, %p) Base: %p Items: %p", pArray, ulIndex, pItem, pArray->item.asArray.value, pArray->item.asArray.value->pItems));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      PHB_ITEM pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

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
   else
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
      else
      {
         pItem->type = HB_IT_NIL;
      }
   }

   return FALSE;
}

BOOL HB_EXPORT hb_arrayGetByRef( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetByRef(%p, %lu, %p) Base: %p Items: %p", pArray, ulIndex, pItem, pArray->item.asArray.value, pArray->item.asArray.value->pItems));

   if( HB_IS_COMPLEX( pItem ) )
   {
      hb_itemClear( pItem );
   }

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      PHB_ITEM pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

      pItem->type = HB_IT_BYREF;

      pItem->item.asRefer.value = ulIndex - 1;
      pItem->item.asRefer.offset = 0;
      pItem->item.asRefer.BasePtr.itemsbase = &( pArray->item.asArray.value->pItems );

      if( pElement->type == HB_IT_STRING && ( pElement->item.asString.bStatic || *( pElement->item.asString.puiHolders ) > 1 ) )
      {
         char *sString = (char*) hb_xgrab( pElement->item.asString.length + 1 );

         memcpy( sString, pElement->item.asString.value, pElement->item.asString.length + 1 );

         if( pElement->item.asString.bStatic == FALSE )
         {
            hb_itemReleaseString( pElement );
         }

         pElement->item.asString.value = sString;
         pElement->item.asString.bStatic = FALSE;
         pElement->item.asString.puiHolders = (USHORT *) hb_xgrab( sizeof( USHORT ) );
         *( pElement->item.asString.puiHolders ) = 1;
      }

      return TRUE;
   }
   else if( HB_IS_STRING( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asString.length )
   {
      unsigned char cChar = pArray->item.asString.value[ ulIndex - 1 ];

      pItem->type = HB_IT_STRING;
      pItem->item.asString.value   = hb_vm_acAscii[ cChar ];
      pItem->item.asString.bStatic = TRUE;
      pItem->item.asString.length  = 1;

      return TRUE;
   }

   pItem->type = HB_IT_NIL;

   return FALSE;
}

char HB_EXPORT * hb_arrayGetDS( PHB_ITEM pArray, ULONG ulIndex, char * szDate )
{
   char *exData;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetDS(%p, %lu, %s)", pArray, ulIndex, szDate));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      exData = hb_itemGetDS( pArray->item.asArray.value->pItems + ulIndex - 1, szDate );
   }
   else
   {
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDS(). [vszakats] */
      exData = hb_itemGetDS( NULL, szDate );
   }
   return exData;
}

long HB_EXPORT hb_arrayGetDL( PHB_ITEM pArray, ULONG ulIndex )
{
   long lData;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetDL(%p, %lu)", pArray, ulIndex ));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      lData = hb_itemGetDL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDL(). [vszakats] */
      lData = hb_itemGetDL( NULL );
   }

   return lData;
}

/*
 * This function returns a pointer to an item occupied by the specified
 * array element - it doesn't return an item's value
 */

PHB_ITEM HB_EXPORT hb_arrayGetItemPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   PHB_ITEM pItem = NULL;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetItemPtr(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      pItem = pArray->item.asArray.value->pItems + ulIndex - 1;
   }

   return pItem;
}

BOOL HB_EXPORT hb_arrayGetL( PHB_ITEM pArray, ULONG ulIndex )
{
   BOOL bRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetL(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      bRet = hb_itemGetL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      bRet =  FALSE;
   }
   return bRet;
}

int HB_EXPORT hb_arrayGetNI( PHB_ITEM pArray, ULONG ulIndex )
{
   int iRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNI(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      iRet =  hb_itemGetNI( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      iRet = 0;
   }
   return iRet;
}

long HB_EXPORT hb_arrayGetNL( PHB_ITEM pArray, ULONG ulIndex )
{
   long lRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNL(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      lRet = hb_itemGetNL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      lRet = 0;
   }
   return lRet;
}

double HB_EXPORT hb_arrayGetND( PHB_ITEM pArray, ULONG ulIndex )
{
   double dRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetND(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      dRet = hb_itemGetND( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      dRet = 0;
   }

   return dRet;
}

ULONG HB_EXPORT hb_arrayCopyC( PHB_ITEM pArray, ULONG ulIndex, char * szBuffer, ULONG ulLen )
{
   ULONG ulRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopyC(%p, %lu, %s, %lu)", pArray, ulIndex, szBuffer, ulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      ulRet = hb_itemCopyC( pArray->item.asArray.value->pItems + ulIndex - 1, szBuffer, ulLen );
   }
   else
   {
      ulRet = 0;
   }
   return ulRet;
}

char HB_EXPORT * hb_arrayGetC( PHB_ITEM pArray, ULONG ulIndex )
{
   char *cRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetC(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      cRet = hb_itemGetC( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      cRet = NULL;
   }
   return cRet;
}

char HB_EXPORT * hb_arrayGetCPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   char *cRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCPtr(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      cRet = hb_itemGetCPtr( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      cRet = "";
   }
   return cRet;
}

ULONG HB_EXPORT hb_arrayGetCLen( PHB_ITEM pArray, ULONG ulIndex )
{
   ULONG ulRet;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCLen(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      ulRet = hb_itemGetCLen( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      ulRet = 0;
   }
   return ulRet;
}

USHORT HB_EXPORT hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex )
{
   USHORT uType;
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetType(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      uType = hb_itemType( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }
   else
   {
      uType = 0;
   }
   return uType;
}

BOOL HB_EXPORT hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLast(%p, %p)", pArray, pResult));

   if( HB_IS_ARRAY( pArray ) )
   {
      if( pArray->item.asArray.value->ulLen > 0 )
      {
         hb_itemCopy( pResult, pArray->item.asArray.value->pItems + ( pArray->item.asArray.value->ulLen - 1 ) );
      }
      else
      {
         if( HB_IS_COMPLEX( pResult ) )
         {
            hb_itemClear( pResult );
         }
         else
         {
            pResult->type = HB_IT_NIL;
         }
      }

      return TRUE;
   }

   if( HB_IS_COMPLEX( pResult ) )
   {
      hb_itemClear( pResult );
   }
   else
   {
      pResult->type = HB_IT_NIL;
   }
   return FALSE;
}

void HB_EXPORT hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG ulStart, ULONG ulCount )
{
   PHB_BASEARRAY pBaseArray;
   ULONG ulLen;
   PHB_ITEM pElement;

   pBaseArray = pArray->item.asArray.value;
   ulLen = pBaseArray->ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFill(%p, %p, %i, %i)", pArray, pValue, ulStart, ulCount));

   if( ulStart <= ulLen )
   {
      if( ulStart + ulCount > ulLen + 1 )
      {
         ulCount = ulLen - ulStart + 1;
      }

      ulStart--;

      for( ; ulCount > 0; ulCount--, ulStart++ )
      {
         pElement = pBaseArray->pItems + ulStart;

         if( HB_IS_BYREF( pElement ) )
         {
            hb_itemCopy( hb_itemUnRef( pElement ), pValue );
         }
         else
         {
            hb_itemCopy( pElement, pValue );
         }
      }
   }
}

ULONG HB_EXPORT hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount, BOOL bExact )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayScan(%p, %p, %p, %p)", pArray, pValue, pulStart, pulCount, bExact));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && ( *pulStart >= 1 ) )
      {
         ulStart = *pulStart;
      }
      else
      {
         ulStart = 1;
      }

      if( ulStart <= ulLen )
      {
         if( pulCount && ( *pulCount <= ulLen - ulStart ) )
         {
            ulCount = *pulCount;
         }
         else
         {
            ulCount = ulLen - ulStart + 1;
         }

         if( ulStart + ulCount > ulLen )             /* check range */
         {
            ulCount = ulLen - ulStart + 1;
         }

         /* Make separate search loops for different types to find, so that
            the loop can be faster. */

         if( HB_IS_BLOCK( pValue ) )
         {
            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pValue );
               hb_vmPush( pBaseArray->pItems + ulStart );
               hb_vmPushNumber( ( double ) ( ulStart + 1 ), 0 );
               hb_vmSend( 2 );

               if( HB_IS_LOGICAL( &(HB_VM_STACK.Return) ) && HB_VM_STACK.Return.item.asLogical.value )
               {
                  return ulStart + 1;                  /* arrays start from 1 */
               }
            }
         }
         else if( HB_IS_STRING( pValue ) ) // Must precede HB_IS_NUMERIC()
         {
            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

               /* NOTE: The order of the pItem and pValue parameters passed to
                        hb_itemStrCmp() is significant, please don't change it. [vszakats] */
               if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, bExact ) == 0 )
               {
                  return ulStart + 1;
               }
            }
         }
         else if( HB_IS_DATE( pValue ) ) // Must precede HB_IS_NUMERIC()
         {
            long lValue = hb_itemGetDL( pValue );

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

               if( HB_IS_DATE( pItem ) && hb_itemGetDL( pItem ) == lValue )
               {
                  return ulStart + 1;
               }
            }
         }
         else if( HB_IS_NUMERIC( pValue ) )
         {
            double dValue = hb_itemGetND( pValue );

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                HB_TRACE( HB_TR_INFO, ( "hb_arrayScan() %p, %d", pItem, dValue ) );

               if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
               {
                  return ulStart + 1;
               }
            }
         }
         else if( HB_IS_LOGICAL( pValue ) )
         {
            BOOL bValue = hb_itemGetL( pValue ); /* NOTE: This is correct: Get the date as a long value. [vszakats] */

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

               if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
               {
                  return ulStart + 1;
               }
            }
         }
         else if( HB_IS_NIL( pValue ) )
         {
            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               if( HB_IS_NIL( pBaseArray->pItems + ulStart ) )
               {
                  return ulStart + 1;
               }
            }
         }
      }
   }

   return 0;
}

BOOL HB_EXPORT hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG * pulStart, ULONG * pulCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayEval(%p, %p, %p, %p)", pArray, bBlock, pulStart, pulCount));

   if( HB_IS_ARRAY( pArray ) && HB_IS_BLOCK( bBlock ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && ( *pulStart >= 1 ) )
      {
         ulStart = *pulStart;
      }
      else
      {
         ulStart = 1;
      }

      if( ulStart <= ulLen )
      {
         if( pulCount && ( *pulCount <= ulLen - ulStart ) )
         {
            ulCount = *pulCount;
         }
         else
         {
            ulCount = ulLen - ulStart + 1;
         }

         if( ulStart + ulCount > ulLen )             /* check range */
         {
            ulCount = ulLen - ulStart + 1;
         }

         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            PHB_ITEM pItem = pBaseArray->pItems + ulStart;

            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( bBlock );
            hb_vmPush( pItem );
            hb_vmPushNumber( ( double ) ( ulStart + 1 ), 0 );
            hb_vmSend( 2 );
         }
      }

      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

void hb_arrayReleaseBase( PHB_BASEARRAY pBaseArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "pBaseArray %p", pBaseArray ) );

   //TraceLog( NULL, "Releasing Basearray %p\n", pBaseArray );

   // Called recursively from hb_arrayReleaseGarbage!
   if( pBaseArray->pItems == (PHB_ITEM) 1 )
   {
      return;
   }

   /* Release object tree as needed */
   if( pBaseArray->puiClsTree )
   {
      hb_xfree( pBaseArray->puiClsTree );
      pBaseArray->puiClsTree = NULL;
   }

   if( pBaseArray->pItems )
   {
      HB_ITEM_PTR pItems = pBaseArray->pItems, pItem; //, pValue;
      ULONG ulLen = pBaseArray->ulLen;

      //TraceLog( NULL, "Releasing BaseArray %p\n", pBaseArray );

      // HACK! Avoid possible recursion problem when one of the items in turn points to this Array.
      pBaseArray->pItems = (PHB_ITEM) 1;

      pItem = pItems;

      while( ulLen-- )
      {
         HB_TRACE( HB_TR_INFO, ( "Array Item %p type:%i", pItem, pItem->type ) );

         //TraceLog( NULL, "Releasing Element: %i %p Class: %s\n", pBaseArray->ulLen - ulLen, pItem, hb_objGetClsName( pItem ) );

         //printf( "Array Item %i %p type:%i\n", ulLen, pItem, pItem->type );

         /* The Reference should NOT be cleared when an Array with element refering it is released!
         pValue = pItem; // Subsequent pItem below were pValue, other then pItem++!!!
         if( HB_IS_BYREF( pValue ) )
         {
            pValue = hb_itemUnRef( pValue );
         }
         */

         /*-----------------12/21/2001 8:01PM----------------
          * The item is not released because it was not
          * allocated by the GC, its just a portion of the
          * pItems chunk, which will be released as one piece.
          * --------------------------------------------------*/
         if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value == pBaseArray )
         {
            HB_TRACE( HB_TR_DEBUG, ("Warning! Nested Release (Cyclic) %p %p", pItem, pItem->item.asArray.value ) );
            TraceLog( NULL, "Warning! Nested Release (Cyclic) %p %p\n", pItem, pItem->item.asArray.value );
         }
         else if( HB_IS_COMPLEX( pItem ) )
         {
            //TraceLog( NULL, "Releasing Element: %i %p Type: %i\n", pBaseArray->ulLen - ulLen, pItem, pItem->type );
            hb_itemClear( pItem );
            //TraceLog( NULL, "DONE Releasing Element: %i %p\n", pBaseArray->ulLen - ulLen, pItem );
         }

         ++pItem;
      }

      HB_TRACE( HB_TR_INFO, ( "Release pItems %p", pItems ) );
      hb_xfree( pItems );

      pBaseArray->pItems = NULL;
   }

   HB_TRACE( HB_TR_INFO, ( "Release pBaseArray %p", pBaseArray ) );
   hb_gcFree( ( void * ) pBaseArray );

   //TraceLog( NULL, "DONE Releasing Basearray %p\n", pBaseArray );
}

BOOL HB_EXPORT hb_arrayRelease( PHB_ITEM pArray )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_arrayRelease(%p) %p", pArray, pArray->item.asArray.value ) );

   //printf( "hb_arrayRelease(%p) type: %i %p\n", pArray, pArray->type, pArray->item.asArray.value );

   if( HB_IS_ARRAY( pArray ) )
   {
      #ifdef HB_ARRAY_USE_COUNTER
          hb_arrayReleaseBase( pArray->item.asArray.value );
      #else
          if( pArray->item.asArray.value && pArray->item.asArray.value->pOwners )
          {
             PHB_ARRAY_HOLDER pOwners = pArray->item.asArray.value->pOwners;

             while( pOwners )
             {
                char szProc[64], szModule[64];
                USHORT uiLine;

                hb_procinfo( 0, szProc, &uiLine, szModule  );

                if( pOwners->pOwner != (void *) pArray )
                {
                   TraceLog( NULL, "Warning! (1) Residual owner %p of array %p [%s->%s(%i)]\n",
                                   pOwners, pArray->item.asArray.value,
                                   szModule, szProc, uiLine );
                }

                pOwners = pOwners->pNext;
             }

             //TraceLog( NULL, "Diverting to ReleaseHolder() - Class '%s'\n", hb_objGetClsName( pArray ) );
             hb_arrayReleaseHolder( pArray->item.asArray.value, (void *) pArray );
          }
      #endif

      pArray->type = HB_IT_NIL;
      pArray->item.asArray.value = NULL;

      //printf( "\nDone! hb_arrayRelease(%p) %p", pArray, pArray->item.asArray.value );
      return TRUE;
   }
   else
   {
      char szProc[64], szModule[64];
      USHORT uiLine;

      hb_procinfo( 0, szProc, &uiLine, szModule  );
      TraceLog( NULL, "Warning! not an array %p [%s->%s(%i)]\n", pArray, szModule, szProc, uiLine );
      return FALSE;
   }
}

/* NOTE: CA-Cl*pper 5.3a has a fix for the case when the starting position
         is greater than the length of the array. [vszakats] */
BOOL HB_EXPORT hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG * pulStart,
                   ULONG * pulCount, ULONG * pulTarget )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopy(%p, %p, %p, %p, %p)", pSrcArray, pDstArray, pulStart, pulCount, pulTarget));

   if( HB_IS_ARRAY( pSrcArray ) && HB_IS_ARRAY( pDstArray ) )
   {
      PHB_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PHB_BASEARRAY pDstBaseArray = pDstArray->item.asArray.value;
      ULONG ulSrcLen = pSrcBaseArray->ulLen;
      ULONG ulDstLen = pDstBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;
      ULONG ulTarget;

      if( pulStart && ( *pulStart >= 1 ) )
      {
         ulStart = *pulStart;
      }
      else
      {
         ulStart = 1;
      }

      if( pulTarget && ( *pulTarget >= 1 ) )
      {
         ulTarget = *pulTarget;
      }
      else
      {
         ulTarget = 1;
      }

#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
      if( ulStart <= ulSrcLen )
#else
      if( ulSrcLen > 0 )
#endif
      {
#ifndef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
         if( ulStart > ulSrcLen )
         {
            ulStart = ulSrcLen;
         }
#endif
         if( pulCount && ( *pulCount <= ulSrcLen - ulStart ) )
         {
            ulCount = *pulCount;
         }
         else
         {
            ulCount = ulSrcLen - ulStart + 1;
         }

/* This is probably a bug, present in all versions of CA-Cl*pper. */
#ifdef HB_FIX_ACOPY_BUG
         if( ulTarget <= ulDstLen )
         {
#else
         if( ulDstLen > 0 )
         {
            if( ulTarget > ulDstLen )
            {
               ulTarget = ulDstLen;
            }
#endif

            if( ulCount > ulDstLen - ulTarget )
            {
               ulCount = ulDstLen - ulTarget + 1;
            }

            for( ulTarget--, ulStart--; ulCount > 0; ulCount--, ulStart++, ulTarget++ )
            {
               hb_itemCopy( pDstBaseArray->pItems + ulTarget, pSrcBaseArray->pItems + ulStart );
            }
         }
      }

      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

PHB_ITEM HB_EXPORT hb_arrayClone( PHB_ITEM pSrcArray, PHB_NESTED_CLONED pClonedList )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayClone(%p, %p)", pSrcArray, pClonedList));

   if( HB_IS_ARRAY( pSrcArray ) )
   {
      PHB_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PHB_BASEARRAY pDstBaseArray;
      ULONG ulSrcLen = pSrcBaseArray->ulLen;
      ULONG ulCount;
      PHB_ITEM pDstArray;
      PHB_NESTED_CLONED pCloned;
      BOOL bTop;

      pDstArray = hb_itemNew( NULL );
      hb_arrayNew( pDstArray, ulSrcLen );

      if( pClonedList == NULL )
      {
         bTop = TRUE;

         pClonedList = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );

         pCloned = pClonedList;

         pCloned->pSrcBaseArray = pSrcBaseArray;
         pCloned->pDest         = pDstArray;

         pCloned->pNext = NULL;
      }
      else
      {
         bTop = FALSE;

         pCloned = pClonedList;
         while( pCloned->pNext )
         {
            pCloned = pCloned->pNext;
         }

         pCloned->pNext = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );
         pCloned = pCloned->pNext;

         pCloned->pSrcBaseArray = pSrcBaseArray;
         pCloned->pDest         = pDstArray;

         pCloned->pNext = NULL;
      }

      pDstBaseArray = pDstArray->item.asArray.value;
      pDstBaseArray->uiClass = pSrcBaseArray->uiClass;

      pDstBaseArray->puiClsTree = NULL;

      for( ulCount = 0; ulCount < ulSrcLen; ulCount++ )
      {
         PHB_ITEM pSrcItem = pSrcBaseArray->pItems + ulCount;

         /* Clipper clones nested array ONLY if NOT an Object!!! */
         if( HB_IS_ARRAY( pSrcItem ) && pSrcItem->item.asArray.value->uiClass == 0 )
         {
            PHB_ITEM pClone;

            /* Broken down like this to avoid redundant comparisons. */
            pCloned = pClonedList;

            if( pCloned->pSrcBaseArray == pSrcItem->item.asArray.value )
            {
               pClone = pCloned->pDest;
               goto DontClone;
            }

            while( pCloned->pNext )
            {
               pCloned = pCloned->pNext;

               if( pCloned->pSrcBaseArray == pSrcItem->item.asArray.value )
               {
                  pClone = pCloned->pDest;
                  goto DontClone;
               }
            }

            if( pCloned->pSrcBaseArray == pSrcItem->item.asArray.value )
            {
               pClone = pCloned->pDest;
               goto DontClone;
            }

            pClone = hb_arrayClone( pSrcItem, pClonedList );

           DontClone :
            hb_arraySet( pDstArray, ulCount + 1, pClone );
         }
         else
         {
            hb_arraySet( pDstArray, ulCount + 1, pSrcItem );
         }
      }

      /* Top Level - Release the created list. */
      if( bTop )
      {
         /* 1st. Chain alway exists, and points to our top level pDstArray, which should NOT be (item) released. */
         pCloned     = pClonedList;
         pClonedList = pClonedList->pNext;
         hb_xfree( pCloned );

         while( pClonedList )
         {
            pCloned     = pClonedList;
            pClonedList = pClonedList->pNext;

            hb_itemRelease( pCloned->pDest );
            hb_xfree( pCloned );
         }
      }

      return pDstArray;
   }
   else
   {
      return hb_itemNew( NULL );
   }
}

PHB_ITEM HB_EXPORT hb_arrayFromStack( USHORT uiLen )
{
   PHB_ITEM pArray = hb_itemNew( NULL );
   PHB_BASEARRAY pBaseArray;
   USHORT uiPos;

   //printf( "Got: %p\n", pBaseArray );

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFromStack(%iu)", uiLen));

   /* JC1: prevent being interrupted/killed and prevents GC from taking pBaseArray*/
   pBaseArray = ( PHB_BASEARRAY ) hb_gcAlloc( sizeof( HB_BASEARRAY ), hb_arrayReleaseGarbage );
   pArray->item.asArray.value = pBaseArray;
   pArray->type = HB_IT_ARRAY;

   if( uiLen > 0 )
   {
      pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * uiLen );
   }
   else
   {
      pBaseArray->pItems = NULL;
   }

   pBaseArray->ulLen      = uiLen;

   #ifdef HB_ARRAY_USE_COUNTER
      pBaseArray->uiHolders  = 1;
   #else
      pBaseArray->pOwners = NULL;
      hb_arrayRegisterHolder( pBaseArray, (void *) pArray );
   #endif

   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;
   pBaseArray->puiClsTree = NULL;

   for( uiPos = 0; uiPos < uiLen; uiPos++ )
   {
      ( pBaseArray->pItems + uiPos )->type = HB_IT_NIL;
      hb_itemCopy( pBaseArray->pItems + uiPos, hb_stackItemFromTop( uiPos - uiLen ) );
   }

   return pArray;
}

PHB_ITEM HB_EXPORT hb_arrayFromParams( PHB_ITEM *pBase )
{
   PHB_ITEM pArray;
   PHB_BASEARRAY pBaseArray;
   USHORT uiPos, uiPCount;

   pArray = hb_itemNew( NULL );
   uiPCount = (*pBase)->item.asSymbol.paramcnt;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFromParams(%p)", pBase));

   pBaseArray = ( PHB_BASEARRAY ) hb_gcAlloc( sizeof( HB_BASEARRAY ), hb_arrayReleaseGarbage );

   //printf( "Got: %p\n", pBaseArray );

   pArray->type = HB_IT_ARRAY;

   // SomeFunc( ... ) Variable paramaters.
   if( uiPCount > 255 )
   {
      uiPCount -= 256;
   }

   if( uiPCount > 0 )
   {
      pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * uiPCount );
   }
   else
   {
      pBaseArray->pItems = NULL;
   }

   pBaseArray->ulLen      = uiPCount;

   #ifdef HB_ARRAY_USE_COUNTER
      pBaseArray->uiHolders  = 1;
   #else
      pBaseArray->pOwners = NULL;
      hb_arrayRegisterHolder( pBaseArray, (void *) pArray );
   #endif

   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;
   pBaseArray->puiClsTree = NULL;

   for( uiPos = 0; uiPos < uiPCount; uiPos++ )
   {
      ( pBaseArray->pItems + uiPos )->type = HB_IT_NIL;
      hb_itemCopy( pBaseArray->pItems + uiPos, *( pBase + uiPos + 2 ) );
   }

   pArray->item.asArray.value = pBaseArray;

   /*Notice: the thread could still be killed HERE, causing a memory leak */

   return pArray;
}

PHB_ITEM HB_EXPORT hb_arrayFromParamsLocked( PHB_ITEM *pBase )
{
   PHB_ITEM pArray;
   PHB_BASEARRAY pBaseArray;
   USHORT uiPos, uiPCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFromParams(%p)", pBase));

   pArray = hb_itemNew( NULL );
   uiPCount = (*pBase)->item.asSymbol.paramcnt;


   /* Thread safety: the array type is set AFTER the array locking */
   pBaseArray = ( PHB_BASEARRAY ) hb_gcAlloc( sizeof( HB_BASEARRAY ), hb_arrayReleaseGarbage );
   hb_gcLock( pBaseArray );

   //printf( "Got: %p\n", pBaseArray );

   pArray->type = HB_IT_ARRAY;

   // SomeFunc( ... ) Variable paramaters.
   if( uiPCount > 255 )
   {
      uiPCount -= 256;
   }

   if( uiPCount > 0 )
   {
      pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * uiPCount );
   }
   else
   {
      pBaseArray->pItems = NULL;
   }

   pBaseArray->ulLen      = uiPCount;

   #ifdef HB_ARRAY_USE_COUNTER
      pBaseArray->uiHolders  = 1;
   #else
      pBaseArray->pOwners = NULL;
      hb_arrayRegisterHolder( pBaseArray, (void *) pArray );
   #endif

   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;
   pBaseArray->puiClsTree = NULL;

   for( uiPos = 0; uiPos < uiPCount; uiPos++ )
   {
      ( pBaseArray->pItems + uiPos )->type = HB_IT_NIL;
      hb_itemCopy( pBaseArray->pItems + uiPos, *( pBase + uiPos + 2 ) );
   }

   pArray->item.asArray.value = pBaseArray;

   /*Notice: the thread could still be killed HERE, causing a memory leak */
   return pArray;
}

/* This releases array when called from the garbage collector */
HB_GARBAGE_FUNC( hb_arrayReleaseGarbage )
{
   PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) Cargo;

   #ifndef HB_ARRAY_USE_COUNTER
      PHB_ARRAY_HOLDER pOwners, pFree;
   #endif

   HB_TRACE( HB_TR_INFO, ( "hb_arrayReleaseGarbage( %p )", pBaseArray ) );

   //TraceLog( NULL, "hb_arrayReleaseGarbage( %p )\n", pBaseArray );

   /* Release object tree as needed */
   if( pBaseArray->puiClsTree )
   {
      HB_TRACE( HB_TR_INFO, ( "Release Tree, %p )", pBaseArray ) );
      hb_xfree( pBaseArray->puiClsTree );
      pBaseArray->puiClsTree = NULL;
   }

   if( pBaseArray->pItems )
   {
      PHB_ITEM pItems = pBaseArray->pItems, pItem;
      ULONG ulLen = pBaseArray->ulLen;

      // HACK! Avoid possible recursion problem when one of the items in turn points to this Array.
      pBaseArray->pItems = (PHB_ITEM) 1;

      pItem = pItems;

      while( ulLen-- )
      {
         HB_TRACE( HB_TR_INFO, ( "Array Item %p type:%i", pItem, pItem->type ) );

          /*-----------------12/21/2001 8:01PM----------------
           * The item is not released because it was not
           * allocated by the GC, its just a portion of the
           * pItems chunk, which will be released as one piece.
           * --------------------------------------------------*/
         if( HB_IS_ARRAY( pItem ) )
         {
            #ifdef HB_ARRAY_USE_COUNTER
               if( pItem->item.asArray.value == pBaseArray )
               {
                  // Cyclic!
               }
               else
               {
                  hb_itemClear( pItem );
               }
            #else
               if( pItem->item.asArray.value )
               {
                  hb_arrayReleaseHolder( pItem->item.asArray.value, pItem );
               }
            #endif
         }
         // 03-07-2002 RP commented out - Needs further testing.
         else if( HB_IS_COMPLEX( pItem ) )
         {
            hb_itemClear( pItem );
         }

         ++pItem;
      }

      HB_TRACE( HB_TR_INFO, ( "Release pItems %p", pItems ) );
      hb_xfree( pItems );
      pBaseArray->pItems = NULL;
   }

   // Has to be AFTER the array elements have been released!
   #ifndef HB_ARRAY_USE_COUNTER
      pOwners = pBaseArray->pOwners;

      while( pOwners )
      {
         if( HB_IS_ARRAY( (PHB_ITEM) (pOwners->pOwner) ) )
         {
            //TraceLog( NULL, "Warning! (2) Residual owner %p of array %p\n", pOwners->pOwner, pBaseArray );

            if( ((PHB_ITEM) (pOwners->pOwner))->item.asArray.value == pBaseArray || ((PHB_ITEM) (pOwners->pOwner))->item.asArray.value == NULL )
            {
               // Forcing reset of the orphan refernce or else a GPF will folow when that item will be passed to hb_itemClear().
               ((PHB_ITEM) (pOwners->pOwner) )->type = HB_IT_NIL;
            }
            else
            {
               TraceLog( NULL, "Warning! (4) Invalid Residual owner %p of array %p Stack: %p\n", pOwners->pOwner, pBaseArray, hb_stackItemFromTop( 1 ) );
            }
         }
         else if( ((HB_CODEBLOCK_PTR) (pOwners->pOwner))->pSelfBase == pBaseArray )
         {
            //TraceLog( NULL, "Warning! (3) Residual owner %p of array %p\n", pOwners->pOwner, pBaseArray );

            // Forcing reset of the orphan refernce or else a GPF will folow when that item will be passed to hb_itemClear().
            ((HB_CODEBLOCK_PTR) (pOwners->pOwner) )->pSelfBase = NULL;
         }
         else
         {
            TraceLog( NULL, "Warning! (5) Invalid Residual owner %p Type: %i of array %p Stack: %p\n", pOwners->pOwner, ((PHB_ITEM) (pOwners->pOwner))->type, pBaseArray, hb_stackItemFromTop( 1 ) );
         }

         pFree = pOwners;

         pOwners = pOwners->pNext;

         hb_xfree( pFree );
      }

      pBaseArray->pOwners = NULL;
   #endif

   //TraceLog( NULL, "DONE hb_arrayReleaseGarbage( %p )\n", pBaseArray );
}

#ifndef HB_ARRAY_USE_COUNTER
   void hb_arrayRegisterHolder( PHB_BASEARRAY pBaseArray, void *pHolder )
   {
      PHB_ARRAY_HOLDER pOwner = (PHB_ARRAY_HOLDER) hb_xgrab( sizeof( HB_ARRAY_HOLDER ) );

      if( pBaseArray == NULL )
      {
         hb_errInternal( HB_EI_ERRUNRECOV, "Invalid base array passed to hb_arrayRegisterHolder().", NULL, NULL );
      }

      #ifdef DEBUG_ARRAYS
         TraceLog( NULL, "Allocated %p Registring: %p of %p Next %p\n", pOwner, pBaseArray, pHolder, pBaseArray->pOwners );
      #endif

      pOwner->pOwner = pHolder;
      pOwner->pNext = pBaseArray->pOwners;

      pBaseArray->pOwners = pOwner;
   }

   void hb_arrayResetHolder( PHB_BASEARRAY pBaseArray, void *pOldHolder, void *pNewHolder )
   {
      PHB_ARRAY_HOLDER pOwners = pBaseArray->pOwners;

      if( pBaseArray == NULL )
      {
         hb_errInternal( HB_EI_ERRUNRECOV, "Invalid base array passed to hb_arrayResetHolder().", NULL, NULL );
      }

      #ifdef DEBUG_ARRAYS
         TraceLog( NULL, "*Resetting: %p of %p to %p\n", pBaseArray, pOldHolder, pNewHolder );
      #endif

      while( pOwners )
      {
         if( pOwners->pOwner == pOldHolder )
         {
            pOwners->pOwner = pNewHolder;
            return;
         }

         pOwners = pOwners->pNext;
      }

      {
         char szProc[64], szModule[64];
         USHORT uiLine;

         hb_procinfo( 0, szProc, &uiLine, szModule  );
         TraceLog( NULL, "Warning! Could not locate old owner %p of array %p [%s->%s(%i)] Stack: %p\n",
                         pOldHolder, pBaseArray, szModule, szProc, uiLine, hb_stackItemFromTop(1) );
      }

      hb_arrayRegisterHolder( pBaseArray, pNewHolder );
   }

   void hb_arrayReleaseHolder( PHB_BASEARRAY pBaseArray, void *pHolder )
   {
      PHB_ARRAY_HOLDER pOwners = pBaseArray->pOwners, pPrevious = NULL;

      //TraceLog( NULL, "hb_arrayReleaseHolder( %p, %p )\n", pBaseArray, pHolder );

      if( pBaseArray == NULL )
      {
         hb_errInternal( HB_EI_ERRUNRECOV, "Invalid base array passed to hb_arrayReleaseHolder().", NULL, NULL );
      }

      #ifdef DEBUG_ARRAYS
         TraceLog( NULL, "-UNRegistering: %p of %p\n", pBaseArray, pHolder );
      #endif

      while( pOwners )
      {
         if( pOwners->pOwner == pHolder )
         {
            if( pPrevious )
            {
               pPrevious->pNext = pOwners->pNext;
            }
            else
            {
               pBaseArray->pOwners = pOwners->pNext;
            }

            #ifdef DEBUG_ARRAYS
               TraceLog( NULL, "Released pOwners: %p\n", pOwners );
            #endif

            hb_xfree( (void *) pOwners );
            break;
         }

         pPrevious = pOwners;
         pOwners = pOwners->pNext;
      }

      if( pOwners )
      {
         if( pBaseArray->pOwners == NULL )
         {
            #ifdef DEBUG_ARRAYS
               TraceLog( NULL, "Last Owner - Releasing array %p\n", pBaseArray );
            #endif

            //TraceLog( NULL, "Last Owner - Releasing array %p\n", pBaseArray );
            hb_arrayReleaseBase( pBaseArray );
         }
      }
      else
      {
         char szProc[64], szModule[64];
         USHORT uiLine;

         hb_procinfo( 0, szProc, &uiLine, szModule  );
         TraceLog( NULL, "Warning! Could not locate owner %p of array %p [%s->%s(%i)]\n", pHolder, pBaseArray, szModule, szProc, uiLine );
      }

      //TraceLog( NULL, "DONE hb_arrayReleaseHolder( %p, %p )\n", pBaseArray, pHolder );
   }
#endif

#ifndef HB_LONG_LONG_OFF
   LONGLONG HB_EXPORT hb_arrayGetNLL( PHB_ITEM pArray, ULONG ulIndex )
   {
      LONGLONG llRet;
      HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNLL(%p, %lu)", pArray, ulIndex));

      if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      {
         llRet = hb_itemGetNLL( pArray->item.asArray.value->pItems + ulIndex - 1 );
      }
      else
      {
         llRet = 0;
      }

      return llRet;
   }
#endif
