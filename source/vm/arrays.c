/*
 * $Id$
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
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *    hb_arrayGetDTS()
 *    hb_arrayGetT()
 *    hb_arrayGetDTsec()
 *    hb_arrayGetDTD()

 * See doc/license.txt for licensing terms.
 *
 */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbstack.h"
#include "classes.h"
#include "hbdate.h"
#include "hbset.h"

HB_EXTERN_BEGIN
#ifndef HB_ARRAY_USE_COUNTER
extern BOOL hb_gc_bReleaseAll;
#endif
extern int hb_arrayMode( void );
HB_EXTERN_END

BOOL hb_arrayNew( PHB_ITEM pItem, HB_SIZE ulLen ) /* creates a new array */
{
   PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) hb_gcAlloc( sizeof( HB_BASEARRAY ), hb_arrayReleaseGarbage );

   /* #define DEBUG_ARRAYS
    */
   #define DEBUG_OWNERS

#ifdef DEBUG_ARRAYS
   char        szProc[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ], szModule[ HB_PATH_MAX ];
   static int  s_i = 0;

   if( s_i++ > 0 )
   {
      hb_procinfo( 0, szProc, NULL, szModule  );
      TraceLog( NULL, "New array %p of %i items (%s->%s)\n", pBaseArray, ulLen, szModule, szProc );
   }
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayNew(%p, %lu)", pItem, ulLen ) );

   if( HB_IS_COMPLEX( pItem ) )
      hb_itemClear( pItem );

   pBaseArray->pItems = NULL;

   if( ulLen > 0 )
      pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * ulLen );

#ifdef HB_ARRAY_USE_COUNTER
   pBaseArray->ulHolders   = HB_ARRAY_COUNTER_DEFAULT_HOLDERS;
#else
   pBaseArray->pOwners     = NULL;
   hb_arrayRegisterHolder( pBaseArray, ( void * ) pItem );
#endif

   pBaseArray->ulLen       = ulLen;
   pBaseArray->ulAllocated = ulLen;
   pBaseArray->uiClass     = 0;
   pBaseArray->uiPrevCls   = 0;
   pBaseArray->ulBlock     = 0;
   pBaseArray->uiFlags     = 0;

   if( ulLen )
   {
      register PHB_ITEM pItems = pBaseArray->pItems + ulLen;

      do
      {
         ( --pItems )->type = HB_IT_NIL;
      }
      while( --ulLen );
   }

   pItem->item.asArray.value  = pBaseArray;

   /* ITEM ARRAY MUST BE SET FOR LAST!
    */
   pItem->type                = HB_IT_ARRAY;

   return TRUE;
}

BOOL hb_arraySize( PHB_ITEM pArray, HB_SIZE ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySize(%p, %lu)", pArray, ulLen ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

      if( ulLen != pBaseArray->ulLen )
      {
         HB_SIZE  ulAllocated = pBaseArray->ulAllocated;
         HB_SIZE  ulPos;
         PHB_ITEM pItems;

         /* release old items
          */
         if( pBaseArray->ulLen > ulLen )
         {
            /* Clipper defers all aSize() shrinking operations when in aEval()!
             */
            if( ( pBaseArray->uiFlags & 0xF000 ) != 0 )
            {
               pBaseArray->ulLen = ulLen;
               return TRUE;
            }

            pItems   = pBaseArray->pItems + ulLen;
            ulPos    = pBaseArray->ulLen - ulLen;

            do
            {
               hb_itemSetNil( pItems ); /* don't use ++ here */
               pItems++;
            }
            while( --ulPos );
         }
         else if( ( pBaseArray->uiFlags & 0xF000 ) != 0 && ulLen > pBaseArray->ulLen && ulAllocated > pBaseArray->ulLen )
         {
            pItems = pBaseArray->pItems + pBaseArray->ulLen;
            ulPos  = ( ulLen <= ulAllocated ) ? (ulLen - pBaseArray->ulLen) : (ulAllocated - pBaseArray->ulLen);

            do
            {
               hb_itemSetNil( pItems ); /* don't use ++ here */
               pItems++;
            }
            while( --ulPos );
         }

         if( pBaseArray->ulBlock != 0 )
         {
            /* Uses specified allocation size pBaseArray->ulBlock
             */
            if( ulLen == 0 )
               ulAllocated = 0;
            else if( ulLen > ulAllocated )
               ulAllocated = ulLen + pBaseArray->ulBlock;
            else if( ulLen + pBaseArray->ulBlock * 2 < ulAllocated )
               ulAllocated = ulLen + pBaseArray->ulBlock;
         }
         else if( ulLen > ulAllocated )
         {
            /* Requires more space
             */
            if( ulLen < 100 )
               /* At least 10 more allocated items
                */
               ulAllocated = ulLen + 10;
            else
               /* At least 10 percent more allocated items
                */
               ulAllocated = ( ULONG ) ( ulLen * 1.1 );
         }
         else if( ulLen < ulAllocated )
         {
            if( ulLen == 0 )
               /*  No allocated items
                */
               ulAllocated = 0;
            else if( ulLen < ulAllocated / 2 && ulAllocated > 10 )
            {
               /* Resizes only if new size is less than a half of the allocated items
                */
               if( ulLen < 10 )
                  /* At least 10 allocated items
                   */
                  ulAllocated = 10;
               else
                  /* Reduces allocated items to new size
                   */
                  ulAllocated = ulLen;
            }
         }

         if( ulAllocated != pBaseArray->ulAllocated )
         {
            /* Buffer will be changed
             */
            if( ulAllocated == 0 )
            {
               /* Array is empty
                */
               hb_xfree( pBaseArray->pItems );
               pBaseArray->pItems = NULL;
            }
            else
            {
               /* Resizes array buffer
                */
               if( pBaseArray->pItems )
               {
                  /* Reallocates buffer
                   */
#ifndef HB_ARRAY_USE_COUNTER
                  PHB_ITEM pOldItems = pBaseArray->pItems;
#endif

                  pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulAllocated );

#ifndef HB_ARRAY_USE_COUNTER
                  if( pBaseArray->pItems != pOldItems )
                  {
                     pItems = pBaseArray->pItems;

                     for( ulPos = 0; ulPos < pBaseArray->ulLen; ulPos++ )
                     {
                        if( pItems->type == HB_IT_ARRAY && pItems->item.asArray.value )
                           hb_arrayResetHolder( pItems->item.asArray.value, ( void * ) ( pOldItems + ulPos ), ( void * ) pItems );
                        else if( pItems->type == HB_IT_BYREF && pItems->item.asRefer.offset == 0 )
                           hb_arrayResetHolder( pItems->item.asRefer.BasePtr.pBaseArray, ( void * ) ( pOldItems + ulPos ), ( void * ) pItems );

                        pItems++;
                     }
                  }
#endif
               }
               else
                  /* New buffer
                   */
                  pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( ulAllocated * sizeof( HB_ITEM ) );
            }

            /* Clears new allocated items
             */
            if( pBaseArray->ulAllocated < ulAllocated )
            {
               pItems = pBaseArray->pItems + pBaseArray->ulAllocated;
               ulPos  = ulAllocated - pBaseArray->ulAllocated;

               do
               {
                  ( pItems++ )->type = HB_IT_NIL;
               }
               while( --ulPos );
            }

            pBaseArray->ulAllocated = ulAllocated;
         }

         pBaseArray->ulLen = ulLen;
      }

      return TRUE;
   }

   return FALSE;
}

HB_SIZE hb_arrayLen( PHB_ITEM pArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayLen(%p)", pArray ) );

   return ( HB_IS_ARRAY( pArray ) ? pArray->item.asArray.value->ulLen : 0 );
}

BOOL hb_arrayIsObject( PHB_ITEM pArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayIsObject(%p)", pArray ) );

   return ( HB_IS_ARRAY( pArray ) ? ( pArray->item.asArray.value->uiClass != 0 ) : FALSE );
}

/* retrieves the array unique ID */
void * hb_arrayId( PHB_ITEM pArray )
{
   return ( HB_IS_ARRAY( pArray ) ? ( void * ) pArray->item.asArray.value : NULL );
}

BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayAdd(%p, %p)", pArray, pValue ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->ulLen < ULONG_MAX )
      {
         if( pBaseArray->ulAllocated > pBaseArray->ulLen )
            pBaseArray->ulLen++;
         else
            hb_arraySize( pArray, pBaseArray->ulLen + 1 );

         pBaseArray     = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         pValue->type   &= ~HB_IT_MEMOFLAG;
         hb_itemCopy( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );

         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_arrayAddForward( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayAddForward(%p, %p)", pArray, pValue ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->ulLen < ULONG_MAX )
      {
         if( pBaseArray->ulAllocated > pBaseArray->ulLen )
            pBaseArray->ulLen++;
         else
            hb_arraySize( pArray, pBaseArray->ulLen + 1 );

         pBaseArray     = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         pValue->type   &= ~HB_IT_MEMOFLAG;
         hb_itemForwardValue( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );

         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_arrayDel( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayDel(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      HB_SIZE ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex < ulLen )
      {
         PHB_ITEM pItems = pArray->item.asArray.value->pItems;

         for( ulIndex--; ulIndex < ulLen - 1; ulIndex++ )       /* move items */
            hb_itemForwardValue( pItems + ulIndex, pItems + ( ulIndex + 1 ) );

         return TRUE;
      }
      else if( ulIndex == ulLen )
      {
         hb_itemSetNil( pArray->item.asArray.value->pItems + ( ulLen - 1 ) );

         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_arrayIns( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayIns(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      HB_SIZE ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PHB_ITEM pItems = pArray->item.asArray.value->pItems;

         for( ulLen--; ulLen >= ulIndex; ulLen-- )          /* move items */
            hb_itemForwardValue( pItems + ulLen, pItems + ( ulLen - 1 ) );

         hb_itemSetNil( pItems + ulLen );
      }

      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySet( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySet(%p, %lu, %p)", pArray, ulIndex, pItem ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      PHB_ITEM pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

      if( HB_IS_BYREF( pElement ) )
         pElement = hb_itemUnRef( pElement );

      pItem->type &= ~HB_IT_MEMOFLAG;
      hb_itemCopy( pElement, pItem );

      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetForward( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )
{
   PHB_ITEM pElement;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetForward(%p, %lu, %p)", pArray, ulIndex, pItem ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

      if( HB_IS_BYREF( pElement ) )
         pElement = hb_itemUnRef( pElement );

      pItem->type &= ~HB_IT_MEMOFLAG;
      hb_itemForwardValue( pElement, pItem );

      return TRUE;
   }

   return FALSE;
}

BOOL hb_arrayGet( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGet(%p, %lu, %p) Base: %p Items: %p", pArray, ulIndex, pItem, pArray->item.asArray.value, pArray->item.asArray.value->pItems ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      PHB_ITEM pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

      if( HB_IS_BYREF( pElement ) )
         pElement = hb_itemUnRef( pElement );

      hb_itemCopy( pItem, pElement );
      return TRUE;
   }

   hb_itemSetNil( pItem );
   return FALSE;
}

BOOL hb_arrayGetForward( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetForward(%p, %lu, %p) Base: %p Items: %p", pArray, ulIndex, pItem, pArray->item.asArray.value, pArray->item.asArray.value->pItems ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      PHB_ITEM pElement = pArray->item.asArray.value->pItems + ( ulIndex - 1 );

      if( HB_IS_BYREF( pElement ) )
         pElement = hb_itemUnRef( pElement );

      hb_itemForwardValue( pItem, pElement );
      return TRUE;
   }

   hb_itemSetNil( pItem );
   return FALSE;
}

BOOL hb_arrayGetByRef( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetByRef(%p, %lu, %p) Base: %p Items: %p", pArray, ulIndex, pItem, pArray->item.asArray.value, pArray->item.asArray.value->pItems ) );

   if( HB_IS_COMPLEX( pItem ) )
      hb_itemClear( pItem );
   else
      pItem->type = HB_IT_NIL;

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
#ifdef HB_UNSHARE_REFERENCES
      hb_itemUnShare( pArray->item.asArray.value->pItems + ( ulIndex - 1 ) );
#endif

      if( pItem == pArray->item.asArray.value->pItems + ( ulIndex - 1 ) )
      {
         assert( 0 );
         hb_errInternal( HB_EI_ERRUNRECOV, "Cyclic Reference assignment.", NULL, NULL );
      }

      pItem->type                            = HB_IT_BYREF;
      pItem->item.asRefer.value              = ( LONG ) ( ulIndex - 1 );
      pItem->item.asRefer.offset             = 0;
      pItem->item.asRefer.BasePtr.pBaseArray = pArray->item.asArray.value;

#ifdef HB_ARRAY_USE_COUNTER
      HB_ATOMIC_INC( pArray->item.asArray.value->ulHolders );
#else
      hb_arrayRegisterHolder( pArray->item.asArray.value, ( void * ) pItem );
#endif

      return TRUE;
   }
   else if( pArray->type == HB_IT_STRING && ulIndex > 0 && ulIndex <= pArray->item.asString.length )
   {
      hb_itemPutCLStatic( pItem, hb_szAscii[ ( UCHAR ) ( pArray->item.asString.value[ ulIndex - 1 ] ) ], 1 );
      return TRUE;
   }

   return FALSE;
}

char * hb_arrayGetDS( PHB_ITEM pArray, HB_SIZE ulIndex, char * szDate )
{
   char * exData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetDS(%p, %lu, %s)", pArray, ulIndex, szDate ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      exData = hb_itemGetDS( pArray->item.asArray.value->pItems + ulIndex - 1, szDate );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDS(). [vszakats] */
      exData = hb_itemGetDS( NULL, szDate );

   return exData;
}

char * hb_arrayGetDTS( PHB_ITEM pArray, HB_SIZE ulIndex, char * szDateTime )
{
   char * exData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetDTS(%p, %lu, %s)", pArray, ulIndex, szDateTime ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      exData = hb_itemGetDTS( pArray->item.asArray.value->pItems + ulIndex - 1, szDateTime );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDTS(). */
      exData = hb_itemGetDTS( NULL, szDateTime );

   return exData;
}

long hb_arrayGetDL( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetDL(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetDL( pArray->item.asArray.value->pItems + ulIndex - 1 );

   /* NOTE: Intentionally calling it with a bad parameter in order to get
            the default value from hb_itemGetDL(). [vszakats] */
   return hb_itemGetDL( NULL );
}

long hb_arrayGetT( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetT(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetT( pArray->item.asArray.value->pItems + ulIndex - 1 );

   /* NOTE: Intentionally calling it with a bad parameter in order to get
            the default value from hb_itemGetT(). */
   return hb_itemGetT( NULL );
}

double hb_arrayGetDTsec( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetDTsec(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetDTsec( pArray->item.asArray.value->pItems + ulIndex - 1 );

   /* NOTE: Intentionally calling it with a bad parameter in order to get
            the default value from hb_itemGetDTsec().  */
   return hb_itemGetDTsec( NULL );
}

double hb_arrayGetDTD( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetDTD(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetDTD( pArray->item.asArray.value->pItems + ulIndex - 1 );

   /* NOTE: Intentionally calling it with a bad parameter in order to get
            the default value from hb_itemGetDTD(). */
   return hb_itemGetDTD( NULL );
}

/*
 * This function returns a pointer to an item occupied by the specified
 * array element - it doesn't return an item's value
 */

PHB_ITEM hb_arrayGetItemPtr( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetItemPtr(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return pArray->item.asArray.value->pItems + ulIndex - 1;

   return NULL;
}

BOOL hb_arrayGetL( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetL(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetL( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return FALSE;
}

int hb_arrayGetNI( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNI(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNI( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return 0;
}

long hb_arrayGetNL( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNL(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNL( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return 0;
}

#ifndef HB_LONG_LONG_OFF
LONGLONG hb_arrayGetNLL( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNLL(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNLL( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return 0;
}
#endif

HB_LONG hb_arrayGetNInt( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNInt(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNInt( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return 0;
}

double hb_arrayGetND( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetND(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetND( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return 0.0;
}

HB_SIZE hb_arrayCopyC( PHB_ITEM pArray, HB_SIZE ulIndex, char * szBuffer, HB_SIZE ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayCopyC(%p, %lu, %s, %lu)", pArray, ulIndex, szBuffer, ulLen ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen && ( pArray->item.asArray.value->pItems + ulIndex - 1 )->type == HB_IT_STRING )
      return hb_itemCopyC( pArray->item.asArray.value->pItems + ulIndex - 1, szBuffer, ulLen );

   return 0;
}

char * hb_arrayGetC( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetC(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen && ( pArray->item.asArray.value->pItems + ulIndex - 1 )->type == HB_IT_STRING )
      return hb_itemGetC( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return NULL;
}

char * hb_arrayGetCPtr( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetCPtr(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen && ( pArray->item.asArray.value->pItems + ulIndex - 1 )->type == HB_IT_STRING )
      return ( pArray->item.asArray.value->pItems + ulIndex - 1 )->item.asString.value;

   return NULL;
}

HB_SIZE hb_arrayGetCLen( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetCLen(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen && ( pArray->item.asArray.value->pItems + ulIndex - 1 )->type == HB_IT_STRING )
      return ( pArray->item.asArray.value->pItems + ulIndex - 1 )->item.asString.length;

   return 0;
}

void * hb_arrayGetPtr( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetPtr(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetPtr( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return NULL;
}

HB_TYPE hb_arrayGetType( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetType(%p, %lu)", pArray, ulIndex ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemType( pArray->item.asArray.value->pItems + ulIndex - 1 );

   return 0;
}

BOOL hb_arraySetDS( PHB_ITEM pArray, HB_SIZE ulIndex, char * szDate )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetDS(%p, %lu, %s)", pArray, ulIndex, szDate ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutDS( pArray->item.asArray.value->pItems + ulIndex - 1, szDate );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetDL( PHB_ITEM pArray, HB_SIZE ulIndex, long lDate )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetDL(%p, %lu, %ld)", pArray, ulIndex, lDate ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutDL( pArray->item.asArray.value->pItems + ulIndex - 1, lDate );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetTDT( PHB_ITEM pArray, HB_SIZE ulIndex, long lDate, long lMilliSec  )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetTDT(%p, %lu, %ld, %ld )", pArray, ulIndex, lDate, lMilliSec ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutTDT( pArray->item.asArray.value->pItems + ulIndex - 1, lDate, lMilliSec );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetL( PHB_ITEM pArray, HB_SIZE ulIndex, BOOL fValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetL(%p, %lu, %d)", pArray, ulIndex, fValue ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutL( pArray->item.asArray.value->pItems + ulIndex - 1, fValue );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetNI( PHB_ITEM pArray, HB_SIZE ulIndex, int iNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNI(%p, %lu, %d)", pArray, ulIndex, iNumber ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNI( pArray->item.asArray.value->pItems + ulIndex - 1, iNumber );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetNL( PHB_ITEM pArray, HB_SIZE ulIndex, long lNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNL(%p, %lu, %lu)", pArray, ulIndex, lNumber ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNL( pArray->item.asArray.value->pItems + ulIndex - 1, lNumber );
      return TRUE;
   }

   return FALSE;
}

#ifndef HB_LONG_LONG_OFF
BOOL hb_arraySetNLL( PHB_ITEM pArray, HB_SIZE ulIndex, LONGLONG llNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNLL(%p, %lu, %" PFLL "d)", pArray, ulIndex, llNumber ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNLL( pArray->item.asArray.value->pItems + ulIndex - 1, llNumber );
      return TRUE;
   }

   return FALSE;
}
#endif

BOOL hb_arraySetNInt( PHB_ITEM pArray, HB_SIZE ulIndex, HB_LONG lNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNInt(%p, %lu, %" PFHL "d)", pArray, ulIndex, lNumber ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNInt( pArray->item.asArray.value->pItems + ulIndex - 1, lNumber );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetND( PHB_ITEM pArray, HB_SIZE ulIndex, double dNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetND(%p, %lu, %lf)", pArray, ulIndex, dNumber ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutND( pArray->item.asArray.value->pItems + ulIndex - 1, dNumber );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetC( PHB_ITEM pArray, HB_SIZE ulIndex, const char * szText )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetC(%p, %lu, %p)", pArray, ulIndex, szText ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutC( pArray->item.asArray.value->pItems + ulIndex - 1, szText );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetCL( PHB_ITEM pArray, HB_SIZE ulIndex, const char * szText, HB_SIZE ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetC(%p, %lu, %p, %lu)", pArray, ulIndex, szText, ulLen ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutCL( pArray->item.asArray.value->pItems + ulIndex - 1, szText, ulLen );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetCPtr( PHB_ITEM pArray, HB_SIZE ulIndex, char * szText, HB_SIZE ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetCPtr(%p, %lu)", pArray, ulIndex, szText, ulLen ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutCPtr( pArray->item.asArray.value->pItems + ulIndex - 1, szText, ulLen );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetPtr( PHB_ITEM pArray, HB_SIZE ulIndex, void * pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetPtr(%p, %lu, %p)", pArray, ulIndex, pValue ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutPtr( pArray->item.asArray.value->pItems + ulIndex - 1, pValue );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arraySetPtrGC( PHB_ITEM pArray, HB_SIZE ulIndex, void * pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetPtrGC(%p, %lu, %p)", pArray, ulIndex, pValue ) );

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutPtrGC( pArray->item.asArray.value->pItems + ulIndex - 1, pValue );
      return TRUE;
   }

   return FALSE;
}

BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayLast(%p, %p)", pArray, pResult ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      if( pArray->item.asArray.value->ulLen > 0 )
         hb_itemCopy( pResult, pArray->item.asArray.value->pItems +
                      ( pArray->item.asArray.value->ulLen - 1 ) );
      else
         hb_itemSetNil( pResult );

      return TRUE;
   }

   hb_itemSetNil( pResult );
   return FALSE;
}

void hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE ulStart, HB_SIZE ulCount )
{
   PHB_ITEM pElement;
   HB_SIZE  ulLen = pArray->item.asArray.value->ulLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayFill(%p, %p, %i, %i)", pArray, pValue, ulStart, ulCount ) );

   if( ulStart <= ulLen )
   {
      PHB_ITEM pItems = pArray->item.asArray.value->pItems;

      if( ulStart + ulCount > ulLen + 1 )
         ulCount = ulLen - ulStart + 1;

      ulStart--;

      for(; ulCount > 0; ulCount--, ulStart++ )
      {
         pElement = pItems + ulStart;

         if( HB_IS_BYREF( pElement ) )
            /* hb_itemCopy( hb_itemUnRef( pElement ), pValue );
             */
            pElement = hb_itemUnRef( pElement );

         pValue->type &= ~HB_IT_MEMOFLAG;
         hb_itemCopy( pElement, pValue );
      }
   }
}

HB_SIZE hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE * pulStart, HB_SIZE * pulCount, BOOL bExact, BOOL bAllowChar )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayScan(%p, %p, %p, %p)", pArray, pValue, pulStart, pulCount, bExact ) );

   if( HB_IS_ARRAY( pArray ) || pArray->type == HB_IT_HASH )
   {
      HB_SIZE           ulLen;
      HB_SIZE           ulStart = 1;
      HB_SIZE           ulCount;
      register PHB_ITEM pItems;

      /* TODO: Create hb_hashScan()
       * Select array type
       */
      if( pArray->type == HB_IT_HASH )
      {
         pItems   = pArray->item.asHash.value->pValues;
         ulLen    = pArray->item.asHash.value->ulLen;
      }
      else
      {
         pItems   = pArray->item.asArray.value->pItems;
         ulLen    = pArray->item.asArray.value->ulLen;
      }

      /* sanitize scan range */
      if( pulStart && ( *pulStart >= 1 ) )
         ulStart = *pulStart;

      if( ulStart > ulLen )
         return 0;

      ulCount = ulLen - ulStart + 1;

      if( pulCount && ( *pulCount <= ulLen - ulStart ) )
         ulCount = *pulCount;

      if( ulStart + ulCount > ulLen )             /* check range */
         ulCount = ulLen - ulStart + 1;

      /* work with subhashes
       */
      if( pArray->type == HB_IT_HASH && pArray->item.asHash.value->uiLevel > 0 )
      {
         HB_SIZE           ulPos;
         register HB_SIZE  ulTotal = 0;

         /* skip first items
          */
         while( ulTotal + pItems->item.asHash.value->ulTotalLen < ulStart )
         {
            ulTotal += pItems->item.asHash.value->ulTotalLen;
            ++pItems;
         }

         ulStart  -= ulTotal;
         ulPos    = hb_arrayScan( pItems, pValue, &ulStart, &ulCount, bExact, bAllowChar );

         while( ulCount > pItems->item.asHash.value->ulTotalLen && ulPos == 0 )
         {
            ulPos = hb_arrayScan( pItems, pValue, NULL, &ulCount, bExact, bAllowChar );

            if( ulPos == 0 )
            {
               ulCount  -= pItems->item.asHash.value->ulTotalLen;
               ulTotal  += pItems->item.asHash.value->ulTotalLen;
               ++pItems;
            }
         }

         if( ulPos == 0 )
            ulPos = hb_arrayScan( pItems, pValue, NULL, &ulCount, bExact, bAllowChar );

         return ulPos + ulTotal;
      }

      /* Make separate search loops for different types to find, so that
         the loop can be faster. */

      if( HB_IS_BLOCK( pValue ) )
      {
         HB_SIZE ulParams = 2;

         if( HB_IS_HASH( pArray ) )
            ulParams = 3;

         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pValue );

            if( ulParams == 3 )
               hb_vmPush( pArray->item.asHash.value->pKeys + ulStart );

            hb_vmPush( pItems + ulStart );
            hb_vmPushSize( ulStart + 1 );
            hb_vmSend( ( USHORT ) ulParams );

            if( HB_IS_LOGICAL( &( HB_VM_STACK.Return ) ) && HB_VM_STACK.Return.item.asLogical.value )
               return ulStart + 1;                  /* arrays start from 1 */
         }
      }
      else if( HB_IS_STRING( pValue ) ) /* Must precede HB_IS_NUMERIC() */
      {
         if( ! bAllowChar || ! HB_IS_NUMERIC( pValue ) )
         {
            PHB_ITEM pItem;

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               pItem = pItems + ulStart;

               /* NOTE: The order of the pItem and pValue parameters passed to
                *       hb_itemStrCmp() is significant, please don't change it. [vszakats]
                */
               if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, bExact ) == 0 )
                  return ulStart + 1;
            }
         }
         else
         {
            double   dValue = hb_itemGetND( pValue );
            PHB_ITEM pItem;

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               pItem = pItems + ulStart;

               /* NOTE: The order of the pItem and pValue parameters passed to
                *       hb_itemStrCmp() is significant, please don't change it. [vszakats]
                */
               if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, bExact ) == 0 )
                  return ulStart + 1;
               else if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
                  return ulStart + 1;
            }
         }
      }
      else if( pValue->type == HB_IT_DATE ) /* Must precede HB_IS_NUMERIC() */
      {
         HB_ISIZ  lValue = pValue->item.asDate.value;
         PHB_ITEM pItem;

         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            pItem = pItems + ulStart;

            if( pItem->type == HB_IT_DATE && pItem->item.asDate.value == lValue &&
                pValue->item.asDate.time == pItem->item.asDate.time )
               return ulStart + 1;
         }
      }
      else if( HB_IS_NUMERIC( pValue ) )
      {
         double   dValue = hb_itemGetND( pValue );
         PHB_ITEM pItem;

         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            pItem = pItems + ulStart;

            HB_TRACE( HB_TR_INFO, ( "hb_arrayScan() %p, %d", pItem, dValue ) );

            if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue && ( bAllowChar || ! HB_IS_STRING( pItem ) ) )
               return ulStart + 1;
         }
      }
      else if( HB_IS_LOGICAL( pValue ) )
      {
         BOOL     bValue = hb_itemGetL( pValue ); /* NOTE: This is correct: Get the date as a LONG value. [vszakats] */
         PHB_ITEM pItem;

         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            pItem = pItems + ulStart;

            if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
               return ulStart + 1;
         }
      }
      else if( HB_IS_NIL( pValue ) )
      {
         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            if( HB_IS_NIL( pItems + ulStart ) )
               return ulStart + 1;
         }
      }
      else if( bExact && pValue->type == HB_IT_ARRAY )
      {
         PHB_ITEM pItem;

         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            pItem = pItems + ulStart;

            HB_TRACE( HB_TR_INFO, ( "hb_arrayScan() %p, %p", pItem, pValue->item.asArray.value ) );

            if( pItem->type == HB_IT_ARRAY && pItem->item.asArray.value == pValue->item.asArray.value )
               return ulStart + 1;
         }
      }
   }

   return 0;
}

BOOL hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, HB_SIZE * pulStart, HB_SIZE * pulCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayEval(%p, %p, %p, %p)", pArray, bBlock, pulStart, pulCount ) );

   if( HB_IS_BLOCK( bBlock ) )
   {
      HB_SIZE  ulLen;
      HB_SIZE  ulCount;
      HB_SIZE  ulStart = 1;

      if( pArray->type == HB_IT_ARRAY )
      {
         PHB_BASEARRAY  pBaseArray  = pArray->item.asArray.value;
         UINT           uiFlags     = pBaseArray->uiFlags;

         ulLen = pBaseArray->ulLen;

         if( pulStart && ( *pulStart >= 1 ) )
            ulStart = *pulStart;

         if( ulStart <= ulLen )
         {
            PHB_ITEM pItems = pBaseArray->pItems;

            ulCount = ulLen - ulStart + 1;

            if( pulCount && ( *pulCount <= ulLen - ulStart ) )
               ulCount = *pulCount;

            if( ulStart + ulCount > ulLen )             /* check range */
               ulCount = ulLen - ulStart + 1;

            pBaseArray->uiFlags |= 0xF000;

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( bBlock );
               hb_vmPush( pItems + ulStart );
               hb_vmPushSize( ulStart + 1 );

               hb_vmSend( 2 );
            }

            pBaseArray->uiFlags = uiFlags;

            if( pBaseArray->ulLen != ulLen )
            {
               HB_SIZE ulNew = pBaseArray->ulLen;

               pBaseArray->ulLen = ulLen;
               hb_arraySize( pArray, ulNew );
            }
         }
      }
      else if( pArray->type == HB_IT_HASH )
      {
         PHB_BASEHASH pBaseHash = pArray->item.asHash.value;

         ulLen = pBaseHash->ulTotalLen;

         if( pulStart && ( *pulStart >= 1 ) )
            ulStart = *pulStart;

         if( ulStart <= ulLen )
         {
            ulCount = ulLen - ulStart + 1;

            if( pulCount && ( *pulCount <= ulLen - ulStart ) )
               ulCount = *pulCount;

            if( ulStart + ulCount > ulLen )             /* check range */
               ulCount = ulLen - ulStart + 1;

            /* work with subhashes */
            if( pBaseHash->uiLevel > 0 )
            {
               register HB_SIZE  ulTotal  = 0;
               PHB_ITEM          pItems   = pBaseHash->pValues;

               /* skip first items
                */
               while( ulTotal + pItems->item.asHash.value->ulTotalLen < ulStart )
               {
                  ulTotal += pItems->item.asHash.value->ulTotalLen;
                  ++pItems;
               }

               ulStart -= ulTotal;

               while( ulCount > pItems->item.asHash.value->ulTotalLen )
               {
                  hb_arrayEval( pItems, bBlock, &ulStart, &ulCount );
                  ulCount  -= pItems->item.asHash.value->ulTotalLen;
                  ulStart  = 1;
                  ++pItems;
               }

               hb_arrayEval( pItems, bBlock, NULL, &ulCount );
               return TRUE;
            }
            else
            {
               for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
               {
                  hb_vmPushSymbol( &hb_symEval );
                  hb_vmPush( bBlock );
                  hb_vmPush( pBaseHash->pKeys + ulStart );
                  hb_vmPush( pBaseHash->pValues + ulStart );
                  hb_vmPushSize( ulStart + 1 );

                  hb_vmSend( 3 );
               }
            }
         }
      }
      else
         return FALSE;

      return TRUE;
   }

   return FALSE;
}

void hb_arrayReleaseBase( PHB_BASEARRAY pBaseArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "pBaseArray %p", pBaseArray ) );

   /* TraceLog( NULL, "Releasing Basearray %p\n", pBaseArray );
    */

   /* Called recursively from hb_arrayReleaseGarbage!
    */
   if( ( pBaseArray->uiFlags & ( 1 | 2 ) ) != 0 )
      return;

   pBaseArray->uiFlags |= 1;  /* First step of Release, avoid second call to destructor, first call from hb_arrayReleaseGarbage() */

   if( pBaseArray->uiClass )
   {
      HB_ITEM FakedObject;

      FakedObject.type                 = HB_IT_ARRAY;
      FakedObject.item.asArray.value   = pBaseArray;

      /* To avoid DOUBLE freeing - when poped off in hb_clsFinalize()
       */
#ifdef HB_ARRAY_USE_COUNTER
      FakedObject.item.asArray.value->ulHolders = LONG_MAX;    /* don't use ULONG_MAX!!! */
#else
      hb_arrayRegisterHolder( pBaseArray, ( void * ) &FakedObject );
#endif

      hb_clsFinalize( &FakedObject );

#ifdef HB_ARRAY_USE_COUNTER
      FakedObject.item.asArray.value->ulHolders = 0;
#else
      /* Avoid infinite recursion - we know this is the only pOwner
       * hb_arrayReleaseHolder( pBaseArray, (void *) &FakedObject );
       */
      hb_xfree( ( void * ) pBaseArray->pOwners );
      pBaseArray->pOwners = NULL;
#endif
   }

   pBaseArray->uiFlags |= 2;  /* Second step of Release, avoid call to methods of this object from others destructor in the same GC recollection session. */

   if( pBaseArray->pItems )
   {
      register HB_SIZE  ulLen    = pBaseArray->ulLen;
      PHB_ITEM       pItems   = pBaseArray->pItems;
      PHB_ITEM       pItem    = pItems; /* pValue; */

      /* TraceLog( NULL, "Releasing BaseArray %p\n", pBaseArray );
       */

      while( ulLen-- )
      {
         HB_TRACE( HB_TR_INFO, ( "Array Item %p type:%i", pItem, pItem->type ) );

         /* TraceLog( NULL, "Releasing Element: %i %p Class: %s\n", pBaseArray->ulLen - ulLen, pItem, hb_objGetClsName( pItem ) );
          */

         /* printf( "Array Item %i %p type:%i\n", ulLen, pItem, pItem->type );
          */

         /* The Reference should NOT be cleared when an Array with element refering it is released!
          * pValue = pItem; // Subsequent pItem below were pValue, other then pItem++!!!
          * if( HB_IS_BYREF( pValue ) )
          *    pValue = hb_itemUnRef( pValue );
          */

         if( HB_IS_COMPLEX( pItem ) )
         {
            /*------------------12/21/2001 8:01PM----------------
             * The item is not released because it was not
             * allocated by the GC, its just a portion of the
             * pItems chunk, which will be released as one piece.
             * --------------------------------------------------*/
            if( pItem->type == HB_IT_ARRAY && pItem->item.asArray.value == pBaseArray )
               HB_TRACE( HB_TR_DEBUG, ( "Warning! Nested Release (Cyclic) %p %p", pItem, pItem->item.asArray.value ) );
               /* TraceLog( NULL, "Warning! Nested Release (Cyclic) %p %p\n", pItem, pItem->item.asArray.value );
                */
            else
               /* TraceLog( NULL, "Releasing Element: %i %p Type: %i\n", pBaseArray->ulLen - ulLen, pItem, pItem->type );
                */
               hb_itemClear( pItem );
               /* TraceLog( NULL, "DONE Releasing Element: %i %p\n", pBaseArray->ulLen - ulLen, pItem );
                */
         }

         ++pItem;
      }

      HB_TRACE( HB_TR_INFO, ( "Release pItems %p", pItems ) );
      hb_xfree( pItems );

      pBaseArray->pItems   = NULL;
      pBaseArray->ulLen    = 4070707070U; /* Intentionally - debugging flag indicating array was explictly released. */
   }

   HB_TRACE( HB_TR_INFO, ( "Release pBaseArray %p", pBaseArray ) );
   hb_gcFree( ( void * ) pBaseArray );

   /* TraceLog( NULL, "DONE Releasing Basearray %p\n", pBaseArray );
    */
}

BOOL hb_arrayRelease( PHB_ITEM pArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayRelease(%p) %p", pArray, pArray->item.asArray.value ) );

   /* printf( "hb_arrayRelease(%p) type: %i %p\n", pArray, pArray->type, pArray->item.asArray.value );
    */

   if( pArray->type == HB_IT_ARRAY )
   {
#ifdef HB_ARRAY_USE_COUNTER
      hb_arrayReleaseBase( pArray->item.asArray.value );
#else
      if( pArray->item.asArray.value && pArray->item.asArray.value->pOwners )
      {
         PHB_ARRAY_HOLDER pOwners = pArray->item.asArray.value->pOwners;

         while( pOwners )
         {
            /* char szProc[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ], szModule[ HB_PATH_MAX ];
             * USHORT uiLine;
             */
            if( pOwners->pOwner != ( void * ) pArray )
            {
               /*
                * hb_procinfo( 0, szProc, &uiLine, szModule  );
                * TraceLog( NULL, "Warning! (1) Residual owner %p of array %p [%s->%s(%i)]\n",
                *                 pOwners, pArray->item.asArray.value,
                *                 szModule, szProc, uiLine );
                */
            }

            pOwners = pOwners->pNext;
         }

         /* TraceLog( NULL, "Diverting to ReleaseHolder() - Class '%s'\n", hb_objGetClsName( pArray ) );
          */
         hb_arrayReleaseHolder( pArray->item.asArray.value, ( void * ) pArray );
      }
#endif

      pArray->type               = HB_IT_NIL;
      pArray->item.asArray.value = NULL;

      /* printf( "\nDone! hb_arrayRelease(%p) %p", pArray, pArray->item.asArray.value );
       */
      return TRUE;
   }
   else
      /* char szProc[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ], szModule[ HB_PATH_MAX ];
       * USHORT uiLine;
       *
       * hb_procinfo( 0, szProc, &uiLine, szModule  );
       * TraceLog( NULL, "Warning! not an array %p [%s->%s(%i)]\n", pArray, szModule, szProc, uiLine );
       */
      return FALSE;
}

/* NOTE: CA-Cl*pper 5.3a has a fix for the case when the starting position
         is greater than the length of the array. [vszakats] */
BOOL hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, HB_SIZE * pulStart,
                   HB_SIZE * pulCount, HB_SIZE * pulTarget )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayCopy(%p, %p, %p, %p, %p)", pSrcArray, pDstArray, pulStart, pulCount, pulTarget ) );

   if( pSrcArray->type == HB_IT_ARRAY && pDstArray->type == HB_IT_ARRAY )
   {
      HB_SIZE  ulSrcLen = pSrcArray->item.asArray.value->ulLen;
      HB_SIZE  ulDstLen = pDstArray->item.asArray.value->ulLen;
      HB_SIZE  ulStart  = 1;
      HB_SIZE  ulTarget = 1;
      HB_SIZE  ulCount;

      if( pulStart && ( *pulStart >= 1 ) )
         ulStart = *pulStart;

      if( pulTarget && ( *pulTarget >= 1 ) )
         ulTarget = *pulTarget;

#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
      if( ulStart <= ulSrcLen )
#else
      if( ulSrcLen > 0 )
#endif
      {
#ifndef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
         if( ulStart > ulSrcLen )
            ulStart = ulSrcLen;
#endif
         ulCount = ulSrcLen - ulStart + 1;

         if( pulCount && ( *pulCount <= ulSrcLen - ulStart ) )
            ulCount = *pulCount;

/* This is probably a bug, present in all versions of CA-Cl*pper. */
#ifdef HB_FIX_ACOPY_BUG
         if( ulTarget <= ulDstLen )
         {
#else
         if( ulDstLen > 0 )
         {
            PHB_ITEM pDstItems = pDstArray->item.asArray.value->pItems, pSrcItems = pSrcArray->item.asArray.value->pItems;

            if( ulTarget > ulDstLen )
               ulTarget = ulDstLen;
#endif
            if( ulCount > ulDstLen - ulTarget )
               ulCount = ulDstLen - ulTarget + 1;

            for( ulTarget--, ulStart--; ulCount > 0; ulCount--, ulStart++, ulTarget++ )
               hb_itemCopy( pDstItems + ulTarget, pSrcItems + ulStart );
         }
      }

      return TRUE;
   }

   return FALSE;
}


PHB_ITEM hb_arrayClone2( PHB_ITEM pSrcArray, PHB_NESTED_CLONED pClonedList )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayClone2(%p, %p)", pSrcArray, pClonedList ) );

   return hb_arrayCloneEx( pSrcArray, hb_itemNew( NULL ), pClonedList );
}

PHB_ITEM hb_arrayCloneEx( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, PHB_NESTED_CLONED pClonedList )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayCloneEx(%p, %p, %p)", pSrcArray, pDstArray, pClonedList ) );

   if( pSrcArray->type == HB_IT_ARRAY )
   {
      PHB_BASEARRAY     pSrcBaseArray  = pSrcArray->item.asArray.value;
      PHB_BASEARRAY     pDstBaseArray;
      HB_SIZE           ulSrcLen       = pSrcBaseArray->ulLen;
      register HB_SIZE  ulCount;
      PHB_ITEM          pSrcItem;
      PHB_NESTED_CLONED pCloned;
      BOOL              bTop;
      PHB_ITEM          pSrcItems = pSrcBaseArray->pItems;

      hb_arrayNew( pDstArray, ulSrcLen );

      if( pClonedList == NULL )
      {
         bTop        = TRUE;
         pClonedList = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );
         pCloned     = pClonedList;
      }
      else
      {
         bTop     = FALSE;
         pCloned  = pClonedList;

         while( pCloned->pNext )
         {
            pCloned = pCloned->pNext;
         }
         pCloned->pNext = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );
         pCloned        = pCloned->pNext;
      }

      pCloned->pSrcBaseArray  = pSrcBaseArray;
      pCloned->pDest          = pDstArray;
      pCloned->pNext          = NULL;

      pDstBaseArray           = pDstArray->item.asArray.value;
      pDstBaseArray->uiClass  = pSrcBaseArray->uiClass;

      for( ulCount = 0; ulCount < ulSrcLen; ulCount++ )
      {
         pSrcItem = pSrcItems + ulCount;

         /* Clipper clones nested array ONLY if NOT an Object!!! */
         if( pSrcItem->type == HB_IT_ARRAY && pSrcItem->item.asArray.value->uiClass == 0 )
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

            DontClone:
            hb_arraySet( pDstArray, ulCount + 1, pClone );
         }
         else
            hb_arraySet( pDstArray, ulCount + 1, pSrcItem );
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
   }
   return pDstArray;
}

PHB_ITEM hb_arrayClone( PHB_ITEM pSrcArray, PHB_NESTED_CLONED pClonedList )
{
   if( pSrcArray->type == HB_IT_ARRAY && pSrcArray->item.asArray.value->uiClass == 0 )
      return hb_arrayClone2( pSrcArray, pClonedList );

   return hb_objClone( pSrcArray );
}

PHB_ITEM hb_arrayFromStack( USHORT uiLen )
{
   PHB_ITEM          pArray = hb_itemNew( NULL );
   register USHORT   uiPos;

   /* printf( "Got: %p\n", pBaseArray );
    */

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayFromStack(%iu)", uiLen ) );

   hb_arrayNew( pArray, uiLen );

   for( uiPos = 1; uiPos <= uiLen; uiPos++ )
      hb_arraySet( pArray, uiPos, hb_stackItemFromTop( uiPos - uiLen - 1 ) );

   return pArray;
}

PHB_ITEM hb_arrayFromParams( PHB_ITEM * pBase )
{
   PHB_ITEM pArray = NULL;

   if( pBase && HB_IS_SYMBOL( *pBase ) )
   {
      USHORT   uiPos;
      USHORT   uiPCount;
      USHORT   uiOffset;
      PHB_ITEM pBaseItem = *pBase;

      pArray = hb_itemNew( NULL );

      HB_TRACE( HB_TR_DEBUG, ( "hb_arrayFromParams(%p)", pBase ) );

      uiPCount = pBaseItem->item.asSymbol.pCargo->arguments;

      if( pBaseItem->item.asSymbol.pCargo->params == HB_VAR_PARAM_FLAG )
         uiOffset = pBaseItem->item.asSymbol.pCargo->locals;
      else
         uiOffset = 0;

      hb_arrayNew( pArray, uiPCount );

      for( uiPos = 1; uiPos <= uiPCount; uiPos++ )
         hb_arraySet( pArray, uiPos, *( pBase + 1 + uiPos + uiOffset ) );
   }
   else
      hb_errInternal( HB_EI_ERRUNRECOV, "Invalid argument to hb_arrayFromParams().", NULL, NULL );

   return pArray;
}

PHB_ITEM hb_arrayBaseParams( void )
{
   return hb_arrayFromParams( hb_stackGetBase( 0 ) );
}

PHB_ITEM hb_arraySelfParams( void )
{
   PHB_ITEM          pArray;
   register USHORT   uiPos;
   register USHORT   uiPCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySelfParams()" ) );

   pArray   = hb_itemNew( NULL );
   uiPCount = hb_stackBaseItem()->item.asSymbol.pCargo->arguments;

   hb_arrayNew( pArray, uiPCount + 1 );

   for( uiPos = 0; uiPos <= uiPCount; uiPos++ )
      hb_arraySet( pArray, uiPos + 1, hb_stackItemFromBase( uiPos ) );

   return pArray;
}

/* This releases array when called from the garbage collector */
HB_GARBAGE_FUNC( hb_arrayReleaseGarbage )
{
   PHB_BASEARRAY     pBaseArray = ( PHB_BASEARRAY ) Cargo;

#ifndef HB_ARRAY_USE_COUNTER
   PHB_ARRAY_HOLDER  pOwners, pFree;
#endif

   HB_TRACE( HB_TR_INFO, ( "hb_arrayReleaseGarbage( %p )", pBaseArray ) );

   /* Can be called from GC post hb_arrayReleaseBase() execution.
    */
   if( ( pBaseArray->uiFlags & ( 1 | 2 ) ) != 0 )
      return;

   pBaseArray->uiFlags |= 1;  /* First step of Release, avoid second call to destructor, first call from hb_arrayReleaseBase() */

   if( pBaseArray->uiClass )
   {
      HB_ITEM FakedObject;

      FakedObject.type                 = HB_IT_ARRAY;
      FakedObject.item.asArray.value   = pBaseArray;

      hb_clsFinalize( &FakedObject );
   }

   pBaseArray->uiFlags |= 2;  /* Second step of Release, avoid call to methods of this object from others destructor in the same GC recollection session. */

   /* TraceLog( NULL, "hb_arrayReleaseGarbage( %p )\n", pBaseArray );
    */

   if( pBaseArray->pItems )
   {
      register HB_SIZE  ulLen = pBaseArray->ulLen;
      PHB_ITEM          pItem = pBaseArray->pItems;

      while( ulLen-- )
      {
         HB_TRACE( HB_TR_INFO, ( "Array Item %p type:%i", pItem, pItem->type ) );

         /* All other complex types will be released directly by the GC.
          */
         if( pItem->type == HB_IT_STRING )
            hb_itemReleaseString( pItem );

         ++pItem;
      }

      HB_TRACE( HB_TR_INFO, ( "Release pItems %p", pBaseArray->pItems ) );

      hb_xfree( pBaseArray->pItems );
      pBaseArray->pItems   = NULL;
      pBaseArray->ulLen    = 4171717171U; /* Intentionally - debugging flag indicating array was released by GC. */
   }

   /* Has to be AFTER the array elements have been released!
    */
#ifndef HB_ARRAY_USE_COUNTER
   pOwners = pBaseArray->pOwners;

   if( hb_gc_bReleaseAll == FALSE )
   {
      while( pOwners )
      {
         if( ( ( PHB_ITEM ) ( pOwners->pOwner ) )->type == HB_IT_ARRAY )
         {
            /* TraceLog( NULL, "Warning! (2) Residual owner %p of array %p\n", pOwners->pOwner, pBaseArray );
             */

            if( ( ( PHB_ITEM ) ( pOwners->pOwner ) )->item.asArray.value == pBaseArray || ( ( PHB_ITEM ) ( pOwners->pOwner ) )->item.asArray.value == NULL )
               /* Forcing reset of the orphan refernce or else a GPF will folow when that item will be passed to hb_itemClear().
                */
               ( ( PHB_ITEM ) ( pOwners->pOwner ) )->type = HB_IT_NIL;
            else
            {
#ifdef DEBUG_OWNERS
               TraceLog( NULL, "Warning! (4) Invalid Residual owner %p of array %p\n", pOwners->pOwner, pBaseArray );
#endif
            }
         }
         else if( ( ( PHB_ITEM ) ( pOwners->pOwner ) )->type == HB_IT_BYREF &&
                  ( ( PHB_ITEM ) ( pOwners->pOwner ) )->item.asRefer.offset == 0 )
         {
            if( ( ( PHB_ITEM ) ( pOwners->pOwner ) )->item.asRefer.BasePtr.pBaseArray == pBaseArray || ( ( PHB_ITEM ) ( pOwners->pOwner ) )->item.asRefer.BasePtr.pBaseArray == NULL )
               /* Forcing reset of the orphan refernce or else a GPF will folow when that item will be passed to hb_itemClear().
                */
               ( ( PHB_ITEM ) ( pOwners->pOwner ) )->type = HB_IT_NIL;
            else
            {
#ifdef DEBUG_OWNERS
               TraceLog( NULL, "Warning! (4-2) Invalid Residual owner %p of array %p\n", pOwners->pOwner, pBaseArray );
#endif
            }

         }
         else if( HB_IS_NIL( ( PHB_ITEM ) ( pOwners->pOwner ) ) )
         {
         }
         else
#ifdef DEBUG_OWNERS
            TraceLog( NULL, "Warning! (5) Invalid Residual owner %p type: %i\n", pOwners->pOwner, ( ( PHB_ITEM ) ( pOwners->pOwner ) )->type );
#endif
         pFree    = pOwners;
         pOwners  = pOwners->pNext;

         hb_xfree( pFree );
      }
   }

   pBaseArray->pOwners = NULL;
#endif

   /* TraceLog( NULL, "DONE hb_arrayReleaseGarbage( %p )\n", pBaseArray );
    */
}

#ifndef HB_ARRAY_USE_COUNTER
void hb_arrayRegisterHolder( PHB_BASEARRAY pBaseArray, void * pHolder )
{
   PHB_ARRAY_HOLDER pOwner = ( PHB_ARRAY_HOLDER ) hb_xgrab( sizeof( HB_ARRAY_HOLDER ) );

   if( pBaseArray == NULL )
      hb_errInternal( HB_EI_ERRUNRECOV, "Invalid base array passed to hb_arrayRegisterHolder().", NULL, NULL );

#ifdef DEBUG_ARRAYS
   TraceLog( NULL, "Allocated %p Registring: %p of %p Next %p\n", pOwner, pBaseArray, pHolder, pBaseArray->pOwners );
#endif

   pOwner->pOwner       = pHolder;
   pOwner->pNext        = pBaseArray->pOwners;

   pBaseArray->pOwners  = pOwner;
}

void hb_arrayResetHolder( PHB_BASEARRAY pBaseArray, void * pOldHolder, void * pNewHolder )
{
   PHB_ARRAY_HOLDER pOwners = pBaseArray->pOwners;

   if( pBaseArray == NULL )
      hb_errInternal( HB_EI_ERRUNRECOV, "Invalid base array passed to hb_arrayResetHolder().", NULL, NULL );

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
#ifdef DEBUG_OWNERS
      char     szProc[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ], szModule[ HB_PATH_MAX ];
      USHORT   uiLine;

      hb_procinfo( 0, szProc, &uiLine, szModule  );
      TraceLog( NULL, "Warning! Could not locate old owner %p of array %p [%s->%s(%i)] Stack: %p\n",
                pOldHolder, pBaseArray, szModule, szProc, uiLine, hb_stackItemFromTop( 1 ) );
#endif
   }

   hb_arrayRegisterHolder( pBaseArray, pNewHolder );
}

void hb_arrayReleaseHolder( PHB_BASEARRAY pBaseArray, void * pHolder )
{
   PHB_ARRAY_HOLDER pOwners = pBaseArray->pOwners, pPrevious = NULL;

   /* TraceLog( NULL, "hb_arrayReleaseHolder( %p, %p )\n", pBaseArray, pHolder );
    */

   if( pBaseArray == NULL )
      hb_errInternal( HB_EI_ERRUNRECOV, "Invalid base array passed to hb_arrayReleaseHolder().", NULL, NULL );

#ifdef DEBUG_ARRAYS
   TraceLog( NULL, "-UNRegistering: %p of %p\n", pBaseArray, pHolder );
#endif

   while( pOwners )
   {
      if( pOwners->pOwner == pHolder )
      {
         if( pPrevious )
            pPrevious->pNext = pOwners->pNext;
         else
            pBaseArray->pOwners = pOwners->pNext;

#ifdef DEBUG_ARRAYS
         TraceLog( NULL, "Released pOwners: %p\n", pOwners );
#endif

         hb_xfree( ( void * ) pOwners );
         break;
      }

      pPrevious   = pOwners;
      pOwners     = pOwners->pNext;
   }

   if( pOwners )
   {
      if( pBaseArray->pOwners == NULL )
      {
#ifdef DEBUG_ARRAYS
         TraceLog( NULL, "Last Owner - Releasing array %p\n", pBaseArray );
#endif
         /* TraceLog( NULL, "Last Owner - Releasing array %p\n", pBaseArray );
          */
         hb_arrayReleaseBase( pBaseArray );
      }
   }
   else
   {
#ifdef DEBUG_OWNERS
      char     szProc[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ], szModule[ HB_PATH_MAX ];
      USHORT   uiLine;

      hb_procinfo( 0, szProc, &uiLine, szModule  );
      TraceLog( NULL, "Warning! Could not locate owner %p of array %p [%s->%s(%i)]\n", pHolder, pBaseArray, szModule, szProc, uiLine );
#endif
   }

   /* TraceLog( NULL, "DONE hb_arrayReleaseHolder( %p, %p )\n", pBaseArray, pHolder );
    */
}

void hb_arrayReleaseHolderGarbage( PHB_BASEARRAY pBaseArray, void * pHolder )
{
   PHB_ARRAY_HOLDER pOwners = pBaseArray->pOwners, pPrevious = NULL;

   /* TraceLog( NULL, "hb_arrayReleaseHolderGarbage( %p, %p )\n", pBaseArray, pHolder );
    */

   if( pBaseArray == NULL )
      hb_errInternal( HB_EI_ERRUNRECOV, "Invalid base array passed to hb_arrayReleaseHolderGarbage().", NULL, NULL );

#ifdef DEBUG_ARRAYS
   TraceLog( NULL, "-UNRegistering: %p of %p\n", pBaseArray, pHolder );
#endif

   while( pOwners )
   {
      if( pOwners->pOwner == pHolder )
      {
         if( pPrevious )
            pPrevious->pNext = pOwners->pNext;
         else
            pBaseArray->pOwners = pOwners->pNext;

#ifdef DEBUG_ARRAYS
         TraceLog( NULL, "Released pOwners: %p\n", pOwners );
#endif

         hb_xfree( ( void * ) pOwners );
         break;
      }

      pPrevious   = pOwners;
      pOwners     = pOwners->pNext;
   }

   if( pOwners )
   {
      /*
       * We do not want to release it directly, it will be released by the GC shortly.
       *
       *  if( pBaseArray->pOwners == NULL )
       *  {

       *  #ifdef DEBUG_ARRAYS
       *     TraceLog( NULL, "Last Owner - Releasing array %p\n", pBaseArray );
       *  #endif
       *
       *  TraceLog( NULL, "Last Owner - Releasing array %p\n", pBaseArray );
       *
       *  hb_arrayReleaseBase( pBaseArray );
       *  }
       */
   }
   else
   {
#ifdef DEBUG_OWNERS
      char     szProc[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ], szModule[ HB_PATH_MAX ];
      USHORT   uiLine;

      hb_procinfo( 0, szProc, &uiLine, szModule  );
      TraceLog( NULL, "Warning! Could not locate owner %p of garbage array %p [%s->%s(%i)]\n", pHolder, pBaseArray, szModule, szProc, uiLine );
#endif
   }

   /* TraceLog( NULL, "DONE hb_arrayReleaseHolder( %p, %p )\n", pBaseArray, pHolder );
    */
}
#endif

int hb_arrayMode( void )
{
#ifdef HB_ARRAY_USE_COUNTER
   return 0;
#else
   return 1;
#endif
}

HB_ISIZ hb_arrayGetNS( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNS(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNS( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

BOOL hb_arraySetNS( PHB_ITEM pArray, HB_SIZE nIndex, HB_ISIZ nNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNS(%p, %" HB_PFS "u, %" HB_PFS "d)", pArray, nIndex, nNumber ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNS( pArray->item.asArray.value->pItems + nIndex - 1, nNumber );
      return TRUE;
   }
   else
      return FALSE;
}
