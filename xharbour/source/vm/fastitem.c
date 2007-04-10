/*
 * $Id: fastitem.c,v 1.104 2007/04/08 07:20:57 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * The FastItem Optimization API
 *
 * Copyright 2001 Ron Pinkas <ron@@ronpinkas.com>
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 * hb_itemClear() and hb_itemCopy() are derivative work of original code
 * in the Harbour Project http://harbour-project.org (source/vm/itemapi.c)
 * Copyright of Antonio Linares <alinares@fivetech.com>
 *
 */

#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"
#include "hashapi.h"

extern char *hb_vm_sNull;

HB_EXTERN_BEGIN

/* Forward decalarations. */
void hb_itemForwardValue( PHB_ITEM pDest, PHB_ITEM pSource );

void HB_EXPORT hb_itemPushForward( PHB_ITEM pItem )
{
   HB_THREAD_STUB_STACK

   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemPushForward(%p)", pItem ) );

   hb_itemForwardValue( hb_stackTopItem(), pItem );
   hb_stackPush();
}

void HB_EXPORT hb_itemForwardValue( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemForwardValue(%p, %p) %i", pDest, pSource, pDest->type ) );

   if( pDest == pSource )
   {
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemForwardValue()", NULL );
   }

   if( HB_IS_COMPLEX( pDest ) )
   {
      hb_itemClear( pDest );
   }

   /* Forward. */
   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   #ifndef HB_ARRAY_USE_COUNTER
      if( pSource->type == HB_IT_ARRAY && pSource->item.asArray.value )
      {
         hb_arrayResetHolder( pSource->item.asArray.value, (void *) pSource, (void *) pDest );
      }
      else if( pSource->type == HB_IT_BYREF && ( pSource )->item.asRefer.offset == 0 )
      {
         hb_arrayResetHolder( pSource->item.asRefer.BasePtr.pBaseArray, (void *) pSource, (void *) pDest );
      }
   #endif

   /* Now fake clear the transferer. */
   pSource->type = HB_IT_NIL;
}

PHB_ITEM HB_EXPORT hb_itemReturn( PHB_ITEM pItem )
{
   HB_THREAD_STUB_STACK

   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemReturn(%p)", pItem ) );

   if( pItem )
   {
      hb_itemCopy( hb_stackReturnItem(), pItem );
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemReturnForward( PHB_ITEM pItem )
{
   HB_THREAD_STUB_STACK

   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemReturnForward(%p)", pItem ) );

   if( pItem )
   {
      hb_itemForwardValue( hb_stackReturnItem(), pItem );
   }

   return pItem;
}

void HB_EXPORT hb_itemReleaseString( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemReleaseString(%p), '%s'", pItem, pItem->item.asString.value ) );

   if( pItem->item.asString.allocated )
   {
      if( *( pItem->item.asString.pulHolders ) == 0 )
      {
         hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature String Release detected: '%s'", pItem->item.asString.value, NULL );
      }

      if( --*( pItem->item.asString.pulHolders ) == 0  )
      {
         HB_TRACE_STEALTH( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.pulHolders ) );
         hb_xfree( pItem->item.asString.pulHolders );
         //pItem->item.asString.pulHolders = NULL;

         HB_TRACE_STEALTH( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.value ) );
         hb_xfree( pItem->item.asString.value );
         //pItem->item.asString.value = NULL;
         //pItem->item.asString.allocated = 0;
      }
   }
}

void HB_EXPORT hb_itemClear( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemClear(%p) type: %i", pItem, pItem ? pItem->type : 0 ) );

#if defined( HB_FM_STATISTICS ) && defined( HB_PARANOID_MEM_CHECK )
   if( HB_IS_BADITEM( pItem ) )
      hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_itemClear()", NULL );
#endif

   if( pItem->type & HB_IT_STRING )
   {
      hb_itemReleaseString( pItem );
      pItem->type = HB_IT_NIL;
      return;
   }
   else if( pItem->type & HB_IT_MEMVAR )
   {
      hb_memvarValueDecRef( pItem->item.asMemvar.value );
      pItem->type = HB_IT_NIL;
      return;
   }

   switch( pItem->type )
   {
      case HB_IT_ARRAY :
      {
         #ifdef HB_ARRAY_USE_COUNTER
           if( pItem->item.asArray.value->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
           {
              hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature Array/Object Release detected %p", (char *) ( pItem->item.asArray.value ), NULL );
           }

           if( --( pItem->item.asArray.value->ulHolders ) == 0 )
           {
              hb_arrayRelease( pItem );
           }
         #else
           hb_arrayReleaseHolder( pItem->item.asArray.value, (void *) pItem );
         #endif

         break;
      }

      case HB_IT_POINTER:
      {
         if ( pItem->item.asPointer.collect )
         {
            hb_gcDecRef( (void *) pItem->item.asPointer.value );
         }
         break;
      }

      case HB_IT_BLOCK :
      {
         hb_codeblockDelete( pItem );

         break;
      }

      case HB_IT_BYREF :
      {
         if( pItem->item.asRefer.offset == 0 )
         {
            HB_ITEM FakeArray;

            //TraceLog( NULL, "BYREF Faked %p\n", pItem->item.asRefer.BasePtr.pBaseArray );

            FakeArray.type = HB_IT_ARRAY;
            FakeArray.item.asArray.value = pItem->item.asRefer.BasePtr.pBaseArray;

            #ifdef HB_ARRAY_USE_COUNTER
               if( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
               {
                  hb_errInternal( HB_EI_PREMATURE_RELEASE, "BYREF Premature Array/Object Release detected", NULL, NULL );
               }

               if( --( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders ) == 0 )
               {
                  hb_arrayRelease( &FakeArray );
               }
            #else
               hb_arrayResetHolder( pItem->item.asRefer.BasePtr.pBaseArray, (void *) pItem, &FakeArray );
               hb_arrayReleaseHolder( pItem->item.asRefer.BasePtr.pBaseArray, (void *) &FakeArray );
            #endif
         }

         break;
      }

      case HB_IT_HASH :
      {
         if( --( pItem->item.asHash.value->ulHolders ) == 0 )
         {
            hb_hashRelease( pItem );
         }

         break;
      }

   }

   pItem->type = HB_IT_NIL;
}

#ifdef HB_THREAD_SUPPORT

void HB_EXPORT hb_itemClearMT( PHB_ITEM pItem, HB_STACK *pStack )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemClearMT(%p) type: %i", pItem, pItem->type ) );

   if( pItem->type & HB_IT_STRING )
   {
      hb_itemReleaseString( pItem );

      pItem->type = HB_IT_NIL;

      return;
   }
   else if( pItem->type & HB_IT_MEMVAR )
   {
      hb_memvarValueDecRefMT( pItem->item.asMemvar.value, pStack );

      pItem->type = HB_IT_NIL;

      return;
   }

   switch( pItem->type )
   {
      case HB_IT_ARRAY :
      {
         #ifdef HB_ARRAY_USE_COUNTER
           if( pItem->item.asArray.value->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
           {
              hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature Array/Object Release detected", NULL, NULL );
           }

           if( --( pItem->item.asArray.value->ulHolders ) == 0 )
           {
              hb_arrayRelease( pItem );
           }
         #else
           hb_arrayReleaseHolder( pItem->item.asArray.value, (void *) pItem );
         #endif

         break;
      }

      case HB_IT_POINTER:
      {
         if ( pItem->item.asPointer.collect )
         {
            hb_gcDecRef( (void *) pItem->item.asPointer.value );
         }
         break;
      }

      case HB_IT_BLOCK :
      {
         hb_codeblockDelete( pItem );

         break;
      }

      case HB_IT_BYREF :
      {
         if( pItem->item.asRefer.offset == 0 )
         {
            HB_ITEM FakeArray;

            FakeArray.type = HB_IT_NIL;
            FakeArray.item.asArray.value = pItem->item.asRefer.BasePtr.pBaseArray;

            #ifdef HB_ARRAY_USE_COUNTER
               if( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
               {
                  hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature Array/Object Release detected", NULL, NULL );
               }

               if( --( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders ) == 0 )
               {
                  hb_arrayRelease( &FakeArray );
               }
            #else
               hb_arrayResetHolder( pItem->item.asRefer.BasePtr.pBaseArray, (void *) pItem, &FakeArray );
               hb_arrayReleaseHolder( pItem->item.asRefer.BasePtr.pBaseArray, (void *) &FakeArray );
            #endif
         }

         break;
      }

      case HB_IT_HASH :
      {
         if( --( pItem->item.asHash.value->ulHolders ) == 0 )
         {
            hb_hashRelease( pItem );
         }

         break;
      }

   }

   pItem->type = HB_IT_NIL;
}

#endif

void HB_EXPORT hb_itemSwap( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_ITEM temp;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemSwap(%p, %p)", pItem1, pItem2));

   /*
   temp.type = HB_IT_NIL;
   hb_itemForwardValue( &temp, pItem2 );
   hb_itemForwardValue( pItem2, pItem1 );
   hb_itemForwardValue( pItem1, &temp );
   */

   #ifndef HB_ARRAY_USE_COUNTER
      if( pItem1->type == HB_IT_ARRAY && pItem1->item.asArray.value )
      {
         hb_arrayResetHolder( pItem1->item.asArray.value, (void *) pItem1, (void *) pItem2 );
      }
      else if( pItem1->type == HB_IT_BYREF && pItem1->item.asRefer.offset == 0 )
      {
         hb_arrayResetHolder( pItem1->item.asRefer.BasePtr.pBaseArray, (void *) pItem1, (void *) pItem2 );
      }

      if( pItem2->type == HB_IT_ARRAY && pItem2->item.asArray.value )
      {
         hb_arrayResetHolder( pItem2->item.asArray.value, (void *) pItem2, (void *) pItem1 );
      }
      else if( pItem2->type == HB_IT_BYREF && pItem2->item.asRefer.offset == 0 )
      {
         hb_arrayResetHolder( pItem2->item.asRefer.BasePtr.pBaseArray, (void *) pItem2, (void *) pItem1 );
      }
   #endif

   memcpy( &temp, pItem2, sizeof( HB_ITEM ) );
   memcpy( pItem2, pItem1, sizeof( HB_ITEM ) );
   memcpy( pItem1, &temp, sizeof( HB_ITEM ) );
}

void HB_EXPORT hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", pDest, pSource));

   if( pDest == pSource )
   {
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemCopy()", NULL );
   }

   if( HB_IS_COMPLEX( pDest ) )
   {
      hb_itemClear( pDest );
   }

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   if( HB_IS_COMPLEX( pSource ) )
   {
      if( pSource->type & HB_IT_STRING )
      {
         if( pSource->item.asString.allocated )
         {
            ++*( pSource->item.asString.pulHolders );
         }
         return;
      }
      else if( pSource->type & HB_IT_MEMVAR ) // intentionally & instead of ==
      {
         hb_memvarValueIncRef( pSource->item.asMemvar.value );
         return;
      }

      switch( pSource->type )
      {
         case HB_IT_ARRAY :
         {
            #ifdef HB_ARRAY_USE_COUNTER
               pSource->item.asArray.value->ulHolders++;
            #else
                hb_arrayRegisterHolder( pDest->item.asArray.value, (void *) pDest );
            #endif
            break;
         }

         case HB_IT_POINTER:
         {
            if( pSource->item.asPointer.collect )
            {
                hb_gcIncRef( (void *) pSource->item.asPointer.value );
            }
            break;
         }

         case HB_IT_BLOCK :
         {
            pSource->item.asBlock.value->ulCounter++;
            break;
         }
         case HB_IT_BYREF :
         {
            if( pSource->item.asRefer.offset == 0 )
            {
               #ifdef HB_ARRAY_USE_COUNTER
                  pSource->item.asRefer.BasePtr.pBaseArray->ulHolders++;
               #else
                  hb_arrayRegisterHolder( pSource->item.asRefer.BasePtr.pBaseArray, (void *) pSource->item.asRefer.BasePtr.pBaseArray );
               #endif
            }
            break;
         }
         case HB_IT_HASH :
         {
            pSource->item.asHash.value->ulHolders++;
            break;
         }
      }
   }
}

PHB_ITEM HB_EXPORT hb_itemPutC( PHB_ITEM pItem, const char * szText )
{
   ULONG ulLen = ( szText ? strlen( szText ) : 0 );

   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", pItem, szText));

   if( pItem == NULL )
   {
      pItem = hb_itemNew( NULL );
   }

   if( HB_IS_STRING( pItem ) )
   {
      // Recycle!
      if( pItem->item.asString.allocated &&  *( pItem->item.asString.pulHolders ) == 1 )
      {
         // Reset MEMO flag if any.
         pItem->type = HB_IT_STRING;

         if( szText == pItem->item.asString.value )
         {
            pItem->item.asString.value[ ulLen ] = '\0';
            pItem->item.asString.length = ulLen;

            return pItem;
         }
         else if( szText > pItem->item.asString.value && szText <= pItem->item.asString.value + pItem->item.asString.length )
         {
            char *sCopy = (char *) hb_xgrab( ulLen + 1 );

            hb_xmemcpy( (void *) sCopy, (void *) szText, ulLen );

            return hb_itemPutCPtr( pItem, sCopy, ulLen );
         }
         else
         {
            // Safe to realocate if needed.
            __HB_STRING_REALLOC( pItem, ulLen );

            // Safe, no need to use memmove()
            memcpy( pItem->item.asString.value, szText, ulLen );

            return pItem;
         }
      }
      else
      {
         // No need to check buffer overlapping - string is will NOT be released!
         hb_itemReleaseString( pItem );
      }
   }
   else if( HB_IS_COMPLEX( pItem ) )
   {
      hb_itemClear( pItem );
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.allocated = 0;

   if( ulLen )
   {
      if( ulLen == 1 )
      {
         pItem->item.asString.value  = ( char * ) hb_szAscii[ ( UCHAR ) ( szText[0] ) ];
         pItem->item.asString.length = 1;
      }
      else
      {
         pItem->item.asString.value           = ( char * ) hb_xgrab( ulLen + 1 );
         pItem->item.asString.value[ ulLen ]  = '\0';

         pItem->item.asString.pulHolders      = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
         *( pItem->item.asString.pulHolders ) = 1;

         pItem->item.asString.length          = ulLen;
         pItem->item.asString.allocated       = ulLen + 1;

         // Alocation above already set the 'length and the terminator!
         hb_xmemcpy( (void *) pItem->item.asString.value, (void *) szText, ulLen );
      }
   }
   else
   {
      pItem->item.asString.value  = hb_vm_sNull;
      pItem->item.asString.length = 0;
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCL( PHB_ITEM pItem, const char * szText, ULONG ulLen )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCL(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem == NULL )
   {
      pItem = hb_itemNew( NULL );
   }

   if( HB_IS_STRING( pItem ) )
   {
      // Recycle!
      if( pItem->item.asString.allocated &&  *( pItem->item.asString.pulHolders ) == 1 )
      {
         // Reset MEMO flag if any.
         pItem->type = HB_IT_STRING;

         if( szText == pItem->item.asString.value )
         {
            pItem->item.asString.value[ ulLen ] = '\0';
            pItem->item.asString.length = ulLen;

            return pItem;
         }
         else if( szText > pItem->item.asString.value && szText <= pItem->item.asString.value + pItem->item.asString.length )
         {
            char *sCopy = (char *) hb_xgrab( ulLen + 1 );

            hb_xmemcpy( (void *) sCopy, (void *) szText, ulLen );

            return hb_itemPutCPtr( pItem, sCopy, ulLen );
         }
         else
         {
            // Safe to realocate if needed.
            __HB_STRING_REALLOC( pItem, ulLen );

            // Safe, no need to use memmove()
            memcpy( pItem->item.asString.value, szText, ulLen );

            return pItem;
         }
      }
      else
      {
         // No need to check buffer overlapping - string is will NOT be released!
         hb_itemReleaseString( pItem );
      }
   }
   else if( HB_IS_COMPLEX( pItem ) )
   {
      hb_itemClear( pItem );
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.allocated = 0;

   if( ulLen )
   {
      if( ulLen == 1 )
      {
         pItem->item.asString.value  = ( char * ) hb_szAscii[ ( UCHAR ) szText[0] ];
         pItem->item.asString.length = 1;
      }
      else
      {
         pItem->item.asString.value           = ( char * ) hb_xgrab( ulLen + 1 );
         pItem->item.asString.value[ ulLen ]  = '\0';

         pItem->item.asString.pulHolders      = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
         *( pItem->item.asString.pulHolders ) = 1;

         pItem->item.asString.length          = ulLen;
         pItem->item.asString.allocated       = ulLen + 1;

         // Alocation above already set the 'length and the terminator!
         hb_xmemcpy( (void *) pItem->item.asString.value, (void *) szText, ulLen );
      }
   }
   else
   {
      pItem->item.asString.value  = hb_vm_sNull;
      pItem->item.asString.length = 0;
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCPtr( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCPtr(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_STRING;

   if( ulLen )
   {
      szText[ulLen] = '\0';

      pItem->item.asString.pulHolders      = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( pItem->item.asString.pulHolders ) = 1;
      pItem->item.asString.allocated       = ulLen + 1;
      pItem->item.asString.value           = szText;
   }
   else
   {
      hb_xfree( szText );

      pItem->item.asString.allocated      = 0;
      pItem->item.asString.value          = hb_vm_sNull;
   }

   pItem->item.asString.length    = ulLen;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCRaw( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCRaw(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_STRING;

   if( ulLen )
   {
      pItem->item.asString.pulHolders      = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( pItem->item.asString.pulHolders ) = 1;
   }
   else
   {
      if( szText )
      {
         hb_xfree( szText );
      }
      szText = hb_vm_sNull;
   }

   pItem->item.asString.length    = ulLen;
   pItem->item.asString.value     = szText;
   pItem->item.asString.allocated = ulLen;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCRawStatic( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCRawStatic(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.length    = ulLen;
   pItem->item.asString.value     = szText;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCStatic( PHB_ITEM pItem, const char * szText )
{
   ULONG ulLen = ( szText ? strlen( szText ) : 0 );

   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCStatic(%p, %s)", pItem, szText) );

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.length    = ulLen;

   if( ulLen )
   {
      pItem->item.asString.value  = ( char * ) szText;
   }
   else
   {
      pItem->item.asString.value  = hb_vm_sNull;
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCLStatic( PHB_ITEM pItem, const char * szText, ULONG ulLen )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCLStatic(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.length    = ulLen;

   if( ulLen )
   {
      pItem->item.asString.value  = ( char * ) szText;
   }
   else
   {
      pItem->item.asString.value  = hb_vm_sNull;
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutPtr( PHB_ITEM pItem, void * pValue )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutPtr(%p, %p)", pItem, pValue));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value = pValue;
   pItem->item.asPointer.collect = FALSE;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutPtrGC( PHB_ITEM pItem, void * pValue )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutPtr(%p, %p)", pItem, pValue));

   hb_gcIncRef( pValue );

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value   = pValue;
   pItem->item.asPointer.collect = TRUE;

   return pItem;
}


void HB_EXPORT hb_itemPushStaticString( const char * szText, ULONG length )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pTop = hb_stackTopItem();

   HB_TRACE_STEALTH(HB_TR_DEBUG, ( "hb_itemPushStaticString( \"%s\", %lu ) %p %p", szText, length, pTop, szText ) );

   pTop->type = HB_IT_STRING;
   pTop->item.asString.length    = length;
   pTop->item.asString.value     = ( char * ) szText;
   pTop->item.asString.allocated = 0;

   hb_stackPush();
}

#undef hb_retcAdopt
void HB_EXPORT hb_retcAdopt( char * szText )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retcAdopt(%s)", szText ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      hb_itemClear( &(HB_VM_STACK.Return) );
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.length = strlen( szText );

   if( ( &(HB_VM_STACK.Return) )->item.asString.length )
   {
      ( &(HB_VM_STACK.Return) )->item.asString.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( ( &(HB_VM_STACK.Return) )->item.asString.pulHolders ) = 1;
      ( &(HB_VM_STACK.Return) )->item.asString.allocated  = ( &(HB_VM_STACK.Return) )->item.asString.length + 1;
   }
   else
   {
      hb_xfree( szText );
      szText = hb_vm_sNull;
      ( &(HB_VM_STACK.Return) )->item.asString.allocated = 0;
   }

   ( &(HB_VM_STACK.Return) )->item.asString.value = szText;
}

#undef hb_retclenAdopt
void HB_EXPORT hb_retclenAdopt( char * szText, ULONG ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retclenAdopt( '%s', %lu )", szText, ulLen ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      hb_itemClear( &(HB_VM_STACK.Return) );
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;

   if( ulLen )
   {
      szText[ulLen] = '\0';

      ( &(HB_VM_STACK.Return) )->item.asString.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( ( &(HB_VM_STACK.Return) )->item.asString.pulHolders ) = 1;
      ( &(HB_VM_STACK.Return) )->item.asString.allocated  = ulLen + 1;
   }
   else
   {
      hb_xfree( szText );
      szText = hb_vm_sNull;
      ( &(HB_VM_STACK.Return) )->item.asString.allocated = 0;
   }

   ( &(HB_VM_STACK.Return) )->item.asString.value     = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length    = ulLen;
}

#undef hb_retclenAdoptRaw
void HB_EXPORT hb_retclenAdoptRaw( char * szText, ULONG ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retclenAdoptRaw( '%s', %lu )", szText, ulLen ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      hb_itemClear( &(HB_VM_STACK.Return) );
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;

   if( ulLen )
   {
      ( &(HB_VM_STACK.Return) )->item.asString.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( ( &(HB_VM_STACK.Return) )->item.asString.pulHolders ) = 1;
   }
   else
   {
      hb_xfree( szText );
      szText = hb_vm_sNull;
   }

   ( &(HB_VM_STACK.Return) )->item.asString.value     = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length    = ulLen;
   ( &(HB_VM_STACK.Return) )->item.asString.allocated = ulLen;
}

#undef hb_retcStatic
void HB_EXPORT hb_retcStatic( const char * szText )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retcStatic(%s)", szText ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      hb_itemClear( &(HB_VM_STACK.Return) );
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.allocated = 0;
   ( &(HB_VM_STACK.Return) )->item.asString.value     = ( char * ) szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length    = strlen( szText );
}

#undef hb_retclenStatic
void HB_EXPORT hb_retclenStatic( const char * szText, ULONG ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retclenStatic(%s)", szText ) );

   if( ( &(HB_VM_STACK.Return) )->type )
   {
      hb_itemClear( &(HB_VM_STACK.Return) );
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.allocated = 0;
   ( &(HB_VM_STACK.Return) )->item.asString.value     = ( char * ) szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length    = ulLen;

}

BYTE HB_EXPORT hb_itemParamId( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   PHB_ITEM *pBase = HB_VM_STACK.pBase + 1;
   PHB_ITEM *pTop;
   BYTE iId = 1;


   pTop = pBase + hb_stackBaseItem()->item.asSymbol.paramcnt + 1;

   while( pBase < pTop )
   {
     if( *pBase == pItem )
     {
        //printf( "\nId: %i", iId );
        return iId;
     }

     pBase++;
     iId++;
   }

   return 0;
}

HB_EXTERN_END
