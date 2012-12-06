/*
 * $Id$
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

#include "classes.h"
#include "hboo.ch"

HB_EXTERN_BEGIN
extern char * hb_vm_sNull;
HB_EXTERN_END

void hb_itemPushForward( PHB_ITEM pItem )
{
   HB_THREAD_STUB_STACK

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPushForward(%p)", pItem ) );

   hb_itemForwardValue( hb_stackTopItem(), pItem );
   hb_stackPush();
}

void hb_itemForwardValue( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemForwardValue(%p, %p) %i", pDest, pSource, pDest->type ) );

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemForwardValue()", NULL );

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   /* Forward. */
   hb_itemRawCpy( pDest, pSource );

#ifndef HB_ARRAY_USE_COUNTER
   if( pSource->type == HB_IT_ARRAY && pSource->item.asArray.value )
      hb_arrayResetHolder( pSource->item.asArray.value, ( void * ) pSource, ( void * ) pDest );
   else if( pSource->type == HB_IT_BYREF && ( pSource )->item.asRefer.offset == 0 )
      hb_arrayResetHolder( pSource->item.asRefer.BasePtr.pBaseArray, ( void * ) pSource, ( void * ) pDest );
#endif

   /* Now fake clear the transferer. */
   pSource->type = HB_IT_NIL;
}

PHB_ITEM hb_itemReturn( PHB_ITEM pItem )
{
   HB_THREAD_STUB_STACK

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemReturn(%p)", pItem ) );

   if( pItem )
      hb_itemCopy( hb_stackReturnItem(), pItem );

   return pItem;
}

PHB_ITEM hb_itemReturnForward( PHB_ITEM pItem )
{
   HB_THREAD_STUB_STACK

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemReturnForward(%p)", pItem ) );

   if( pItem )
      hb_itemForwardValue( hb_stackReturnItem(), pItem );

   return pItem;
}

void hb_itemReturnRelease( PHB_ITEM pItem )
{
   HB_THREAD_STUB_STACK

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemReturnRelease(%p)", pItem ) );

   if( pItem )
   {
      hb_itemMove( hb_stackReturnItem(), pItem );
      hb_itemRelease( pItem );
   }
}

void hb_itemReleaseString( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemReleaseString(%p), '%s'", pItem, pItem->item.asString.value ) );

   if( pItem->item.asString.allocated )
   {
      if( *( pItem->item.asString.pulHolders ) == 0 )
      {
         assert( 0 );
         hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature String Release detected: '%s'", pItem->item.asString.value, NULL );
      }

      if( HB_ATOMIC_DEC( *( pItem->item.asString.pulHolders ) ) == 0 )
      {
         HB_TRACE_STEALTH( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.pulHolders ) );
         hb_xfree( ( void * ) pItem->item.asString.pulHolders );
         /* pItem->item.asString.pulHolders = NULL; */

         HB_TRACE_STEALTH( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.value ) );
         hb_xfree( pItem->item.asString.value );
         /* pItem->item.asString.value = NULL; */
         /* pItem->item.asString.allocated = 0; */
      }
   }
}

void hb_itemClear( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemClear(%p) type: %i", pItem, pItem ? pItem->type : 0 ) );

#if defined( HB_FM_STATISTICS ) && defined( HB_PARANOID_MEM_CHECK )
   if( HB_IS_BADITEM( pItem ) )
      hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_itemClear()", NULL );
#endif

   if( pItem->type & HB_IT_STRING )
      hb_itemReleaseString( pItem );
   else if( pItem->type & HB_IT_BYREF )
   {
      if( pItem->type & HB_IT_MEMVAR )
         hb_memvarValueDecRef( ( HB_HANDLE ) pItem->item.asMemvar.value );
      else if( pItem->type & HB_IT_EXTREF )
         pItem->item.asExtRef.func->clear( pItem->item.asExtRef.value );
      else if( pItem->item.asRefer.offset == 0 /* && pItem->item.asRefer.value >= 0 */ )
      {
         HB_ITEM FakeArray;

         /* TraceLog( NULL, "BYREF Faked %p\n", pItem->item.asRefer.BasePtr.pBaseArray );
          */

         FakeArray.type                = HB_IT_ARRAY;
         FakeArray.item.asArray.value  = pItem->item.asRefer.BasePtr.pBaseArray;

#ifdef HB_ARRAY_USE_COUNTER
         if( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
         {
            assert( 0 );
            hb_errInternal( HB_EI_PREMATURE_RELEASE, "BYREF Premature Array/Object Release detected", NULL, NULL );
         }

         if( HB_ATOMIC_DEC( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders ) == 0 )
            hb_arrayRelease( &FakeArray );
#else
         hb_arrayResetHolder( pItem->item.asRefer.BasePtr.pBaseArray, ( void * ) pItem, &FakeArray );
         hb_arrayReleaseHolder( pItem->item.asRefer.BasePtr.pBaseArray, ( void * ) &FakeArray );
#endif
      }
   }
   else if( HB_IS_ARRAY( pItem ) )
   {
#ifdef HB_ARRAY_USE_COUNTER
      if( pItem->item.asArray.value->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
      {
         assert( 0 );
         hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature Array/Object Release detected %p", ( char * ) ( pItem->item.asArray.value ), NULL );
      }

      if( HB_ATOMIC_DEC( pItem->item.asArray.value->ulHolders ) == 0 )
         hb_arrayRelease( pItem );
#else
      hb_arrayReleaseHolder( pItem->item.asArray.value, ( void * ) pItem );
#endif
   }
   else if( HB_IS_BLOCK( pItem ) )
      hb_codeblockDelete( pItem );
   else if( HB_IS_HASH( pItem ) )
   {
      if( HB_ATOMIC_DEC( pItem->item.asHash.value->ulHolders ) == 0 )
         hb_hashRelease( pItem );
   }
   else if( HB_IS_POINTER( pItem ) )
   {
      if( pItem->item.asPointer.collect )
         hb_gcDecRef( ( void * ) pItem->item.asPointer.value );
   }
   else if( HB_IS_SYMBOL( pItem ) )
   {
      assert( pItem->item.asSymbol.pCargo );
      hb_xfree( ( void * ) pItem->item.asSymbol.pCargo );
      pItem->item.asSymbol.pCargo = NULL;
   }

   pItem->type = HB_IT_NIL;
}

#ifdef HB_THREAD_SUPPORT

void hb_itemClearMT( PHB_ITEM pItem, HB_STACK * pStack )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemClearMT(%p) type: %i", pItem, pItem->type ) );

   if( pItem->type & HB_IT_STRING )
      hb_itemReleaseString( pItem );
   else if( pItem->type & HB_IT_BYREF )
   {
      if( pItem->type & HB_IT_MEMVAR )
         hb_memvarValueDecRefMT( ( HB_HANDLE ) pItem->item.asMemvar.value, pStack );
      else if( pItem->type & HB_IT_EXTREF )
         pItem->item.asExtRef.func->clear( pItem->item.asExtRef.value );
      else if( pItem->item.asRefer.offset == 0 )
      {
         HB_ITEM FakeArray;

         FakeArray.type                = HB_IT_NIL;
         FakeArray.item.asArray.value  = pItem->item.asRefer.BasePtr.pBaseArray;

#ifdef HB_ARRAY_USE_COUNTER
         if( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
         {
            assert( 0 );
            hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature Array/Object Release detected", NULL, NULL );
         }

         if( HB_ATOMIC_DEC( pItem->item.asRefer.BasePtr.pBaseArray->ulHolders ) == 0 )
            hb_arrayRelease( &FakeArray );
#else
         hb_arrayResetHolder( pItem->item.asRefer.BasePtr.pBaseArray, ( void * ) pItem, &FakeArray );
         hb_arrayReleaseHolder( pItem->item.asRefer.BasePtr.pBaseArray, ( void * ) &FakeArray );
#endif
      }
   }
   else if( HB_IS_ARRAY( pItem ) )
   {
#ifdef HB_ARRAY_USE_COUNTER
      if( pItem->item.asArray.value->ulHolders == HB_ARRAY_COUNTER_DEFAULT_HOLDERS - 1 )
      {
         assert( 0 );
         hb_errInternal( HB_EI_PREMATURE_RELEASE, "Premature Array/Object Release detected %p", ( char * ) ( pItem->item.asArray.value ), NULL );
      }

      if( HB_ATOMIC_DEC( pItem->item.asArray.value->ulHolders ) == 0 )
         hb_arrayRelease( pItem );
#else
      hb_arrayReleaseHolder( pItem->item.asArray.value, ( void * ) pItem );
#endif
   }
   else if( HB_IS_BLOCK( pItem ) )
      hb_codeblockDelete( pItem );
   else if( HB_IS_HASH( pItem ) )
   {
      if( HB_ATOMIC_DEC( pItem->item.asHash.value->ulHolders ) == 0 )
         hb_hashRelease( pItem );
   }
   else if( HB_IS_POINTER( pItem ) )
   {
      if( pItem->item.asPointer.collect )
         hb_gcDecRef( ( void * ) pItem->item.asPointer.value );
   }
   else if( HB_IS_SYMBOL( pItem ) )
   {
      assert( pItem->item.asSymbol.pCargo );
      hb_xfree( ( void * ) pItem->item.asSymbol.pCargo );
      pItem->item.asSymbol.pCargo = NULL;
   }

   pItem->type = HB_IT_NIL;
}

#endif

void hb_itemSwap( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_ITEM temp;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemSwap(%p, %p)", pItem1, pItem2 ) );

   /*
      temp.type = HB_IT_NIL;
      hb_itemForwardValue( &temp, pItem2 );
      hb_itemForwardValue( pItem2, pItem1 );
      hb_itemForwardValue( pItem1, &temp );
    */

#ifndef HB_ARRAY_USE_COUNTER
   if( pItem1->type == HB_IT_ARRAY && pItem1->item.asArray.value )
      hb_arrayResetHolder( pItem1->item.asArray.value, ( void * ) pItem1, ( void * ) pItem2 );
   else if( pItem1->type == HB_IT_BYREF && pItem1->item.asRefer.offset == 0 )
      hb_arrayResetHolder( pItem1->item.asRefer.BasePtr.pBaseArray, ( void * ) pItem1, ( void * ) pItem2 );

   if( pItem2->type == HB_IT_ARRAY && pItem2->item.asArray.value )
      hb_arrayResetHolder( pItem2->item.asArray.value, ( void * ) pItem2, ( void * ) pItem1 );
   else if( pItem2->type == HB_IT_BYREF && pItem2->item.asRefer.offset == 0 )
      hb_arrayResetHolder( pItem2->item.asRefer.BasePtr.pBaseArray, ( void * ) pItem2, ( void * ) pItem1 );
#endif

   hb_itemRawCpy( &temp, pItem2 );
   hb_itemRawCpy( pItem2, pItem1 );
   hb_itemRawCpy( pItem1, &temp );
}

void hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemCopy(%p, %p)", pDest, pSource ) );

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemCopy()", NULL );

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   hb_itemRawCpy( pDest, pSource );
   pDest->type &= ~HB_IT_DEFAULT;

   if( HB_IS_COMPLEX( pSource ) )
   {
      if( pSource->type & HB_IT_STRING )
      {
         if( pSource->item.asString.allocated )
            HB_ATOMIC_INC( *( pSource->item.asString.pulHolders ) );
      }
      else if( pSource->type & HB_IT_BYREF )
      {
         if( pSource->type & HB_IT_MEMVAR ) /* intentionally & instead of == */
            hb_memvarValueIncRef( ( HB_HANDLE ) pSource->item.asMemvar.value );
         else if( pSource->type & HB_IT_ENUM )    /* enumerators cannnot be copied */
            pDest->type = HB_IT_NIL;
         else if( pSource->type & HB_IT_EXTREF )
            pSource->item.asExtRef.func->copy( pDest );
         else if( pSource->item.asRefer.offset == 0 /* && pSource->item.asRefer.value >= 0 */ )
#ifdef HB_ARRAY_USE_COUNTER
            HB_ATOMIC_INC( pSource->item.asRefer.BasePtr.pBaseArray->ulHolders );
#else
            hb_arrayRegisterHolder( pSource->item.asRefer.BasePtr.pBaseArray, ( void * ) pSource->item.asRefer.BasePtr.pBaseArray );
#endif

         if( hb_itemUnRef( pSource ) == pDest )
         {
            assert( 0 );
            /* hb_errRT_BASE( EG_ARG, 9000, NULL, "Cyclic Reference assignment", 2, pSource, pDest ); */
            hb_errInternal( HB_EI_ERRUNRECOV, "Cyclic Reference assignment.", NULL, NULL );
         }
      }
      else if( HB_IS_ARRAY( pSource ) )
      {
#ifdef HB_ARRAY_USE_COUNTER
         HB_ATOMIC_INC( pSource->item.asArray.value->ulHolders );
#else
         hb_arrayRegisterHolder( pDest->item.asArray.value, ( void * ) pDest );
#endif
      }
      else if( HB_IS_BLOCK( pSource ) )
         HB_ATOMIC_INC( pSource->item.asBlock.value->ulCounter );
      else if( HB_IS_HASH( pSource ) )
         HB_ATOMIC_INC( pSource->item.asHash.value->ulHolders );
      else if( HB_IS_POINTER( pSource ) )
      {
         if( pSource->item.asPointer.collect )
            hb_gcIncRef( ( void * ) pSource->item.asPointer.value );
      }
      else if( HB_IS_SYMBOL( pSource ) )
      {
         PHB_SYMBCARGO pSymCargo = ( PHB_SYMBCARGO ) hb_xgrab( sizeof( HB_SYMBCARGO ) );

         assert( pSource->item.asSymbol.pCargo );
         HB_MEMCPY( pSymCargo, ( void * ) pSource->item.asSymbol.pCargo, sizeof( HB_SYMBCARGO ) );

         pDest->item.asSymbol.pCargo = pSymCargo;
      }
      else
         assert( 0 );
   }
}

PHB_ITEM hb_itemPutC( PHB_ITEM pItem, const char * szText )
{
   HB_SIZE ulLen = 0;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutC(%p, %s)", pItem, szText ) );

   if( szText )
      ulLen = strlen( szText );

   return hb_itemPutCL( pItem, szText, ulLen );

/*
 * if( pItem == NULL )
 * {
 *    pItem = hb_itemNew( NULL );
 * }
 *
 * if( HB_IS_STRING( pItem ) )
 * {
 *    // Recycle!
 *    if( pItem->item.asString.allocated &&  *( pItem->item.asString.pulHolders ) == 1 )
 *    {
 *       // Reset MEMO flag if any.
 *       pItem->type = HB_IT_STRING;
 *
 *       if( szText == pItem->item.asString.value )
 *       {
 *          pItem->item.asString.value[ ulLen ] = '\0';
 *          pItem->item.asString.length = ulLen;
 *
 *          return pItem;
 *       }
 *       else if( szText > pItem->item.asString.value && szText <= pItem->item.asString.value + pItem->item.asString.length )
 *       {
 *          char *sCopy = (char *) hb_xgrab( ulLen + 1 );
 *
 *          hb_xmemcpy( (void *) sCopy, (void *) szText, ulLen );
 *
 *          return hb_itemPutCPtr( pItem, sCopy, ulLen );
 *       }
 *       else
 *       {
 *          // Safe to realocate if needed.
 *          __HB_STRING_REALLOC( pItem, ulLen );
 *
 *          // Safe, no need to use memmove()
 *          hb_xmemcpy( pItem->item.asString.value, szText, ulLen );
 *
 *          return pItem;
 *       }
 *    }
 *    else
 *       // No need to check buffer overlapping - string is will NOT be released!
 *       hb_itemReleaseString( pItem );
 * }
 * else if( HB_IS_COMPLEX( pItem ) )
 *    hb_itemClear( pItem );
 *
 * pItem->type = HB_IT_STRING;
 * pItem->item.asString.value = hb_vm_sNull;
 * pItem->item.asString.length = 0;
 * pItem->item.asString.allocated = 0;
 *
 * if( ulLen )
 * {
 *    if( ulLen == 1 )
 *    {
 *       pItem->item.asString.value  = ( char * ) hb_szAscii[ ( UCHAR ) ( szText[0] ) ];
 *       pItem->item.asString.length = 1;
 *    }
 *    else
 *    {
 *       pItem->item.asString.value           = ( char * ) hb_xgrab( ulLen + 1 );
 *       pItem->item.asString.value[ ulLen ]  = '\0';
 *
 *       pItem->item.asString.pulHolders      = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
 *       *( pItem->item.asString.pulHolders ) = 1;
 *
 *       pItem->item.asString.length          = ulLen;
 *       pItem->item.asString.allocated       = ulLen + 1;
 *
 *       // Alocation above already set the 'length and the terminator!
 *       hb_xmemcpy( (void *) pItem->item.asString.value, (void *) szText, ulLen );
 *    }
 * }
 *
 * return pItem;
 */
}

PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, const char * szText, HB_SIZE ulLen )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutCL(%p, %s, %lu)", pItem, szText, ulLen ) );

   if( pItem == NULL )
      pItem = hb_itemNew( NULL );
   else
   {
      if( HB_IS_STRING( pItem ) )
      {
         /* Recycle! */
         if( pItem->item.asString.allocated && *( pItem->item.asString.pulHolders ) == 1 )
         {
            /* Reset MEMO flag if any. */
            pItem->type = HB_IT_STRING;

            if( szText == pItem->item.asString.value )
            {
               pItem->item.asString.value[ ulLen ] = '\0';
               pItem->item.asString.length         = ulLen;

               return pItem;
            }
            else if( szText > pItem->item.asString.value && szText <= pItem->item.asString.value + pItem->item.asString.length )
            {
               char * sCopy = ( char * ) hb_xgrab( ulLen + 1 );

               hb_xmemcpy( ( void * ) sCopy, ( void * ) szText, ( size_t ) ulLen );

               return hb_itemPutCPtr( pItem, sCopy, ulLen );
            }
            else
            {
               /* Safe to realocate if needed. */
               __HB_STRING_REALLOC( pItem, ulLen );

               /* Safe, no need to use memmove() */
               HB_MEMCPY( pItem->item.asString.value, szText, ( size_t ) ulLen );

               return pItem;
            }
         }
         else
            /* No need to check buffer overlapping - string is will NOT be released!
             */
            hb_itemReleaseString( pItem );
      }
      else if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }

   pItem->type                      = HB_IT_STRING;
   pItem->item.asString.value       = hb_vm_sNull;
   pItem->item.asString.allocated   = 0;
   pItem->item.asString.length      = 0;

   if( ulLen == 1 )
   {
      pItem->item.asString.value    = ( char * ) hb_szAscii[ ( UCHAR ) szText[ 0 ] ];
      pItem->item.asString.length   = 1;
   }
   else if( ulLen > 1 )
   {
      pItem->item.asString.value             = ( char * ) hb_xgrab( ulLen + 1 );
      pItem->item.asString.value[ ulLen ]    = '\0';

      pItem->item.asString.pulHolders        = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( pItem->item.asString.pulHolders )   = 1;

      pItem->item.asString.length            = ulLen;
      pItem->item.asString.allocated         = ulLen + 1;

      /* Alocation above already set the 'length and the terminator! */
      hb_xmemcpy( ( void * ) pItem->item.asString.value, ( void * ) szText, ( size_t ) ulLen );
   }

   return pItem;
}

PHB_ITEM hb_itemPutCPtr( PHB_ITEM pItem, char * szText, HB_SIZE ulLen )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutCPtr(%p, %s, %lu)", pItem, szText, ulLen ) );

   if( ulLen )
   {
      szText[ ulLen ]   = '\0';
      pItem             = hb_itemPutCRaw( pItem, szText, ulLen );
      pItem->item.asString.allocated++;
   }
   else
      pItem = hb_itemPutCRaw( pItem, szText, 0 );

   return pItem;

/*
 * if( pItem )
 * {
 *    if( HB_IS_COMPLEX( pItem ) )
 *    {
 *       hb_itemClear( pItem );
 *    }
 * }
 * else
 * {
 *    pItem = hb_itemNew( NULL );
 * }
 *
 * pItem->type = HB_IT_STRING;
 *
 * if( ulLen )
 * {
 *    szText[ulLen] = '\0';
 *
 *    pItem->item.asString.pulHolders      = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
 * *( pItem->item.asString.pulHolders ) = 1;
 *    pItem->item.asString.allocated       = ulLen + 1;
 *    pItem->item.asString.value           = szText;
 * }
 * else
 * {
 *    hb_xfree( szText );
 *
 *    pItem->item.asString.allocated      = 0;
 *    pItem->item.asString.value          = hb_vm_sNull;
 * }
 *
 * pItem->item.asString.length    = ulLen;
 *
 * return pItem;
 */
}

PHB_ITEM hb_itemPutCRaw( PHB_ITEM pItem, char * szText, HB_SIZE ulLen )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutCRaw(%p, %s, %lu)", pItem, szText, ulLen ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type = HB_IT_STRING;
   if( ulLen )
   {
      pItem->item.asString.pulHolders        = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( pItem->item.asString.pulHolders )   = 1;
   }
   else
   {
      if( szText )
         hb_xfree( szText );

      szText = hb_vm_sNull;
   }
   pItem->item.asString.length      = ulLen;
   pItem->item.asString.value       = szText;
   pItem->item.asString.allocated   = ulLen;

   return pItem;
}

PHB_ITEM hb_itemPutCRawStatic( PHB_ITEM pItem, const char * szText, HB_SIZE ulLen )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutCRawStatic(%p, %s, %lu)", pItem, szText, ulLen ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                      = HB_IT_STRING;
   pItem->item.asString.allocated   = 0;
   pItem->item.asString.length      = ulLen;
   pItem->item.asString.value       = ( char * ) szText;

   return pItem;
}

PHB_ITEM hb_itemPutCStatic( PHB_ITEM pItem, const char * szText )
{
   HB_SIZE ulLen = 0;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutCStatic(%p, %s)", pItem, szText ) );

   if( szText )
      ulLen = strlen( szText );

   return hb_itemPutCLStatic( pItem, szText, ulLen );

/*
 * if( pItem )
 * {
 *    if( HB_IS_COMPLEX( pItem ) )
 *    {
 *       hb_itemClear( pItem );
 *    }
 * }
 * else
 * {
 *    pItem = hb_itemNew( NULL );
 * }

 * pItem->type = HB_IT_STRING;
 * pItem->item.asString.allocated = 0;
 * pItem->item.asString.length    = ulLen;
 *
 * if( ulLen )
 * {
 *    pItem->item.asString.value  = ( char * ) szText;
 * }
 * else
 * {
 *    pItem->item.asString.value  = hb_vm_sNull;
 * }
 *
 * return pItem;
 */
}

PHB_ITEM hb_itemPutCLStatic( PHB_ITEM pItem, const char * szText, HB_SIZE ulLen )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutCLStatic(%p, %s, %lu)", pItem, szText, ulLen ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                      = HB_IT_STRING;
   pItem->item.asString.value       = hb_vm_sNull;
   pItem->item.asString.length      = ulLen;
   pItem->item.asString.allocated   = 0;

   if( ulLen )
      pItem->item.asString.value = ( char * ) szText;

   return pItem;
}

PHB_ITEM hb_itemPutPtr( PHB_ITEM pItem, void * pValue )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutPtr(%p, %p)", pItem, pValue ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                   = HB_IT_POINTER;
   pItem->item.asPointer.value   = pValue;
   pItem->item.asPointer.collect = FALSE;

   return pItem;
}

PHB_ITEM hb_itemPutPtrGC( PHB_ITEM pItem, void * pValue )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutPtr(%p, %p)", pItem, pValue ) );

   hb_gcIncRef( pValue );

   pItem                         = hb_itemPutPtr( pItem, pValue );
   pItem->item.asPointer.collect = TRUE;

   return pItem;

/*
 * hb_gcIncRef( pValue );
 *
 * if( pItem )
 * {
 *    if( HB_IS_COMPLEX( pItem ) )
 *    {
 *       hb_itemClear( pItem );
 *    }
 * }
 * else
 * {
 *    pItem = hb_itemNew( NULL );
 * }
 *
 * pItem->type = HB_IT_POINTER;
 * pItem->item.asPointer.value   = pValue;
 * pItem->item.asPointer.collect = TRUE;
 *
 * return pItem;
 */
}

void hb_itemPushStaticString( const char * szText, HB_SIZE length )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pTop = hb_stackTopItem();

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPushStaticString( \"%s\", %lu ) %p %p", szText, length, pTop, szText ) );

   pTop->type                    = HB_IT_STRING;
   pTop->item.asString.length    = length;
   pTop->item.asString.value     = ( char * ) szText;
   pTop->item.asString.allocated = 0;

   hb_stackPush();
}

#undef hb_retcAdopt
void hb_retcAdopt( char * szText )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_retcAdopt(%s)", szText ) );

   if( ( &( HB_VM_STACK.Return ) )->type )
      hb_itemClear( &( HB_VM_STACK.Return ) );

   ( &( HB_VM_STACK.Return ) )->type = HB_IT_STRING;

   if( szText )
   {
      ( &( HB_VM_STACK.Return ) )->item.asString.length = strlen( szText );

      if( ( &( HB_VM_STACK.Return ) )->item.asString.length )
      {
         ( &( HB_VM_STACK.Return ) )->item.asString.pulHolders       = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
         *( ( &( HB_VM_STACK.Return ) )->item.asString.pulHolders )  = 1;
         ( &( HB_VM_STACK.Return ) )->item.asString.allocated        = ( &( HB_VM_STACK.Return ) )->item.asString.length + 1;
      }
      else
      {
         hb_xfree( szText );
         szText                                                = hb_vm_sNull;
         ( &( HB_VM_STACK.Return ) )->item.asString.allocated  = 0;
      }
   }
   else
   {
      szText                                                = hb_vm_sNull;
      ( &( HB_VM_STACK.Return ) )->item.asString.allocated  = 0;
   }

   ( &( HB_VM_STACK.Return ) )->item.asString.value = szText;
}

#undef hb_retclenAdopt
void hb_retclenAdopt( char * szText, HB_SIZE ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_retclenAdopt( '%s', %lu )", szText, ulLen ) );

   if( ( &( HB_VM_STACK.Return ) )->type )
      hb_itemClear( &( HB_VM_STACK.Return ) );

   ( &( HB_VM_STACK.Return ) )->type = HB_IT_STRING;

   if( ulLen )
   {
      szText[ ulLen ]                                             = '\0';

      ( &( HB_VM_STACK.Return ) )->item.asString.pulHolders       = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( ( &( HB_VM_STACK.Return ) )->item.asString.pulHolders )  = 1;
      ( &( HB_VM_STACK.Return ) )->item.asString.allocated        = ulLen + 1;
   }
   else
   {
      if( szText )
         hb_xfree( szText );

      szText                                                = hb_vm_sNull;
      ( &( HB_VM_STACK.Return ) )->item.asString.allocated  = 0;
   }

   ( &( HB_VM_STACK.Return ) )->item.asString.value   = szText;
   ( &( HB_VM_STACK.Return ) )->item.asString.length  = ulLen;
}

#undef hb_retclenAdoptRaw
void hb_retclenAdoptRaw( char * szText, HB_SIZE ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_retclenAdoptRaw( '%s', %lu )", szText, ulLen ) );

   if( ( &( HB_VM_STACK.Return ) )->type )
      hb_itemClear( &( HB_VM_STACK.Return ) );

   ( &( HB_VM_STACK.Return ) )->type = HB_IT_STRING;

   if( ulLen )
   {
      ( &( HB_VM_STACK.Return ) )->item.asString.pulHolders       = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( ( &( HB_VM_STACK.Return ) )->item.asString.pulHolders )  = 1;
   }
   else
   {
      if( szText )
         hb_xfree( szText );

      szText = hb_vm_sNull;
   }

   ( &( HB_VM_STACK.Return ) )->item.asString.value      = szText;
   ( &( HB_VM_STACK.Return ) )->item.asString.length     = ulLen;
   ( &( HB_VM_STACK.Return ) )->item.asString.allocated  = ulLen;
}

#undef hb_retcStatic
void hb_retcStatic( const char * szText )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_retcStatic(%s)", szText ) );

   if( ( &( HB_VM_STACK.Return ) )->type )
      hb_itemClear( &( HB_VM_STACK.Return ) );

   ( &( HB_VM_STACK.Return ) )->type                     = HB_IT_STRING;
   ( &( HB_VM_STACK.Return ) )->item.asString.allocated  = 0;
   ( &( HB_VM_STACK.Return ) )->item.asString.value      = ( char * ) szText;
   ( &( HB_VM_STACK.Return ) )->item.asString.length     = szText ? strlen( szText ) : 0;
}

#undef hb_retclenStatic
void hb_retclenStatic( const char * szText, HB_SIZE ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_retclenStatic(%s)", szText ) );

   if( ( &( HB_VM_STACK.Return ) )->type )
      hb_itemClear( &( HB_VM_STACK.Return ) );

   ( &( HB_VM_STACK.Return ) )->type                     = HB_IT_STRING;
   ( &( HB_VM_STACK.Return ) )->item.asString.allocated  = 0;
   ( &( HB_VM_STACK.Return ) )->item.asString.value      = ( char * ) szText;
   ( &( HB_VM_STACK.Return ) )->item.asString.length     = ulLen;
}

BYTE hb_itemParamId( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   PHB_ITEM *  pBase = HB_VM_STACK.pBase + 1;
   PHB_ITEM *  pTop;
   BYTE        iId   = 1;

   pTop = pBase + hb_stackBaseItem()->item.asSymbol.pCargo->arguments + 1;
   while( pBase < pTop )
   {
      if( *pBase == pItem )
         /* printf( "\nId: %i", iId ); */
         return iId;

      pBase++;
      iId++;
   }

   return 0;
}
