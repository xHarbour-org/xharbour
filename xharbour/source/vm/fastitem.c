/*
 * $Id: fastitem.c,v 1.59 2004/02/25 15:10:56 lculik Exp $
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

#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"
#include "hashapi.h"

extern char *hb_vm_sNull;
extern char *hb_vm_acAscii[256];

HB_EXTERN_BEGIN

/* Forward decalarations. */
void hb_itemForwardValue( PHB_ITEM pDest, PHB_ITEM pSource );

void HB_EXPORT hb_itemPushForward( PHB_ITEM pItem )
{
   HB_THREAD_STUB

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
      if( HB_IS_STRING( pDest ) )
      {
         hb_itemReleaseString( pDest );
      }
      else
      {
         hb_itemFastClear( pDest );
      }
   }

   /* Forward. */
   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   #ifndef HB_ARRAY_USE_COUNTER
      if( HB_IS_ARRAY( pSource ) && pSource->item.asArray.value )
      {
         hb_arrayResetHolder( pSource->item.asArray.value, (void *) pSource, (void *) pDest );
      }
   #endif

   /* Now fake clear the transferer. */
   //pSource->item.asString.bStatic = FALSE;
   pSource->type = HB_IT_NIL;
}

PHB_ITEM HB_EXPORT hb_itemReturn( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemReturn(%p)", pItem ) );

   if( pItem )
   {
      hb_itemForwardValue( &(HB_VM_STACK.Return), pItem );
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemReturnCopy( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemReturnCopy(%p)", pItem ) );

   if( pItem )
   {
      hb_itemCopy( &(HB_VM_STACK.Return), pItem );
   }

   return pItem;
}

void HB_EXPORT hb_itemReleaseString( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemReleaseString(%p), '%s'", pItem, pItem->item.asString.value ) );

   if( pItem->item.asString.bStatic == FALSE )
   {
      if( --*( pItem->item.asString.puiHolders ) == 0 )
      {
         HB_TRACE_STEALTH( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.puiHolders ) );
         hb_xfree( pItem->item.asString.puiHolders );
         HB_TRACE_STEALTH( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.value ) );
         hb_xfree( pItem->item.asString.value );
      }
   }

   //pItem->item.asString.bStatic = FALSE;
   pItem->item.asString.value = NULL;
   //pItem->item.asString.length = 0;

}

void HB_EXPORT hb_itemClear( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemClear(%p) type: %i", pItem, pItem->type ) );

   if( HB_IS_STRING( pItem ) )
   {
      if( pItem->item.asString.value )
      {
         hb_itemReleaseString( pItem );
      }
   }
   else if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value )
   {
      #ifdef HB_ARRAY_USE_COUNTER
         if( --( ( pItem->item.asArray.value )->uiHolders ) == 0 )
         {
            hb_arrayRelease( pItem );
         }
      #else
         hb_arrayReleaseHolder( pItem->item.asArray.value, (void *) pItem );
      #endif
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      hb_codeblockDelete( pItem );
   }
   else if( HB_IS_MEMVAR( pItem ) )
   {
      hb_memvarValueDecRef( pItem->item.asMemvar.value );
   }

   pItem->type = HB_IT_NIL;

}

#ifdef HB_THREAD_SUPPORT

void HB_EXPORT hb_itemClearMT( PHB_ITEM pItem, HB_STACK *pStack )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemClear(%p) type: %i", pItem, pItem->type ) );

   if( HB_IS_STRING( pItem ) )
   {
      if( pItem->item.asString.value )
      {
         hb_itemReleaseString( pItem );
      }
   }
   else if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value )
   {
      #ifdef HB_ARRAY_USE_COUNTER
         if( --( ( pItem->item.asArray.value )->uiHolders ) == 0 )
         {
            hb_arrayRelease( pItem );
         }
      #else
         hb_arrayReleaseHolder( pItem->item.asArray.value, (void *) pItem );
      #endif
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      hb_codeblockDelete( pItem );
   }
   else if( HB_IS_MEMVAR( pItem ) )
   {
      hb_memvarValueDecRefMT( pItem->item.asMemvar.value, pStack );
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
      if( HB_IS_ARRAY( pItem1 ) && pItem1->item.asArray.value )
      {
         hb_arrayResetHolder( pItem1->item.asArray.value, (void *) pItem1, (void *) pItem2 );
      }

      if( HB_IS_ARRAY( pItem2 ) && pItem2->item.asArray.value )
      {
         hb_arrayResetHolder( pItem2->item.asArray.value, (void *) pItem2, (void *) pItem1 );
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
      if( HB_IS_STRING( pDest ) && pDest->item.asString.value )
      {
         hb_itemReleaseString( pDest );
      }
      else
      {
         hb_itemFastClear( pDest );
      }
   }

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   if( HB_IS_STRING( pSource ) )
   {
      if( pSource->item.asString.bStatic == FALSE )
      {
         ++*( pSource->item.asString.puiHolders );
      }
   }
   else if( HB_IS_ARRAY( pSource ) )
   {
      #ifdef HB_ARRAY_USE_COUNTER
         ( pSource->item.asArray.value )->uiHolders++;
      #else
          hb_arrayRegisterHolder( pDest->item.asArray.value, (void *) pDest );
      #endif
   }
   else if( HB_IS_BLOCK( pSource ) )
   {
      ( pSource->item.asBlock.value )->ulCounter++;
   }
   else if( HB_IS_MEMVAR( pSource ) )
   {
      hb_memvarValueIncRef( pSource->item.asMemvar.value );
   }
}

PHB_ITEM HB_EXPORT hb_itemPutC( PHB_ITEM pItem, char * szText )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", pItem, szText));


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

   if( szText == NULL || szText[0] == '\0' )
   {
      pItem->item.asString.length  = 0;
      pItem->item.asString.value   = hb_vm_sNull;
      pItem->item.asString.bStatic = TRUE;
   }
   else if( szText[1] == '\0' )
   {
      pItem->item.asString.length  = 1;
      pItem->item.asString.value   = hb_vm_acAscii[ (BYTE) ( szText[0] ) ];
      pItem->item.asString.bStatic = TRUE;
   }
   else
   {
      pItem->item.asString.puiHolders      = (ULONG*) hb_xgrab( sizeof( ULONG ) );
      *( pItem->item.asString.puiHolders ) = 1;
      pItem->item.asString.bStatic         = FALSE;
      pItem->item.asString.length          = strlen( szText );
      pItem->item.asString.value           = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
      strcpy( pItem->item.asString.value, szText );
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCL( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCL(%p, %s, %lu)", pItem, szText, ulLen));



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

   if( szText == NULL || ulLen == 0 )
   {
      pItem->item.asString.length  = 0;
      pItem->item.asString.value   = hb_vm_sNull;
      pItem->item.asString.bStatic = TRUE;
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.length  = 1;
      pItem->item.asString.value   = hb_vm_acAscii[ (BYTE) ( szText[0] ) ];
      pItem->item.asString.bStatic = TRUE;
   }
   else
   {
      pItem->item.asString.puiHolders      = (ULONG*) hb_xgrab( sizeof( ULONG ) );
      *( pItem->item.asString.puiHolders ) = 1;
      pItem->item.asString.bStatic         = FALSE;
      pItem->item.asString.length          = ulLen;
      pItem->item.asString.value           = ( char * ) hb_xgrab( ulLen + 1 );
      hb_xmemcpy( pItem->item.asString.value, szText, ulLen );
      pItem->item.asString.value[ ulLen ]  = '\0';
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
   pItem->item.asString.puiHolders = (ULONG*) hb_xgrab( sizeof( ULONG ) );
   *( pItem->item.asString.puiHolders ) = 1;
   pItem->item.asString.bStatic = FALSE;
   pItem->item.asString.length  = ulLen;
   pItem->item.asString.value   = szText;
   pItem->item.asString.value[ ulLen ] = '\0';


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
   pItem->item.asString.puiHolders = (ULONG*) hb_xgrab( sizeof( ULONG ) );
   *( pItem->item.asString.puiHolders ) = 1;
   pItem->item.asString.bStatic = FALSE;
   pItem->item.asString.length  = ulLen;
   pItem->item.asString.value   = szText;


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
   pItem->item.asString.bStatic = TRUE;
   pItem->item.asString.length  = ulLen;
   pItem->item.asString.value   = szText;


   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCStatic( PHB_ITEM pItem, char * szText )
{
   ULONG ulLen = strlen( szText );

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
   pItem->item.asString.bStatic = TRUE;
   pItem->item.asString.length  = ulLen;
   pItem->item.asString.value   = szText;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCLStatic( PHB_ITEM pItem, char * szText, ULONG ulLen )
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
   pItem->item.asString.bStatic = TRUE;
   pItem->item.asString.length  = ulLen;
   pItem->item.asString.value   = szText;

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
   pItem->item.asPointer.collect = TRUE;

   return pItem;
}


void HB_EXPORT hb_itemFastClear( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ( "hb_itemFastClear(%p) Type: %i", pItem, pItem->type ) );

   if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value )
   {
      //printf( "\nFastClear Array %p uiHolders: %i Cyclic: %i", pItem, ( pItem->item.asArray.value )->uiHolders, hb_itemArrayCyclicCount( pItem ) );

      #ifdef HB_ARRAY_USE_COUNTER
         if( ( pItem->item.asArray.value )->uiHolders == 0 || --( ( pItem->item.asArray.value )->uiHolders ) == 0 )
         {
            hb_arrayRelease( pItem );
         }
      #else
         hb_arrayReleaseHolder( pItem->item.asArray.value, (void *) pItem );
      #endif
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      hb_codeblockDelete( pItem );
   }
   else if( HB_IS_MEMVAR( pItem ) )
   {
      hb_memvarValueDecRef( pItem->item.asMemvar.value );
   }

   pItem->type    = HB_IT_NIL;


   HB_TRACE_STEALTH(HB_TR_DEBUG, ( "DONE hb_itemFastClear(%p)", pItem ) );
}

void HB_EXPORT hb_itemPushStaticString( char * szText, ULONG length )
{
   HB_THREAD_STUB

   PHB_ITEM pTop = hb_stackTopItem();

   HB_TRACE_STEALTH(HB_TR_DEBUG, ( "hb_itemPushStaticString( \"%s\", %lu ) %p %p", szText, length, pTop, szText ) );

   pTop->type = HB_IT_STRING;
   pTop->item.asString.length  = length;
   pTop->item.asString.value   = szText;
   pTop->item.asString.bStatic = TRUE;

   hb_stackPush();
}

#undef hb_retcAdopt
void HB_EXPORT hb_retcAdopt( char * szText )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retcAdopt(%s)", szText ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      if( HB_IS_STRING( &(HB_VM_STACK.Return) ) )
      {
         hb_itemReleaseString( &(HB_VM_STACK.Return) );
      }
      else
      {
         hb_itemFastClear( &(HB_VM_STACK.Return) );
      }
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.puiHolders = (ULONG*) hb_xgrab( sizeof( ULONG ) );
   *( ( &(HB_VM_STACK.Return) )->item.asString.puiHolders ) = 1;
   ( &(HB_VM_STACK.Return) )->item.asString.bStatic = FALSE;
   ( &(HB_VM_STACK.Return) )->item.asString.value   = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length  = strlen( szText );

}

#undef hb_retclenAdopt
void HB_EXPORT hb_retclenAdopt( char * szText, ULONG ulLen )
{
   HB_THREAD_STUB

   szText[ulLen] = '\0';

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retclenAdopt( '%s', %lu )", szText, ulLen ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      if( HB_IS_STRING( &(HB_VM_STACK.Return) ) )
      {
         hb_itemReleaseString( &(HB_VM_STACK.Return) );
      }
      else
      {
         hb_itemFastClear( &(HB_VM_STACK.Return) );
      }
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.puiHolders = (ULONG*) hb_xgrab( sizeof( ULONG ) );
   *( ( &(HB_VM_STACK.Return) )->item.asString.puiHolders ) = 1;
   ( &(HB_VM_STACK.Return) )->item.asString.bStatic = FALSE;
   ( &(HB_VM_STACK.Return) )->item.asString.value   = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length  = ulLen;

}

#undef hb_retclenAdoptRaw
void HB_EXPORT hb_retclenAdoptRaw( char * szText, ULONG ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retclenAdopt( '%s', %lu )", szText, ulLen ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      if( HB_IS_STRING( &(HB_VM_STACK.Return) ) )
      {
         hb_itemReleaseString( &(HB_VM_STACK.Return) );
      }
      else
      {
         hb_itemFastClear( &(HB_VM_STACK.Return) );
      }
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.puiHolders = (ULONG*) hb_xgrab( sizeof( ULONG ) );
   *( ( &(HB_VM_STACK.Return) )->item.asString.puiHolders ) = 1;
   ( &(HB_VM_STACK.Return) )->item.asString.bStatic = FALSE;
   ( &(HB_VM_STACK.Return) )->item.asString.value   = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length  = ulLen;

}

#undef hb_retclenAdoptRawStatic
void HB_EXPORT hb_retclenAdoptRawStatic( char * szText, ULONG ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retclenAdopt( '%s', %lu )", szText, ulLen ) );

   if( ( &(HB_VM_STACK.Return) )->type )
   {
      if( HB_IS_STRING( &(HB_VM_STACK.Return) ) )
      {
         hb_itemReleaseString( &(HB_VM_STACK.Return) );
      }
      else
      {
         hb_itemFastClear( &(HB_VM_STACK.Return) );
      }
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.puiHolders = (ULONG*) hb_xgrab( sizeof( ULONG ) );
   *( ( &(HB_VM_STACK.Return) )->item.asString.puiHolders ) = 1;
   ( &(HB_VM_STACK.Return) )->item.asString.bStatic = TRUE;
   ( &(HB_VM_STACK.Return) )->item.asString.value   = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length  = ulLen;

}

#undef hb_retcAdoptStatic
void HB_EXPORT hb_retcAdoptStatic( char * szText )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retcAdoptStatic(%s)", szText ) );


   if( ( &(HB_VM_STACK.Return) )->type )
   {
      if( HB_IS_STRING( &(HB_VM_STACK.Return) ) )
      {
         hb_itemReleaseString( &(HB_VM_STACK.Return) );
      }
      else
      {
         hb_itemFastClear( &(HB_VM_STACK.Return) );
      }
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.bStatic = TRUE;
   ( &(HB_VM_STACK.Return) )->item.asString.value   = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length  = strlen( szText );

}

#undef hb_retclenAdoptStatic
void HB_EXPORT hb_retclenAdoptStatic( char * szText, ULONG ulLen )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_INFO, ("hb_retclenAdoptStatic(%s)", szText ) );

   if( ( &(HB_VM_STACK.Return) )->type )
   {
      if( HB_IS_STRING( &(HB_VM_STACK.Return) ) )
      {
         hb_itemReleaseString( &(HB_VM_STACK.Return) );
      }
      else
      {
         hb_itemFastClear( &(HB_VM_STACK.Return) );
      }
   }

   ( &(HB_VM_STACK.Return) )->type = HB_IT_STRING;
   ( &(HB_VM_STACK.Return) )->item.asString.bStatic = TRUE;
   ( &(HB_VM_STACK.Return) )->item.asString.value   = szText;
   ( &(HB_VM_STACK.Return) )->item.asString.length  = ulLen;

}

BYTE HB_EXPORT hb_itemParamId( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   PHB_ITEM *pBase = HB_VM_STACK.pBase + 1;
   PHB_ITEM *pTop;
   BYTE iId = 1;


   if( hb_stackBaseItem()->item.asSymbol.paramcnt < 256 )
   {
      pTop = pBase + hb_stackBaseItem()->item.asSymbol.paramcnt + 1;
   }
   else
   {
      pTop = pBase + hb_stackBaseItem()->item.asSymbol.paramcnt + 1 - 256;
   }

   while( pBase < pTop )
   {
     if( *pBase == pItem )
     {
        //printf( "\nId: %i", iId );
        return iId;
     }
     pBase++;iId++;
   }

   return 0;
}

HB_EXTERN_END
