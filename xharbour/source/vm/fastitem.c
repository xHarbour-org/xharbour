/*
 * $Id: fastitem.c,v 1.12 2002/01/20 19:18:20 andijahja Exp $
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
 */

#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"

/* Forward decalarations. */
void hb_itemForwardValue( PHB_ITEM pDest, PHB_ITEM pSource );

void hb_itemPushForward( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPushForward(%p)", pItem));

   hb_itemForwardValue( hb_stackTopItem(), pItem );
   hb_stackPush();
}

void hb_itemForwardValue( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_itemForwardValue(%p, %p)", pDest, pSource ) );

   if( pDest == pSource )
   {
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemFastCopy()", NULL );
   }

   if( pDest->type )
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

   /* Now fake clear the transferer. */
   //pSource->item.asString.bStatic = FALSE;
   pSource->type = HB_IT_NIL;
}

void hb_itemReleaseString( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemReleaseString(%p), '%s'", pItem, pItem->item.asString.value ) );

   if( pItem->item.asString.bStatic == FALSE )
   {
      if( --*( pItem->item.asString.puiHolders ) == 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.puiHolders ) );
         hb_xfree( pItem->item.asString.puiHolders );
         HB_TRACE( HB_TR_DEBUG, ( "Will FREE %p", pItem->item.asString.value ) );
         hb_xfree( pItem->item.asString.value );
      }
   }

   //pItem->item.asString.bStatic = FALSE;
   pItem->item.asString.value = NULL;
   //pItem->item.asString.length = 0;
}

void hb_itemClear( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemClear(%p)", pItem));

   if( HB_IS_STRING( pItem ) )
   {
      if( pItem->item.asString.value )
      {
         hb_itemReleaseString( pItem );
      }
   }
   else if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value )
   {
      if( ( pItem->item.asArray.value )->uiHolders && --( pItem->item.asArray.value )->uiHolders == 0 )
      {
         hb_arrayRelease( pItem );
      }
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

void hb_itemSwap( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_ITEM temp;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemSwap(%p, %p)", pItem1, pItem2));

   /*
   temp.type = HB_IT_NIL;
   hb_itemCopy( &temp, pItem2 );
   hb_itemCopy( pItem2, pItem1 );
   hb_itemCopy( pItem1, &temp );
   hb_itemClear( &temp );
   */

   memcpy( &temp, pItem2, sizeof( HB_ITEM ) );
   memcpy( pItem2, pItem1, sizeof( HB_ITEM ) );
   memcpy( pItem1, &temp, sizeof( HB_ITEM ) );
}

void hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", pDest, pSource));

   if( pDest == pSource )
   {
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemCopy()", NULL );
   }

   if( pDest->type )
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

   if( HB_IS_STRING( pSource ) && pSource->item.asString.bStatic == FALSE )
   {
      ++*( pSource->item.asString.puiHolders );
   }
   else if( HB_IS_ARRAY( pSource ) )
   {
      ( pSource->item.asArray.value )->uiHolders++;
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

PHB_ITEM hb_itemPutC( PHB_ITEM pItem, char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", pItem, szText));

   if( pItem )
   {
      hb_itemClear( pItem );
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   if( szText == NULL )
   {
      szText = "";
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.puiHolders = (USHORT*) hb_xgrab( sizeof( USHORT ) );
   *( pItem->item.asString.puiHolders ) = 1;
   pItem->item.asString.bStatic = FALSE;
   pItem->item.asString.length = strlen( szText );
   pItem->item.asString.value = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
   strcpy( pItem->item.asString.value, szText );

   return pItem;
}

PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCL(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      hb_itemClear( pItem );
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   if( szText == NULL )
   {
      szText = "";
      ulLen = 0;
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.puiHolders = (USHORT*) hb_xgrab( sizeof( USHORT ) );
   *( pItem->item.asString.puiHolders ) = 1;
   pItem->item.asString.bStatic = FALSE;
   pItem->item.asString.length = ulLen;
   pItem->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   hb_xmemcpy( pItem->item.asString.value, szText, ulLen );
   pItem->item.asString.value[ ulLen ] = '\0';

   return pItem;
}

PHB_ITEM hb_itemPutCPtr( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCPtr(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      hb_itemClear( pItem );
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }

   pItem->type = HB_IT_STRING;
   pItem->item.asString.puiHolders = (USHORT*) hb_xgrab( sizeof( USHORT ) );
   *( pItem->item.asString.puiHolders ) = 1;
   pItem->item.asString.bStatic = FALSE;
   pItem->item.asString.length = ulLen;
   pItem->item.asString.value = szText;
   pItem->item.asString.value[ ulLen ] = '\0';

   return pItem;
}

void hb_itemFastClear( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_itemFastClear(%p) Type: %i", pItem, pItem->type ) );

   if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value )
   {
      if( ( pItem->item.asArray.value )->uiHolders && --( pItem->item.asArray.value )->uiHolders == 0 )
      {
         hb_arrayRelease( pItem );
      }
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

   HB_TRACE(HB_TR_DEBUG, ( "DONE hb_itemFastClear(%p)", pItem ) );
}

void hb_itemPushStaticString( char * szText, ULONG length )
{
   PHB_ITEM pTop = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ( "hb_itemPushStaticString( \"%s\", %lu ) %p %p", szText, length, pTop, szText ) );

   pTop->type = HB_IT_STRING;
   //pTop->item.asString.puiHolders = (USHORT*) hb_xgrab( sizeof( USHORT ) );
   //*( pTop->item.asString.puiHolders ) = 1;
   pTop->item.asString.bStatic = TRUE;
   pTop->item.asString.length = length;
   pTop->item.asString.value = szText;

   hb_stackPush();
}

void hb_retcAdopt( char * szText )
{
   HB_TRACE( HB_TR_INFO, ("hb_retcAdopt(%s) %p", &hb_stack.Return, szText ) );

   if( &hb_stack.Return )
   {
      if( HB_IS_STRING( &hb_stack.Return ) )
      {
         hb_itemReleaseString( &hb_stack.Return );
      }
      else
      {
         hb_itemFastClear( &hb_stack.Return );
      }
   }

   ( &hb_stack.Return )->type = HB_IT_STRING;
   ( &hb_stack.Return )->item.asString.puiHolders = (USHORT*) hb_xgrab( sizeof( USHORT ) );
   *( ( &hb_stack.Return )->item.asString.puiHolders ) = 1;
   ( &hb_stack.Return )->item.asString.bStatic = FALSE;
   ( &hb_stack.Return )->item.asString.value = szText;
   ( &hb_stack.Return )->item.asString.length = strlen( szText );
}

void hb_retclenAdopt( char * szText, ULONG ulLen )
{
   szText[ulLen] = '\0';

   HB_TRACE( HB_TR_INFO, ("hb_retclenAdopt( %p, %lu ) %p \"%s\"", szText, ulLen, &hb_stack.Return, szText ) );

   if( &hb_stack.Return )
   {
      if( HB_IS_STRING( &hb_stack.Return ) )
      {
         hb_itemReleaseString( &hb_stack.Return );
      }
      else
      {
         hb_itemFastClear( &hb_stack.Return );
      }
   }

   ( &hb_stack.Return )->type = HB_IT_STRING;
   ( &hb_stack.Return )->item.asString.puiHolders = (USHORT*) hb_xgrab( sizeof( USHORT ) );
   *( ( &hb_stack.Return )->item.asString.puiHolders ) = 1;
   ( &hb_stack.Return )->item.asString.bStatic = FALSE;
   ( &hb_stack.Return )->item.asString.value = szText;
   ( &hb_stack.Return )->item.asString.length = ulLen;
}
