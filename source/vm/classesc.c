/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base-routines for OOPS system
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
 *  2007-04-11 08:56 UTC-0700 Andres Reyes <andresreyes_mzt/at/yahoo.com.mx>
 *    - Keep in this file only Functions for Compatibility for older versions
 *      of xharbour
 *    - This file only will be linked if neccesary
 *
 *    __CLS_PARAM
 *    __CLS_PAR00
 */

/* NOTE: Used by the preprocessor to implement Classy compatibility to Harbour
        Receive an variable number of param and return an array of it.
        No param will return a NULL array */

#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbapiitm.h"
#include "classes.h"

#ifdef HB_THREAD_SUPPORT
#include "thread.h"
#endif

HB_FUNC( __CLS_PARAM )
{
   HB_THREAD_STUB_API

   HB_ITEM_NEW( Array );
   USHORT   uiParam = ( USHORT ) hb_pcount();
   USHORT   n;

   if( uiParam >= 1 )
   {
      hb_arrayNew( &Array, uiParam );

      for( n = 1; n <= uiParam; n++ )
      {
         hb_arraySet( &Array, n, hb_param( n, HB_IT_ANY ) );
      }
   }
   else
   {
      hb_arrayNew( &Array, 1 );
      hb_itemPutCStatic( hb_arrayGetItemPtr( &Array, 1 ), ( char * ) "HBObject" );
   }

   hb_itemReturnForward( &Array );
}

/* This one is used when HB_NOTOBJECT is defined before HBCLASS.CH
 * it will avoid any default object to be inherited
 */

HB_FUNC( __CLS_PAR00 )
{
   HB_THREAD_STUB_API

   HB_ITEM  Array;
   USHORT   uiParam = ( USHORT ) hb_pcount();
   USHORT   n;

   Array.type = HB_IT_NIL;
   hb_arrayNew( &Array, uiParam );

   for( n = 1; n <= uiParam; n++ )
   {
      hb_arraySet( &Array, n, hb_param( n, HB_IT_ANY ) );
   }

   hb_itemReturnForward( &Array );
}

USHORT hb_objGetClass( PHB_ITEM pItem )
{
   return ( pItem && HB_IS_ARRAY( pItem ) ) ? pItem->item.asArray.value->uiClass : 0;
}

