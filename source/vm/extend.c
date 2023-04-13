/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Extend API
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
 *    hb_retnlen()
 *    hb_retnilen()
 *    hb_retnllen()
 *    hb_retndlen()
 *    hb_retdl()
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 *    hb_retd()
 *
 * Copyright 2002 Marek Paliwoda <paliwoda@inetia.pl>
 *    hb_parptr()
 *    hb_retptr()
 *
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *    hb_pardts()
 *    hb_pardtsbuff()
 *    hb_part()
 *    hb_pardtsec()
 *    hb_pardtd()
 *    hb_retdts()
 *    hb_retdt()
 *    hb_retdtd()
 *    hb_retdtl()
 *    hb_stordts()
 *    hb_stord()
 *    hb_stordt()
 *    hb_stordtl()
 *    hb_stordtd()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbset.h"
#include "hbdate.h"
#include "thread.h"

/* NOTE: iParam = -1 can be used to access the return value. */
/* NOTE: iParam = 0 can be used to access the SELF object. */

PHB_ITEM hb_param( int iParam, long lMask )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_param(%d, %ld)", iParam, lMask ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam  );

      if( pItem->type & HB_IT_BYREF )
      {
         pItem = hb_itemUnRef( pItem );

         if( ( HB_TYPE ) lMask == HB_IT_BYREF )
            return pItem;
      }

      if( ( HB_TYPE ) lMask == HB_IT_ANY || pItem->type & ( HB_TYPE ) lMask )
         return pItem;
      else
      {
         if( ( HB_TYPE ) lMask == HB_IT_NUMERIC && HB_IS_NUMERIC( pItem ) )
            return pItem;
      }
   }

   return NULL;
}

PHB_ITEM  hb_paramError( int iParam )
{
   static HB_ITEM s_NIL;

   PHB_ITEM       pParam = hb_param( iParam, HB_IT_ANY );

   if( pParam == NULL )
   {
      ( &s_NIL )->type  = HB_IT_NIL;
      pParam            = &s_NIL;
   }

   return pParam;
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an array item */

BOOL hb_extIsArray( int iParam )
{
   HB_THREAD_STUB_ANY

   if( iParam == -1 )
      return HB_IS_ARRAY( hb_stackReturnItem() );

   else if( iParam >= 0 && iParam <= hb_pcount() )
      return HB_IS_ARRAY( hb_stackItemFromBase( iParam ) );

   else
      return FALSE;
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an object item */

BOOL hb_extIsObject( int iParam )
{
   HB_THREAD_STUB_ANY

   if( iParam == -1 )
      return HB_IS_OBJECT( hb_stackReturnItem() );
   else if( iParam >= 0 && iParam <= hb_pcount() )
      return HB_IS_OBJECT( hb_stackItemFromBase( iParam ) );

   else
      return FALSE;
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * hb_parc( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parc(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetCPtr( pItem, ulArrayIndex );
      }
   }

   return NULL;
}

const char * hb_parcx( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parcx(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         char *   szElement;
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex   = va_arg( va, ULONG );
         va_end( va );

         szElement      = hb_arrayGetCPtr( pItem, ulArrayIndex );

         if( szElement )
            return szElement;
      }
   }

   return "";
}

HB_SIZE hb_parclen( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parclen(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.length;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetCLen( pItem, ulArrayIndex );
      }
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

HB_SIZE  hb_parcsiz( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parcsiz(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      /* NOTE: hb_parcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. [vszakats] */

      if( HB_IS_BYREF( pItem ) )
      {
         pItem = hb_itemUnRef( pItem );

         if( HB_IS_STRING( pItem ) )
            return pItem->item.asString.length + 1;
         else if( HB_IS_ARRAY( pItem ) )
         {
            va_list  va;
            ULONG    ulArrayIndex;

            va_start( va, iParam );
            ulArrayIndex = va_arg( va, ULONG );
            va_end( va );

            return hb_arrayGetCLen( pItem, ulArrayIndex ) + 1;
         }
      }
   }

   return 0;
}

/* NOTE: Using HB_VM_STACK.szDate as a temporary date buffer guaranties
         good behavior when multithreading. */

const char * hb_pards( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pards(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_dateDecStr( hb_stackDateBuffer(), pItem->item.asDate.value );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDS( pItem, ulArrayIndex, hb_stackDateBuffer() );
      }
   }

   return hb_dateDecStr( hb_stackDateBuffer(), 0 );
}

/* NOTE: Using HB_VM_STACK.szDate as a temporary date buffer guaranties
         good behavior when multithreading. */

char * hb_pardts( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pardts(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_datetimeDecStr( hb_stackDateBuffer(), pItem->item.asDate.value, pItem->item.asDate.time );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDTS( pItem, ulArrayIndex, hb_stackDateBuffer() );
      }
   }

   return hb_datetimeDecStr( hb_stackDateBuffer(), 0, 0 );
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char  * hb_pardsbuff( char * szDate, int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pardsbuff(%p, %d, ...)", szDate, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_dateDecStr( szDate, pItem->item.asDate.value );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDS( pItem, ulArrayIndex, szDate );
      }
   }

   return hb_dateDecStr( szDate, 0 );
}

/* retrieve a date as long integer - number of days from Julian's day */

long hb_pardl( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pardl(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return pItem->item.asDate.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDL( pItem, ulArrayIndex );
      }
   }

   return hb_itemGetDL( NULL );
}

/* NOTE: szDateTime must be a 26 chars wide buffer. [walter negro] */

char * hb_pardtsbuff( char * szDateTime, int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pardtsbuff(%p, %d, ...)", szDateTime, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_datetimeDecStr( szDateTime, pItem->item.asDate.value, pItem->item.asDate.time );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDTS( pItem, ulArrayIndex, szDateTime );
      }
   }

   return hb_datetimeDecStr( szDateTime, 0, 0 );
}

/* retrieve a time as LONG - number of seconds in the time part */

long hb_part( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_part(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_itemGetT( pItem );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetT( pItem, ulArrayIndex );
      }
   }

   return hb_itemGetT( NULL );
}

/* retrieve a datetime as double - number of seconds from Julian's day plus time */

double hb_pardtsec( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pardtd(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_itemGetDTsec( pItem );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDTsec( pItem, ulArrayIndex );
      }
   }

   return hb_itemGetDTsec( NULL );
}

/* retrieve a datetime as double - number of days from Julian's day plus time in decimal part of day*/

double hb_pardtd( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pardtd(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_itemGetDTD( pItem );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDTD( pItem, ulArrayIndex );
      }
   }

   return hb_itemGetDTD( NULL );
}

int  hb_parl( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parl(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;
      else if( HB_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value != 0 ? 1 : 0;
      else if( HB_IS_LONG( pItem ) )
         return pItem->item.asLong.value != 0 ? 1 : 0;
      else if( HB_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value != 0.0 ? 1 : 0;
      else if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
         return ( BYTE ) pItem->item.asString.value[ 0 ];
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetL( pItem, ulArrayIndex ) ? 1 : 0;
      }
   }

   return 0;
}

double  hb_parnd( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parnd(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( double ) pItem->item.asInteger.value;
      else if( HB_IS_LONG( pItem ) )
         return ( double ) pItem->item.asLong.value;
      else if( HB_IS_LOGICAL( pItem ) )
         return ( double ) pItem->item.asLogical.value;
      else if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
         return ( double ) ( BYTE ) pItem->item.asString.value[ 0 ];
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetND( pItem, ulArrayIndex );
      }
   }

   return 0;
}

int  hb_parni( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parni(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;
      else if( HB_IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;
      else if( HB_IS_DOUBLE( pItem ) )
         return ( int ) pItem->item.asDouble.value;
      else if( HB_IS_LOGICAL( pItem ) )
         return ( int ) pItem->item.asLogical.value;
      else if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
         return ( int ) ( BYTE ) pItem->item.asString.value[ 0 ];
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNI( pItem, ulArrayIndex );
      }
   }

   return 0;
}

long hb_parnl( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parnl(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( long ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#ifdef __GNUC__
         return ( long ) ( ULONG ) pItem->item.asDouble.value;
#else
         return ( long ) pItem->item.asDouble.value;
#endif
      else if( HB_IS_DATETIME( pItem ) )
         return ( long ) pItem->item.asDate.value;
      else if( HB_IS_LOGICAL( pItem ) )
         return ( long ) pItem->item.asLogical.value;
      else if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
         return ( long ) ( BYTE ) pItem->item.asString.value[ 0 ];
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNL( pItem, ulArrayIndex );
      }
   }

   return 0;
}

HB_LONG hb_parnint( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parnl(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_LONG ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_LONG ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#ifdef __GNUC__
         return ( HB_LONG ) ( HB_ULONG ) pItem->item.asDouble.value;
#else
         return ( HB_LONG ) pItem->item.asDouble.value;
#endif
      else if( HB_IS_DATETIME( pItem ) )
         return ( HB_LONG ) pItem->item.asDate.value;
      else if( HB_IS_LOGICAL( pItem ) )
         return ( HB_LONG ) pItem->item.asLogical.value;
      else if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
         return ( HB_LONG ) ( BYTE ) pItem->item.asString.value[ 0 ];
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNInt( pItem, ulArrayIndex );
      }
   }

   return 0;
}

/* NEW function - to retrieve a pointer from a harbour level */
void * hb_parptr( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parptr(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_POINTER( pItem ) )
         return pItem->item.asPointer.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetPtr( pItem, ulArrayIndex );
      }
   }

   return ( void * ) 0;
}

void * hb_parptrGC( PHB_GARBAGE_FUNC pFunc, int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parptrGC(%p,%d, ...)", pFunc, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_POINTER( pItem ) )
      {
         if( pItem->item.asPointer.collect && hb_gcFunc( pItem->item.asPointer.value ) == pFunc )
            return pItem->item.asPointer.value;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex   = va_arg( va, ULONG );
         va_end( va );

         pItem          = hb_arrayGetItemPtr( pItem, ulArrayIndex );

         if( pItem && HB_IS_POINTER( pItem ) && pItem->item.asPointer.collect && hb_gcFunc( pItem->item.asPointer.value ) == pFunc )
            return pItem->item.asPointer.value;
      }
   }

   return NULL;
}

HB_SIZE  hb_parinfa( int iParamNum, HB_SIZE uiArrayIndex )
{
   PHB_ITEM pArray;

   HB_TRACE( HB_TR_DEBUG, ( "hb_parinfa(%d, %lu)", iParamNum, uiArrayIndex ) );

   pArray = hb_param( iParamNum, HB_IT_ARRAY );

   if( pArray )
   {
      if( uiArrayIndex == 0 )
         return pArray->item.asArray.value->ulLen;
      else
         return ( ULONG ) hb_arrayGetType( pArray, uiArrayIndex );
   }
   else
      return 0;
}

HB_SIZE  hb_parinfo( int iParam )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_parinfo(%d)", iParam ) );

   if( iParam == 0 )
      return ( ULONG ) hb_pcount();
   else
   {
      if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
      {
         HB_TYPE uiType = ( iParam == -1 ) ? hb_stackReturnItem()->type : ( hb_stackItemFromBase( iParam ) )->type;

         if( uiType & HB_IT_BYREF )
         {
            PHB_ITEM pItem = hb_itemUnRef( ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam ) );

            if( pItem )
               uiType |= pItem->type;
         }

         return ( ULONG ) uiType;
      }
      else
         return 0;
   }
}

#undef hb_pcount
int hb_pcount( void )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_pcount()" ) );

   return ( int ) ( hb_stackBaseItem()->item.asSymbol.pCargo->arguments );
}

#undef hb_ret
void hb_ret( void )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_ret()" ) );

   if( HB_IS_COMPLEX( hb_stackReturnItem() ) )
      hb_itemClear( hb_stackReturnItem() );
   else
      ( hb_stackReturnItem() )->type = HB_IT_NIL;
}

/* JC1: tunrning off optimization; from now on, HB_VM_STACK is referenced just once */
#if defined( HB_THREAD_SUPPORT )
   #undef HB_VM_STACK
   #define HB_VM_STACK ( *hb_threadGetCurrentStack() )
#endif

#undef hb_reta
void hb_reta( HB_SIZE ulLen )  /* undocumented hb_reta() */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_reta(%lu)", ulLen ) );

   hb_arrayNew( hb_stackReturnItem(), ulLen );
}

#undef hb_retc
void hb_retc( const char * szText )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retc(%s)", szText ) );

   hb_itemPutC( hb_stackReturnItem(), szText );
}

#undef hb_retc_null
void hb_retc_null( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retc_null()" ) );

   hb_itemPutC( hb_stackReturnItem(), NULL );
}

#undef hb_retclen
void hb_retclen( const char * szText, HB_SIZE ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retclen(%s, %lu)", szText, ulLen ) );

   hb_itemPutCL( hb_stackReturnItem(), szText, ulLen );
}

/* szDate must have YYYYMMDD format */

#undef hb_retds
void hb_retds( const char * szDate )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retds(%s)", szDate ) );

   hb_itemPutDS( hb_stackReturnItem(), szDate );
}

/* szDate must have YYYYMMDDHHMMSS.CCC format */

#undef hb_retdts
void hb_retdts( const char * szDateTime )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retdts(%s)", szDateTime ) );

   hb_itemPutDTS( hb_stackReturnItem(), szDateTime );
}

#undef hb_retd
void hb_retd( int iYear, int iMonth, int iDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retd(%04i, %02i, %02i)", iYear, iMonth, iDay ) );

   hb_itemPutD( hb_stackReturnItem(), iYear, iMonth, iDay );
}

#undef hb_retdt
void hb_retdt( int iYear, int iMonth, int iDay, int iHour, int iMin, double dSec, int iAmPm )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retdt(%04i, %02i, %02i, %02i, %02i, %f, %d)", iYear, iMonth, iDay, iHour, iMin, dSec, iAmPm ) );

   hb_itemPutDT( hb_stackReturnItem(), iYear, iMonth, iDay, iHour, iMin, dSec, iAmPm );
}

#undef hb_retdl
void hb_retdl( long lJulian )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retdl(%ld)", lJulian ) );

   hb_itemPutDL( hb_stackReturnItem(), lJulian );
}

#undef hb_retdtd
void hb_retdtd( double dDateTime )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retdtd(%f)", dDateTime ) );

   hb_itemPutDTD( hb_stackReturnItem(), dDateTime );
}

#undef hb_retdtl
void hb_retdtl( long lDate, long lTime )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retdtl(%ld,%ld)", lDate, lTime ) );

   hb_itemPutDTL( hb_stackReturnItem(), lDate, lTime );
}

#undef hb_retl
void hb_retl( int iLogical )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retl(%d)", iLogical ) );

   hb_itemPutL( hb_stackReturnItem(), iLogical ? TRUE : FALSE );
}

#undef hb_retnd
void hb_retnd( double dNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnd(%lf)", dNumber ) );

   hb_itemPutND( hb_stackReturnItem(), dNumber );
}

#undef hb_retni
void hb_retni( int iNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retni(%d)", iNumber ) );

   hb_itemPutNI( hb_stackReturnItem(), iNumber );
}

#undef hb_retnl
void hb_retnl( long lNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnl(%ld)", lNumber ) );

   hb_itemPutNL( hb_stackReturnItem(), lNumber );
}

#undef hb_retnlen
void hb_retnlen( double dNumber, int iWidth, int iDec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnlen(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   hb_itemPutNLen( hb_stackReturnItem(), dNumber, iWidth, iDec );
}

#undef hb_retndlen
void hb_retndlen( double dNumber, int iWidth, int iDec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retndlen(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   hb_itemPutNDLen( hb_stackReturnItem(), dNumber, iWidth, iDec );
}

#undef hb_retnilen
void hb_retnilen( int iNumber, int iWidth )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnilen(%d, %d)", iNumber, iWidth ) );

   hb_itemPutNILen( hb_stackReturnItem(), iNumber, iWidth );
}

#undef hb_retnllen
void hb_retnllen( long lNumber, int iWidth )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnllen(%ld, %d)", lNumber, iWidth ) );

   hb_itemPutNLLen( hb_stackReturnItem(), lNumber, iWidth );
}

/* NEW function - to return a pointer to a harbour level */
#undef hb_retptr
void hb_retptr( void * voidPtr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retptr(%p)", voidPtr ) );

   hb_itemPutPtr( hb_stackReturnItem(), voidPtr );
}

#undef hb_retptrGC
void hb_retptrGC( void * voidPtr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retptrGC(%p)", voidPtr ) );

   hb_itemPutPtrGC( hb_stackReturnItem(), voidPtr );
}

#undef hb_retnint
void hb_retnint( HB_LONG lNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnint(%" PFHL "d)", lNumber ) );

   hb_itemPutNInt( hb_stackReturnItem(), lNumber );
}

#undef hb_retnintlen
void hb_retnintlen( HB_LONG lNumber, int iWidth )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnintlen(%" PFHL "d, %d)", lNumber, iWidth ) );

   hb_itemPutNIntLen( hb_stackReturnItem(), lNumber, iWidth );
}

int hb_stor( int iParam )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stor(%d)", iParam ) );

   if( iParam == -1 )
   {
      hb_itemClear( hb_stackReturnItem() );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemClear( hb_itemUnRef( pItem ) );
         return 1;
      }
   }

   return 0;
}

void hb_storc( const char * szText, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_storc(%s, %d, ...)", szText, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;
         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutC( pDstItem, szText );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutC( pItem, szText );
   }
}

void hb_storclen( const char * szText, HB_SIZE ulLen, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_storclen(%s, %lu, %d, ...)", szText, ulLen, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;
         va_start( va, iParam );

         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutCL( pDstItem, szText, ulLen );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutCL( pItem, szText, ulLen );
   }
}

int hb_storclenAdopt( char * szText, HB_SIZE ulLen, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_storclenAdopt(%s, %lu, %d, ...)", szText, ulLen, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutCPtr( pDstItem, szText, ulLen );
         va_end( va );
         return pDstItem ? 1 : 0;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutCPtr( pItem, szText, ulLen );
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

void hb_stords( const char * szDate, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stords(%s, %d, ...)", szDate, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutDS( pDstItem, szDate );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutDS( pItem, szDate );
   }
}

/* szDate must have YYYYMMDDHHMMSS.CCC format */

void hb_stordts( const char * szDateTime, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stordts(%s, %d, ...)", szDateTime, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutDTS( pDstItem, szDateTime );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutDTS( pItem, szDateTime );
   }
}

void hb_stordl( long lJulian, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stordl(%ld, %d, ...)", lJulian, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutDL( pDstItem, lJulian );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutDL( pItem, lJulian );
   }
}

void hb_stordtd( double dDateTime, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stordtd(%f, %d, ...)", dDateTime, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutDTD( pDstItem, dDateTime );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutDTD( pItem, dDateTime );
   }
}

void hb_stordtl( long lDate, long lTime, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stordtl(%ld, %ld, %d, ...)", lDate, lTime, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutDTL( pDstItem, lDate, lTime );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutDTL( pItem, lDate, lTime );
   }
}

void hb_stord( int iYear, int iMonth, int iDay, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stord(%d, %d, %d, %d, ...)", iYear, iMonth, iDay, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutD( pDstItem, iYear, iMonth, iDay );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutD( pItem, iYear, iMonth, iDay );
   }
}

void hb_stordt( int iYear, int iMonth, int iDay, int iHour, int iMin, double dSec, int iAmPm, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stord(%d, %d, %d, %d, %d, %f, %d, %d, ...)", iYear, iMonth, iDay, iHour, iMin, dSec, iAmPm, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutDT( pDstItem, iYear, iMonth, iDay, iHour, iMin, dSec, iAmPm );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutDT( pItem, iYear, iMonth, iDay, iHour, iMin, dSec, iAmPm );
   }
}

void hb_storl( int iLogical, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_storl(%d, %d, ...)", iLogical, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutL( pDstItem, iLogical ? TRUE : FALSE );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutL( pItem, iLogical ? TRUE : FALSE );
   }
}

void hb_storni( int iValue, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_storni(%d, %d, ...)", iValue, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutNI( pDstItem, iValue );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutNI( pItem, iValue );
   }
}

void hb_stornl( long lValue, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stornl(%ld, %d, ...)", lValue, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutNL( pDstItem, lValue );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutNL( pItem, lValue );
   }
}

void hb_stornint( HB_LONG lValue, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stornint(%" PFHL "d, %d, ...)", lValue, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         PHB_ITEM pItemNew = hb_itemPutNInt( NULL, lValue );

         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
         hb_itemRelease( pItemNew );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutNInt( pItem, lValue );
   }
}

void hb_stornd( double dNumber, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stornd(%lf, %d, ...)", dNumber, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutND( pDstItem, dNumber );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutND( pItem, dNumber );
   }
}

void hb_storptr( void * pointer, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_storptr(%p, %d, ...)", pointer, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutPtr( pDstItem, pointer );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutPtr( pItem, pointer );
   }
}

#ifndef HB_LONG_LONG_OFF
/* LONGLONG support */
void hb_stornll( LONGLONG llNumber, int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stornll(%" PFLL "d, %d, ...)", llNumber, iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL     bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         PHB_ITEM pDstItem;
         va_list  va;

         va_start( va, iParam );
         pDstItem = hb_arrayGetItemPtr( pItem, va_arg( va, ULONG ) );
         if( pDstItem )
            hb_itemPutNLL( pDstItem, llNumber );
         va_end( va );
      }
      else if( bByRef || iParam == -1 )
         hb_itemPutNLL( pItem, llNumber );
   }
}

LONGLONG  hb_parnll( int iParam, ... )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_parnll(%d, ...)", iParam ) );

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DOUBLE( pItem ) )
#ifdef __GNUC__
         return ( LONGLONG ) ( ULONGLONG ) pItem->item.asDouble.value;
#else
         return ( LONGLONG ) pItem->item.asDouble.value;
#endif
      else if( HB_IS_INTEGER( pItem ) )
         return ( LONGLONG ) pItem->item.asInteger.value;
      else if( HB_IS_LONG( pItem ) )
         return ( LONGLONG ) pItem->item.asLong.value;
      else if( HB_IS_LOGICAL( pItem ) )
         return ( LONGLONG ) pItem->item.asLogical.value;
      else if( HB_IS_DATETIME( pItem ) )
         return ( LONGLONG ) pItem->item.asDate.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         ULONG    ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNLL( pItem, ulArrayIndex );
      }
   }

   return 0;
}

#undef hb_retnll
void hb_retnll( LONGLONG llNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnll(%" PFLL "d)", llNumber ) );

   hb_itemPutNLL( hb_stackReturnItem(), llNumber );
}

#undef hb_retnlllen
void hb_retnlllen( LONGLONG llNumber, int iWidth )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_retnlllen(%" PFLL "d, %d)", llNumber, iWidth ) );

   hb_itemPutNLLLen( hb_stackReturnItem(), llNumber, iWidth );
}
#endif

#undef hb_retns
void hb_retns( HB_ISIZ nNumber )
{
#if 0
   HB_THREAD_STUB_ANY
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_retns(%" HB_PFS "d )", nNumber ) );

   hb_itemPutNS( hb_stackReturnItem(), nNumber );
}

HB_ISIZ hb_parns( int iParam, ... )
{
#if 0
   HB_THREAD_STUB_ANY
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_parns(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_ISIZ ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_ISIZ ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
         return ( HB_ISIZ ) pItem->item.asDouble.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list  va;
         HB_SIZE  nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetNS( pItem, nArrayIndex );
      }
      else if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
         return ( HB_ISIZ ) ( BYTE ) pItem->item.asString.value[ 0 ];      
   }

   return 0;
}

int hb_storns( HB_ISIZ nValue, int iParam, ... )
{
#if 0
   HB_THREAD_STUB_ANY
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_storvns(%" HB_PFS "d, %d, ...)", nValue, iParam ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem    = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL  bByRef   = HB_IS_BYREF( pItem );

      if( bByRef )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int      iRetVal;
         va_list  va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNS( pItem, va_arg( va, HB_SIZE ), nValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNS( pItem, nValue );
         return 1;
      }
   }

   return 0;
}


BOOL hb_partdt( long * plJulian, long * plMilliSec, int iParam )
{

   HB_TRACE( HB_TR_DEBUG, ( "hb_partdt(%p,%p,%d)", plJulian, plMilliSec, iParam ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
      {
         *plJulian = pItem->item.asDate.value;
         *plMilliSec = pItem->item.asDate.time;
         return TRUE;
      }
   }

   return FALSE;
}
