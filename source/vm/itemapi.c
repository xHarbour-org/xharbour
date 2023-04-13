/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Item API
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
 *    hb_itemPCount()
 *    hb_itemParamPtr()
 *    hb_itemPutDL()
 *    hb_itemPutNI()
 *    hb_itemGetDL()
 *    hb_itemGetNI()
 *    hb_itemGetCPtr()
 *    hb_itemGetCLen()
 *    hb_itemGetNLen()
 *    hb_itemPutNLen()
 *    hb_itemPutNDLen()
 *    hb_itemPutNILen()
 *    hb_itemPutNLLen()
 *    hb_itemPutD()
 *    hb_itemSetCMemo()
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    hb_itemStrCmp()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_itemStr(), hb_itemString(), and hb_itemValToStr().
 *
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *    hb_itemGetDTS()
 *    hb_itemGetT()
 *    hb_itemGetTsec()
 *    hb_itemGetDTsec()
 *    hb_itemGetDTD()
 *    hb_itemGetDTL()
 *    hb_itemGetD()
 *    hb_itemGetDT()
 *    hb_itemPutDTS()
 *    hb_itemPutDT()
 *    hb_itemPutDTL()
 *    hb_itemPutDTD()
 *    hb_itemPutDTsec()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#if ! defined( __DJGPP__ )
#  include <math.h>                                                                                      /* For log() */
#endif
#if ( defined( _MSC_VER ) || ( __BORLANDC__ > 0x410 ) || defined( __WATCOMC__ ) ) && ! defined( __DMC__ ) /* Use this only above Borland C++ 3.1 */
#  include <float.h>                                                                                     /* for _finite() and _isnan() */
#endif

#include <stdio.h>

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbmath.h"
#include "hashapi.h"

#ifndef HB_CDP_SUPPORT_OFF
#   include "hbapicdp.h"
#endif

#if defined( HB_OS_SUNOS )
#   include <ieeefp.h>
#endif

#ifdef HB_THREAD_SUPPORT
extern HB_CRITICAL_T hb_gcCollectionMutex;
#endif

PHB_ITEM hb_itemapiCheck( PHB_ITEM pItem )
{
   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   return pItem;
}

PHB_ITEM hb_itemNew( PHB_ITEM pNull )
{
#ifndef HB_NO_TRACE
   pNull = hb_gcGripGet( pNull );

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemNew(%p)", pNull ) );

   return pNull;
#else
   /* inline optimize by compiler */
   return pNull = hb_gcGripGet( pNull );
#endif
}

PHB_ITEM hb_itemParam( USHORT uiParam )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemParam(%hu)", uiParam ) );

   return hb_itemNew( hb_param( uiParam, HB_IT_ANY ) );
}

/* Internal Item API. Use this with care. */

PHB_ITEM hb_itemParamPtr( USHORT uiParam, long lMask )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemParamPtr(%hu, %ld)", uiParam, lMask ) );

   return hb_param( ( int ) uiParam, lMask );
}

USHORT hb_itemPCount( void )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPCount()" ) );

   return ( USHORT ) hb_pcount();
}

BOOL hb_itemRelease( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemRelease(%p)", pItem ) );

   if( pItem )
   {
      hb_gcGripDrop( pItem );
      return TRUE;
   }
   else
      return FALSE;
}

PHB_ITEM hb_itemArrayNew( HB_SIZE ulLen )
{
   PHB_ITEM pItem;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemArrayNew(%lu)", ulLen ) );

   pItem = hb_itemNew( NULL );

   hb_arrayNew( pItem, ulLen );

   return pItem;
}

PHB_ITEM hb_itemArrayGet( PHB_ITEM pArray, HB_SIZE ulIndex )
{
   PHB_ITEM pItem;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemArrayGet(%p, %lu)", pArray, ulIndex ) );

   pItem = hb_itemNew( NULL );

   if( pArray )
      hb_arrayGet( pArray, ulIndex, pItem );

   return pItem;
}

PHB_ITEM hb_itemArrayPut( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemArrayPut(%p, %lu, %p)", pArray, ulIndex, pItem ) );

   if( pArray )
      hb_arraySet( pArray, ulIndex, pItem );

   return pArray;
}

void hb_itemSetCMemo( PHB_ITEM pItem )
{
   if( pItem && HB_IS_STRING( pItem ) )
      pItem->type |= HB_IT_MEMOFLAG;
}

/* NOTE: The caller should free the pointer if it's not NULL. */

char * hb_itemGetC( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetC(%p)", pItem ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      char * szResult = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
      hb_xmemcpy( szResult, pItem->item.asString.value, ( size_t ) pItem->item.asString.length );
      szResult[ pItem->item.asString.length ] = '\0';

      return szResult;
   }

   return NULL;
}

/* NOTE: Caller should not modify the buffer returned by this function. */

char * hb_itemGetCPtr( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetCPtr(%p)", pItem ) );

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.value;

   return ( char * ) "";
}

HB_SIZE hb_itemGetCLen( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetCLen(%p)", pItem ) );

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.length;

   return 0;
}

HB_SIZE hb_itemCopyC( PHB_ITEM pItem, char * szBuffer, HB_SIZE ulLen )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemCopyC(%p, %s, %lu)", pItem, szBuffer, ulLen ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( ulLen == 0 || ulLen > pItem->item.asString.length )
         ulLen = pItem->item.asString.length;

      hb_xmemcpy( szBuffer, pItem->item.asString.value, ( size_t ) ulLen );

      return ulLen;
   }

   return 0;
}

BOOL hb_itemFreeC( char * szText )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemFreeC(%s)", szText ) );

   if( szText )
   {
      hb_xfree( szText );

      return TRUE;
   }

   return FALSE;
}

/* NOTE: Clipper is buggy and will not append a trailing zero, although
         the NG says that it will. Check your buffers, since what may have
         worked with Clipper could overrun the buffer with Harbour.
         The correct buffer size is 9 bytes: char szDate[ 9 ] */

char * hb_itemGetDS( PHB_ITEM pItem, char * szDate )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetDS(%p, %s)", pItem, szDate ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_dateDecStr( szDate, pItem->item.asDate.value );

   return hb_dateDecStr( szDate, 0 );
}

/* NOTE: The correct buffer size is 19 bytes: char szDate[ 19 ]
         [walter negro] */

char * hb_itemGetDTS( PHB_ITEM pItem, char * szDateTime )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetDTS(%p, %s)", szDateTime ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_datetimeDecStr( szDateTime, pItem->item.asDate.value, pItem->item.asDate.time );

   return hb_datetimeDecStr( szDateTime, 0, 0 );
}

long hb_itemGetDL( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetDL(%p)", pItem ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      return pItem->item.asDate.value;

   return 0;
}

long hb_itemGetT( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetT(%p)", pItem ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      return pItem->item.asDate.time;

   return 0;
}

double hb_itemGetTsec( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetTsec(%p)", pItem ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_timeL2Sec( pItem->item.asDate.time );

   return 0.0;
}

double hb_itemGetDTsec( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetDTsec(%p)", pItem ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_datetimePackInSec( pItem->item.asDate.value, pItem->item.asDate.time );

   return 0.0;
}

double hb_itemGetDTD( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetDTD(%p)", pItem ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_datetimePack( pItem->item.asDate.value, pItem->item.asDate.time );

   return 0.0;
}

void hb_itemGetDTL( PHB_ITEM pItem, long * plDate, long * plTime )
{
   long  lDate = 0;
   long  lTime = 0;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetDT2L(%p,%p,%p)", pItem, plDate, plTime ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
   {
      lDate = pItem->item.asDate.value;
      lTime = pItem->item.asDate.time;
   }

   if( plDate )
      *plDate = lDate;

   if( plTime )
      *plTime = lTime;
}

void hb_itemGetD( PHB_ITEM pItem, int * piYear, int * piMonth, int * piDay )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetD(%p,%p,%p,%p)", pItem, piYear, piMonth, piDay ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
      hb_dateDecode( pItem->item.asDate.value, piYear, piMonth, piDay );
   else
   {
      if( piYear )
         *piYear = 0;

      if( piMonth )
         *piMonth = 0;

      if( piDay )
         *piDay = 0;
   }
}

void hb_itemGetDT( PHB_ITEM pItem, int * piYear, int * piMonth, int * piDay, int * piHour, int * piMin, double * pdSec )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetDT(%p,%p,%p,%p,%p,%p,%p)", pItem, piYear, piMonth, piDay, piHour, piMin, pdSec ) );

   if( pItem && HB_IS_DATETIME( pItem ) )
   {
      hb_dateDecode( pItem->item.asDate.value, piYear, piMonth, piDay );
      hb_timeDecode( pItem->item.asDate.time, piHour, piMin, pdSec );
   }
   else
   {
      if( piYear )
         *piYear = 0;

      if( piMonth )
         *piMonth = 0;

      if( piDay )
         *piDay = 0;

      if( piHour )
         *piHour = 0;

      if( piMin )
         *piMin = 0;

      if( pdSec )
         *pdSec = 0;
   }
}

BOOL hb_itemGetL( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetL(%p)", pItem ) );

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_LOGICAL:
            return pItem->item.asLogical.value;

         case HB_IT_INTEGER:
            return pItem->item.asInteger.value != 0;

         case HB_IT_LONG:
            return pItem->item.asLong.value != 0;

         case HB_IT_DOUBLE:
            return pItem->item.asDouble.value != 0.0;

         case  HB_IT_STRING:
            if( pItem->item.asString.length == 1 )
               return ( BYTE ) pItem->item.asString.value[ 0 ];
      }
   }

   return FALSE;
}

double hb_itemGetND( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetND(%p)", pItem ) );

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_DOUBLE:
            return pItem->item.asDouble.value;

         case HB_IT_INTEGER:
            return ( double ) pItem->item.asInteger.value;

         case HB_IT_LONG:
            return ( double ) pItem->item.asLong.value;

         case HB_IT_DATE:
            return ( double ) pItem->item.asDate.value;

         case HB_IT_TIMEFLAG:
            return ( double ) hb_datetimePack( pItem->item.asDate.value, pItem->item.asDate.time );

         case HB_IT_LOGICAL:
            return ( double ) pItem->item.asLogical.value;

         case  HB_IT_STRING:
            if( pItem->item.asString.length == 1 )
               return ( double ) ( BYTE ) pItem->item.asString.value[ 0 ];
      }
   }

   return 0.0;
}

double hb_itemGetNDDec( PHB_ITEM pItem, int * piDec )
{
   double dNumber;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemGetNDDec(%p)", piDec ) );

   *piDec = 0;
   switch( pItem->type )
   {
      case HB_IT_INTEGER:
         dNumber = ( double ) pItem->item.asInteger.value;
         break;

      case HB_IT_LONG:
         dNumber = ( double ) pItem->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         *piDec   = ( int ) pItem->item.asDouble.decimal;
         dNumber  = pItem->item.asDouble.value;
         break;

      case HB_IT_DATE:
         dNumber = ( double ) pItem->item.asDate.value;
         break;

      case HB_IT_TIMEFLAG:
         *piDec   = 255;
         dNumber  = ( double ) hb_datetimePack( pItem->item.asDate.value, pItem->item.asDate.time );
         break;

      case HB_IT_STRING:
         dNumber = ( double ) ( BYTE ) pItem->item.asString.value[ 0 ];
         break;

      default:
         dNumber = 0.0;  /* To avoid GCC -O2 warning */
         hb_errInternal( HB_EI_VMPOPINVITEM, "hb_itemGetNDDec()", NULL, NULL );
         break;
   }

   return dNumber;
}

int hb_itemGetNI( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetNI(%p)", pItem ) );

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_INTEGER:
            return pItem->item.asInteger.value;

         case HB_IT_LONG:
            return ( int ) pItem->item.asLong.value;

         case HB_IT_DOUBLE:
            return ( int ) pItem->item.asDouble.value;

         case HB_IT_DATE:
         case HB_IT_TIMEFLAG:
            return ( int ) pItem->item.asDate.value;

         case HB_IT_LOGICAL:
            return ( int ) pItem->item.asLogical.value;

         case  HB_IT_STRING:
            if( pItem->item.asString.length == 1 )
               return ( int ) ( BYTE ) pItem->item.asString.value[ 0 ];
      }
   }

   return 0;
}

long hb_itemGetNL( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetNL(%p)", pItem ) );

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_LONG:
            return ( long ) pItem->item.asLong.value;

         case HB_IT_INTEGER:
            return ( long ) pItem->item.asInteger.value;

         case HB_IT_DOUBLE:
#if defined( __GNUC__ )
            return ( long ) ( HB_ULONG ) pItem->item.asDouble.value;
#else
            return ( long ) pItem->item.asDouble.value;
#endif

         case HB_IT_DATE:
         case HB_IT_TIMEFLAG:
            return ( long ) pItem->item.asDate.value;

         case HB_IT_LOGICAL:
            return ( long ) pItem->item.asLogical.value;

         case  HB_IT_STRING:
            if( pItem->item.asString.length == 1 )
               return ( long ) ( BYTE ) pItem->item.asString.value[ 0 ];
      }
   }

   return 0;
}

void * hb_itemGetPtr( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetPtr(%p)", pItem ) );

   if( pItem && HB_IS_POINTER( pItem ) )
      return pItem->item.asPointer.value;

   return NULL;
}

void * hb_itemGetPtrGC( PHB_ITEM pItem, PHB_GARBAGE_FUNC pFunc )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemGetPtrGC(%p,%p)", pItem, pFunc ) );

   if( pItem && HB_IS_POINTER( pItem ) && pItem->item.asPointer.collect && hb_gcFunc( pItem->item.asPointer.value ) == pFunc )
      return pItem->item.asPointer.value;

   return NULL;
}

PHB_SYMB hb_itemGetSymbol( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemGetSymbol(%p)", pItem ) );

   if( pItem && HB_IS_SYMBOL( pItem ) )
      return pItem->item.asSymbol.value;

   return NULL;
}

/* Defed out - using FastApi Version in source/vm/fastitem.c. */
#if 0
PHB_ITEM hb_itemReturn( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemReturn(%p)", pItem ) );

   if( pItem )
      hb_itemCopy( &( HB_VM_STACK.Return ), pItem );

   return pItem;
}
#endif

/* Internal Item API. Use this with care. */

PHB_ITEM hb_itemPutDate( PHB_ITEM pItem, long lDate )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDate(%p, %l)", pItem, lDate ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                = HB_IT_DATE;
   pItem->item.asDate.time    = 0;
   pItem->item.asDate.value   = lDate;

   return pItem;
}

PHB_ITEM hb_itemPutDS( PHB_ITEM pItem, const char * szDate )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDS(%p, %s)", pItem, szDate ) );

   return hb_itemPutDate( pItem, hb_dateEncStr( szDate ) );
}

PHB_ITEM hb_itemPutDTS( PHB_ITEM pItem, const char * szDateTime )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDTS(%p, %s)", pItem, szDateTime ) );

   pItem       = hb_itemPutDate( pItem, 0 );
   hb_datetimeEncStr( szDateTime, &pItem->item.asDate.value, &pItem->item.asDate.time );
   pItem->type = HB_IT_TIMEFLAG;

   return pItem;
}

PHB_ITEM hb_itemPutD( PHB_ITEM pItem, int iYear, int iMonth, int iDay )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutD(%p, %04i, %02i, %02i)", pItem, iYear, iMonth, iDay ) );

   return hb_itemPutDate( pItem, hb_dateEncode( iYear, iMonth, iDay ) );
}

PHB_ITEM hb_itemPutDT( PHB_ITEM pItem, int iYear, int iMonth, int iDay, int iHour, int iMin, double dSec, int iAmPm )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDT(%p, %04i, %02i, %02i, %02i, %02i, %f, %d)", pItem, iYear, iMonth, iDay, iHour, iMin, dSec, iAmPm ) );

   pItem       = hb_itemPutDate( pItem, 0 );
   hb_datetimeEncode( &pItem->item.asDate.value, &pItem->item.asDate.time,
                      iYear, iMonth, iDay, iHour, iMin, dSec, iAmPm, NULL );
   pItem->type = HB_IT_TIMEFLAG;

   return pItem;
}

PHB_ITEM hb_itemPutTDT( PHB_ITEM pItem, long lJulian, long lMilliSec )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutTDT(%p, %ld, %ld)", pItem, lJulian, lMilliSec ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                = HB_IT_TIMEFLAG;
   pItem->item.asDate.value   = lJulian;
   pItem->item.asDate.time    = lMilliSec;

   return pItem;
}

PHB_ITEM hb_itemPutDL( PHB_ITEM pItem, long lJulian )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDL(%p, %ld)", pItem, lJulian ) );

   return hb_itemPutDate( pItem, lJulian );
}

PHB_ITEM hb_itemPutDTL( PHB_ITEM pItem, long lDate, long lTime )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDTL(%p, %l, %l)", pItem, lDate, lTime ) );

   pItem                   = hb_itemPutDate( pItem, lDate );
   pItem->item.asDate.time = lTime % ( 86400 * HB_DATETIMEINSEC );
   pItem->type             = HB_IT_TIMEFLAG;

   return pItem;
}

PHB_ITEM hb_itemPutDTsec( PHB_ITEM pItem, double dDateTime )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDTsec(%p, %f)", pItem, dDateTime ) );

   pItem       = hb_itemPutDate( pItem, 0 );
   hb_datetimeUnpack( dDateTime / 86400, &pItem->item.asDate.value, &pItem->item.asDate.time );
   pItem->type = HB_IT_TIMEFLAG;

   return pItem;
}

PHB_ITEM hb_itemPutDTD( PHB_ITEM pItem, double dDateTime )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutDTD(%p, %f)", pItem, dDateTime ) );

   pItem       = hb_itemPutDate( pItem, 0 );
   hb_datetimeUnpack( dDateTime, &pItem->item.asDate.value, &pItem->item.asDate.time );
   pItem->type = HB_IT_TIMEFLAG;

   return pItem;
}

PHB_ITEM hb_itemPutL( PHB_ITEM pItem, BOOL bValue )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutL(%p, %d)", pItem, ( int ) bValue ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                   = HB_IT_LOGICAL;
   pItem->item.asLogical.value   = bValue;

   return pItem;
}

PHB_ITEM hb_itemPutND( PHB_ITEM pItem, double dNumber )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutND(%p, %lf)", pItem, dNumber ) );

   return hb_itemPutNDDec( pItem, dNumber, HB_DEFAULT_DECIMALS );
}

PHB_ITEM hb_itemPutNDDec( PHB_ITEM pItem, double dNumber, int iDec )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutND(%p, %lf)", pItem, dNumber ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                   = HB_IT_DOUBLE;
   pItem->item.asDouble.value    = dNumber;
   pItem->item.asDouble.length   = ( UINT ) HB_DBL_LENGTH( dNumber );
   pItem->item.asDouble.decimal  = ( UINT ) iDec;

   if( iDec == HB_DEFAULT_DECIMALS )
      pItem->item.asDouble.decimal = ( UINT ) hb_stackSetStruct()->HB_SET_DECIMALS;

   return pItem;
}

PHB_ITEM hb_itemPutNI( PHB_ITEM pItem, int iNumber )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNI(%p, %d)", pItem, iNumber ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = iNumber;
   pItem->item.asInteger.length  = HB_INT_LENGTH( iNumber );

   return pItem;
}

PHB_ITEM hb_itemPutNL( PHB_ITEM pItem, LONG lNumber )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNL(%p, %ld)", pItem, lNumber ) );

   pItem = hb_itemapiCheck( pItem );

   /*
#if HB_INT_MAX >= LONG_MAX
   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = ( int ) lNumber;
   pItem->item.asInteger.length  = ( UINT ) HB_INT_LENGTH( lNumber );
#else
   pItem->type                   = HB_IT_LONG;
   pItem->item.asLong.value      = ( HB_LONG ) lNumber;
   pItem->item.asLong.length     = ( UINT ) HB_LONG_LENGTH( lNumber );
#endif
*/
HB_ITEM_PUT_LONGRAW( pItem, lNumber );

   return pItem;
}

PHB_ITEM hb_itemPutNLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec ) );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_DBL_LENGTH( dNumber );

   if( iDec < 0 )
      iDec = hb_stackSetStruct()->HB_SET_DECIMALS;

   if( iDec == 0 )
   {
      HB_LONG lNumber = ( HB_LONG ) dNumber;

      if( ( double ) lNumber == dNumber )
      {
         if( HB_LIM_INT( lNumber ) )
            return hb_itemPutNILen( pItem, ( int ) lNumber, iWidth );
         else
#ifdef HB_LONG_LONG_OFF
            return hb_itemPutNLLen( pItem, ( long ) lNumber, iWidth );
#else
            return hb_itemPutNLLLen( pItem, ( LONGLONG ) lNumber, iWidth );
#endif
      }
   }

   return hb_itemPutNDLen( pItem, dNumber, iWidth, iDec );
}

PHB_ITEM hb_itemPutNDLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNDLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec ) );

   pItem = hb_itemapiCheck( pItem );

   if( iWidth <= 0 || iWidth > 99 )
   {
#if defined( __BORLANDC__ ) && ( __BORLANDC__ > 0x410 ) /* Use this only above Borland C++ 3.1 */
      /* Borland C compiled app crashes if a "NaN" double is compared with another double [martin vogel] */
      if( _isnan( dNumber ) )
         iWidth = 20;
      else
#endif
      iWidth = HB_DBL_LENGTH( dNumber );
   }

   if( iDec < 0 )
      iDec = hb_stackSetStruct()->HB_SET_DECIMALS;

   pItem->type                   = HB_IT_DOUBLE;
   pItem->item.asDouble.length   = ( UINT ) iWidth;
   pItem->item.asDouble.decimal  = ( UINT ) iDec;
   pItem->item.asDouble.value    = dNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNILen( PHB_ITEM pItem, int iNumber, int iWidth )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNILen(%p, %d, %d)", pItem, iNumber, iWidth ) );

   pItem = hb_itemapiCheck( pItem );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_INT_LENGTH( iNumber );

   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = iNumber;
   pItem->item.asInteger.length  = ( UINT ) iWidth;

   return pItem;
}

PHB_ITEM hb_itemPutNLLen( PHB_ITEM pItem, long lNumber, int iWidth )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNLLen(%p, %ld, %d)", pItem, lNumber, iWidth ) );

   pItem = hb_itemapiCheck( pItem );

#if HB_INT_MAX == LONG_MAX
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_INT_LENGTH( lNumber );

   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = ( int ) lNumber;
   pItem->item.asInteger.length  = ( UINT ) iWidth;
#else
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( lNumber );

   pItem->type                = HB_IT_LONG;
   pItem->item.asLong.value   = ( HB_LONG ) lNumber;
   pItem->item.asLong.length  = ( UINT ) iWidth;
#endif

   return pItem;
}

PHB_ITEM hb_itemPutSymbol( PHB_ITEM pItem, PHB_SYMB pSym )
{
   PHB_SYMBCARGO pSymCargo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutSymbol(%p,%p)", pItem, pSym ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                   = HB_IT_SYMBOL;
   pItem->item.asSymbol.value    = pSym;
   pSymCargo                     = ( PHB_SYMBCARGO ) hb_xgrab( sizeof( HB_SYMBCARGO ) );
   pSymCargo->stackbase          = 0;
   pSymCargo->privatesbase       = 0;
   pSymCargo->lineno             = 0;
   pSymCargo->uiSuperClass       = 0;
   pSymCargo->params             = 0;
   pSymCargo->locals             = 0;
   pSymCargo->arguments          = 0;
   pItem->item.asSymbol.pCargo   = pSymCargo;

   return pItem;
}

void hb_itemGetNLen( PHB_ITEM pItem, int * piWidth, int * piDecimal )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetNLen(%p, %p, %p)", pItem, piWidth, piDecimal ) );

   if( ! piDecimal && ! piWidth )
      return;

   if( pItem )
   {
      if( piDecimal )
         *piDecimal = 0;

      /* Only piDecimal? shoud be HB_IT_DOUBLE */
      if( ! piWidth && pItem->type != HB_IT_DOUBLE )
         return;

      switch( pItem->type )
      {
         case HB_IT_DOUBLE:
            if( piWidth )
               *piWidth = ( int ) pItem->item.asDouble.length;

            if( piDecimal )
               *piDecimal = ( int ) pItem->item.asDouble.decimal;

            break;

         case HB_IT_LONG:
            *piWidth = ( int ) pItem->item.asLong.length;
            break;

         case HB_IT_INTEGER:
            *piWidth = ( int ) pItem->item.asInteger.length;
            break;

         default:
            *piWidth = 0;
            break;
      }
   }
}

HB_SIZE hb_itemSize( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemSize(%p)", pItem ) );

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_ARRAY:
            return hb_arrayLen( pItem );

         case HB_IT_HASH:
            return hb_hashLen( pItem );

         case HB_IT_STRING:
            return pItem->item.asString.length;
      }
   }

   return 0;
}

HB_TYPE hb_itemType( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemType(%p)", pItem ) );

   return  pItem ? ( HB_TYPE ) pItem->type : HB_IT_NIL;
}

char * hb_itemTypeStr( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemTypeStr(%p)", pItem ) );

   switch( pItem->type )
   {
      case HB_IT_STRING:
      case HB_IT_STRING | HB_IT_NULL:
         return ( char * ) "C";

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
      case HB_IT_INTEGER | HB_IT_NULL:
      case HB_IT_LONG | HB_IT_NULL:
      case HB_IT_DOUBLE | HB_IT_NULL:
         return ( char * ) "N";

      case HB_IT_ARRAY:
      case HB_IT_ARRAY | HB_IT_NULL:
         return ( char * ) ( hb_arrayIsObject( pItem ) ? "O" : "A" );

      case HB_IT_BLOCK:
      case HB_IT_BLOCK | HB_IT_NULL:
         return ( char * ) "B";

      case HB_IT_DATE:
      case HB_IT_DATE | HB_IT_NULL:
         return ( char * ) "D";

      case HB_IT_TIMEFLAG:
      case HB_IT_DATETIME:
      case HB_IT_DATETIME | HB_IT_NULL:
         return ( char * ) "T";

      case HB_IT_LOGICAL:
      case HB_IT_LOGICAL | HB_IT_NULL:
         return ( char * ) "L";

      case HB_IT_MEMO:
         return ( char * ) "M";

      case HB_IT_POINTER:
      case HB_IT_POINTER | HB_IT_NULL:
         return ( char * ) "P";

      case HB_IT_HASH:
      case HB_IT_HASH | HB_IT_NULL:
         return ( char * ) "H";
   }

   return ( char * ) "U";
}

/* Internal API, not standard Clipper */
/* Defed out - using String Sharing Versions in source/vm/fastitem.c. */

#if 0
void hb_itemClear( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemClear(%p) Type: %i", pItem, pItem->type ) );

   if( HB_IS_STRING( pItem ) )
      hb_itemReleaseString( pItem );
   else if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value )
      if( ( pItem->item.asArray.value )->ulHolders && --( pItem->item.asArray.value )->ulHolders == 0 )
         hb_arrayRelease( pItem );
   else if( HB_IS_BLOCK( pItem ) )
      hb_codeblockDelete( pItem );
   else if( HB_IS_MEMVAR( pItem ) )
      hb_memvarValueDecRef( pItem->item.asMemvar.value );

   pItem->type = HB_IT_NIL;
}

void hb_itemSwap( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_ITEM temp;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemSwap(%p, %p)", pItem1, pItem2 ) );

   temp.type = HB_IT_NIL;
   hb_itemCopy( &temp, pItem2 );
   hb_itemCopy( pItem2, pItem1 );
   hb_itemCopy( pItem1, &temp );
   hb_itemClear( &temp );

/* Faster, but less safe way
   HB_MEMCPY( &temp, pItem2, sizeof( HB_ITEM ) );
   HB_MEMCPY( pItem2, pItem1, sizeof( HB_ITEM ) );
   HB_MEMCPY( pItem1, &temp, sizeof( HB_ITEM ) );
 */
}
#endif

/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRef( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemUnRef(%p)", pItem ) );

   while( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRefOnce( pItem );

   return pItem;
}

/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRefOnce( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemUnRefOnce(%p)", pItem ) );

   if( HB_IS_BYREF( pItem ) )
   {
      if( HB_IS_MEMVAR( pItem ) )
      {
         PHB_VALUE pValue;

         pValue   = *( pItem->item.asMemvar.itemsbase ) + pItem->item.asMemvar.offset + pItem->item.asMemvar.value;
         pItem    = pValue->pVarItem;
      }
      else if( HB_IS_EXTREF( pItem ) )
         pItem = pItem->item.asExtRef.func->read( pItem );
      else
      {
         if( pItem->item.asRefer.value >= 0 )
         {
            if( pItem->item.asRefer.offset == 0 )
            {
               if( pItem->item.asRefer.BasePtr.pBaseArray->pItems == NULL || pItem->item.asRefer.BasePtr.pBaseArray->ulLen <= ( HB_SIZE ) pItem->item.asRefer.value )
               {
                  HB_ITEM  Array;
                  HB_ITEM  Index;

                  Array.type                 = HB_IT_ARRAY;
                  Array.item.asArray.value   = pItem->item.asRefer.BasePtr.pBaseArray;

#ifdef HB_ARRAY_USE_COUNTER
                  HB_ATOMIC_INC( Array.item.asArray.value->ulHolders );
#else
                  hb_arrayRegisterHolder( Array.item.asArray.value, ( void * ) &Array );
#endif

                  Index.type = HB_IT_NIL;
                  hb_itemPutNL( &Index, ( const LONG ) pItem->item.asRefer.value + 1 );

                  hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, &Array, &Index );
               }

               /* a reference to an Array Member (like static var) */
               pItem = pItem->item.asRefer.BasePtr.pBaseArray->pItems + pItem->item.asRefer.value;
            }
            else
            {
               /* a reference to a local variable */
               PHB_ITEM * pLocal;
               pLocal   = *( pItem->item.asRefer.BasePtr.itemsbasePtr ) + pItem->item.asRefer.offset + pItem->item.asRefer.value;
               pItem    = *pLocal;
            }
         }
         else
            /* local variable referenced in a codeblock
             */
            pItem = hb_codeblockGetRef( pItem->item.asRefer.BasePtr.block, pItem );
      }
   }

   return pItem;
}

/* Internal API, not standard Clipper */
/* UnShare string buffer of given string item */

PHB_ITEM hb_itemUnShareString( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemUnShareString(%p)", pItem ) );

   if( pItem->item.asString.allocated == 0 ||
       *( pItem->item.asString.pulHolders ) > 1 )
   {
      HB_SIZE  ulLen             = pItem->item.asString.length + 1;
      char *   szText            = ( char * ) hb_xmemcpy( hb_xgrab( ulLen ),
                                                          pItem->item.asString.value, ( size_t ) ulLen );
      BOOL     bRecycleHolders   = FALSE;

      if( pItem->item.asString.allocated )
      {
         if( HB_ATOMIC_DEC( *( pItem->item.asString.pulHolders ) ) == 0 )
         {
            hb_xfree( pItem->item.asString.value );
            bRecycleHolders = TRUE;
         }
      }

      if( ! bRecycleHolders )
         pItem->item.asString.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );

      *( pItem->item.asString.pulHolders )   = 1;
      pItem->item.asString.value             = szText;
      pItem->item.asString.allocated         = ulLen;
   }
   pItem->type &= ~HB_IT_DEFAULT;

   return pItem;
}

/* Internal API, not standard Clipper */
/* UnShare string buffer of given item */

PHB_ITEM hb_itemUnShare( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemUnShare(%p)", pItem ) );

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   if( HB_IS_STRING( pItem ) )
      return hb_itemUnShareString( pItem );

   return pItem;
}

BOOL hb_itemGetWriteCL( PHB_ITEM pItem, char ** pszValue, HB_SIZE * pulLen )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetWriteCL(%p,%p,%p)", pItem, pszValue, pulLen ) );

   if( pItem )
   {
      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
      {
         hb_itemUnShareString( pItem );
         *pulLen     = pItem->item.asString.length;
         *pszValue   = pItem->item.asString.value;
         return TRUE;
      }
   }
   return FALSE;
}

/* Internal API, not standard Clipper */
/* clone the given item */
PHB_ITEM hb_itemClone( PHB_ITEM pItem )
{
   if( HB_IS_ARRAY( pItem ) )
      return hb_arrayClone( pItem, NULL );
   else if( HB_IS_HASH( pItem ) )
      return hb_hashClone( pItem, NULL );

   return hb_itemNew( pItem );
}

/* Internal API, not standard Clipper */

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact )
{
   HB_SIZE  ulLenFirst;
   HB_SIZE  ulLenSecond;
   HB_SIZE  ulMinLen;
   int      iRet = 0; /* Current status */
   char *   szFirst;
   char *   szSecond;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemStrCmp(%p, %p, %d)", pFirst, pSecond, ( int ) bForceExact ) );

   szFirst     = pFirst->item.asString.value;
   szSecond    = pSecond->item.asString.value;
   ulLenFirst  = pFirst->item.asString.length;
   ulLenSecond = pSecond->item.asString.length;

   if( szFirst == szSecond && ulLenFirst == ulLenSecond )
      return 0;
   
   if( ! bForceExact && hb_stackSetStruct()->HB_SET_EXACT )
   {
      /* SET EXACT ON and not using == */
      /* Don't include trailing spaces */
      while( ulLenFirst > ulLenSecond && szFirst[ ulLenFirst - 1 ] == ' ' )
         ulLenFirst--;
      while( ulLenSecond > ulLenFirst && szSecond[ ulLenSecond - 1 ] == ' ' )
         ulLenSecond--;
      bForceExact = TRUE;
   }

   ulMinLen = ulLenSecond;
   if( ulLenFirst < ulLenSecond )
   {
      ulMinLen = ulLenFirst;
   }

   /* Both strings not empty */
   if( ulMinLen )
   {
#ifndef HB_CDP_SUPPORT_OFF
      if( ( hb_cdppage() )->lSort )
         iRet = hb_cdpcmp( szFirst, ulLenFirst, szSecond, ulLenSecond,
                           hb_cdppage(), bForceExact );
      else
#endif
      {
         do
         {
            if( *szFirst != *szSecond )
            {
               iRet = 1;

               if( ( BYTE ) *szFirst < ( BYTE ) *szSecond )
                  iRet = -1;

               break;
            }
            szFirst++;
            szSecond++;
         }
         while( --ulMinLen );

         /* If equal and length is different ! */
         if( ! iRet && ulLenFirst != ulLenSecond )
         {
            /* Force an exact comparison? */
            if( bForceExact || ulLenSecond > ulLenFirst )
            {
               iRet = 1;

               if( ulLenFirst < ulLenSecond )
                  iRet = -1;
            }
         }
      }
   }
   else
   {
      /* Both empty ? */
      if( ulLenFirst != ulLenSecond )
      {
         if( bForceExact )
            iRet = ( ulLenFirst < ulLenSecond ) ? -1 : 1;
         else
            iRet = ( ulLenSecond == 0 ) ? 0 : -1;
         
      }
      /* Both empty => Equal!, iRet = 0 */
   }

   return iRet;
}

/* converts a numeric to a padded string iSize length and iDec number
   of digits after dot.
   Note: szResult has to be at least iSize + 1 length.
 */
BOOL hb_itemStrBuf( char * szResult, PHB_ITEM pNumber, int iSize, int iDec )
{
   register int   iPos  = iSize;
   register int   iDot  = 0;
   BOOL           fNeg;

   if( iDec < 0 )
      iDec = 0;
   else if( iDec > 0 )
   {
      iDot  = iSize - iDec - 1;
      iPos  = iDot;
   }

   if( HB_IS_DOUBLE( pNumber ) || HB_IS_DATETIME( pNumber ) )
   {
      double dNumber = hb_itemGetND( pNumber );

/* TODO: look if finite()/_finite() or isinf()/_isinf and isnan()/_isnan
   does exist for your compiler and add this to the check below */

#if defined( __RSXNT__ ) || defined( __EMX__ ) || \
      defined( __XCC__ ) || defined( __POCC__ ) || defined( __DMC__ ) || \
      defined( HB_OS_HPUX )
#  define HB_FINITE_DBL( d )  ( isfinite( d ) != 0 )
#elif defined( __WATCOMC__ ) || defined( __BORLANDC__ ) || defined( _MSC_VER )
#  define HB_FINITE_DBL( d )  ( _finite( d ) != 0 )
#elif defined( __GNUC__ ) || defined( __DJGPP__ ) || defined( __MINGW32__ ) || \
      defined( __LCC__ )
#  define HB_FINITE_DBL( d )  ( finite( d ) != 0 )
#else
      /* added infinity check for Borland C [martin vogel] */
      /* Borland C 5.5 has _finite() function, if it's necessary
         we can reenable this code for older DOS BCC versions
         Now this code is for generic C compilers undefined above
         [druzus] */
      static BOOL    s_bInfinityInit   = FALSE;
      static double  s_dInfinity       = 0;

      if( ! s_bInfinityInit )
      {
         /* set math handler to NULL for evaluating log(0),
            to avoid error messages [martin vogel]*/
         HB_MATH_HANDLERPROC fOldMathHandler = hb_mathSetHandler( NULL );
         s_dInfinity       = -log( ( double ) 0 );
         hb_mathSetHandler( fOldMathHandler );
         s_bInfinityInit   = TRUE;
      }
#  define HB_FINITE_DBL( d ) ( ( d ) != s_dInfinity && ( d ) != -s_dInfinity )
#endif

      /* I would like to know why finite() function is not used for MinGW instead
         of this hack with hb_snprintf and "#IND" if there are some important reasons
         for the code below reenable it and please add description WHY? [druzus] */
      /*
         #elif defined(__MINGW32__)
       || dNumber == s_dInfinity || dNumber == -s_dInfinity ||
            ( hb_snprintf( szResult, iSize + 1, "%f", dNumber ) > 0 && strstr( szResult, "#IND" ) )
       */

      if( pNumber->item.asDouble.length == 99 || ! HB_FINITE_DBL( dNumber ) )
         /* Numeric overflow */
         iPos = -1;
      else
      {
         double   dInt, dFract, dDig, doBase = 10.0;
         int      iPrec, iFirst = -1;

         /* dNumber = hb_numRound( dNumber, iDec ); */

#ifdef HB_NUM_PRECISION
         iPrec = HB_NUM_PRECISION;
#else
         iPrec = 16;
#endif

         if( dNumber < 0 )
         {
            fNeg     = TRUE;
            dFract   = modf( -dNumber, &dInt );
         }
         else
         {
            fNeg     = FALSE;
            dFract   = modf( dNumber, &dInt );
         }

         while( iPos-- > 0 )
         {
            dDig              = modf( dInt / doBase + 0.01, &dInt ) * doBase;
            szResult[ iPos ]  = '0' + ( char ) ( dDig + 0.01 );
            if( szResult[ iPos ] != '0' )
               iFirst = iPos;
            if( dInt < 1 )
               break;
         }

         if( iPos > 0 )
            memset( szResult, ' ', iPos );

         if( iDec > 0 && iPos >= 0 )
         {
            for( iPos = iDot + 1; iPos < iSize; iPos++ )
            {
               dFract            = modf( dFract * doBase, &dDig );
               szResult[ iPos ]  = '0' + ( char ) ( dDig + 0.01 );
               if( iFirst < 0 )
               {
                  if( szResult[ iPos ] != '0' )
                     iFirst = iPos - 1;
               }
               else if( iPos - iFirst >= iPrec )
                  break;
            }
         }

         /* now try to round the results and set 0 in places over defined
            precision, the same is done by Clipper */
         if( iPos >= 0 )
         {
            int   iZer = 0;
            int   iLast;

            if( iFirst >= 0 )
            {
               iZer = iSize - iFirst - iPrec;
               if( iDec > 0 )
                  iZer--;
            }
            dFract   = modf( dFract * doBase, &dDig );
            iLast    = ( int ) ( dDig + 0.01 );

            /* hack for x.xxxx4999999999, f.e. 8.995 ~FL 8.994999999999999218.. */
            if( iLast == 4 && iZer < 0 )
            {
               for( iPos = -iZer; iPos > 0; --iPos )
               {
                  dFract = modf( dFract * doBase, &dDig );
                  if( dDig + 0.01 < 9 && ( iPos != 1 || dDig < 2 ) )
                     break;
               }
               if( iPos == 0 )
                  iLast = 5;
            }
            iLast = iLast >= 5 ? 1 : 0;

            iPos  = iSize;
            while( iPos-- > 0 )
            {
               if( iDec == 0 || iPos != iDot )
               {
                  if( iZer > 0 )
                  {
                     if( iDec == 0 || iPos <= iDot + 1 )
                        iLast = szResult[ iPos ] >= '5' ? 1 : 0;

                     szResult[ iPos ] = '0';
                     --iZer;
                  }
                  else if( iLast > 0 )
                  {
                     if( szResult[ iPos ] == '9' )
                        szResult[ iPos ] = '0';
                     else
                     {
                        if( szResult[ iPos ] < '0' ) /* '-' or ' ' */
                        {
                           szResult[ iPos ]  = '1';
                           iFirst            = iPos;
                        }
                        else
                        {
                           szResult[ iPos ]++;
                           if( iFirst < 0 )
                              iFirst = iPos;
                        }
                        break;
                     }
                  }
                  else
                     break;
               }
            }
            if( fNeg && iFirst >= 0 && iPos >= 0 )
            {
               iPos = ( iDot > 0 && iFirst >= iDot ) ? iDot - 2 : iFirst - 1;
               if( iPos >= 0 )
                  szResult[ iPos ] = '-';
            }
         }
      }
   }
   else
   {
      HB_LONG lNumber;

      switch( pNumber->type )
      {
         case HB_IT_INTEGER:
            lNumber = pNumber->item.asInteger.value;
            break;

         case HB_IT_LONG:
            lNumber = pNumber->item.asLong.value;
            break;

         case HB_IT_DATE:
         case HB_IT_TIMEFLAG:
            lNumber = pNumber->item.asDate.value;
            break;

         case HB_IT_STRING:
            lNumber = ( BYTE ) pNumber->item.asString.value[ 0 ];
            break;

         default:
            lNumber  = 0;
            iPos     = -1;
            break;
      }

      fNeg = ( lNumber < 0 );
      while( iPos-- > 0 )
      {
         szResult[ iPos ]  = '0' + ( char ) ( fNeg ? -( lNumber % 10 ) : ( lNumber % 10 ) );
         lNumber           /= 10;
         if( lNumber == 0 )
            break;
      }
      if( fNeg && iPos-- > 0 )
         szResult[ iPos ] = '-';

      if( iPos > 0 )
         memset( szResult, ' ', iPos );

      if( iDec > 0 && iPos >= 0 )
         memset( &szResult[ iSize - iDec ], '0', iDec );
   }

   szResult[ iSize ] = '\0';
   /* Set to asterisks in case of overflow */
   if( iPos < 0 )
   {
      memset( szResult, '*', iSize );
      return FALSE;
   }
   else if( iDot > 0 )
      szResult[ iDot ] = '.';

   return TRUE;
}

/* converts a numeric to a string with optional width & precision.
   This function should be used by any function that wants to format numeric
   data for displaying, printing, or putting in a database.

   Note: The caller is responsible for calling hb_xfree to free the results
         buffer, but ONLY if the return value is not a NULL pointer! (If a NULL
         pointer is returned, then there was a conversion error.)
 */
char * hb_itemStr( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec )
{
   char * szResult = NULL;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemStr(%p, %p, %p)", pNumber, pWidth, pDec ) );

   if( pNumber )
   {
      /* Default to the width and number of decimals specified by the item,
         with a limit of 90 integer places, plus one space for the sign. */
      int iWidth, iDec;

      hb_itemGetNLen( pNumber, &iWidth, &iDec );

      if( iWidth > 90 )
         iWidth = 90;

      /* Limit the number of decimal places. */
      if( hb_stackSetStruct()->HB_SET_FIXED )
         /* If fixed mode is enabled, always use the default. */
         iDec = hb_stackSetStruct()->HB_SET_DECIMALS;

      if( pWidth )
      {
         /* If the width parameter is specified, override the default value
            and set the number of decimals to zero */
         int iWidthPar = hb_itemGetNI( pWidth );

         iWidth   = iWidthPar;
         if( iWidthPar < 1 )
            iWidth = 10;                  /* If 0 or negative, use default */
         iDec     = 0;
      }

      if( pDec )
      {
         /* This function does not include the decimal places in the width,
            so the width must be adjusted downwards, if the decimal places
            parameter is greater than 0  */
         int iDecPar = hb_itemGetNI( pDec );

         if( iDecPar < 0 )
            iDec = 0;
         else if( iDecPar > 0 )
         {
            iDec     = iDecPar;
            iWidth   -= ( iDec + 1 );
         }
      }

      if( iWidth )
      {
         /* We at least have a width value */
         int iSize = iWidth;

         if( iDec > 0 )
            iSize += iDec + 1;

         szResult = ( char * ) hb_xgrab( iSize + 1 );
         hb_itemStrBuf( szResult, pNumber, iSize, iDec );
      }
   }

   return szResult;
}

/* NOTE: The caller must free the pointer if the bFreeReq param gets set to
         TRUE, this trick is required to stay thread safe, while minimize
         memory allocation and buffer copying.
         As a side effect the caller should never modify the returned buffer
         since it may point to a constant value. */

char * hb_itemString( PHB_ITEM pItem, HB_SIZE * ulLen, BOOL * bFreeReq )
{
   char * buffer;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemString(%p, %p, %p)", pItem, ulLen, bFreeReq ) );

   switch( pItem->type )
   {
      case HB_IT_STRING:
      case HB_IT_MEMO:
         buffer      = hb_itemGetCPtr( pItem );
         *ulLen      = hb_itemGetCLen( pItem );
         *bFreeReq   = FALSE;
         break;

      case HB_IT_DATE:
      {
         char szDate[ 9 ];

         hb_dateDecStr( szDate, pItem->item.asDate.value );

         buffer      = ( char * ) hb_xgrab( 11 );
         hb_dateFormat( szDate, buffer, hb_stackSetStruct()->HB_SET_DATEFORMAT );
         *ulLen      = strlen( buffer );
         *bFreeReq   = TRUE;
         break;
      }

      case HB_IT_TIMEFLAG:
      {
         char szDate[ 19 ];
         buffer = ( char * ) hb_xgrab( 26 );

         hb_datetimeDecStr( szDate, pItem->item.asDate.value, pItem->item.asDate.time );
         hb_datetimeFormat( szDate, buffer, hb_stackSetStruct()->HB_SET_DATEFORMAT, hb_stackSetStruct()->HB_SET_TIMEFORMAT );

         *ulLen      = strlen( buffer );
         *bFreeReq   = TRUE;
         break;
      }

      case HB_IT_DOUBLE:
      case HB_IT_INTEGER:
      case HB_IT_LONG:
         buffer = hb_itemStr( pItem, NULL, NULL );
         if( buffer )
         {
            *ulLen      = strlen( buffer );
            *bFreeReq   = TRUE;
         }
         else
         {
            buffer      = ( char * ) "";
            *ulLen      = 0;
            *bFreeReq   = FALSE;
         }
         break;

      case HB_IT_NIL:
         buffer      = ( char * ) "NIL";
         *ulLen      = 3;
         *bFreeReq   = FALSE;
         break;

      case HB_IT_LOGICAL:
         buffer      = ( char * ) ( hb_itemGetL( pItem ) ? "T" : "F" );
         *ulLen      = 1;
         *bFreeReq   = FALSE;
         break;

      case HB_IT_POINTER:
      {
         int         size  = ( sizeof( void * ) << 1 ) + 3; /* n bytes for address + 0x + \0 */
         HB_PTRDIFF  addr  = ( HB_PTRDIFF ) hb_itemGetPtr( pItem );

         *ulLen            = size - 1;
         *bFreeReq         = TRUE;
         buffer            = ( char * ) hb_xgrab( size );
         buffer[ 0 ]       = '0';
         buffer[ 1 ]       = 'x';
         buffer[ --size ]  = '\0';
         do
         {
            UCHAR uc = ( UCHAR ) ( addr & 0xf );
            buffer[ --size ]  = ( char ) ( uc + ( uc < 10 ? '0' : 'A' - 10 ) );
            addr              >>= 4;
         }
         while( size > 2 );
         break;
      }
      default:
         buffer      = ( char * ) "";
         *ulLen      = 0;
         *bFreeReq   = FALSE;
   }

   return buffer;
}

/* This function is used by all of the PAD functions to prepare the argument
   being padded. If date, convert to string using hb_dateFormat(). If numeric,
   convert to unpadded string. Return pointer to string and set string length */

char * hb_itemPadConv( PHB_ITEM pItem, HB_SIZE * pulSize, BOOL * bFreeReq )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPadConv(%p, %p, %p)", pItem, pulSize, bFreeReq ) );

   /* to be clipper compatible don't convert HB_IT_BYREF items */
   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_STRING:
         case HB_IT_MEMO:
         case HB_IT_DATE:
         case HB_IT_TIMEFLAG:
            return hb_itemString( pItem, pulSize, bFreeReq );

         case HB_IT_DOUBLE:
         case HB_IT_INTEGER:
         case HB_IT_LONG:
         {
            register int   i;
            char *         buffer = hb_itemString( pItem, pulSize, bFreeReq );

            /* remove leading spaces if any, a little bit redundant but
             * I don't want to complicate the API interface more. Druzus
             */
            for( i = 0; buffer[ i ] == ' '; i++ )
            {
            }
            ;

            if( i > 0 )
            {
               register int j = 0;

               *pulSize -= i;
               do
               {
                  buffer[ j++ ] = buffer[ i ];
               }
               while( buffer[ i++ ] );
            }
            return buffer;
         }
         default:
            break;
      }
   }
   return NULL;
}

PHB_ITEM hb_itemValToStr( PHB_ITEM pItem )
{
   PHB_ITEM pResult;
   char *   buffer;
   HB_SIZE  ulLen;
   BOOL     bFreeReq;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemValToStr(%p)", pItem ) );

   buffer   = hb_itemString( pItem, &ulLen, &bFreeReq );
   pResult  = hb_itemPutCL( NULL, buffer, ulLen );

   if( bFreeReq )
      hb_xfree( buffer );

   return pResult;
}

#ifndef HB_LONG_LONG_OFF
LONGLONG hb_itemGetNLL( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetNLL(%p)", pItem ) );

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_DOUBLE:
#ifdef __GNUC__
            return ( LONGLONG ) ( ULONGLONG ) pItem->item.asDouble.value;
#else
            return ( LONGLONG ) pItem->item.asDouble.value;
#endif

         case HB_IT_INTEGER:
            return ( LONGLONG ) pItem->item.asInteger.value;

         case HB_IT_LONG:
            return ( LONGLONG ) pItem->item.asLong.value;

         case HB_IT_DATE:
         case HB_IT_TIMEFLAG:
            return ( LONGLONG ) pItem->item.asDate.value;

         case HB_IT_LOGICAL:
            return ( LONGLONG ) pItem->item.asLogical.value;

         case HB_IT_STRING:
            return ( LONGLONG ) ( BYTE ) pItem->item.asString.value[ 0 ];
      }
   }

   return 0;
}

PHB_ITEM hb_itemPutNLL( PHB_ITEM pItem, LONGLONG llNumber )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNLL(%p, %Ld)", pItem, llNumber ) );

   pItem = hb_itemapiCheck( pItem );

#if HB_LONG_MAX >= LONGLONG_MAX
   pItem->type                   = HB_IT_LONG;
   pItem->item.asLong.value      = ( HB_LONG ) llNumber;
   pItem->item.asLong.length     = ( UINT ) HB_LONG_LENGTH( llNumber );
#else
   pItem->type                   = HB_IT_DOUBLE;
   pItem->item.asDouble.value    = ( double ) llNumber;
   pItem->item.asDouble.length   = HB_DBL_LENGTH( pItem->item.asDouble.value );
   pItem->item.asDouble.decimal  = 0;
#endif

   return pItem;
}

PHB_ITEM hb_itemPutNLLLen( PHB_ITEM pItem, LONGLONG llNumber, int iWidth )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemPutNLLLen(%p, %Ld, %d)", pItem, llNumber, iWidth ) );

   pItem = hb_itemapiCheck( pItem );

#if HB_LONG_MAX >= LONGLONG_MAX
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( llNumber );
   pItem->type                   = HB_IT_LONG;
   pItem->item.asLong.value      = ( HB_LONG ) llNumber;
   pItem->item.asLong.length     = ( UINT ) iWidth;
#else
   pItem->type                   = HB_IT_DOUBLE;
   pItem->item.asDouble.value    = ( double ) llNumber;
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( pItem->item.asDouble.value );
   pItem->item.asDouble.length   = iWidth;
   pItem->item.asDouble.decimal  = 0;
#endif

   return pItem;
}
#endif

PHB_ITEM hb_itemPutNInt( PHB_ITEM pItem, HB_LONG lNumber )
{
   if( HB_LIM_INT( lNumber ) )
      return hb_itemPutNI( pItem, ( int ) lNumber );

#ifdef HB_LONG_LONG_OFF
   return hb_itemPutNS( pItem, ( long ) lNumber );
#else
   return hb_itemPutNLL( pItem, ( LONGLONG ) lNumber );
#endif
}

PHB_ITEM hb_itemPutNIntLen( PHB_ITEM pItem, HB_LONG lNumber, int iWidth )
{
   if( HB_LIM_INT( lNumber ) )
      return hb_itemPutNILen( pItem, ( int ) lNumber, iWidth );

#ifdef HB_LONG_LONG_OFF
   return hb_itemPutNLLen( pItem, ( long ) lNumber, iWidth );
#else
   return hb_itemPutNLLLen( pItem, ( LONGLONG ) lNumber, iWidth );
#endif
}

HB_LONG hb_itemGetNInt( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_itemGetNLL(%p)", pItem ) );

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_DOUBLE:
#ifdef __GNUC__
            return ( HB_LONG ) ( HB_ULONG ) pItem->item.asDouble.value;
#else
            return ( HB_LONG ) pItem->item.asDouble.value;
#endif

         case HB_IT_INTEGER:
            return ( HB_LONG ) pItem->item.asInteger.value;

         case HB_IT_LONG:
            return ( HB_LONG ) pItem->item.asLong.value;

         case HB_IT_DATE:
         case HB_IT_TIMEFLAG:
            return ( HB_LONG ) pItem->item.asDate.value;

         case HB_IT_LOGICAL:
            return ( HB_LONG ) pItem->item.asLogical.value;

         case HB_IT_STRING:
            return ( HB_LONG ) ( BYTE ) pItem->item.asString.value[ 0 ];
      }
   }

   return 0;
}

PHB_ITEM hb_itemPutHBLong( PHB_ITEM pItem, HB_LONG lNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutHBLong( %p, %" PFHL "d)", pItem, lNumber ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                = HB_IT_LONG;
   pItem->item.asLong.value   = lNumber;
   pItem->item.asLong.length  = ( UINT ) HB_LONG_LENGTH( lNumber );

   return pItem;
}

PHB_ITEM hb_itemPutNumType( PHB_ITEM pItem, double dNumber, int iDec, int iType1, int iType2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutNumType( %p, %lf, %d, %i, %i)", pItem, dNumber, iDec, iType1, iType2 ) );

   if( iDec || iType1 & HB_IT_DOUBLE || iType2 & HB_IT_DOUBLE )
      return hb_itemPutNDDec( pItem, dNumber, iDec );
   else if( HB_DBL_LIM_INT( dNumber ) )
      return hb_itemPutNI( pItem, ( int ) dNumber );
   else if( HB_DBL_LIM_LONG( dNumber ) )
      return hb_itemPutHBLong( pItem, ( HB_LONG ) dNumber );

   return hb_itemPutND( pItem, dNumber );
}

PHB_ITEM hb_itemPutNull( PHB_ITEM pItem )
{
   pItem->type |= HB_IT_NULL;
   return pItem;
}

#if 0
PHB_ITEM hb_itemPutCPtr2( PHB_ITEM pItem, char * szText )
{
   ULONG ulLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutCPtr2(%p, %s)", pItem, szText ) );

   pItem = hb_itemapiCheck( pItem );

   ulLen                         = szText ? strlen( szText ) : 0;

   pItem->type                   = HB_IT_STRING;
   pItem->item.asString.length   = ulLen;
   if( ulLen == 0 )
   {
      pItem->item.asString.allocated   = 0;
      pItem->item.asString.value       = "";
      hb_xfree( szText );
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.allocated   = 0;
      pItem->item.asString.value       = ( char * ) hb_szAscii[ ( unsigned char ) ( szText[ 0 ] ) ];
      hb_xfree( szText );
   }
   else
   {
      szText[ ulLen ]                  = '\0';
      pItem->item.asString.allocated   = ulLen + 1;
      pItem->item.asString.value       = szText;
   }

   return pItem;
}

/*
   PHB_ITEM hb_itemPutCLPtr( PHB_ITEM pItem, char * szText, HB_SIZE ulLen )
   {
   return hb_itemPutCPtr( pItem, szText, ulLen );
   }
 */
PHB_ITEM hb_itemPutCLPtr( PHB_ITEM pItem, char * szText, HB_SIZE ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutCLPtr(%p, %s, %lu)", pItem, szText, ulLen ) );

   pItem = hb_itemapiCheck( pItem );

   pItem->type                   = HB_IT_STRING;
   pItem->item.asString.length   = ulLen;

   if( ulLen == 0 )
   {
      pItem->item.asString.allocated   = 0;
      pItem->item.asString.value       = ( char * ) "";
      hb_xfree( szText );
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.allocated   = 0;
      pItem->item.asString.value       = ( char * ) hb_szAscii[ ( unsigned char ) ( szText[ 0 ] ) ];
      hb_xfree( szText );
   }
   else
   {
      szText[ ulLen ]                  = '\0';
      pItem->item.asString.allocated   = ulLen + 1;
      pItem->item.asString.value       = szText;
   }

   return pItem;
}
#endif

PHB_ITEM hb_itemPutNS( PHB_ITEM pItem, HB_ISIZ nNumber )
{
   HB_THREAD_STUB_ANY

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutNS(%p, %" HB_PFS "d)", pItem, nNumber ) );

   pItem = hb_itemapiCheck( pItem );

#if HB_SIZE_MAX <= HB_UINT_MAX
   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = ( int ) nNumber;
   /* EXP limit used intentionally */
   pItem->item.asInteger.length  = HB_INT_EXPLENGTH( nNumber );
#else
   if( HB_LIM_INT( nNumber ) )
   {
      pItem->type                   = HB_IT_INTEGER;
      pItem->item.asInteger.value   = ( int ) nNumber;
      /* EXP limit used intentionally */
      pItem->item.asInteger.length  = HB_INT_EXPLENGTH( nNumber );
   }
   else
   {
      pItem->type                = HB_IT_LONG;
      pItem->item.asLong.value   = nNumber;
      pItem->item.asLong.length  = HB_LONG_LENGTH( nNumber );
   }
#endif

   return pItem;
}

HB_ISIZ hb_itemGetNS( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemGetNS(%p)", pItem ) );

   if( pItem )
   {
	  
	      
      if( HB_IS_LONG( pItem ) )
         return ( HB_ISIZ ) pItem->item.asLong.value;

      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_ISIZ ) pItem->item.asInteger.value;

      else if( HB_IS_DOUBLE( pItem ) )
         return ( HB_ISIZ ) pItem->item.asDouble.value;
         
	  else if( HB_IS_STRING( pItem ) )
        if( pItem->item.asString.length == 1 )
        return ( HB_ISIZ ) ( BYTE ) pItem->item.asString.value[ 0 ];         
   }

   return 0;
}

BOOL hb_itemEqual( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   BOOL fResult = 0;

   if( HB_IS_NUMERIC( pItem1 ) )
   {
      if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
         fResult = HB_ITEM_GET_NUMINTRAW( pItem1 ) == HB_ITEM_GET_NUMINTRAW( pItem2 );
      else
         fResult = HB_IS_NUMERIC( pItem2 ) &&
                   hb_itemGetND( pItem1 ) == hb_itemGetND( pItem2 );
   }
   else if( HB_IS_STRING( pItem1 ) )
      fResult = HB_IS_STRING( pItem2 ) &&
                pItem1->item.asString.length == pItem2->item.asString.length &&
                memcmp( pItem1->item.asString.value,
                        pItem2->item.asString.value,
                        pItem1->item.asString.length ) == 0;

   else if( HB_IS_NIL( pItem1 ) )
      fResult = HB_IS_NIL( pItem2 );

   else if( HB_IS_DATETIME( pItem1 ) )
      if( HB_IS_TIMEFLAG( pItem1 ) && HB_IS_TIMEFLAG( pItem2 ) )         
      fResult = HB_IS_DATETIME( pItem2 ) &&
                pItem1->item.asDate.value == pItem2->item.asDate.value &&
                pItem1->item.asDate.time == pItem2->item.asDate.time;
      else
      fResult = HB_IS_DATE( pItem2 ) &&
                pItem1->item.asDate.value == pItem2->item.asDate.value ;
                

   else if( HB_IS_LOGICAL( pItem1 ) )
      fResult = HB_IS_LOGICAL( pItem2 ) && ( pItem1->item.asLogical.value ?
                pItem2->item.asLogical.value : ! pItem2->item.asLogical.value );

   else if( HB_IS_ARRAY( pItem1 ) )
      fResult = HB_IS_ARRAY( pItem2 ) &&
                pItem1->item.asArray.value == pItem2->item.asArray.value;

   else if( HB_IS_HASH( pItem1 ) )
      fResult = HB_IS_HASH( pItem2 ) &&
                pItem1->item.asHash.value == pItem2->item.asHash.value;

   else if( HB_IS_POINTER( pItem1 ) )
      fResult = HB_IS_POINTER( pItem2 ) &&
                pItem1->item.asPointer.value == pItem2->item.asPointer.value;

   else if( HB_IS_BLOCK( pItem1 ) )
      fResult = HB_IS_BLOCK( pItem2 ) &&
                pItem1->item.asBlock.value == pItem2->item.asBlock.value;

   return fResult;
}
