/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Sample date functions rewritten in C
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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
 * Copyright 1999 Jon Berg <jmberg@pnh10.med.navy.mil>
 *    DateTime()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <time.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"
#include "hbapilng.h"

#if 0
static int s_daysinmonth[ 12 ] =
{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
#endif

static BOOL hb_isleapyear( int iYear )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_isleapyear(%d)", iYear));

   return ( iYear % 4 == 0 && iYear % 100 != 0 ) || ( iYear % 400 == 0 );
}


/* DATEASAGE( <dDate> ) --> <nYears>
*/
HB_FUNC( DATEASAGE )
{
   int iAge = 0;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;
      int iThisYear, iThisMonth, iThisDay;

      hb_dateToday( &iThisYear, &iThisMonth, &iThisDay );
      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      
      if( iThisYear > iYear )
      {
         iAge = iThisYear - iYear;
      
         if( iThisMonth < iMonth || ( iThisMonth == iMonth && iThisDay < iDay ) )
            --iAge;
      }
   }

   hb_retni( iAge );
}

#if 0
/* ADDMONTH( <dDate>, <nMonths> ) --> <dNewDate>
   NOTE: Caller must validate dNewDate.
*/
HB_FUNC( ADDMONTH )
{
   int iMonths = hb_parni( 2 );
   int iYear, iMonth, iDay;

   int iLimit, iMonthAdd, iYearAdd;
   LONG lNew;

   hb_dateDecode( hb_parnl( 1 ), &iYear, &iMonth, &iDay );

   iLimit = 12 - iMonth + 1;
   iYearAdd = iMonths / 12;
   iMonths %= 12;

   if( iMonths >= iLimit )
      iYearAdd++;

   iYear += iYearAdd;

   iMonthAdd = iMonths % 12;
   iMonth    = ( iMonth + iMonthAdd ) % 12;

   if( iMonth == 0 )
      iMonth = 12;

   lNew = hb_dateEncode( iYear, iMonth, iDay );

   if( !lNew )
   {
      iMonth = ( iMonth + 1 ) % 12;
      iDay = 1;
      iYear = iYear + ( ( iMonth + 1 ) / 12 );
   }

   hb_retd( iYear, iMonth, iDay );
}
#endif

/* DATEASARRAY( <dDate> ) --> { Year, Month, Day }
   NOTE: Returns an empty array if <dDate> is not a valid date.
*/
HB_FUNC( DATEASARRAY )
{
   HB_ITEM Return;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   Return.type = HB_IT_NIL;
   hb_arrayNew( &Return, 0 );     /* Create array */

   if( pDate )
   {
      HB_ITEM Item;
      int iYear, iMonth, iDay;

      Item.type = HB_IT_NIL;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_itemPutNL( &Item, iYear );
      hb_arrayAddForward( &Return, &Item );

      hb_itemPutNL( &Item, iMonth );
      hb_arrayAddForward( &Return, &Item );

      hb_itemPutNL( &Item, iDay );
      hb_arrayAddForward( &Return, &Item );
   }

   hb_itemReturnForward( &Return );
}

/* ARRAYASDATE( { <Year>, <Month>, <Day> } ) --> <dDate>
*/
HB_FUNC( ARRAYASDATE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
      hb_retd( hb_arrayGetNL( pArray, 1 ), hb_arrayGetNL( pArray, 2 ), hb_arrayGetNL( pArray, 3 ) );
   else
      hb_retdl( 0 );
}

/* DATEISLEAP( <dDate> ) --> <lIsLeapYear>
*/
HB_FUNC( DATEISLEAP )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retl( ( iYear % 4 == 0 && iYear % 100 != 0 ) || ( iYear % 400 == 0 ) );
   }
   else
      hb_retl( FALSE );
}

/* NTOD( <nMonth>, <nDay>, <nYear> ) --> <dDate>
*/
HB_FUNC( NTOD )
{
   hb_retd( hb_parni( 3 ), hb_parni( 1 ), hb_parni( 2 ) );
}

HB_FUNC( AMONTHS )
{
   HB_ITEM Return, Tmp;
   int i;

   Tmp.type = HB_IT_NIL;
   Return.type = HB_IT_NIL;
   hb_arrayNew( &Return, 0 );

   for( i = 0; i < 12; i++ )
   {
      hb_arrayAddForward( &Return, hb_itemPutC( &Tmp, ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + i ) ) );
   }

   hb_itemReturn( &Return );
}

HB_FUNC( ADAYS )
{
   HB_ITEM Return, Tmp;
   int i;

   Tmp.type = HB_IT_NIL;
   Return.type = HB_IT_NIL;

   hb_arrayNew( &Return, 0 );

   for( i = 0; i < 7; i++ )
   {
      hb_arrayAddForward( &Return, hb_itemPutC( &Tmp, ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_DAY + i ) ));
   }

   hb_itemReturn( &Return );
}

HB_FUNC( ISLEAPYEAR )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      hb_retl( hb_isleapyear( iYear ) );
   }
   else
      hb_retl( FALSE );
}


#if 0

static int hb_daysinmonth( int iYear, int iMonth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_daysinmonth(%d, %d)", iYear, iMonth));

   if( iMonth > 0 && iMonth < 13 )
      return s_daysinmonth[ iMonth - 1 ] +
             ( ( iMonth == 2 && hb_isleapyear( iYear ) ) ? 1 : 0 );
   else
      return 0;
}

HB_FUNC( DAYSINMONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      hb_retni( hb_daysinmonth( iYear, iMonth ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( DATETIME )
{
   time_t current_time;

   time( &current_time );

   hb_retc( ctime( &current_time ) );
}

/* MDY( <dDate> ) --> "month dd, yyyy"
*/
HB_FUNC( MDY )
{
   int iYear, iMonth, iDay;
   char szDate[ 9 ];
   char szFormatted[ 11 ];
   char * szReturn;
   int iBufferLen;
   int iLen;

   hb_dateDecode( hb_parnl( 1 ), &iYear, &iMonth, &iDay );
   hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, "MM/DD/YYYY" );

   iLen = strlen( hb_dateCMonth( iMonth ) );

   iBufferLen = iLen + ( hb_setGetCentury() ? 9 : 7 );
   szReturn = ( char * ) hb_xgrab( iBufferLen );

   memset( szReturn, ' ', iBufferLen + 1 );
   memcpy( szReturn, hb_dateCMonth( iMonth ), iLen );
   memcpy( szReturn + iLen + 1, szFormatted + 3, 2 );
   szReturn[ iLen + 3 ] = ',';
   memcpy( szReturn + iLen + 5, szFormatted + 6 + ( hb_setGetCentury() ? 0 : 2 ), 2 + ( hb_setGetCentury() ? 2 : 0 ) );

   hb_retclen( szReturn, iBufferLen );
   hb_xfree( szReturn );
}

/* DMY( <dDate> ) --> "dd month yyyy" (european date format)
*/
HB_FUNC( DMY )
{
   int  iYear, iMonth, iDay;
   char szDate[ 9 ];
   char szFormatted[ 11 ];
   char * szReturn;
   int iBufferLen;
   int iLen;

   hb_dateDecode( hb_parnl( 1 ), &iYear, &iMonth, &iDay );
   hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, "MM/DD/YYYY" );

   iLen = strlen( hb_dateCMonth( iMonth ) );

   iBufferLen = iLen + ( hb_setGetCentury() ? 9 : 7 );
   szReturn = ( char * ) hb_xgrab( iBufferLen );

   memset( szReturn, ' ', iBufferLen );
   memcpy( szReturn, szFormatted + 3, 2 );
   memcpy( szReturn + 3, hb_dateCMonth( iMonth ), iLen );
   memcpy( szReturn + iLen + 4, szFormatted + 6 + ( hb_setGetCentury() ? 0 : 2 ), 2 + ( hb_setGetCentury() ? 2 : 0 ) );

   hb_retclen( szReturn, iBufferLen );
   hb_xfree( szReturn );
}

#endif
