/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Date & Time functions, part II: - ADDMONTH()
 *                                       - CTODOW()
 *                                       - CTOMONTH()
 *                                       - DAYSINMONTH()
 *                                       - DAYSTOMONTH()
 *                                       - DMY()
 *                                       - DOY()
 *                                       - ISLEAP()
 *                                       - LASTDAYOM()
 *                                       - MDY()
 *                                       - NTOCDOW()
 *                                       - NTOCMONTH()
 *                                       - QUARTER()
 *                                       - WEEK()
 *
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbset.h"

static BOOL ct_isleap( int iYear )
{
   return ( ( iYear & 3 ) == 0 && iYear % 100 != 0 ) || iYear % 400 == 0;
}

static int ct_daysinmonth( int iMonth, BOOL bLeap )
{
   if( iMonth == 2 )
   {
      return bLeap ? 29 : 28;
   }
   else if( iMonth == 4 || iMonth == 6 || iMonth == 9 || iMonth == 11 )
   {
      return 30;
   }
   else
   {
      return 31;
   }
}

static int ct_daystomonth( int iMonth, BOOL bLeap )
{
   static const int iMonths[] =
   { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

   return ( iMonth < 1 && iMonth > 12 ) ? 0 : iMonths[ iMonth - 1 ] +
          ( ( bLeap && iMonth > 2 ) ? 1 : 0 );
}

static int ct_doy( LONG lDate )
{
   int   iYear, iMonth, iDay;
   LONG  lFirst;

   hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
   lFirst = hb_dateEncode( iYear, 1, 1 );
   return ( int ) ( lDate - lFirst + 1 );
}

HB_FUNC( CTODOW )
{
   if( ISCHAR( 1 ) )
   {
      char *   szParam = ( char * ) hb_parc( 1 ), * szDow;
      int      iDow, iEqual;

      hb_strupr( szParam );

      for( iDow = 0; iDow < 7; iDow++ )
      {
         szDow = hb_strdup( ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_DAY + iDow ) );
         hb_strupr( szDow );

         if( hb_setGetL( HB_SET_EXACT ) )
         {
            iEqual = ( strlen( szDow ) == strlen( szParam ) )
                     && ! memcmp( szDow, szParam, strlen( szParam ) );
         }
         else
         {
            iEqual = ! memcmp( szDow, szParam, strlen( szParam ) );
         }

         hb_xfree( szDow );
         if( iEqual )
         {
            break;
         }
      }

      if( iDow == 7 )
      {
         hb_retni( 0 );
      }
      else
      {
         hb_retnl( iDow + 1 );
      }
   }
   else
   {
      hb_retni( 0 );
   }
}

HB_FUNC( CTOMONTH )
{
   if( ISCHAR( 1 ) )
   {
      char *   szParam = ( char * ) hb_parc( 1 ), * szMonth;
      int      iMonth, iEqual;

      hb_strupr( szParam );

      for( iMonth = 1; iMonth <= 12; iMonth++ )
      {
         szMonth = hb_strdup( ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 ) );
         hb_strupr( szMonth );

         if( hb_setGetL( HB_SET_EXACT ) )
         {
            iEqual = ( strlen( szMonth ) == strlen( szParam ) )
                     && ! memcmp( szMonth, szParam, strlen( szParam ) );
         }
         else
         {
            iEqual = ! memcmp( szMonth, szParam, strlen( szParam ) );
         }

         hb_xfree( szMonth );
         if( iEqual )
         {
            break;
         }
      }

      if( iMonth > 12 )
      {
         iMonth = 0;
      }
      hb_retnl( iMonth );

   }
   else
   {
      hb_retni( 0 );
   }
}

HB_FUNC( DMY )
{
   int   iYear, iMonth, iDay;
   BOOL  bMode = FALSE;

   if( ISDATE( 1 ) )
   {
      PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
   }

   if( ISLOG( 2 ) )
   {
      bMode = hb_parl( 2 );
   }

   if( iMonth >= 1 && iMonth <= 12 )
   {
      char *   szMonth  = ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 );
      HB_SIZE  iMonLen  = strlen( szMonth );
      HB_SIZE  iLen     = 0;
      HB_SIZE  iBufLen = iMonLen + 10;
      char *   szMDY    = ( char * ) hb_xgrab( iBufLen );

      if( iDay < 10 )
      {
         szMDY[ iLen ] = ( char ) ( iDay + 0x30 );
         iLen++;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iDay );
         iLen += 2;
      }

      if( bMode )
      {
         szMDY[ iLen ] = '.';
         iLen++;
      }
      szMDY[ iLen ] = ' ';
      iLen++;

      hb_strncpy( szMDY + iLen, szMonth, iBufLen - iLen - 1 );
      iLen           += iMonLen;
      szMDY[ iLen ]  = ' ';
      iLen++;

      if( hb_setGetCentury() )
      {
         hb_snprintf( szMDY + iLen, 5, "%04d", iYear );
         iLen += 4;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iYear % 100 );
         iLen += 2;
      }

      hb_retclen( szMDY, iLen );
      hb_xfree( szMDY );
   }
   else
   {
      hb_retc( NULL );
   }
}

HB_FUNC( MDY )
{
   int iYear, iMonth, iDay;

   if( ISDATE( 1 ) )
   {
      PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
   }

   if( iMonth >= 1 && iMonth <= 12 )
   {
      char *   szMonth  = ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 );
      HB_SIZE  iLen     = strlen( szMonth );
      HB_SIZE  iBufLen  = iLen + 9;
      char *   szMDY    = ( char * ) hb_xgrab( iBufLen );

      hb_strncpy( szMDY, szMonth, iBufLen - 1 );
      szMDY[ iLen++ ] = ' ';

      if( iDay < 10 )
      {
         szMDY[ iLen ] = ( char ) ( iDay + 0x30 );
         iLen++;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iDay );
         iLen += 2;
      }
      szMDY[ iLen++ ] = ' ';

      if( hb_setGetCentury() )
      {
         hb_snprintf( szMDY + iLen, 5, "%04d", iYear );
         iLen += 4;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iYear % 100 );
         iLen += 2;
      }

      hb_retclen( szMDY, iLen );
      hb_xfree( szMDY );
   }
   else
   {
      hb_retc( NULL );
   }
}

HB_FUNC( ADDMONTH )
{
   int iYear, iMonth, iDay, iNum, iDays;

   if( ISDATE( 1 ) )
   {
      PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      iNum = hb_parni( 2 );
   }
   else if( ISNUM( 1 ) )
   {
      iNum = hb_parni( 1 );
      hb_dateToday( &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_retdl( 0 );
      return;
   }

   iMonth += iNum;
   while( iMonth <= 0 )
   {
      iMonth += 12;
      iYear--;
   }
   while( iMonth > 12 )
   {
      iMonth -= 12;
      iYear++;
   }

   iDays = ct_daysinmonth( iMonth, ct_isleap( iYear ) );
   if( iDay > iDays )
   {
      iDay = iDays;
   }

   hb_retd( iYear, iMonth, iDay );
}

HB_FUNC( DOY )
{
   LONG     lDate;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      lDate = hb_itemGetDL( pDate );
   }
   else
   {
      int iYear, iMonth, iDay;

      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }

   hb_retni( ct_doy( lDate ) );
}

HB_FUNC( ISLEAP )
{
   int      iYear, iMonth, iDay;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate && hb_itemGetDL( pDate ) )
   {
      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
   }

   hb_retl( ct_isleap( iYear ) );
}

HB_FUNC( DAYSTOMONTH )
{
   int   iMonth   = ( ISNUM( 1 ) ? hb_parni( 1 ) : 0 );
   BOOL  bLeap    = ( ISLOG( 2 ) ? hb_parl( 2 ) : 0 );

   hb_retni( ct_daystomonth( iMonth, bLeap ) );
}

HB_FUNC( DAYSINMONTH )
{
   int   iMonth   = ( ISNUM( 1 ) ? hb_parni( 1 ) : 0 );
   BOOL  bLeap    = ( ISLOG( 2 ) ? hb_parl( 2 ) : 0 );

   hb_retni( ct_daysinmonth( iMonth, bLeap ) );
}

HB_FUNC( QUARTER )
{
   int      iYear, iMonth, iDay;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      if( hb_itemGetDL( pDate ) )
      {
         hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      }
      else
      {
         hb_retni( 0 );
         return;
      }
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
   }

   hb_retni( ( iMonth + 2 ) / 3 );
}

HB_FUNC( LASTDAYOM )
{
   BOOL  bLeap = 0;
   int   iYear, iMonth, iDay;

   if( ISDATE( 1 ) )
   {
      PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );
      LONG     lDate = hb_itemGetDL( pDate );

      if( lDate )
      {
         hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      }
      else
      {
         hb_dateToday( &iYear, &iMonth, &iDay );
      }
      bLeap = ct_isleap( iYear );
   }
   else if( ISNUM( 1 ) )
   {
      iMonth = hb_parni( 1 );
   }
   else
   {
      iMonth = 0;
   }

   hb_retni( ( iMonth && ( iMonth <= 12 ) ? ct_daysinmonth( iMonth, bLeap ) : 0 ) );

}

HB_FUNC( NTOCDOW )
{
   hb_retc( hb_dateCDOW( hb_parni( 1 ) ) );
}

HB_FUNC( NTOCMONTH )
{
   hb_retc( hb_dateCMonth( hb_parni( 1 ) ) );
}

HB_FUNC( WEEK )
{
   int      iYear, iMonth, iDay, iWeek;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );
   LONG     lDate = 0;
   BOOL     bSWN  = ( ISLOG( 2 ) ? hb_parl( 2 ) : FALSE );

   if( ISDATE( 1 ) )
   {
      lDate = hb_itemGetDL( pDate );
      if( ! lDate )
      {
         hb_retni( 0 );
         return;
      }
   }

   if( lDate )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }

   if( bSWN )
   {
      int   iDays = ct_daystomonth( iMonth, ct_isleap( iYear ) ) + iDay;
      int   iPart = ( iDays % 7 );

      iWeek = iDays / 7;
      if( iPart > 0 )
         iWeek++;
   }
   else
   {
      LONG lDate2;

      if( hb_setGetCPtr( HB_SET_DATEFORMAT ) && ( hb_setGetCPtr( HB_SET_DATEFORMAT )[ 0 ] == 'd' ||
                                                  hb_setGetCPtr( HB_SET_DATEFORMAT )[ 0 ] == 'D' ) )
         lDate2 = lDate + 3 - ( hb_dateDOW( iYear, iMonth, iDay ) + 5 ) % 7;
      else
         lDate2 = lDate + 4 - hb_dateDOW( iYear, iMonth, iDay );

      iWeek = ( ct_doy( lDate2 ) - 1 ) / 7 + 1;
   }

   hb_retni( iWeek );
}
