/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date conversion module
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
 *    hb_dateEncStr()
 *    hb_dateDecStr()
 *    hb_dateStrPut()
 *    hb_dateStrGet()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_dateFormat()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_dateDOW()
 *
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *    hb_timeFormat()
 *    hb_datetimeFormat()
 *    hb_timeEncodeSec()
 *    hb_timeEncode()
 *    hb_timeDecodeSec()
 *    hb_timeDecode()
 *    hb_datetimeEncode()
 *    hb_datetimeDecode()
 *    hb_timeEncStr()
 *    hb_timeDecStr()
 *    hb_datetimeEncStr()
 *    hb_datetimeDecStr()
 *    hb_datetimePack()
 *    hb_datetimePackInSec()
 *    hb_datetimeUnpack()
 *    hb_comp_datetimeEncStr()
 *    hb_comp_datetimeDecStr()
 *    hb_comp_datetimeEncode()
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include <time.h>

#include "hbapi.h"
#include "hbdate.h"
#include "hbmath.h"
#include "hbcomp.h"


#include <time.h>
#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
#  include <sys/time.h>
#elif defined( HB_OS_WIN )
#  include <windows.h>
#else
#  include <sys/timeb.h>
#  if defined( _MSC_VER )
#     define timeb _timeb
#     define ftime _ftime
#  endif
#  ifndef TIME_ZONE_ID_INVALID
#     define TIME_ZONE_ID_INVALID ( DWORD ) 0xFFFFFFFF
#  endif
#endif

#ifdef HB_C52_STRICT
   #define HB_DATE_YEAR_LIMIT 2999
#else
   #define HB_DATE_YEAR_LIMIT 9999
#endif

#define HB_STR_DATE_BASE      1721060     /* 0000/01/01 */
#define HB_SYS_DATE_BASE      2440588     /* 1970/01/01 */

long hb_dateEncode( int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncode(%d, %d, %d)", iYear, iMonth, iDay));

   /* Perform date validation */
   if( iYear >= 0 && iYear <= HB_DATE_YEAR_LIMIT &&
       iMonth >= 1 && iMonth <= 12 &&
       iDay >= 1 )
   {
      /* Month, year, and lower day limits are simple,
         but upper day limit is dependent upon month and leap year */
      static const int auiDayLimit[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

      if( iDay <= auiDayLimit[ iMonth - 1 ] ||
          ( iDay == 29 && iMonth == 2 &&
            ( iYear & 3 ) == 0 && ( iYear % 100 != 0 || iYear % 400 == 0 ) ) )
      {
         int iFactor = ( iMonth < 3 ) ? -1 : 0;

         return ( ( long )( iFactor + 4800 + iYear ) * 1461 / 4 ) +
                ( ( long )( iMonth - 2 - ( iFactor * 12 ) ) * 367 ) / 12 -
                ( ( long )( ( iFactor + 4900 + iYear ) / 100 ) * 3 / 4 ) +
                ( long ) iDay - 32075;
      }
   }

   return 0;
}

void hb_dateDecode( long lJulian, int * piYear, int * piMonth, int * piDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateDecode(%ld, %p, %p, %p)", lJulian, piYear, piMonth, piDay ) );

   if( lJulian >= HB_STR_DATE_BASE )
   {
      long U, V, W, X;

      lJulian  += 68569;
      W        = ( lJulian * 4 ) / 146097;
      lJulian  -= ( ( 146097 * W ) + 3 ) / 4;
      X        = 4000 * ( lJulian + 1 ) / 1461001;
      lJulian  -= ( ( 1461 * X ) / 4 ) - 31;
      V        = 80 * lJulian / 2447;
      U        = V / 11;

      *piYear  = ( int ) ( X + U + ( W - 49 ) * 100 );
      *piMonth = ( int ) ( V + 2 - ( U * 12 ) );
      *piDay   = ( int ) ( lJulian - ( 2447 * V / 80 ) );
   }
   else
   {
      *piYear        =
         *piMonth    =
            *piDay   = 0;
   }
}

void hb_dateStrPut( char * szDate, int iYear, int iMonth, int iDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateStrPut(%p, %d, %d, %d)", szDate, iYear, iMonth, iDay ) );

   if( iYear >= 0 && iMonth > 0 && iDay > 0 )
   {
      szDate[ 0 ] = ( char ) ( ( ( iYear / 1000 ) % 10 ) + '0' );
      szDate[ 1 ] = ( char ) ( ( ( iYear / 100 ) % 10 ) + '0' );
      szDate[ 2 ] = ( char ) ( ( ( iYear / 10 ) % 10 ) + '0' );
      szDate[ 3 ] = ( char ) ( ( iYear % 10 ) + '0' );

      szDate[ 4 ] = ( char ) ( ( iMonth / 10 ) + '0' );
      szDate[ 5 ] = ( char ) ( ( iMonth % 10 ) + '0' );

      szDate[ 6 ] = ( char ) ( ( iDay / 10 ) + '0' );
      szDate[ 7 ] = ( char ) ( ( iDay % 10 ) + '0' );
   }
   else
   {
      memset( szDate, '0', 8 );
   }
}

void hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrGet(%.8s, %p, %p, %p)", szDate, piYear, piMonth, piDay));

#if defined( HB_C52_STRICT ) || 1
   if( szDate )
#else
   if( szDate &&
       szDate[ 0 ] >= '0' && szDate[ 0 ] <= '9' &&
       szDate[ 1 ] >= '0' && szDate[ 1 ] <= '9' &&
       szDate[ 2 ] >= '0' && szDate[ 2 ] <= '9' &&
       szDate[ 3 ] >= '0' && szDate[ 3 ] <= '9' &&
       szDate[ 4 ] >= '0' && szDate[ 4 ] <= '9' &&
       szDate[ 5 ] >= '0' && szDate[ 5 ] <= '9' &&
       szDate[ 6 ] >= '0' && szDate[ 6 ] <= '9' &&
       szDate[ 7 ] >= '0' && szDate[ 7 ] <= '9' )
#endif
   {
      /* Date string has correct length, so attempt to convert */
      *piYear  = ( ( ( int ) ( szDate[ 0 ] - '0' )   * 10 +
                     ( int ) ( szDate[ 1 ] - '0' ) ) * 10 +
                     ( int ) ( szDate[ 2 ] - '0' ) ) * 10 +
                     ( int ) ( szDate[ 3 ] - '0' );
      *piMonth = ( szDate[ 4 ] - '0' ) * 10 + ( szDate[ 5 ] - '0' );
      *piDay   = ( szDate[ 6 ] - '0' ) * 10 + ( szDate[ 7 ] - '0' );
   }
   else
   {
      /* Date string missing or bad length, so force an empty date */
      *piYear  =
      *piMonth =
      *piDay   = 0;
   }
}

/* This function always closes the date with a zero byte, so it needs a
   9 character long buffer. */

char * hb_dateDecStr( char * szDate, long lJulian )
{
   int iYear, iMonth, iDay;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateDecStr(%p, %ld)", szDate, lJulian ) );

   if( lJulian <= 0 )
   {
      memset( szDate, ' ', 8 );
   }
   else
   {
      hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
      hb_dateStrPut( szDate, iYear, iMonth, iDay );
   }
   szDate[ 8 ] = '\0';

   return szDate;
}

long hb_dateEncStr( const char * szDate )
{
   int iYear, iMonth, iDay;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateEncStr(%s)", szDate ) );

   hb_dateStrGet( szDate, &iYear, &iMonth, &iDay );

   return hb_dateEncode( iYear, iMonth, iDay );
}

/* NOTE: szFormattedDate must be an at least 11 chars wide buffer */

char * hb_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat )
{
   /*
    * NOTE: szFormattedDate must point to a buffer of at least 11 bytes.
    *       szDateFormat must point to a buffer holding the date format to use.
    */
   int format_count, digit_count, size;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateFormat(%s, %p, %s)", szDate, szFormattedDate, szDateFormat ) );

   /*
    * Determine the maximum size of the formatted date string
    */
   size = ( int ) strlen( szDateFormat );
   if( size > 10 )
      size = 10;

   if( szDate && szFormattedDate && strlen( szDate ) == 8 ) /* A valid date is always 8 characters */
   {
      const char *   szPtr;
      int            digit;
      BOOL           used_d, used_m, used_y;

      format_count   = 0;
      used_d         = used_m = used_y = FALSE;
      szPtr          = szDateFormat;

      while( format_count < size )
      {
         digit       = HB_TOUPPER( ( UCHAR ) *szPtr );
         szPtr++;
         digit_count = 1;
         while( ( ( int ) HB_TOUPPER( ( UCHAR ) *szPtr ) ) == digit && format_count < size )
         {
            szPtr++;
            if( format_count + digit_count < size )
               digit_count++;
         }
         switch( digit )
         {
            case 'D':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_d && format_count < size )
                     {
/*                        szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  case 3:
                     if( ! used_d && format_count < size )
                     {
/*                        szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  case 2:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  default:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 7 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedDate[ format_count++ ] = ( char ) digit;
               }
               used_d = TRUE;
               break;

            case 'M':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_m && format_count < size )
                     {
                        /* szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  case 3:
                     if( ! used_m && format_count < size )
                     {
                        /* szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  case 2:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  default:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 5 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedDate[ format_count++ ] = ( char ) digit;
               }
               used_m = TRUE;
               break;

            case 'Y':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 0 ];
                        digit_count--;
                     }
                     /* fallthrough */
                  case 3:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 1 ];
                        digit_count--;
                     }
                     /* fallthrough */
                  case 2:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 2 ];
                        digit_count--;
                     }
                     /* fallthrough */
                  default:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 3 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedDate[ format_count++ ] = ( char ) digit;
               }
               used_y = TRUE;
               break;

            default:
               while( digit_count-- > 0 && format_count < size )
                  szFormattedDate[ format_count++ ] = ( char ) digit;
         }
      }
   }
   else
   {
      /* Not a valid date string, so return a blank date with separators */
      format_count = size; /* size is either 8 or 10 */
      hb_strncpy( szFormattedDate, szDateFormat, size );

      for( digit_count = 0; digit_count < size; digit_count++ )
      {
         switch( szFormattedDate[ digit_count ] )
         {
            case 'D':
            case 'd':
            case 'M':
            case 'm':
            case 'Y':
            case 'y':
               szFormattedDate[ digit_count ] = ' ';
         }
      }
   }

   szFormattedDate[ format_count ] = '\0';

   return szFormattedDate;
}

int hb_dateJulianDOW( long lJulian )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateJulianDOW(%ld)", lJulian ) );

   if( lJulian >= HB_STR_DATE_BASE )
      return ( int ) ( ( lJulian + 1 ) % 7 ) + 1;
   else
      return 0;
}

/* NOTE: szFormattedTime must be an at least 16 chars wide buffer. hh:mm:ss.ccc pm */

char * hb_timeFormat( const char * szTime, char * szFormattedTime, const char * szTimeFormat )
{
   /*
    * NOTE: szFormattedTime must point to a buffer of at least 16 bytes.
    *       szTimeFormat must point to a buffer holding the time format to use.
    */
   int   format_count;
   int   digit_count;
   int   size;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeFormat(%s, %p, %s)", szTime, szFormattedTime, szTimeFormat ) );

   /*
    * Determine the maximum size of the formatted time string
    */
   size = ( int ) strlen( szTimeFormat );
   if( size > 15 )
      size = 15;

   if( szTime && szFormattedTime && strlen( szTime ) == ( 7 + HB_DATETIMEDECIMALS ) )
   {
      const char *   szPtr;
      int            digit, pos_pm = 0, pos_h = -1;
      BOOL           used_h, used_m, used_s, used_c, used_pm;

      format_count   = 0;
      used_h         = used_m = used_s = used_c = used_pm = FALSE;
      szPtr          = szTimeFormat;

      while( format_count < size )
      {
         digit       = HB_TOUPPER( *szPtr );
         szPtr++;
         digit_count = 1;
         while( ( ( int ) HB_TOUPPER( ( UCHAR ) *szPtr ) ) == digit && format_count < size )
         {
            szPtr++;
            if( format_count + digit_count < size )
               digit_count++;
         }
         switch( digit )
         {
            case 'H':
               switch( digit_count )
               {
                  case 2:
                     if( ! used_h && format_count < size )
                     {
                        if( pos_h == -1 )
                           pos_h = ( int ) format_count;
                        szFormattedTime[ format_count++ ] = szTime[ 0 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  default:
                     if( ! used_h && format_count < size )
                     {
                        if( pos_h == -1 )
                           pos_h = ( int ) format_count;
                        szFormattedTime[ format_count++ ] = szTime[ 1 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedTime[ format_count++ ] = ( char ) digit;
               }
               used_h = TRUE;
               break;

            case 'M':
               if( pos_pm && format_count - 1 == pos_pm )
               {
                  used_pm                             = TRUE;
                  szFormattedTime[ format_count++ ]   = 'M';
                  break;
               }
               switch( digit_count )
               {
                  case 2:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedTime[ format_count++ ] = szTime[ 2 ];
                        digit_count--;
                     }
					 /* fallthrough */
                  default:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedTime[ format_count++ ] = szTime[ 3 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedTime[ format_count++ ] = ( char ) digit;
               }
               used_m = TRUE;
               break;

            case 'S':
               switch( digit_count )
               {
                  case 2:
                     if( ! used_s && format_count < size )
                     {
                        szFormattedTime[ format_count++ ] = szTime[ 4 ];
                        digit_count--;
                     }
/* fallthrough */
                  default:
                     if( ! used_s && format_count < size )
                     {
                        szFormattedTime[ format_count++ ] = szTime[ 5 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedTime[ format_count++ ] = ( char ) digit;
               }
               used_s = TRUE;
               break;

            case 'C':
            {
               int digit_c = 7;

               switch( digit_count )
               {
                  case 4:
                     if( ! used_c && format_count < size )
                     {
//                        szFormattedTime[ format_count++ ] = '0';
                        szFormattedTime[ format_count++ ] = szTime[ digit_c++ ];
                        digit_count--;
                     }
					 /* fallthrough */
                  case 3:
                     if( ! used_c && format_count < size && digit_c - 9 < HB_DATETIMEDECIMALS )
                     {
//                        szFormattedTime[ format_count++ ] = '0';
                        szFormattedTime[ format_count++ ] = szTime[ digit_c++ ];
                        digit_count--;
                     }
					 /* fallthrough */
                  case 2:
                     if( ! used_c && format_count < size && digit_c - 9 < HB_DATETIMEDECIMALS )
                     {
                        szFormattedTime[ format_count++ ] = szTime[ digit_c++ ];
                        digit_count--;
                     }
					 /* fallthrough */
                  default:
                     if( ! used_c && format_count < size && digit_c - 9 < HB_DATETIMEDECIMALS )
                     {
                        szFormattedTime[ format_count++ ] = szTime[ digit_c ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedTime[ format_count++ ] = ( char ) digit;
               }
               used_c = TRUE;
               break;
            }

            case 'P':
               szFormattedTime[ format_count ]  = ( char ) digit;
               pos_pm                           = ( int ) format_count++;
               break;

            default:
               while( digit_count-- > 0 && format_count < size )
                  szFormattedTime[ format_count++ ] = ( char ) digit;
         }
      }
      if( used_pm && pos_h >= 0 )
      {
         if( szFormattedTime[ pos_h ] == '2' || ( szFormattedTime[ pos_h ] == '1' && szFormattedTime[ pos_h + 1 ] > '1' ) )
         {
            szFormattedTime[ pos_h ]--;
            szFormattedTime[ pos_h + 1 ]  -= ( char ) 2;
            szFormattedTime[ pos_pm ]     = 'P';
         }
         else
         {
            szFormattedTime[ pos_pm ] = 'A';
         }
         szFormattedTime[ pos_pm + 1 ] = 'M';

         if( szFormattedTime[ pos_h ] == '0' && szFormattedTime[ pos_h + 1 ] == '0' )
         {
            szFormattedTime[ pos_h ]      = '1';
            szFormattedTime[ pos_h + 1 ]  = '2';
         }
      }
   }
   else
   {
      /* Not a valid time string, so return a 00:00:00 time with separators */
      format_count = size;
      hb_strncpy( szFormattedTime, szTimeFormat, size );

      for( digit_count = 0; digit_count < size; digit_count++ )
      {
         switch( szFormattedTime[ digit_count ] )
         {
            case 'H':
            case 'h':
            case 'M':
            case 'm':
            case 'S':
            case 's':
            case 'C':
            case 'c':
               szFormattedTime[ digit_count ] = '0';
         }
      }
   }

   szFormattedTime[ format_count ] = '\0';

   return szFormattedTime;
}

/* NOTE: szFormattedDateTime must be an at least 26 chars wide buffer */

char * hb_datetimeFormat( const char * szDateTime, char * szFormattedDateTime, const char * szDateFormat, const char * szTimeFormat )
{
   int   n;
   char  szDate[ 9 ];

   if( szDateFormat )
   {
      HB_MEMCPY( szDate, szDateTime, 8 );
      szDate[ 8 ]                = '\0';
      hb_dateFormat( szDate, szFormattedDateTime, szDateFormat );
      n                          = ( int ) strlen( szFormattedDateTime );
      szFormattedDateTime[ n ]   = ' ';
   }
   else
   {
      n = -1;
   }
   hb_timeFormat( szDateTime + 8, szFormattedDateTime + n + 1, szTimeFormat );
   return szFormattedDateTime;
}

int hb_dateDOW( int iYear, int iMonth, int iDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateDOW(%d, %d, %d)", iYear, iMonth, iDay ) );

   if( iMonth < 3 )
   {
      iMonth += 13;
      iYear--;
   }
   else
      iMonth++;

   return ( iDay + 26 * iMonth / 10 +
            iYear + iYear / 4 - iYear / 100 + iYear / 400 + 6 ) % 7 + 1;
}

void hb_dateToday( int * piYear, int * piMonth, int * piDay )
{
#if defined( HB_OS_WIN )

   SYSTEMTIME st;
   GetLocalTime( &st );

   *piYear  = st.wYear;
   *piMonth = st.wMonth;
   *piDay   = st.wDay;

#elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )

   time_t      t;
   struct tm   st;

   time( &t );
   localtime_r( &t, &st );

   *piYear  = st.tm_year + 1900;
   *piMonth = st.tm_mon + 1;
   *piDay   = st.tm_mday;

#else

   time_t      t;
   struct tm * oTime;

   time( &t );
   oTime    = localtime( &t );

   *piYear  = oTime->tm_year + 1900;
   *piMonth = oTime->tm_mon + 1;
   *piDay   = oTime->tm_mday;

#endif
}

/* NOTE: The passed buffer must be at least 9 chars long */

void hb_dateTimeStr( char * pszTime )
{
#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;
      GetLocalTime( &st );
      hb_snprintf( pszTime, 9, "%02d:%02d:%02d", st.wHour, st.wMinute, st.wSecond );
   }
#elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )
   {
      time_t      t;
      struct tm   st;

      time( &t );
      localtime_r( &t, &st );

      hb_snprintf( pszTime, 9, "%02d:%02d:%02d", st.tm_hour, st.tm_min, st.tm_sec );
   }
#else
   {
      time_t      t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
      hb_snprintf( pszTime, 9, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min, oTime->tm_sec );
   }
#endif
}

/*
   2-4  The next three characters tell the time a user placed the lock. (10h 09h 07h i.e. 16:09:07)
   5-7  The next three characters tell the date a user placed the lock. ( 60h 09h 0Bh i.e. (19)96-09-11 )
 */
void hb_dbaselockEncode( char * pszTimeDate )
{
#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;
      GetLocalTime( &st );
      hb_snprintf( pszTimeDate, 6, "%c%c%c%c%c%c",
                   st.wHour, st.wMinute, st.wSecond, st.wYear - 1900, st.wMonth, st.wDay );
   }
#elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )
   {
      time_t      t;
      struct tm   st;

      time( &t );
      localtime_r( &t, &st );

      hb_snprintf( pszTimeDate, 6, "%c%c%c%c%c%c",
                   st.tm_hour, st.tm_min, st.tm_sec, st.tm_year, st.tm_mon + 1, st.tm_mday );
   }
#else
   {
      time_t      t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
      hb_snprintf( pszTimeDate, 6, "%c%c%c%c%c%c",
                   oTime->tm_hour, oTime->tm_min, oTime->tm_sec, oTime->tm_year, oTime->tm_mon + 1, oTime->tm_mday );
   }
#endif
}

long hb_timeStampEncode( int iHour, int iMinutes, int iSeconds, int iMSec )
{
   long lMillisec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampEncode(%d, %d, %d, %d)", iHour, iMinutes, iSeconds, iMSec ) );

   if( iHour >= 0 && iHour < 24 &&
       iMinutes >= 0 && iMinutes < 60 &&
       iSeconds >= 0 && iSeconds < 60 &&
       iMSec >= 0 && iMSec < 1000 )
   {
      lMillisec = ( ( long ) ( iHour * 60 + iMinutes ) * 60 + iSeconds ) *
                  1000 + iMSec;
   }
   else
   {
      lMillisec = 0;
   }

   return lMillisec;
}

void hb_timeStampDecode( long lMillisec, int * piHour, int * piMinutes,
                         int * piSeconds, int * piMSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampDecode(%ld, %p, %p, %p, %p)", lMillisec, piHour, piMinutes, piSeconds, piMSec ) );

   if( lMillisec <= 0 )
   {
      *piHour = *piMinutes = *piSeconds = *piMSec = 0;
   }
   else
   {
      *piMSec     = lMillisec % 1000;
      lMillisec   /= 1000;
      *piSeconds  = lMillisec % 60;
      lMillisec   /= 60;
      *piMinutes  = lMillisec % 60;
      lMillisec   /= 60;
      if( lMillisec >= 24 )
         *piHour = *piMinutes = *piSeconds = *piMSec = 0;
      else
         *piHour = ( int ) lMillisec;
   }
}

/* This function always closes the time with a zero byte, so it needs a
   13 character long buffer. */

char * hb_timeStampStr( char * szTime, long lMillisec )
{
   int iHour, iMinutes, iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampStr(%p, %ld)", szTime, lMillisec ) );

   hb_timeStampDecode( lMillisec, &iHour, &iMinutes, &iSeconds, &iMSec );
   hb_snprintf( szTime, 13, "%02d:%02d:%02d.%03d",
                iHour, iMinutes, iSeconds, iMSec );
   szTime[ 12 ] = '\0';

   return szTime;
}

/* This function always closes the time with a zero byte, so it needs a
   24 character long buffer. */

char * hb_dateTimeStampStr( char * szDateTime, long lJulian, long lMillisec )
{
   int iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateTimeStampStr(%p, %ld, %ld)", szDateTime, lJulian, lMillisec ) );

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_timeStampDecode( lMillisec, &iHour, &iMinutes, &iSeconds, &iMSec );
   hb_snprintf( szDateTime, 24, "%04d-%02d-%02d %02d:%02d:%02d.%03d",
                iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec );
   szDateTime[ 23 ] = '\0';

   return szDateTime;
}

void hb_dateTimeStampStrGet( const char * szDateTime, long * plJulian, long * plMillisec )
{
   int iLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateTimeStampStr(%s, %p, %p)", szDateTime, plJulian, plMillisec ) );

   *plJulian   = *plMillisec = 0;
   iLen        = szDateTime ? ( int ) hb_strnlen( szDateTime, 23 ) : 0;
   if( iLen >= 10 )
   {
      int iYear, iMonth, iDay;

      iYear = ( ( ( int ) ( szDateTime[ 0 ] - '0' ) * 10 +
                  ( int ) ( szDateTime[ 1 ] - '0' ) ) * 10 +
                ( int ) ( szDateTime[ 2 ] - '0' ) ) * 10 +
              ( int ) ( szDateTime[ 3 ] - '0' );
      iMonth      = ( szDateTime[ 5 ] - '0' ) * 10 + ( szDateTime[ 6 ] - '0' );
      iDay        = ( szDateTime[ 8 ] - '0' ) * 10 + ( szDateTime[ 9 ] - '0' );

      *plJulian   = hb_dateEncode( iYear, iMonth, iDay );
      if( iLen >= 16 )
      {
         int iHour, iMinutes, iSeconds = 0, iMSec = 0;

         iHour    = ( szDateTime[ 11 ] - '0' ) * 10 +
                    ( szDateTime[ 12 ] - '0' );
         iMinutes = ( szDateTime[ 14 ] - '0' ) * 10 +
                    ( szDateTime[ 15 ] - '0' );
         if( iHour >= 0 && iHour < 24 && iMinutes >= 0 && iMinutes < 60 )
         {
            if( iLen >= 19 )
            {
               iSeconds = ( szDateTime[ 17 ] - '0' ) * 10 +
                          ( szDateTime[ 18 ] - '0' );
               if( iSeconds < 0 || iSeconds >= 60 )
                  iSeconds = 0;
               else if( iLen >= 23 )
               {
                  iMSec = ( ( szDateTime[ 20 ] - '0' ) * 10 +
                            ( szDateTime[ 21 ] - '0' ) ) * 10 +
                          ( szDateTime[ 22 ] - '0' );
                  if( iMSec < 0 || iMSec >= 1000 )
                     iMSec = 0;
               }
            }
            *plMillisec = ( ( ( iHour * 60 ) + iMinutes ) * 60 + iSeconds ) *
                          1000 + iMSec;
         }
      }
   }
}

void hb_dateTime( int * piHour, int * piMinute, double * pdSeconds )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateTime(%p,%p,%p)", piHour, piMinute, pdSeconds ) );

#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;
      GetLocalTime( &st );
      *piHour     = ( int ) st.wHour;
      *piMinute   = ( int ) st.wMinute;
      *pdSeconds  = ( double ) st.wSecond + ( double ) st.wMilliseconds / 1000;
   }
#else
   {
      time_t      t;
      struct tm * oTime;

      time( &t );
      oTime       = localtime( &t );
      *piHour     = ( int ) oTime->tm_hour;
      *piMinute   = ( int ) oTime->tm_min;
      *pdSeconds  = ( double ) oTime->tm_sec;
   }
#endif
}

double hb_timeEncodeSec( int iHour, int iMinute, double dSeconds )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeEncode(%d, %d, %f)", iHour, iMinute, dSeconds ) );
   //printf( "hb_timeEncode(%d, %d, %f)", lHour, lMinute, dSeconds);

   if( iHour >= 0 && iHour <= 23 && iMinute >= 0 && iMinute <= 59 && dSeconds >= 0 && dSeconds < 60 )
   {
      return ( double ) ( iHour * 3600 + iMinute * 60 ) + dSeconds;
   }

   return 0;
}

long hb_timeEncode( int iHour, int iMinute, double dSeconds )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeEncode(%d, %d, %f)", iHour, iMinute, dSeconds ) );
   //printf( "hb_timeEncode(%d, %d, %f)", lHour, lMinute, dSeconds);

   if( iHour >= 0 && iHour <= 23 && iMinute >= 0 && iMinute <= 59 && dSeconds >= 0 && dSeconds < 60 )
   {
      return ( long ) ( iHour * 3600 * HB_DATETIMEINSEC ) + ( long ) ( iMinute * 60 * HB_DATETIMEINSEC ) + ( long ) ( dSeconds * HB_DATETIMEINSEC );
   }

   return 0;
}

void hb_timeDecode( long lTime, int * piHour, int * piMinute, double * pdSeconds )
{
   int      iHour = 0, iMin = 0;
   double   dSec  = 0.0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeDecode(%ld, %p, %p %p)", lTime, piHour, piMinute, pdSeconds ) );

   if( lTime > 0 )
   {
      div_t result = div( ( int ) lTime, ( int ) ( 3600 * HB_DATETIMEINSEC ) );
      iHour    = result.quot;
      lTime    = result.rem;
      result   = div( ( int ) lTime, ( int ) ( 60 * HB_DATETIMEINSEC ) );
      iMin     = result.quot;
      lTime    = result.rem;
      dSec     = ( double ) lTime / HB_DATETIMEINSEC;
      //TraceLog(NULL,"iHour=%d iMin=%d dSec=%f\n",iHour,iMin,dSec);
   }

   if( piHour )
   {
      *piHour = iHour;
   }

   if( piMinute )
   {
      *piMinute = iMin;
   }

   if( pdSeconds )
   {
      *pdSeconds = dSec;
   }
}

void hb_timeDecodeSec( double dTime, int * piHour, int * piMinute, double * pdSeconds )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeDecode(%d, %p, %p %p)", dTime, piHour, piMinute, pdSeconds ) );

   if( dTime > 0 )
   {
      double lTime = ( double ) ( dTime * HB_DATETIMEINSEC );
      hb_timeDecodeSec( lTime, piHour, piMinute, pdSeconds );
   }
}

void hb_timeStrGet( const char * szTime, int * piHour, int * piMinutes,
                    int * piSeconds, int * piMSec )
{
   int iHour, iMinutes, iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStrGet(%s, %p, %p, %p, %p)", szTime, piHour, piMinutes, piSeconds, piMSec ) );

   iHour = iMinutes = iSeconds = iMSec = 0;

   if( szTime )
   {
      int iLen = ( int ) hb_strnlen( szTime, 12 );

      if( iLen >= 5 )
      {

         iHour    = ( szTime[ 0 ] - '0' ) * 10 +
                    ( szTime[ 1 ] - '0' );
         iMinutes = ( szTime[ 3 ] - '0' ) * 10 +
                    ( szTime[ 4 ] - '0' );
         if( iHour >= 0 && iHour < 24 && iMinutes >= 0 && iMinutes < 60 )
         {
            if( iLen >= 8 )
            {
               iSeconds = ( szTime[ 6 ] - '0' ) * 10 +
                          ( szTime[ 7 ] - '0' );
               if( iSeconds < 0 || iSeconds >= 60 )
                  iSeconds = 0;
               else if( iLen >= 12 )
               {
                  iMSec = ( ( szTime[ 9 ] - '0' ) * 10 +
                            ( szTime[ 10 ] - '0' ) ) * 10 +
                          ( szTime[ 11 ] - '0' );
                  if( iMSec < 0 || iMSec >= 1000 )
                     iMSec = 0;
               }
            }
         }
      }
   }

   if( piHour )
      *piHour = iHour;
   if( piMinutes )
      *piMinutes = iMinutes;
   if( piSeconds )
      *piSeconds = iSeconds;
   if( piMSec )
      *piMSec = iMSec;
}

void hb_datetimeEncode( long * plDate, long * plTime, int iYear, int iMonth, int iDay,
                        int iHour, int iMinute, double dSeconds, int iAmPm, int * piOk )
{
   long  lDate;
   BOOL  iOk;

   HB_TRACE( HB_TR_DEBUG, ( "hb_datetimeEncode(%d, %d, %d, %d, %d, %f, %d, %p)", iYear, iMonth, iDay, iHour, iMinute, dSeconds, iAmPm, piOk ) );
   /*printf( "hb_datetimeEncode(%d, %d, %d, %d, %d, %f, %d, %p) Que pasa???\n", iYear, iMonth, iDay, iHour, iMinute, dSeconds, iAmPm, iOk);*/

   lDate = ( long ) hb_dateEncode( iYear, iMonth, iDay );
   iOk   = FALSE;

   if( iAmPm == 0 )
   {
      iOk = TRUE;
   }
   else if( iAmPm == 1 )
   {
      if( iHour <= 12 )
      {
         iOk   = TRUE;
         iHour %= 12;
      }
      else
      {
         iHour    = 0;
         iMinute  = 0;
         dSeconds = 0;
      }
   }
   else if( iAmPm == 2 )
   {
      if( iHour <= 12 )
      {
         iOk   = TRUE;
         iHour %= 12;
         iHour += 12;
      }
      else
      {
         iHour    = 0;
         iMinute  = 0;
         dSeconds = 0;
      }
   }

   if( lDate && iHour >= 0 && iHour <= 23 && iMinute >= 0 && iMinute <= 59 && dSeconds >= 0 && dSeconds < 60 )
   {
      if( plDate )
      {
         *plDate = lDate;
      }
      if( plTime )
      {
         //printf( "iHour=%d, iMinute=%d, dSeconds=%f)\n", iHour, iMinute, dSeconds);
         *plTime = hb_timeEncode( iHour, iMinute, dSeconds );
      }
   }
   if( piOk )
   {
      *piOk = iOk;
   }

}

void hb_datetimeDecode( long lDate, long lTime, int * piYear, int * piMonth, int * piDay,
                        int * piHour, int * piMinute, double * pdSeconds )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_datetimeDecode( %d, %d %p, %p, %p, %p, %p, %p)", lDate, lTime, piYear, piMonth, piDay, piHour, piMinute, pdSeconds ) );

   hb_dateDecode( lDate, piYear, piMonth, piDay );
   hb_timeDecode( lTime, piHour, piMinute, pdSeconds );
}

long hb_timeEncStr( const char * szTime )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeEncStr(%s)", szTime ) );

   if( szTime )
   {
      int ulLen = ( int ) strlen( szTime );

      if( ulLen >= 4 )
      {
         return ( long ) hb_strVal( szTime, 2 ) * 3600 * HB_DATETIMEINSEC +
                ( long ) hb_strVal( szTime + 2, 2 ) * 60 * HB_DATETIMEINSEC +
                ( long ) ( hb_strVal( szTime + 4, ulLen - 4 ) * HB_DATETIMEINSEC );
      }
   }

   return 0;
}

char * hb_timeDecStr( char * szTime, long lSeconds )
{
   int      iHour, iMinute;
   double   dSeconds;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeDecStr(%s,%f)", szTime, lSeconds ) );

   hb_timeDecode( lSeconds, &iHour, &iMinute, &dSeconds );

   //printf( "Origen %lf, Hora: %d, Minutos: %d, Segundos: %f\n", dSeconds, iHour, iMinute, dSeconds );
   hb_snprintf( szTime, 8 + HB_DATETIMEDECIMALS, "%02d%02d%0*.*f", iHour, iMinute, HB_DATETIMEDECIMALS + 3, HB_DATETIMEDECIMALS, dSeconds );
   //printf( "Final %lf, Hora: %d, Minutos: %d, Segundos: %f, %s\n", dSeconds, iHour, iMinute, dSeconds, szTime );

   return szTime;
}

void hb_datetimeEncStr( const char * szDateTime, long * plDate, long * plTime )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_datetimeEncStr(%s,%p,%p)", szDateTime, plDate, plTime ) );

   if( plDate )
   {
      char szDate[ 9 ];
      szDate[ 8 ] = '\0';
      HB_MEMCPY( szDate, szDateTime, 8 );
      *plDate     = hb_dateEncStr( szDate );
   }
   if( plTime )
   {
      *plTime = hb_timeEncStr( szDateTime + 8 );
   }
}

char * hb_datetimeDecStr( char * szDateTime, long lDate, long lTime )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_datetimeDecStr(%s,%d,%d)", szDateTime, lDate, lTime ) );

   hb_dateDecStr( szDateTime, lDate );
   hb_timeDecStr( szDateTime + 8, lTime );

   return szDateTime;
}

#undef hb_datetimePack
double hb_datetimePack( long lJulian, long lTime )
{
   return ( double ) lJulian + ( ( double ) lTime / ( double ) ( 86400 * HB_DATETIMEINSEC ) );
}

#undef hb_datetimePackInSec
double hb_datetimePackInSec( long lJulian, long lTime )
{
   return ( double ) ( lJulian * 86400 ) + ( ( double ) lTime / ( double ) ( HB_DATETIMEINSEC ) );
}

void hb_datetimeUnpack( double dDateTime, long * plDate, long * plTime )
{
   double dDate, dTime;

   dTime = modf( dDateTime, &dDate );

   if( plDate )
   {
      *plDate = ( long ) dDate;
   }
   if( plTime )
   {
      dTime    *= ( double ) ( 86400 * HB_DATETIMEINSEC );
      *plTime  = ( long ) dTime;
   }
}

#define hb_datetimePack( lJulian, lTime )       ( double ) ( ( double ) lJulian + ( ( double ) lTime / ( double ) ( 86400 * HB_DATETIMEINSEC ) ) )
#define hb_datetimePackInSec( lJulian, lTime )  ( double ) ( ( double ) ( lJulian * 86400 ) + ( ( double ) lTime / ( double ) ( HB_DATETIMEINSEC ) ) )

void hb_timeStampUnpackDT( double dTimeStamp,
                           long * plJulian, long * plMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampUnpackDT(%f, %p, %p)", dTimeStamp, plJulian, plMilliSec ) );

   {
#if defined( HB_LONG_LONG_OFF )
      double dJulian, dTime;

      dTime = modf( dTimeStamp + 0.5 / HB_MILLISECS_PER_DAY, &dJulian );
      if( plJulian )
         *plJulian = ( long ) dJulian;
      if( plMilliSec )
         *plMilliSec = ( long ) ( dTime * HB_MILLISECS_PER_DAY );
#else
      LONGLONG llMilliSec = ( LONGLONG ) ( dTimeStamp * HB_MILLISECS_PER_DAY + 0.5 );
      if( plJulian )
         *plJulian = ( long ) ( llMilliSec / HB_MILLISECS_PER_DAY );
      if( plMilliSec )
         *plMilliSec = ( long ) ( llMilliSec % HB_MILLISECS_PER_DAY );
#endif
   }
}

/*-------------------------------------------------------------------------------*/

double hb_comp_datetimeEncStr( const char * szDateTime )
{
   long lDate, lTime;

   hb_datetimeEncStr( szDateTime, &lDate, &lTime );

   return hb_datetimePack( lDate, lTime );
}

char * hb_comp_datetimeDecStr( char * szDateTime, double dDateTime )
{
   long lDate, lTime;

   hb_datetimeUnpack( dDateTime, &lDate, &lTime );

   return hb_datetimeDecStr( szDateTime, lDate, lTime );
}

void hb_comp_datetimeEncode( long * plDate, long * plTime, int iYear, int iMonth, int iDay,
                             int iHour, int iMinute, double dSeconds, int iAmPm, int * piOk )
{
   hb_datetimeEncode( plDate, plTime, iYear, iMonth, iDay, iHour, iMinute, dSeconds, iAmPm, piOk );
}


void hb_timeStampGetLocal( int * piYear, int * piMonth, int * piDay,
                           int * piHour, int * piMinutes,
                           int * piSeconds, int * piMSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampGetLocal(%p,%p,%p,%p,%p,%p,%p)", piYear, piMonth, piDay, piHour, piMinutes, piSeconds, piMSec ) );

#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;

      GetLocalTime( &st );

      *piYear    = st.wYear;
      *piMonth   = st.wMonth;
      *piDay     = st.wDay;
      *piHour    = st.wHour;
      *piMinutes = st.wMinute;
      *piSeconds = st.wSecond;
      *piMSec    = st.wMilliseconds;
   }
#else
   {
      struct tm st;
      time_t seconds, millisecs;

#  if defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
      struct timeval tv;
      gettimeofday( &tv, NULL );
      seconds = tv.tv_sec;
      millisecs = tv.tv_usec / 1000;
#  else
      struct timeb tb;
      ftime( &tb );
      seconds = tb.time;
      millisecs = tb.millitm;
#  endif

#  if defined( HB_HAS_LOCALTIME_R )
      localtime_r( &seconds, &st );
#  else
      st = *localtime( &seconds );
#  endif

      *piYear    = st.tm_year + 1900;
      *piMonth   = st.tm_mon + 1;
      *piDay     = st.tm_mday;
      *piHour    = st.tm_hour;
      *piMinutes = st.tm_min;
      *piSeconds = st.tm_sec;
      *piMSec    = millisecs;
   }
#endif
}

/* return local timestamp */
void hb_timeStampGet( long * plJulian, long * plMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampGet(%p,%p)", plJulian, plMilliSec ) );

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
   *plJulian   = hb_dateEncode( iYear, iMonth, iDay );
   *plMilliSec = hb_timeEncode( iHour, iMinute, iSeconds );
}



HB_MAXUINT hb_timerGet( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timerGet()" ) );

#if defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 199309L && defined( CLOCK_REALTIME )
   {
      static int s_iClkId = -1;
      struct timespec ts;

      if( s_iClkId < 0 )
      {
         int i, piClkId[] = {
#  if defined( CLOCK_MONOTONIC )
            CLOCK_MONOTONIC,
#  endif
#  if defined( CLOCK_MONOTONIC_COARSE )
            CLOCK_MONOTONIC_COARSE,
#  endif
#  if defined( CLOCK_REALTIME )
            CLOCK_REALTIME,
#  endif
#  if defined( CLOCK_REALTIME_COARSE )
            CLOCK_REALTIME_COARSE,
#  endif
            0 };

         for( i = 0; i < ( int ) HB_SIZEOFARRAY( piClkId ); ++i )
         {
            s_iClkId = piClkId[ i ];
            if( s_iClkId == 0 || clock_getres( s_iClkId, &ts ) == 0 )
               break;
         }
      }
      if( s_iClkId != 0 && clock_gettime( s_iClkId, &ts ) == 0 )
         return ( HB_MAXUINT ) ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
   }
#endif
#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
   {
      struct timeval tv;
      gettimeofday( &tv, NULL );
      return ( HB_MAXUINT ) tv.tv_sec * 1000 + tv.tv_usec / 1000;
   }
#elif defined( HB_OS_WIN )
   {
      static DWORD s_dwCounter = 0, s_dwLast = 0;
      DWORD dwTime = timeGetTime();

      if( dwTime < s_dwLast )
         ++s_dwCounter;
      s_dwLast = dwTime;
      return ( ( HB_MAXUINT ) s_dwCounter << 32 ) + dwTime;
   }
#else
   {
      struct timeb tb;
      ftime( &tb );
      return ( HB_MAXUINT ) tb.time * 1000 + tb.millitm;
   }
#endif
}

HB_MAXUINT hb_timerInit( HB_MAXINT nTimeOut )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timerInit(%" PFHL "d)", nTimeOut ) );

   return nTimeOut > 0 ? hb_timerGet() : 0;
}

HB_MAXINT hb_timerTest( HB_MAXINT nTimeOut, HB_MAXUINT * pnTimer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timerTest(%" PFHL "d, %p)", nTimeOut, ( void * ) pnTimer ) );

   if( nTimeOut > 0 )
   {
      HB_MAXUINT nTime = hb_timerGet();

      if( nTime > *pnTimer )
      {
         nTimeOut -= nTime - *pnTimer;
         if( nTimeOut < 0 )
            nTimeOut = 0;
      }
      *pnTimer = nTime;
   }
   return nTimeOut;
}

/*-------------------------------------------------------------------------------*/
