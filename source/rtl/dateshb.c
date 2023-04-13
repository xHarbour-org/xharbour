/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date API (Harbour level)
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
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    DAY()
 *    MONTH()
 *    YEAR()
 *    DOW()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    CTOD()
 *    DATE()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    STOD()
 *
 * Copyright 2004 Giancarlo Niccolai <gc -at- niccolai -dot- ws>
 *    TIMEOFDAY()
 *
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *    hb_timectot()
 *    DATETIME()
 *    TTOS()
 *    TTOC()
 *    CTOT()
 *    STOT()
 *    HOUR()
 *    MINUTE()
 *    SECS()
 *    HMS2D()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"

HB_FUNC_EXTERN( TSSECS );

static int hb_datectod( char const * szDate, int * pd_value, int * pm_value, int * py_value )
{
   int   d_value  = 0, m_value = 0, y_value = 0;
   int   fin      = 0;

   if( szDate )
   {
      int            d_pos    = 0, m_pos = 0, y_pos = 0;
      const char *   szFormat = hb_setGetDateFormat();
      int            count, digit, non_digit;
      int            size     = ( int ) strlen( szFormat );

      for( count = 0; count < size; count++ )
      {
         switch( szFormat[ count ] )
         {
            case 'D':
            case 'd':
               if( d_pos == 0 )
               {
                  if( m_pos == 0 && y_pos == 0 )
                     d_pos = 1;
                  else if( m_pos == 0 || y_pos == 0 )
                     d_pos = 2;
                  else
                     d_pos = 3;
               }
               break;

            case 'M':
            case 'm':
               if( m_pos == 0 )
               {
                  if( d_pos == 0 && y_pos == 0 )
                     m_pos = 1;
                  else if( d_pos == 0 || y_pos == 0 )
                     m_pos = 2;
                  else
                     m_pos = 3;
               }
               break;

            case 'Y':
            case 'y':
               if( y_pos == 0 )
               {
                  if( m_pos == 0 && d_pos == 0 )
                     y_pos = 1;
                  else if( m_pos == 0 || d_pos == 0 )
                     y_pos = 2;
                  else
                     y_pos = 3;
               }
         }
      }

      /* If there are non-digits at the start of the date field,
         they are not to be treated as date field separators */
      non_digit   = 1;
      fin         = size = ( int ) strlen( szDate );

      for( count = 0; count < size; count++ )
      {
         digit = szDate[ count ];

         if( HB_ISDIGIT( digit ) )
         {
            /* Process the digit for the current date field */
            if( d_pos == 1 )
               d_value = ( d_value * 10 ) + digit - '0';
            else if( m_pos == 1 )
               m_value = ( m_value * 10 ) + digit - '0';
            else if( y_pos == 1 )
               y_value = ( y_value * 10 ) + digit - '0';

            /* Treat the next non-digit as a date field separator */
            non_digit = 0;
         }
         else if( digit != ' ' )
         {
            if( d_value > 0 && m_value > 0 && y_value > 0 )
            {
               fin = count;
               break;
            }

            /* Process the non-digit */
            if( non_digit++ == 0 )
            {
               /* Only move to the next date field on the first
                  consecutive non-digit that is encountered */
               d_pos--;
               m_pos--;
               y_pos--;
            }
         }
         else if( d_value > 0 && m_value > 0 && y_value > 0 )
         {
            fin = count;
            break;
         }
      }

      if( y_value >= 0 && y_value < 100 )
      {
         count = hb_setGetEpoch() % 100;
         digit = hb_setGetEpoch() / 100;

         if( y_value >= count )
            y_value += ( digit * 100 );
         else
            y_value += ( ( digit * 100 ) + 100 );
      }
   }
   *pd_value   = d_value;
   *pm_value   = m_value;
   *py_value   = y_value;

   return ( int ) fin;
}

static int hb_timectot( char const * szTime, int * ph_value, int * pm_value, double * ps_value )
{
   int            h_value  = 0, m_value = 0;
   double         s_value  = 0;
   int            h_pos    = 0, m_pos = 0, s_pos = 0, c_pos = 0, p_pos = 0;
   const char *   szFormat = hb_setGetTimeFormat();
   int            count, digit, non_digit;
   int            size     = ( int ) strlen( szFormat );
   int            fin      = 0;
   int            pm       = 0, divisor = 10;

   if( szTime )
   {
      digit = 1;
      for( count = 0; count < size; count++ )
      {
         switch( szFormat[ count ] )
         {
            case 'H':
            case 'h':
               if( h_pos == 0 )
                  h_pos = digit++;
               break;
            case 'M':
            case 'm':
               if( m_pos == 0 )
                  m_pos = digit++;
               break;
            case 'S':
            case 's':
               if( s_pos == 0 )
                  s_pos = digit++;
            /* fallthrough */			  
            case 'C':
            case 'c':
               if( c_pos == 0 )
               {
                  if( s_pos != 0 )
                     c_pos = s_pos;
               }
			 /* fallthrough */   
            case 'P':
            case 'p':
               if( p_pos == 0 )
               {
                  if( szFormat[ count + 1 ] == 'M' ||
                      szFormat[ count + 1 ] == 'm' )
                  {
                     p_pos = digit++;
                     count++;
                  }
               }
			   
         }
      }

      /* If there are non-digits at the start of the date field,
         they are not to be treated as date field separators */
      non_digit   = 1;
      fin         = size = ( int ) strlen( szTime );
      for( count = 0; count < size; count++ )
      {
         digit = szTime[ count ];
         if( HB_ISDIGIT( digit ) )
         {
            /* Process the digit for the current date field */
            if( h_pos == 1 )
               h_value = ( h_value * 10 ) + digit - '0';
            else if( m_pos == 1 )
               m_value = ( m_value * 10 ) + digit - '0';
            else if( s_pos == 1 )
               s_value = ( s_value * 10 ) + digit - '0';
            else if( c_pos == 1 )
            {
               s_value  += ( double ) ( digit - '0' ) / divisor;
               divisor  *= 10;
            }
            /* Treat the next non-digit as a date field separator */
            non_digit = 0;
         }
         else if( digit == '.' && s_pos == 1 && c_pos > 0 )
         {
            s_pos = 0;
         }
         else if( ( digit == 'A' || digit == 'P' ) && p_pos == 1 && szTime[ count + 1 ] == 'M' )
         {
            pm = ( digit == 'P' ) ? 1 : -1;
            count++;
            h_pos--;
            m_pos--;
            s_pos--;
            c_pos--;
            p_pos--;
         }
         else if( digit != ' ' )
         {
            if( h_value > 0 && m_value > 0 && s_value > 0 )
            {
               fin = count;
               break;
            }

            /* Process the non-digit */
            if( non_digit++ == 0 )
            {
               /* Only move to the next date field on the first
                  consecutive non-digit that is encountered */
               h_pos--;
               m_pos--;
               s_pos--;
               c_pos--;
               p_pos--;
            }
         }
         else if( digit == ' ' && p_pos > 1 && h_pos < 1 && m_pos < 1 && s_pos < 2 && c_pos < 2 )
         {
            s_pos = 0;
            c_pos = 0;
            p_pos--;
         }
         else if( h_value > 0 && m_value > 0 && s_value > 0 )
         {
            fin = count;
            break;
         }
      }

      if( pm )
      {
         h_value %= 12;
         if( pm == 1 )
            h_value += 12;
      }

      if( h_value > 23 || m_value > 59 || s_value >= 60.0 )
      {
         h_value  = 0;
         m_value  = 0;
         s_value  = 0;
      }
   }
   *ph_value   = h_value;
   *pm_value   = m_value;
   *ps_value   = s_value;

   return ( int ) fin;
}

HB_FUNC( CTOD )
{
   PHB_ITEM pszDate = hb_param( 1, HB_IT_STRING );

   if( pszDate )
   {
      int d_value  = 0, m_value = 0, y_value = 0;

      hb_datectod( hb_itemGetCPtr( pszDate ), &d_value, &m_value, &y_value );

      hb_retd( y_value, m_value, d_value );

      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, "CTOD", 1, hb_paramError( 1 ) );
}

HB_FUNC( DTOC )
{
   PHB_ITEM pDTOC = hb_param( 1, HB_IT_DATETIME );

   if( pDTOC )
   {
      char  szDate[ 9 ];
      char  szFormatted[ 11 ];

      hb_retc( hb_dateFormat( hb_dateDecStr( szDate, hb_itemGetDL( pDTOC ) ), szFormatted, hb_setGetDateFormat() ) );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, "DTOC", 1, hb_paramError( 1 ) );
}

HB_FUNC( DTOS )
{
   PHB_ITEM pszDate = hb_param( 1, HB_IT_DATETIME );

   if( pszDate )
   {
      char szDate[ 9 ];

      hb_retc( hb_dateDecStr( szDate, hb_itemGetDL( pszDate ) ) );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, "DTOS", 1, hb_paramError( 1 ) );
}

HB_FUNC( STOD )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_STRING );

   if( pDate )
   {
      BOOL bOk = FALSE;

      if( hb_itemGetCLen( pDate ) == 8 )
      {
         hb_retds( hb_itemGetCPtr( pDate) );
         bOk = TRUE;
      }
      else if( hb_itemGetCLen( pDate ) > 8 )
      {
         char szDate[ 9 ];
         HB_MEMCPY( szDate, hb_itemGetCPtr( pDate ), 8 );
         szDate[ 8 ] = '\0';
         hb_retds( szDate );
         bOk = TRUE;
      }

      if( bOk )
         return;
   }

   hb_retds( NULL );
}

HB_FUNC( YEAR )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retnllen( iYear, 5 );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, "YEAR", 1, hb_paramError( 1 ) );
}

HB_FUNC( MONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retnilen( iMonth, 3 );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1113, NULL, "MONTH", 1, hb_paramError( 1 ) );
}

HB_FUNC( DAY )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retnilen( iDay, 3 );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1114, NULL, "DAY", 1, hb_paramError( 1 ) );
}

HB_FUNC( TIME )
{
   char szResult[ 9 ];

   hb_dateTimeStr( szResult );
   hb_retclen( szResult, 8 );
}

#if defined( HB_EXTENSION )
HB_FUNC( TIMEOFDAY )
{
   char szResult[ 9 ];

   if( hb_pcount() == 0 )
      hb_dateTimeStr( szResult );
   else
   {
      int iSeconds = hb_parni( 1 );
      iSeconds %= 3600 * 24;
      hb_snprintf( szResult, sizeof( szResult ), "%02d:%02d:%02d",
                   iSeconds / 3600, ( iSeconds % 3600 ) / 60, iSeconds % 60 );
   }
   hb_retclen( szResult, 8 );
}
#endif

HB_FUNC( DATE )
{
   int iYear, iMonth, iDay;

   hb_dateToday( &iYear, &iMonth, &iDay );
   hb_retd( iYear, iMonth, iDay );
}

HB_FUNC( DOW )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      hb_retnilen( hb_dateJulianDOW( hb_itemGetDL( pDate ) ), 3 );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1115, NULL, "DOW", 1, hb_paramError( 1 ) );
}

HB_FUNC( DATETIME )
{
   if( hb_pcount() == 0 )
   {
      int      iYear, iMonth, iDay, iHour, iMinute;
      double   dSeconds;

      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_dateTime( &iHour, &iMinute, &dSeconds );
      hb_retdt( iYear, iMonth, iDay, iHour, iMinute, dSeconds, 0 );
      return;
   }

   hb_retdtl( hb_dateEncode( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ),
              hb_timeStampEncode( hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) ) );

}

HB_FUNC( TTOD )
{
   if( ISDATETIME( 1 ) )
   {
      hb_retdl( hb_pardl( 1 ) );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, "TTOD", 1, hb_paramError( 1 ) );
}

HB_FUNC( TTOS )
{
   if( ISDATETIME( 1 ) )
   {
      char szDateTime[ 19 ];

      hb_retc( hb_pardtsbuff( szDateTime, 1 ) );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, "TTOS", 1, hb_paramError( 1 ) );
}

HB_FUNC( TTOC )
{
   if( ISDATETIME( 1 ) )
   {
      char   szDate[ 19 ];
      char * szFormatted = ( char * ) hb_xgrab( 26 );

      hb_pardtsbuff( szDate, 1 );
      hb_retcAdopt( hb_datetimeFormat(
                      szDate,
                      szFormatted,
                      ( ( ISNUM( 2 ) && hb_parni( 2 ) == 2 ) || ( ISLOG( 2 ) && ! hb_parl( 2 ) ) ) ? NULL: hb_setGetDateFormat(),
                      hb_setGetTimeFormat() ) );

      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, "TTOC", 1, hb_paramError( 1 ) );
}

HB_FUNC( HOUR )
{
   PHB_ITEM pDateTime = hb_param( 1, HB_IT_DATETIME );

   if( pDateTime )
   {
      int iHour;

      hb_timeDecode( hb_itemGetT( pDateTime), &iHour, NULL, NULL );

      hb_retnilen( iHour, 2 );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, "HOUR", 1, hb_paramError( 1 ) );
}

HB_FUNC( MINUTE )
{
   PHB_ITEM pDateTime = hb_param( 1, HB_IT_DATETIME );

   if( pDateTime )
   {
      int iMinute;

      hb_timeDecode( hb_itemGetT( pDateTime), NULL, &iMinute, NULL );

      hb_retnilen( iMinute, 2 );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, "MINUTE", 1, hb_paramError( 1 ) );
}

HB_FUNC( SECS )
{
   PHB_ITEM pDateTime = hb_param( 1, HB_IT_DATETIME );

   if( pDateTime )
   {
      double dSeconds;

      hb_timeDecode( hb_itemGetT( pDateTime), NULL, NULL, &dSeconds );

      hb_retndlen( dSeconds, 3 + HB_DATETIMEDECIMALS, HB_DATETIMEDECIMALS );
      return;
   }

   HB_FUNC_EXEC( TSSECS );
}

HB_FUNC( CTOT )
{
   PHB_ITEM pzCTOT = hb_param( 1, HB_IT_STRING );

   if( pzCTOT )
   {
      int     len      = ( int ) hb_itemGetCLen( pzCTOT );
      int     d_value  = 0, m_value = 0, y_value = 0;
      int     h_value  = 0, n_value = 0, fin;
      double  s_value  = 0;

      if( ( ISNUM( 2 ) && hb_parni( 2 ) == 2 ) || ( ISLOG( 2 ) && ! hb_parl( 2 ) ) )
      {
         fin      = -1;
         d_value  = 30;
         m_value  = 12;
         y_value  = 1899;
      }
      else
         fin = hb_datectod( hb_itemGetCPtr( pzCTOT ), &d_value, &m_value, &y_value );

      if( fin < len )
      {
         char szTime[ 13 ];

         len            -= fin;
         memset( szTime, ' ', 12 );
         HB_MEMCPY( szTime, hb_itemGetCPtr( pzCTOT ) + fin + 1, ( len > 12 ? 12 : len ) );
         szTime[ 12 ]   = '\0';
         hb_timectot( szTime, &h_value, &n_value, &s_value );
      }

      hb_retdt( y_value, m_value, d_value, h_value, n_value, s_value, 0 );

      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, "CTOT", 1, hb_paramError( 1 ) );
}

HB_FUNC( STOT )
{
   PHB_ITEM pSTOT = hb_param( 1, HB_IT_STRING );

   if ( pSTOT && hb_itemGetCLen( pSTOT ) >= 8 )
   {
      long lDate    = 0, lTime = 0;
      char szTime[ 19 ];

      memset( szTime, ' ', 18 );
      szTime[ 18 ] = '\0';
      HB_MEMCPY( szTime, hb_itemGetCPtr( pSTOT ), ( hb_itemGetCLen( pSTOT ) > 18 ? 18 : hb_itemGetCLen( pSTOT ) ) );
      hb_datetimeEncStr( szTime, &lDate, &lTime );

      hb_retdtl( lDate, lTime );
      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, "STOT", 1, hb_paramError( 1 ) );
}

HB_FUNC( HMS2D )
{
   int      iHour = hb_parni( 1 );
   int      iMin  = hb_parni( 2 );
   double   dSec  = hb_parnd( 3 );

   hb_retnd( hb_timeEncode( iHour, iMin, dSec ) );
}


HB_FUNC( HB_TSTOSTR )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      char szBuffer[ 24 ];

      hb_dateTimeStampStr( szBuffer, lDate, lTime );
      if( hb_parl( 2 ) )
      {
         if( lTime == 0 )
         {
            if( lDate == 0 )
               hb_retc_const( "00:00" );
            else
               hb_retclen( szBuffer, 10 );
         }
         else
         {
            int i = 23;
            while( szBuffer[ i - 1 ] == '0' )
               --i;
            if( szBuffer[ i - 1 ] == '.' )
            {
               --i;
               if( szBuffer[ i - 1 ] == '0' && szBuffer[ i - 2 ] == '0' )
                  i -= 3;
            }
            if( lDate == 0 )
               hb_retclen( szBuffer + 11, i - 11 );
            else
               hb_retclen( szBuffer, i );
         }
      }
      else
         hb_retclen( szBuffer, 23 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
