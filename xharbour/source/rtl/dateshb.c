/*
 * $Id: dateshb.c,v 1.19 2007/06/30 03:06:55 peterrees Exp $
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
   int d_value = 0, m_value = 0, y_value = 0;
   int fin = 0;

   if( szDate )
   {
      int d_pos = 0, m_pos = 0, y_pos = 0;
      int count, digit, non_digit, size = strlen( hb_set.HB_SET_DATEFORMAT );

      for( count = 0; count < size; count++ )
      {
         switch( hb_set.HB_SET_DATEFORMAT[ count ] )
         {
            case 'D':
            case 'd':
               if( d_pos == 0 )
               {
                  if( m_pos == 0 && y_pos == 0 ) d_pos = 1;
                  else if( m_pos == 0 || y_pos == 0 ) d_pos = 2;
                  else d_pos = 3;
               }
               break;

            case 'M':
            case 'm':
               if( m_pos == 0 )
               {
                  if( d_pos == 0 && y_pos == 0 ) m_pos = 1;
                  else if( d_pos == 0 || y_pos == 0 ) m_pos = 2;
                  else m_pos = 3;
               }
               break;

            case 'Y':
            case 'y':
               if( y_pos == 0 )
               {
                  if( m_pos == 0 && d_pos == 0 ) y_pos = 1;
                  else if( m_pos == 0 || d_pos == 0 ) y_pos = 2;
                  else y_pos = 3;
               }
         }
      }

      /* If there are non-digits at the start of the date field,
         they are not to be treated as date field separators */
      non_digit = 1;
      fin = size = strlen( szDate );

      for( count = 0; count < size; count++ )
      {
         digit = szDate[ count ];

         if( isdigit( digit ) )
         {
            /* Process the digit for the current date field */
            if( d_pos == 1 )
            {
               d_value = ( d_value * 10 ) + digit - '0';
            }
            else if( m_pos == 1 )
            {
               m_value = ( m_value * 10 ) + digit - '0';
            }
            else if( y_pos == 1 )
            {
               y_value = ( y_value * 10 ) + digit - '0';
            }

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
         count = hb_set.HB_SET_EPOCH % 100;
         digit = hb_set.HB_SET_EPOCH / 100;

         if( y_value >= count )
         {
            y_value += ( digit * 100 );
         }
         else
         {
            y_value += ( ( digit * 100 ) + 100 );
         }
      }
   }
   *pd_value = d_value;
   *pm_value = m_value;
   *py_value = y_value;

   return fin;
}

static int hb_timectot( char const * szTime, int * ph_value, int * pm_value, double * ps_value )
{
   int h_value = 0, m_value = 0;
   double s_value = 0;
   int h_pos = 0, m_pos = 0, s_pos = 0, c_pos = 0, p_pos = 0;
   int count, digit, non_digit, size = strlen( hb_set.HB_SET_TIMEFORMAT );
   int fin = 0, pm = 0, divisor = 10;

   if( szTime )
   {
      digit = 1;
      for( count = 0; count < size; count++ )
      {
         switch( hb_set.HB_SET_TIMEFORMAT[ count ] )
         {
            case 'H':
            case 'h':
               if( h_pos == 0 )
               {
                  h_pos = digit++;
               }
               break;
            case 'M':
            case 'm':
               if( m_pos == 0 )
               {
                  m_pos = digit++;
               }
               break;
            case 'S':
            case 's':
               if( s_pos == 0 )
               {
                  s_pos = digit++;
               }
            case 'C':
            case 'c':
               if( c_pos == 0 )
               {
                  if( s_pos != 0 ) c_pos = s_pos;
               }
            case 'P':
            case 'p':
               if( p_pos == 0 )
               {
                  if( hb_set.HB_SET_TIMEFORMAT[ count+1 ] == 'M' ||
                      hb_set.HB_SET_TIMEFORMAT[ count+1 ] == 'm' )
                  {
                     p_pos = digit++;
                     count++;
                  }
               }
         }
      }

      /* If there are non-digits at the start of the date field,
         they are not to be treated as date field separators */
      non_digit = 1;
      fin = size = strlen( szTime );
      for( count = 0; count < size; count++ )
      {
         digit = szTime[ count ];
         if( isdigit( digit ) )
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
               s_value += (double)( digit - '0' ) / divisor;
               divisor *= 10;
            }
            /* Treat the next non-digit as a date field separator */
            non_digit = 0;
         }
         else if( digit == '.' && s_pos == 1 && c_pos > 0 )
         {
            s_pos = 0;
         }
         else if( ( digit == 'A' || digit == 'P' ) && p_pos == 1 && szTime[ count+1 ] == 'M' )
         {
            if( digit == 'P' )
            {
               pm = 1;
            }
            else
            {
               pm = -1;
            }
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
         if( pm == 1)
         {
            h_value += 12;
         }
      }

      if( h_value > 23 || m_value > 59 || s_value >= 60.0 )
      {
         h_value = 0;
         m_value = 0;
         s_value = 0;
      }
   }
   *ph_value = h_value;
   *pm_value = m_value;
   *ps_value = s_value;

   return fin;
}

HB_FUNC( CTOD )
{
   if( ISCHAR( 1 ) )
   {
      char * szDate = hb_parcx( 1 );
      int d_value = 0, m_value = 0, y_value = 0;

      hb_datectod( szDate, &d_value, &m_value, &y_value );

      hb_retd( y_value, m_value, d_value );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, "CTOD", 1, hb_paramError( 1 ) );
}

HB_FUNC( DTOC )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];

      hb_retc( hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, hb_set.HB_SET_DATEFORMAT ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, "DTOC", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( DTOS )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];

      hb_retc( hb_pardsbuff( szDate, 1 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, "DTOS", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( STOD )
{
   ULONG iLen = hb_parclen( 1 ) ;
   if ( iLen == 8 )
   {
     hb_retds( hb_parc( 1 ) );
   }
   else if ( iLen < 8 )
   {
     hb_retds( NULL );
   }
   else
   {
     char szDate[ 9 ] ;
     char *pDate = hb_parcx( 1 ) ;
     memcpy( szDate, pDate, 8 ) ;
     szDate[ 8 ] = '\0' ;
     hb_retds( szDate );
   }
}

HB_FUNC( YEAR )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( pDate->item.asDate.value, &iYear, &iMonth, &iDay );

      hb_retnllen( iYear, 5 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, "YEAR", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( MONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( pDate->item.asDate.value, &iYear, &iMonth, &iDay );

      hb_retnilen( iMonth, 3 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1113, NULL, "MONTH", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( DAY )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( pDate->item.asDate.value, &iYear, &iMonth, &iDay );

      hb_retnilen( iDay, 3 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1114, NULL, "DAY", 1, hb_paramError( 1 ) );
   }
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

   if ( hb_pcount() == 0 )
   {
      hb_dateTimeStr( szResult );
   }
   else
   {
      int iSeconds = hb_parni(1);
      iSeconds %= 3600*24;
      sprintf( szResult, "%02d:%02d:%02d",
      iSeconds/3600 , (iSeconds % 3600)/60, iSeconds % 60 );
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

HB_FUNC( DATETIME )
{
   int iYear, iMonth, iDay, iHour, iMinute;
   double dSeconds;
   hb_dateToday( &iYear, &iMonth, &iDay );
   hb_dateTime( &iHour, &iMinute, &dSeconds );
   hb_retdt( iYear, iMonth, iDay, iHour, iMinute, dSeconds, 0 );
}

HB_FUNC( DOW )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      hb_retnilen( hb_dateJulianDOW( pDate->item.asDate.value ), 3 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1115, NULL, "DOW", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( TTOD )
{
   if( ISDATE( 1 ) )
   {
      hb_retdl( hb_pardl( 1 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, "TTOD", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( TTOS )
{
   if( ISDATE( 1 ) )
   {
      char szDateTime[ 19 ];

      hb_retc( hb_pardtsbuff( szDateTime, 1 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, "TTOS", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( TTOC )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 19 ];
      char * szFormatted = ( char * ) hb_xgrab( 26 );

      hb_pardtsbuff( szDate, 1);

      if( (ISNUM(2) && hb_parni(2) == 2) || (ISLOG(2) && !hb_parl(2) ) )
      {
         hb_retcAdopt( hb_datetimeFormat( szDate, szFormatted, NULL, hb_set.HB_SET_TIMEFORMAT ) );
      }
      else
      {
         hb_retcAdopt( hb_datetimeFormat( szDate, szFormatted, hb_set.HB_SET_DATEFORMAT, hb_set.HB_SET_TIMEFORMAT ) );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, "TTOC", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( HOUR )
{
   PHB_ITEM pDateTime = hb_param( 1, HB_IT_DATE );

   if( pDateTime )
   {
      int iHour;

      hb_timeDecode( pDateTime->item.asDate.time, &iHour, NULL, NULL );

      hb_retnilen( iHour, 2 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, "HOUR", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( MINUTE )
{
   PHB_ITEM pDateTime = hb_param( 1, HB_IT_DATE );

   if( pDateTime )
   {
      int iMinute;

      hb_timeDecode( pDateTime->item.asDate.time, NULL, &iMinute, NULL );

      hb_retnilen( iMinute, 2 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, "MINUTE", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( SECS )
{
   if( ISDATE( 1 ) )
   {
      double dSeconds;

      hb_timeDecode( hb_param( 1, HB_IT_DATE )->item.asDate.time, NULL, NULL, &dSeconds );

      hb_retndlen( dSeconds, 3 + HB_DATETIMEDECIMALS, HB_DATETIMEDECIMALS );
   }
   else
   {
      HB_FUNCNAME( TSSECS )();
   }
}

HB_FUNC( CTOT )
{
   if( ISCHAR( 1 ) )
   {
      char * szDate = hb_parcx( 1 );
      int len = hb_parclen( 1 );
      int d_value = 0, m_value = 0, y_value = 0;
      int h_value = 0, n_value = 0, fin;
      double s_value = 0;

      if( (ISNUM(2) && hb_parni(2) == 2) || (ISLOG(2) && !hb_parl(2) ) )
      {
         fin = -1;
         d_value = 30;
         m_value = 12;
         y_value = 1899;
      }
      else
      {
         fin = hb_datectod( szDate, &d_value, &m_value, &y_value );
      }

      if( fin < len )
      {
         char szTime[ 13 ];

         len -= fin;
         memset( szTime, ' ', 12 );
         memcpy( szTime, szDate+fin+1, (len>12?12:len) );
         szTime[12] = '\0';
         hb_timectot( szTime, &h_value, &n_value, &s_value );
      }

      hb_retdt( y_value, m_value, d_value, h_value, n_value, s_value, 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, "CTOT", 1, hb_paramError( 1 ) );
}

HB_FUNC( STOT )
{
   int len = hb_parclen( 1 );
   if( ISCHAR( 1 ) && len >= 8 )
   {
      char * szDate = hb_parcx( 1 );
      LONG lDate = 0, lTime = 0;
      char szTime[ 19 ];

      memset( szTime, ' ', 18 );
      szTime[18] = '\0';
      memcpy( szTime, szDate, (len>18?18:len) );
      hb_datetimeEncStr( szTime, &lDate, &lTime );

      hb_retdtl( lDate, lTime );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, "STOT", 1, hb_paramError( 1 ) );
}

HB_FUNC( HMS2D )
{
   int iHour = hb_parni(1);
   int iMin  = hb_parni(2);
   double dSec  = hb_parnd(3);

   hb_retnd( hb_timeEncode( iHour, iMin, dSec ) );
}
