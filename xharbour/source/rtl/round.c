/*
 * $Id: round.c,v 1.8 2004/02/08 12:19:59 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * ROUND(), INT() functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *    INT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <math.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

static double hb_numPow10( int nPrecision )
{
   static const double doBase = 10.0f;
   static double s_dPow10[17];
   static int s_fPowInit = 0;

   if ( ! s_fPowInit )
   {
      int i;
      for ( i = 0; i <= 16; i++ )
      {
         s_dPow10[ i ] = pow( doBase, (double) i );
      }
      s_fPowInit = 1;
   }

   if ( nPrecision < 16 )
   {
      if ( nPrecision >= 0 )
      {
         return s_dPow10[ nPrecision ];
      }
      else if ( nPrecision > -16 )
      {
         return 1 / s_dPow10[ -nPrecision ];
      }
   }
   return pow(doBase, (double) nPrecision);
}


HB_FUNC( INT )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );

   if( pNumber )
   {
      if( HB_IS_NUMBER_INT( pNumber ) )
      {
         hb_itemReturnCopy( pNumber );
      }
      else
      {
         int iWidth;
         double dNumber = hb_itemGetND( pNumber );

         hb_itemGetNLen( pNumber, &iWidth, NULL );
         modf( dNumber, &dNumber );

         if( (double) LONG_MIN <= dNumber && dNumber <= (double) LONG_MAX )
         {
            PHB_ITEM pNumber = hb_itemNew( NULL );
            hb_itemPutNIntLen( pNumber, (LONG) dNumber, iWidth );
            hb_itemRelease( hb_itemReturn( pNumber ) );
         }
#ifndef HB_LONG_LONG_OFF
         else if( (double) LONGLONG_MIN <= dNumber && dNumber <= (double) LONGLONG_MAX )
         {
            PHB_ITEM pNumber = hb_itemNew( NULL );
            hb_itemPutNIntLen( pNumber, (LONGLONG) dNumber, iWidth );
            hb_itemRelease( hb_itemReturn( pNumber ) );
         }
#endif
         else
         {
            hb_retndlen( dNumber, iWidth, 0 );
         }
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1090, NULL, "INT", 1, hb_paramError( 1 ) );
   }
}

double hb_numRound( double dNum, int iDec )
{
   static const double doBase = 10.0f;
   double doComplete5, doComplete5i, dPow;

   HB_TRACE(HB_TR_DEBUG, ("hb_numRound(%lf, %d)", dNum, iDec));

   dPow = hb_numPow10( iDec );
   doComplete5 = dNum * dPow * doBase;

#if 0
   /*
    * this is a hack for people who cannot live without hacked FL values
    * in rounding
    */
   if( dNum < 0.0f )
      doComplete5 -= 5.0f - hb_numPow10( (int) log10( -doComplete5 ) - 15 );
   else
      doComplete5 += 5.0f + hb_numPow10( (int) log10( doComplete5 ) - 15 );
#else
   if( dNum < 0.0f )
      doComplete5 -= 5.0f;
   else
      doComplete5 += 5.0f;
#endif

   doComplete5 /= doBase;
   modf( doComplete5, &doComplete5i );

   return doComplete5i / dPow;
}

HB_FUNC( ROUND )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );

   if( pNumber && hb_param( 2, HB_IT_NUMERIC ) )
   {
      int iDec = hb_parni( 2 );

      if( HB_IS_NUMBER_INT( pNumber ) && iDec >= 0 )
      {
         hb_itemReturnCopy( pNumber );
      }
      else
      {
         if( iDec > 0 )
         {
            hb_retndlen( hb_numRound( hb_parnd( 1 ), iDec ), 0, iDec );
         }
         else
         {
            double dNumber = hb_numRound( hb_parnd( 1 ), iDec );

            if( (double) LONG_MIN <= dNumber && dNumber <= (double) LONG_MAX )
            {
               PHB_ITEM pNumber = hb_itemNew( NULL );

               hb_itemPutNInt( pNumber, (LONG) dNumber );

               hb_itemReturn( pNumber );
            }
#ifndef HB_LONG_LONG_OFF
            else if( (double) LONGLONG_MIN <= dNumber && dNumber <= (double) LONGLONG_MAX )
            {
               PHB_ITEM pNumber = hb_itemNew( NULL );

               hb_itemPutNInt( pNumber, (LONGLONG) dNumber );

               hb_itemReturn( pNumber );
            }
#endif
            else
            {
               hb_retndlen( dNumber, 0, HB_MAX( iDec, 0 ) );
            }
         }
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1094, NULL, "ROUND", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}
