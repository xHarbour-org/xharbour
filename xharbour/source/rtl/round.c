/*
 * $Id: round.c,v 1.13 2004/02/20 16:54:39 ronpinkas Exp $
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
         return 1.0 / s_dPow10[ -nPrecision ];
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
            HB_ITEM Number;

            Number.type = HB_IT_NIL;
            hb_itemPutNIntLen( &Number, (LONG) dNumber, iWidth );

            hb_itemReturn( &Number );
         }
#ifndef HB_LONG_LONG_OFF
         else if( (double) LONGLONG_MIN <= dNumber && dNumber <= (double) LONGLONG_MAX )
         {
            HB_ITEM Number;

            Number.type = HB_IT_NIL;
            hb_itemPutNIntLen( &Number, (LONGLONG) dNumber, iWidth );

            hb_itemReturn( &Number );
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

   if( dNum == 0.0 )
      return 0.0;

   if ( iDec < 0 )
   {
      dPow = hb_numPow10( -iDec );
      doComplete5 = dNum / dPow * doBase;
   }
   else
   {
      dPow = hb_numPow10( iDec );
      doComplete5 = dNum * dPow * doBase;
   }

/*
 * double precision if 15 digit the 16th one is usually wrong but
 * can give some information about number,
 * Clipper display 16 digit only others are set to 0
 * many people don't know/understand FL arithmetic. They expect
 * that it will behaves in the same way as real numbers. It's not
 * true but in business application we can try to hide this problem
 * for them. Usually they not need such big precision in presented
 * numbers so we can decrease the precision to 15 digits and use
 * the use the cut part for proper rounding. It should resolve
 * most of problems. But if someone totally  not understand FL
 * and will try to convert big matrix or sth like that it's quite
 * possible that he chose one of the natural school algorithm which
 * works nice with real numbers but can give very bad results in FL.
 * In such case it could be good to decrease precision even more.
 * It not fixes the used algorithm of course but will make many users
 * happy because they can see nice (proper) result.
 * So maybe it will be good to add SET PRECISION TO <n> for them and
 * use the similar hack in ==, >=, <=, <, > operations if it's set.
 */

//#define HB_NUM_PRECISION  16

#ifdef HB_NUM_PRECISION
   /*
    * this is a hack for people who cannot live without hacked FL values
    * in rounding
    */
   {
      int iDecR, iPrec;
      BOOL fNeg;

      if ( dNum < 0 )
      {
         fNeg = TRUE;
         dNum = -dNum;
      }
      else
      {
         fNeg = FALSE;
      }
      iDecR = (int) log10( dNum );
      iPrec = iDecR + iDec;

      if ( iPrec < -1 )
      {
         return 0.0;
      }
      else
      {
         if ( iPrec > HB_NUM_PRECISION )
         {
            iDec = HB_NUM_PRECISION - ( dNum < 1.0 ? 0 : 1 ) - iDecR;
            iPrec = -1;
         }
         else
         {
            iPrec -= HB_NUM_PRECISION;
         }
      }
      if ( iDec < 0 )
      {
         dPow = hb_numPow10( -iDec );
         doComplete5 = dNum / dPow * doBase + 5.0 + hb_numPow10( iPrec );
      }
      else
      {
         dPow = hb_numPow10( iDec );
         doComplete5 = dNum * dPow * doBase + 5.0 + hb_numPow10( iPrec );
      }

      if ( fNeg )
      {
         doComplete5 = -doComplete5;
      }
   }
#else
   if( dNum < 0.0f )
      doComplete5 -= 5.0f;
   else
      doComplete5 += 5.0f;
#endif

   doComplete5 /= doBase;
   modf( doComplete5, &doComplete5i );

   if ( iDec < 0 )
      return doComplete5i * dPow;
   else
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
               HB_ITEM Number;

               Number.type = HB_IT_NIL;
               hb_itemPutNInt( &Number, (LONG) dNumber );

               hb_itemReturn( &Number );
            }
#ifndef HB_LONG_LONG_OFF
            else if( (double) LONGLONG_MIN <= dNumber && dNumber <= (double) LONGLONG_MAX )
            {
               HB_ITEM Number;

               Number.type = HB_IT_NIL;
               hb_itemPutNInt( &Number, (LONGLONG) dNumber );

               hb_itemReturn( &Number );
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
