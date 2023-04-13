/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Financial functions
 *     - PV
 *     - FV
 *     - PAYMENT
 *     - PERIODS
 *     - RATE
 *
 * NOTE: All these functions were builded using Borland C++ 5.5 (free version)
 *
 * Copyright 2001  Alejandro de Garate  <alex_degarate@hotmail.com>
 *
 * Documentation and changes concerning error handling Copyright 2001
 *   IntTec GmbH, Freiburg, Germany, Author: Martin Vogel <vogel@inttec.de>
 *
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

#include "ct.h"

HB_FUNC( FV )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) )
   {
      double   dPayment = hb_parnd( 1 );
      double   dRate    = hb_parnd( 2 );
      double   dTime    = hb_parnd( 3 );
      double   dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dPayment * dTime;
      }
      else
      {
         hb_mathResetError();
         dResult = dPayment * ( pow( 1.0 + dRate, dTime ) - 1.0 ) / dRate;
         if( hb_mathIsMathErr() )
         {
            /* the C-RTL provides a kind of matherr() mechanism */
            HB_MATH_EXCEPTION hb_exc;
            int               iLastError = hb_mathGetLastError( &hb_exc );
            if( iLastError != HB_MATH_ERR_NONE )
            {
               if( hb_exc.handled )
               {
                  hb_retnd( dPayment * ( hb_exc.retval - 1.0 ) / dRate );
               }
               else
               {
                  /* math exception is up to the Harbour function, so do this as CTIII compatible as possible:
                     replace the errorneous value of pow() with 0.0 */
                  hb_retnd( dPayment * ( -1.0 ) / dRate );
               }
               return;
            }
         }
      }

      hb_retnd( dResult );

   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_FV,
                                  NULL, "FV", 0, EF_CANSUBSTITUTE, 3,
                                  hb_paramError( 1 ), hb_paramError( 2 ),
                                  hb_paramError( 3 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retnd( 0.0 );
      }
   }
}

HB_FUNC( PV )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) )
   {
      double   dPayment = hb_parnd( 1 );
      double   dRate    = hb_parnd( 2 );
      double   dTime    = hb_parnd( 3 );
      double   dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dPayment * dTime;
      }
      else
      {
         hb_mathResetError();
         dResult = dPayment * ( 1.0 - pow( 1.0 + dRate, -dTime ) ) / dRate;
         if( hb_mathIsMathErr() )
         {
            /* the C-RTL provides a kind of matherr() mechanism */
            HB_MATH_EXCEPTION hb_exc;
            int               iLastError = hb_mathGetLastError( &hb_exc );
            if( iLastError != HB_MATH_ERR_NONE )
            {
               if( hb_exc.handled )
               {
                  hb_retnd( dPayment * ( 1.0 - hb_exc.retval ) / dRate );
               }
               else
               {
                  /* math exception is up to the Harbour function, so do this as CTIII compatible as possible:
                     replace the errorneous value of pow() with 0.0 */
                  hb_retnd( dPayment / dRate );
               }
               return;
            }
         }
      }

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_PV,
                                  NULL, "PV", 0, EF_CANSUBSTITUTE, 3,
                                  hb_paramError( 1 ), hb_paramError( 2 ),
                                  hb_paramError( 3 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retnd( 0.0 );
      }
   }
}

HB_FUNC( PAYMENT )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) )
   {
      double   dCapital = hb_parnd( 1 );
      double   dRate    = hb_parnd( 2 );
      double   dTime    = hb_parnd( 3 );
      double   dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dCapital / dTime;
      }
      else
      {
         hb_mathResetError();
         dResult = dCapital * dRate / ( 1.0 - pow( 1.0 + dRate, -dTime ) );
         if( hb_mathIsMathErr() )
         {
            /* the C-RTL provides a kind of matherr() mechanism */
            HB_MATH_EXCEPTION hb_exc;
            int               iLastError = hb_mathGetLastError( &hb_exc );
            if( iLastError != HB_MATH_ERR_NONE )
            {
               if( hb_exc.handled )
               {
                  hb_retnd( dCapital * dRate / ( 1.0 - hb_exc.retval ) );
               }
               else
               {
                  /* math exception is up to the Harbour function, so do this as CTIII compatible as possible:
                     replace the errorneous value of pow() with 0.0 */
                  hb_retnd( dCapital * dRate );
               }
               return;
            }
         }
      }

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_PAYMENT,
                                  NULL, "PAYMENT", 0, EF_CANSUBSTITUTE, 3,
                                  hb_paramError( 1 ), hb_paramError( 2 ),
                                  hb_paramError( 3 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retnd( 0.0 );
      }
   }
}

HB_FUNC( PERIODS )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) )
   {
      double   dCapital = hb_parnd( 1 );
      double   dPayment = hb_parnd( 2 );
      double   dRate    = hb_parnd( 3 );
      double   dResult;

      if( dPayment <= dCapital * dRate )
      {
         /* in this case infinite time is needed to cancel the loan */
         hb_retnd( -1.0 );
         return;
      }

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dCapital / dPayment;
      }
      else
      {
         double dResult2;
         hb_mathResetError();
         /* Note that this first expression will never give an error since dCapital*dRate/dPayment < 1.0, see above */
         dResult2 = -log( 1.0 - ( dCapital * dRate / dPayment ) );
         dResult  = dResult2 / log( 1 + dRate );
         if( hb_mathIsMathErr() )
         {
            /* the C-RTL provides a kind of matherr() mechanism */
            HB_MATH_EXCEPTION hb_exc;
            int               iLastError = hb_mathGetLastError( &hb_exc );
            if( iLastError != HB_MATH_ERR_NONE )
            {
               if( hb_exc.handled )
               {
                  hb_retnd( dResult2 / hb_exc.retval );
               }
               else
               {
                  /* math exception is up to the Harbour function, so do this as CTIII compatible as possible:
                     replace the errorneous value of log() with -INF */
                  hb_retnd( -0.0 );
               }
               return;
            }
         }
      }

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_PERIODS,
                                  NULL, "PERIODS", 0, EF_CANSUBSTITUTE, 3,
                                  hb_paramError( 1 ), hb_paramError( 2 ),
                                  hb_paramError( 3 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retnd( 0.0 );
      }
   }
}

HB_FUNC( RATE )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) )
   {
      double   dCapital = hb_parnd( 1 );
      double   dPayment = hb_parnd( 2 );
      double   dTime    = hb_parnd( 3 );
      double   dAux;                /* estimated payment to compare for      */
      double   dEpsilon = 0.00001;  /* mimimal to consider 2 numbers as equal*/
      double   dScale   = 1.0;      /* fractional step                       */
      double   r;                   /* temptative rate                       */
      double   j        = 1.0;      /* index                                 */
      double   dExp;

      while( j < 1020.0 )     /* maximum anual rate */
      {
         r = j * 0.000833333; /* j * ( 0.01 / 12.0)  mensual's rate */

         /* replace PAYMENT() function overhead */
         hb_mathResetError();
         dExp = pow( ( 1.0 + r ), dTime );
         if( hb_mathIsMathErr() )
         {
            /* the C-RTL provides a kind of matherr() mechanism */
            HB_MATH_EXCEPTION hb_exc;
            int               iLastError = hb_mathGetLastError( &hb_exc );
            if( iLastError != HB_MATH_ERR_NONE )
            {
               if( hb_exc.handled )
               {
                  dExp = hb_exc.retval;
               }
               else
               {
                  /* TODO: Check if this is a correct default correction value for pow() */
                  dExp = 0.0;
               }
            }
         }

         dAux = dCapital * ( ( dExp * r ) / ( dExp - 1.0 ) );

         if( dAux > dPayment )
         {
            j        = j - dScale;
            dScale   = dScale * 0.10;

            if( ( dAux - dPayment ) < dEpsilon )
               break;
         }
         else
            j = j + dScale;

      } /* endwhile */

      hb_retnd( j * 0.000833333 );    /* return as mensual's rate */
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_RATE,
                                  NULL, "RATE", 0, EF_CANSUBSTITUTE, 3,
                                  hb_paramError( 1 ), hb_paramError( 2 ),
                                  hb_paramError( 3 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retnd( 0.0 );
      }
   }
}

