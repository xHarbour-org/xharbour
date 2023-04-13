/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 trigonometric functions
 *     - PI
 *     - SIN
 *     - COS
 *     - TAN
 *     - COT
 *     - ASIN
 *     - ACOS
 *     - ATAN
 *     - SINH
 *     - COSH
 *     - TANH
 *     - ATN2
 *     - RTOD
 *     - DTOR
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

HB_FUNC( PI )
{
   hb_retnd( CT_PI );
}

HB_FUNC( SIN )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = sin( dInput );

      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               hb_retndlen( HUGE_VAL, -1, -1 );
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_SIN,
                                  NULL, "SIN", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( COS )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = cos( dInput );

      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               hb_retndlen( HUGE_VAL, -1, -1 );
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_COS,
                                  NULL, "COS", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( TAN )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = tan( dInput );

      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               hb_retndlen( HUGE_VAL, -1, -1 );
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TAN,
                                  NULL, "TAN", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( COT )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = 1 / tan( dInput );
      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               hb_retndlen( HUGE_VAL, -1, -1 );
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_COT,
                                  NULL, "COT", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( ASIN )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = asin( dInput );
      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               hb_retndlen( HUGE_VAL, -1, -1 ); /* NOTE: CTIII crashes when argument is not between -1 and 1 , but we
                                                   better generate a NaN/overflow here */
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ASIN,
                                  NULL, "ASIN", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( ACOS )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = acos( dInput );
      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               hb_retndlen( HUGE_VAL, -1, -1 ); /* NOTE: CTIII crashes when argument is not between -1 and 1 , but we
                                                   better generate a NaN/overflow here */
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ACOS,
                                  NULL, "ACOS", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( ATAN )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = atan( dInput );

      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               /* atan normally don't error, but it's save to return PI()/2 or -PI()/2, respectively, as these
                  are the boundary result values */
               if( dInput < 0.0 )
               {
                  hb_retnd( -CT_PI / 2.0 );
               }
               else
               {
                  hb_retnd( CT_PI / 2.0 );
               }
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATAN,
                                  NULL, "ATAN", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( ATN2 )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      double   dY = hb_parnd( 1 );
      double   dX = hb_parnd( 2 );
      double   dResult;

      hb_mathResetError();
      dResult = atan2( dY, dX ); /* NOTE: parameters are swapped */

      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               /* DOMAIN error: both arguments to atan2 have been 0 */
               /* CTIII behaves very strange here: atn2 (0.0, 0.0) == -PI
                                                   atn2 (0.0, -0.0) == 0.0
                                                   atn2 (-0.0, 0.0) == -PI
                                                   atn2 (-0.0, -0.0) == -2*PI */
               if( dX < 0.0 )
               {
                  if( dY < 0.0 )
                  {
                     hb_retnd( -2.0 * CT_PI );
                  }
                  else
                  {
                     hb_retnd( 0.0 );
                  }
               }
               else
               {
                  hb_retnd( -CT_PI );
               }
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATN2,
                                  NULL, "ATN2", 0, EF_CANSUBSTITUTE, 2,
                                  hb_paramError( 1 ), hb_paramError( 2 ) );
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

HB_FUNC( SINH )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = sinh( dInput );
      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               /* OVERFLOW error: we have no CTIII behaviour to follow, so return +INF or -INF, respectively */
               if( dInput < 0.0 )
               {
                  hb_retndlen( -HUGE_VAL, -1, -1 );
               }
               else
               {
                  hb_retndlen( HUGE_VAL, -1, -1 );
               }
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_SINH,
                                  NULL, "SINH", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( COSH )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = cosh( dInput );
      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               /* OVERFLOW error: we have no CTIII behaviour to follow, so return +INF */
               hb_retndlen( HUGE_VAL, -1, -1 );
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_COSH,
                                  NULL, "COSH", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( TANH )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = tanh( dInput );
      if( hb_mathIsMathErr() )
      {
         /* the C-RTL provides a kind of matherr() mechanism */
         HB_MATH_EXCEPTION hb_exc;
         int               iLastError = hb_mathGetLastError( &hb_exc );
         if( iLastError != HB_MATH_ERR_NONE )
         {
            if( hb_exc.handled )
            {
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            }
            else
            {
               /* normally, tanh() doesn't give errors, but let's return -1 or +1, respectively, as these
                  are the boundary result values */
               if( dInput < 0.0 )
               {
                  hb_retnd( -1.0 );
               }
               else
               {
                  hb_retnd( 1.0 );
               }
            }
            return;
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TANH,
                                  NULL, "TANH", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( RTOD )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;
      dResult = ( 180.0 / CT_PI ) * dInput;
      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_RTOD,
                                  NULL, "RTOD", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( DTOR )
{
   if( ISNUM( 1 ) )
   {
      double   dInput   = hb_parnd( 1 );
      double   dResult  = ( CT_PI / 180.0 ) * dInput;
      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_RTOD,
                                  NULL, "RTOD", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

