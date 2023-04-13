/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *   CT3 mathematical functions
 *     - FLOOR
 *     - CEILING
 *     - SIGN
 *     - LOG10
 *     - FACT
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

HB_FUNC( FLOOR )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = floor( dInput );

      hb_retnlen( dResult, 0, 0 );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_FLOOR,
                                  NULL, "FLOOR", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( CEILING )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = ceil( dInput );

      hb_retnlen( dResult, 0, 0 );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CEILING,
                                  NULL, "CEILING", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

HB_FUNC( SIGN )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      int      iResult;

      if( dInput == 0.00 )
         iResult = 0;
      else
      {
         if( dInput > 0.00 )
            iResult = 1;
         else
            iResult = -1;
      }
      hb_retni( iResult );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_SIGN,
                                  NULL, "SIGN", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retni( 0 );
      }
   }
}

HB_FUNC( LOG10 )
{
   if( ISNUM( 1 ) )
   {
      double   dInput = hb_parnd( 1 );
      double   dResult;

      hb_mathResetError();
      dResult = log10( dInput );

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
               /* math exception is up to the Harbour function, so do this as CTIII compatible as possible */
               switch( iLastError )
               {
                  case HB_MATH_ERR_SING:                 /* argument to log10 was 0.0 */
                  case HB_MATH_ERR_DOMAIN:               /* argument to log10 was < 0.0 */
                  {
                     hb_retndlen( -HUGE_VAL, -1, -1 );   /* return -infinity */
                  }; break;
                  default:
                     hb_retnd( 0.0 );
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
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_LOG10,
                                  NULL, "LOG10", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retni( 0 );
      }
   }
}

HB_FUNC( FACT )
{
   if( ISNUM( 1 ) )
   {
      int      iInput   = hb_parni( 1 );
      int      i;
      double   dResult  = 1.0;

      if( ( iInput >= 0 ) && ( iInput < 22 ) )
      {
         for( i = 1; i <= iInput; i++ )
         {
            dResult *= ( double ) i;
         }
         hb_retnd( dResult );
      }
      else
      {
         hb_retnd( -1.0 );
      }
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_FACT,
                                  NULL, "FACT", 0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
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

