/*
 * $Id: hbbitfield.c,v 1.1 2003/07/08 06:05:40 jonnymind Exp $
 */

/*
 * xHarbour Project source code:
 * Bitwise related function
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
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
#include "hbapierr.h"


/*******************************
* check for invalid parameters and report error
*/

static BOOL s_invalid_params( char *szFname, PHB_ITEM pParam1, PHB_ITEM pParam2,
      long *ret1, long *ret2 )
{
   if ( pParam1 == NULL || pParam2 == NULL ||
      (pParam1->type & ( HB_IT_INTEGER | HB_IT_LONG )) == 0 ||
      (pParam2->type & ( HB_IT_INTEGER | HB_IT_LONG )) == 0
      )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1089, "Non-integer parameters",
               szFname, 2, pParam1, pParam2 );
      return TRUE;
   }

   *ret1 = pParam1->type == HB_IT_INTEGER ?
         (long) hb_itemGetNI( pParam1 ) : hb_itemGetNL( pParam1 );

   if ( ret2 != NULL )
   {
      *ret2 = pParam2->type == HB_IT_INTEGER ?
         (long) hb_itemGetNI( pParam2 ) : hb_itemGetNL( pParam2 );
   }

   return FALSE;
}

/*******************************
* HB_BitAnd( nAnd1, nAnd2 ) --> nResult
*/
HB_FUNC( HB_BITAND )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   PHB_ITEM pAnd2 = hb_param(2, HB_IT_ANY );
   long ret1, ret2;

   if ( s_invalid_params( "HB_BITAND", pAnd1, pAnd2, &ret1, &ret2 ) )
   {
      return;
   }

   if (pAnd1->type ==  HB_IT_LONG || pAnd2->type ==  HB_IT_LONG )
   {
      hb_retnl( ret1 & ret2 );
   }
   else
   {

      hb_retni( (int) (ret1 & ret2) );
   }
}

/*******************************
* HB_BitOr( nAnd1, nAnd2 ) --> nOr
*/
HB_FUNC( HB_BITOR )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   PHB_ITEM pAnd2 = hb_param(2, HB_IT_ANY );
   long ret1, ret2;

   if ( s_invalid_params( "HB_BITOR", pAnd1, pAnd2, &ret1, &ret2 ) )
   {
      return;
   }

   if (pAnd1->type ==  HB_IT_LONG || pAnd2->type ==  HB_IT_LONG )
   {
      hb_retnl( ret1 | ret2 );
   }
   else
   {
      hb_retni( (int) (ret1 | ret2) );
   }
}

/*******************************
* HB_BitXor( nAnd1, nAnd2 ) --> nXor
*/
HB_FUNC( HB_BITXOR )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   PHB_ITEM pAnd2 = hb_param(2, HB_IT_ANY );
   long ret1, ret2;

   if ( s_invalid_params( "HB_BITXOR", pAnd1, pAnd2, &ret1, &ret2 ) )
   {
      return;
   }

   if (pAnd1->type ==  HB_IT_LONG || pAnd2->type ==  HB_IT_LONG )
   {
      hb_retnl( ret1 ^ ret2 );
   }
   else
   {
      hb_retni( (int) (ret1 ^ ret2) );
   }
}

/*******************************
* HB_BitNot( nAnd1 ) --> nNot
*/
HB_FUNC( HB_BITNOT )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   long ret1;

   if ( s_invalid_params( "HB_BITNOT", pAnd1, pAnd1, &ret1, NULL ) )
   {
      return;
   }

   if (pAnd1->type ==  HB_IT_LONG )
   {
      hb_retnl( ~ ret1 );
   }
   else
   {
      hb_retni( ~ ((int) ret1) );
   }
}

/*******************************
* HB_BitIsSet( nNumber, nBit ) --> lIsSet
*/
HB_FUNC( HB_BITISSET )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   PHB_ITEM pAnd2 = hb_param(2, HB_IT_ANY );
   long ret1, ret2;

   if ( s_invalid_params( "HB_BITISSET", pAnd1, pAnd2, &ret1, &ret2 ) )
   {
      return;
   }

   hb_retl( ret1 & ( 1 << ret2 ) );
}

/*******************************
* HB_BitSet( nNumber, nBit ) --> nResult
*/
HB_FUNC( HB_BITSET )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   PHB_ITEM pAnd2 = hb_param(2, HB_IT_ANY );
   long ret1, ret2;

   if ( s_invalid_params( "HB_BITSET", pAnd1, pAnd2, &ret1, &ret2 ) )
   {
      return;
   }

   if (pAnd1->type ==  HB_IT_LONG )
   {
      hb_retnl( ret1 | ( 1 << ret2 ));
   }
   else
   {
      hb_retni( (int) (ret1 | ( 1 << ret2)) );
   }
}

/*******************************
* HB_BitReset( nNumber, nBit ) --> nResult
*/
HB_FUNC( HB_BITRESET )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   PHB_ITEM pAnd2 = hb_param(2, HB_IT_ANY );
   long ret1, ret2;

   if ( s_invalid_params( "HB_BITRESET", pAnd1, pAnd2, &ret1, &ret2 ) )
   {
      return;
   }

   if (pAnd1->type ==  HB_IT_LONG )
   {
      hb_retnl( ret1 & (~( 1 << ret2 )));
   }
   else
   {
      hb_retni( (int) ( ret1 & (~( 1 << ret2 ))));
   }
}

/*******************************
* HB_BitShift( nNumber, nBit ) --> nResult
*/
HB_FUNC( HB_BITSHIFT )
{
   PHB_ITEM pAnd1 = hb_param(1, HB_IT_ANY );
   PHB_ITEM pAnd2 = hb_param(2, HB_IT_ANY );
   long ret1, ret2;

   if ( s_invalid_params( "HB_BITRESET", pAnd1, pAnd2, &ret1, &ret2 ) )
   {
      return;
   }

   if (pAnd1->type ==  HB_IT_LONG )
   {
      if (ret2 < 0 )
      {
         hb_retnl( ret1 >> (-ret2) );
      }
      else
      {
         hb_retnl( ret1 << ret2 );
      }
   }
   else
   {
      if (ret2 < 0 )
      {
         hb_retni( (int) ret1 >> (-ret2) );
      }
      else
      {
         hb_retni( (int) ret1 << ret2 );
      }

   }
}

