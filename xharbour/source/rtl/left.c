/*
 * $Id: left.c,v 1.8 2003/07/18 21:42:35 andijahja Exp $
 */

/*
 * xHarbour Project source code:
 * Left() function
 *
 * Copyright 2001 Ron Pinkas <ron@@ronpinkas.com>
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbapierr.h"
#include "hbstack.h"

/* returns the left-most n characters in string */
HB_FUNC( LEFT )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && hb_param( 2, HB_IT_NUMERIC ) )
   {
      char *sLeft, *sString = pText->item.asString.value;
      LONG lLeft = hb_parnl( 2 );
      ULONG ulLen = pText->item.asString.length;

      HB_TRACE( HB_TR_DEBUG, ("Left( '%s', %i ) %i", sString, lLeft, ulLen ) );

      /* Must come first, because negative signed always greater than unsigned! */
      if( lLeft <= 0 )
      {
         hb_retclen( "", 0 );
         return;
      }
      else if( (ULONG) lLeft >= ulLen )
      {
         /* No need to retain the 1st parameter - Recycle. */
         hb_itemReturn( pText );
         return;
      }

      sLeft = (char*) hb_xgrab( lLeft + 1 );
      memcpy( sLeft, sString, lLeft );
      sLeft[ lLeft ] = '\0';

      hb_retclenAdopt( sLeft, lLeft );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1124, NULL, "LEFT", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}
