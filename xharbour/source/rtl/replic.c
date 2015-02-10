/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * REPLICATE() function
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbapierr.h"

/* returns n copies of given string */
HB_FUNC( REPLICATE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pNum  = hb_param( 2, HB_IT_NUMERIC );

   if( pText && pNum )
   {
      HB_ISIZ lTimes = hb_itemGetNS( pNum );

      if( lTimes > 0 )
      {
         HB_SIZE ulLen =  hb_itemGetCLen( pText);
         if( (LONGLONG)( (LONGLONG)ulLen *  (LONGLONG)lTimes ) < UINT_MAX)
         {
            char *  szResult = ( char * ) hb_xgrab( ( ulLen * lTimes ) + 1 );
            char *  szPtr    = szResult;
            char *  szText   = ( char *) hb_itemGetCPtr( pText );
            HB_ISIZ    i;

            for( i = 0; i < lTimes; i++ )
               {
                  hb_xmemcpy( szPtr, szText, ulLen );
                  szPtr += ulLen;
               }

            hb_retclenAdopt( szResult, ulLen * lTimes );
         }
         else
            hb_errRT_BASE_SubstR( EG_STROVERFLOW, 1234, NULL, "REPLICATE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      }
      else
         hb_retc( "" );

      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1106, NULL, "REPLICATE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}
