/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * STR() function
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
#include "hbapierr.h"
#include "hbfast.h"
#include "hbset.h"

HB_FUNC( STR )
{
   PHB_ITEM pNumber  = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pWidth   = NULL;
   PHB_ITEM pDec     = NULL;
   BOOL     bValid   = FALSE;
   BOOL     bLtrim   = FALSE;

   if( pNumber  && HB_IS_NUMBER( pNumber ))
   {
      int iParams = hb_pcount();
      bValid = TRUE;

      if( iParams >= 2 )
      {
         pWidth = hb_param( 2, HB_IT_NUMERIC );

         if( ! pWidth && iParams < 4 )
            bValid = FALSE;
      }

      if( bValid && iParams >= 3 )
      {
         pDec = hb_param( 3, HB_IT_NUMERIC );

         if( ! pDec && iParams < 4 )
            bValid = FALSE;

         if( pDec && hb_itemGetNI( pDec ) == 0 )
            pDec = NULL;
      }

      if( bValid && iParams >= 4 )
      {
         PHB_ITEM pLtrim = hb_param( 4, HB_IT_LOGICAL );

         if( pLtrim )
            bLtrim = hb_itemGetL( pLtrim );
      }
   }

   if( bValid )
   {
      char *   szResult;
      BOOL     bLogical = hb_setGetFixed();
      PHB_ITEM pSet = hb_itemNew( NULL );

      hb_itemPutL( pSet, FALSE );

      hb_setSetItem( HB_SET_FIXED, pSet );

      szResult = hb_itemStr( pNumber, pWidth, pDec );
      hb_itemPutL( pSet, bLogical );
      hb_setSetItem( HB_SET_FIXED, pSet );
      hb_itemRelease( pSet ) ;

      if( szResult && bLtrim )
      {
         int iLen = 0;

         while( szResult[ iLen ] == ' ' )
            ++iLen;

         if( iLen )
            memmove( szResult, szResult + iLen, strlen( szResult + iLen ) + 1 );

         hb_retcAdopt( szResult );
         return;
      }
      else if( pWidth && pDec && hb_itemGetNI( pWidth ) == hb_itemGetNI( pDec ) + 1 )
      {
         char * szTemp = ( char * ) hb_xgrab( hb_itemGetNI( pWidth ) + 1 );

         hb_xmemset( szTemp, '*', hb_itemGetNI( pWidth ) );
         szTemp[ hb_itemGetNI( pWidth ) ] = '\0';

         if( szResult )
            hb_xfree( szResult );
         hb_retcAdopt( szTemp );
         return;
      }
      else
         hb_retcAdopt( szResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "STR", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
}
