/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RAT() function
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
/*
HB_FUNC( RAT )
{
   PHB_ITEM pszSub = hb_param( 1, HB_IT_STRING );

   if( pszSub && pszSub->item.asString.length )
   {
      PHB_ITEM pszText = hb_param( 2, HB_IT_STRING );

      if( pszText && pszText->item.asString.length )
      {
         HB_ISIZ nTo = pszText->item.asString.length - pszSub->item.asString.length;

         if( nTo >= 0 )
         {
            HB_ISIZ nStart = hb_parns( 3 );
            HB_ISIZ nFrom  = ( nStart <= 1 ) ? 0 : --nStart ;

            if( nTo >= nFrom )
            {
               HB_SIZE nPos = 0;

               if( ISNUM( 4 ) )
               {
                  HB_ISIZ nEnd = hb_parns( 4 ) - 1;

                  nEnd -= pszSub->item.asString.length - 1;

                  if( nEnd < nTo )
                     nTo = nEnd;
               }

               if( nTo >= nFrom )
               {
                  do
                  {
                     if( pszText->item.asString.value[ nTo ] == *pszSub->item.asString.value &&
                         memcmp( pszSub->item.asString.value, pszText->item.asString.value + nTo, pszSub->item.asString.length ) == 0 )
                     {
                        nPos = nTo + 1;
                        break;
                     }
                  }
                  while( --nTo >= nFrom );
               }

               hb_retns( nPos );
               return;
            }
         }
      }
   }

   hb_retni( 0 );
}
*/

HB_FUNC( RAT )
{
  HB_SIZE ulSubLen = hb_parclen( 1 );

  if( ulSubLen )
  {
     LONG lPos = ( LONG ) ( hb_parclen( 2 ) - ulSubLen );

     if( lPos >= 0 )
     {
        const char *   pszSub   = hb_parcx( 1 );
        const char *   pszText  = hb_parcx( 2 );
        LONG           lEnd     = ISNUM( 4 ) ? hb_parnl( 4 ) - 1 : 0;
        BOOL           bFound   = FALSE;

        if( ISNUM( 3 ) )
           lPos = hb_parnl( 3 ) - 1;

        if ( lPos > ( LONG ) strlen( pszText ) )
           lPos = (LONG)strlen( pszText );

        while( lPos >= lEnd && ! bFound )
        {
           if( *( pszText + lPos ) == *pszSub )
              bFound = ( memcmp( pszSub, pszText + lPos, ( size_t ) ulSubLen ) == 0 );
           lPos--;
        }
        hb_retnl( bFound ? lPos + 2 : 0 );
     }
     else
        hb_retni( 0 );
  }
  else
     // This function never seems to raise an error
     hb_retni( 0 );
}
