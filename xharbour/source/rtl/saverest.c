/*
 * $Id: saverest.c,v 1.7 2004/03/18 03:58:37 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * SAVESCREEN(), RESTSCREEN() functions
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
#include "hbfast.h"
#include "hbapigt.h"

// This function validates parameters. They must be coordinates inside
// the screen size, and the lower number must be first. The input
// parameters from the user's function are signed, and the output
// parameters (to the real work) are unsigned.
void savescreen_coords( USHORT *uiSmall, USHORT *uiBig, int iMax, int iParamNum )
{
   int iParam1, iParam2, iSwap;

   iParam1 = ( ISNUM( iParamNum ) ? hb_parni( iParamNum ) : 0 );
   iParam1 = ( ( iParam1 < 0 ) ? 0 : ( ( iParam1 > iMax ) ? iMax : iParam1 ) );

   iParam2 = ( ISNUM( iParamNum + 2 ) ? hb_parni( iParamNum + 2 ) : iMax );
   iParam2 = ( ( iParam2 < 0 ) ? 0 : ( ( iParam2 > iMax ) ? iMax : iParam2 ) );

   if( iParam1 > iParam2 )
   {
      iSwap = iParam1;
      iParam1 = iParam2;
      iParam2 = iSwap;
   }

   *uiSmall = ( USHORT ) iParam1;
   *uiBig   = ( USHORT ) iParam2;
}

HB_FUNC( SAVESCREEN )
{
   USHORT uiTop, uiLeft, uiBottom, uiRight;
   UINT uiSize;
   void * pBuffer;

   savescreen_coords( &uiTop,  &uiBottom, hb_gtMaxRow(), 1 );
   savescreen_coords( &uiLeft, &uiRight,  hb_gtMaxCol(), 2 );

   hb_gtRectSize( uiTop, uiLeft, uiBottom, uiRight, &uiSize );
   pBuffer = hb_xgrab( uiSize + 1 );  /* why +1? */

   hb_gtSave( uiTop, uiLeft, uiBottom, uiRight, pBuffer );
   hb_retclenAdopt( ( char * ) pBuffer, uiSize );
}

HB_FUNC( RESTSCREEN )
{
   USHORT uiTop, uiLeft, uiBottom, uiRight;

   if( ISCHAR( 5 ) )
   {
      savescreen_coords( &uiTop,  &uiBottom, hb_gtMaxRow(), 1 );
      savescreen_coords( &uiLeft, &uiRight,  hb_gtMaxCol(), 2 );

      hb_gtRest( uiTop, uiLeft, uiBottom, uiRight, ( void * ) hb_parcx( 5 ) );
   }
}
