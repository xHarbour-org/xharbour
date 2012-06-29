/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Harbour interface to TinyMT, Pseudo Random Generator
 * Copyright 2012 Andi Jahja <xharbour@telkom.net.id>
 * www - http://www.harbour-project.org http://www.xharbour.org
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

#include "tinymt32.h"

/*
Syntax: HB_TINYMT32_INIT( nVector1, nVector2, nVector3, nSeed )
*/

static tinymt32_t tinymt;

HB_FUNC( HB_TINYMT32_INIT )
{
   if ( hb_pcount() >= 3 && ISNUM(1) && ISNUM(3) && ISNUM(3) )
   {
      ULONG seed = 1;

      tinymt.mat1 = hb_parnl(1);
      tinymt.mat2 = hb_parnl(2);
      tinymt.tmat = hb_parnl(3);

      if ( hb_pcount() >= 4 && ISNUM(4) )
         seed = hb_parnl( 4 ) > 0 ? hb_parnl( 4 ): 1 ;

      tinymt32_init( &tinymt, seed );
      hb_retl( TRUE );
   }
   else
      hb_retl( FALSE );
}

/*
Syntax: HB_TINYMT32_INIT_BY_ARRAY( { nVector1, nVector2, nVector3 }, nSeed, nKeyLength )
*/

HB_FUNC( HB_TINYMT32_INIT_BY_ARRAY )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if ( pArray && hb_arrayLen( pArray ) == 3 )
   {
       ULONG seed_array[5];
       ULONG seed = 1;
       ULONG nKeyLength = 1;

       if ( ISNUM( 2 ) )
          seed = hb_parnl( 2 ) > 0 ? hb_parnl( 2 ) : 1;

       if ( ISNUM( 3 ) )
          nKeyLength = hb_parnl( 3 ) > 0 ? hb_parnl ( 3 ) : 1;

       tinymt.mat1   = hb_arrayGetNL( pArray, 1 );
       tinymt.mat2   = hb_arrayGetNL( pArray, 2 );
       tinymt.tmat   = hb_arrayGetNL( pArray, 3 );
       seed_array[0] = seed;

       tinymt32_init_by_array( &tinymt, seed_array, nKeyLength );
       hb_retl( TRUE );
   }
   else
       hb_retl( FALSE);
}

HB_FUNC( HB_TINYMT32_GENERATE_UINT32 )
{
   hb_retnd(tinymt32_generate_uint32(&tinymt));
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOAT )
{
   hb_retnd(tinymt32_generate_float(&tinymt));
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOAT12 )
{
   hb_retnd(tinymt32_generate_float12(&tinymt));
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOATOC )
{
   hb_retnd(tinymt32_generate_floatOC(&tinymt));
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOATOO )
{
   hb_retnd(tinymt32_generate_floatOO(&tinymt));
}

HB_FUNC( HB_TINYMT32_GENERATE_32DOUBLE )
{
   hb_retnd(tinymt32_generate_32double(&tinymt));
}
