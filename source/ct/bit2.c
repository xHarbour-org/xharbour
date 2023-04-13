/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - CLEARBIT()
 *                                              - SETBIT()
 *                                              - ISBIT()
 *
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
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

static LONG __getparam( int iParam );

HB_FUNC( CLEARBIT )
{
   LONG  lNumClearBit;
   LONG  lNum1;
   int   iNum2;
   int   iPCount, iFor;

   iPCount  = hb_pcount();

   lNum1    = __getparam( 1 ); /* Obtain the parameter and converting from HEXA
                                  if necessary */

   if( ISNUM( 1 ) || ISCHAR( 1 ) )
   {
      lNumClearBit = lNum1;

      for( iFor = 2; iFor <= iPCount; iFor++ )
      {
         if( ISNUM( iFor ) )
         {
            iNum2 = hb_parni( iFor );

            if( ( iNum2 >= 1 ) && ( iNum2 <= 32 ) )
            {
               /* if bit to clear this between 1 and 32 */

               lNumClearBit = lNumClearBit & ~( 1L << ( iNum2 - 1 ) );
            }
            else
            {
               lNumClearBit = -1L;
               break;
            }
         }
         else
         {
            lNumClearBit = -1L;
            break;
         }
      }
   }
   else
      lNumClearBit = -1L;

   if( lNumClearBit == -1L )
      hb_retnl( lNumClearBit );
   else
      hb_retnd( ( ULONG ) lNumClearBit );
}

HB_FUNC( SETBIT )
{
   LONG  lNumSetBit;
   LONG  lNum1;
   int   iNum2;
   int   iPCount, iFor;

   iPCount  = hb_pcount();

   lNum1    = __getparam( 1 ); /* Obtain the parameter and converting from HEXA
                                  if necessary */

   if( ISNUM( 1 ) || ISCHAR( 1 ) )
   {
      lNumSetBit = lNum1;

      for( iFor = 2; iFor <= iPCount; iFor++ )
      {
         if( ISNUM( iFor ) )
         {
            iNum2 = hb_parni( iFor );

            if( ( iNum2 >= 1 ) && ( iNum2 <= 32 ) )
            {
               /* if bit to clear this between 1 and 32 */

               lNumSetBit = lNumSetBit | ( 1L << ( iNum2 - 1 ) );

            }
            else
            {
               lNumSetBit = -1L;
               break;
            }
         }
         else
         {
            lNumSetBit = -1L;
            break;
         }
      }
   }
   else
      lNumSetBit = -1L;

   if( lNumSetBit == -1L )
      hb_retnl( lNumSetBit );
   else
      hb_retnd( ( ULONG ) lNumSetBit );
}

HB_FUNC( ISBIT )
{
   LONG  lNum1;
   int   iTestBit;

   if( ( ISNUM( 1 ) || ISCHAR( 1 ) ) && ( ISNUM( 2 ) || ISNIL( 2 ) ) )
   {
      lNum1    = __getparam( 1 );
      iTestBit = ISNUM( 2 ) ? hb_parni( 2 ) : 1;

      hb_retl( lNum1 & ( 1L << ( iTestBit - 1 ) ) );
   }
   else
      hb_retl( FALSE );

}

static LONG __getparam( int iParam )
{
   if( ISCHAR( iParam ) )
      return ( LONG ) hb_hextonum( hb_parcx( iParam ) );
   else
      return hb_parnl( iParam );
}
