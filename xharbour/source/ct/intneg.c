/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *   CT3 numeric functions
 *
 * INTNEG(), INTPOS()
 * Copyright 2004 Pavel Tsarenko <tpe2.mail.ru>
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
#include "ct.h"

HB_FUNC( INTNEG )
{
   HB_LONG  lNumber  = 0;
   BOOL  b32      = ( ISLOG( 2 ) && hb_parl( 2 ) ? 1 : 0 );

   if( ISNUM( 1 ) )
   {
      lNumber = hb_parnl( 1 );
   }
   else if( ISCHAR( 1 ) )
   {
      lNumber = hb_hextonum( hb_parc( 1 ) );
   }

   if( b32 )
   {
#ifndef HB_LONG_LONG_OFF
      if( lNumber < 0 )
#endif
      hb_retnl( ( LONG ) lNumber );
#ifndef HB_LONG_LONG_OFF
      else
         hb_retnll( ( LONG ) lNumber );
#endif
   }
   else
   {
      if( lNumber > 65535 )
      {
         lNumber &= 0xFFFF;
      }

      if( lNumber < -32767 )
      {
         hb_retni( 0 );
      }
      else
      {
         hb_retni( ( SHORT ) ( lNumber & 0xFFFF ) );
      }
   }
}

HB_FUNC( INTPOS )
{
   HB_LONG  lNumber  = 0;
   BOOL  b32      = ( ISLOG( 2 ) && hb_parl( 2 ) ? 1 : 0 );

   if( ISNUM( 1 ) )
   {
      lNumber = hb_parnl( 1 );
   }
   else if( ISCHAR( 1 ) )
   {
      lNumber = hb_hextonum( hb_parc( 1 ) );
   }

   if( b32 )
   {
#ifndef HB_LONG_LONG_OFF
      if( lNumber >= 0 )
#endif
      hb_retnl( ( ULONG ) lNumber );
#ifndef HB_LONG_LONG_OFF
      else
         hb_retnll( ( ULONG ) lNumber );
#endif
   }
   else
   {
      if( lNumber > 65535 )
      {
         lNumber &= 0xFFFF;
      }

      if( lNumber < -32767 )
      {
         hb_retni( 0 );
      }
      else
      {
         hb_retni( ( USHORT ) ( lNumber & 0xFFFF ) );
      }
   }
}
