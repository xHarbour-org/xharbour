/*
 * $Id: rt_ccall.prg,v 1.4 2004/05/09 23:40:06 druzus Exp $
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library ( C-Calls )
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
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

#include "rt_main.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

PROCEDURE Main_CCall()

   TEST_LINE( STR(VMCALL_01(),,,.T.)   , "5000000000" )
   TEST_LINE( STR(VMCALL_02(),,,.T.)   , "5000000000" )
   TEST_LINE( STR(VMCALL_03(),,,.T.)   , "5000000000" )
   TEST_LINE( STR(VMCALL_04(),,,.T.)   , "5000000000" )

   FUNCTION VMCALL_01()

   LOCAL n
   EXTEND_01( @N, { 5000000000 } )
   RETURN n

   FUNCTION VMCALL_02()

   LOCAL n
   Extend_02( @n )
   RETURN n

   FUNCTION VMCALL_03()
   RETURN Extend_03(5000000000)

   FUNCTION VMCALL_04()
   RETURN Extend_04(5000000000)

/* Don't change the position of this #include. */
#include "rt_init.ch"

   #PRAGMA BEGINDUMP
   #include "hbapi.h"

   HB_FUNC( EXTEND_01 )
   {
      PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );
      #ifndef HB_LONG_LONG_OFF
         hb_stornll( hb_arrayGetNLL( pArray, 1 ), 1, -1 );
      #else
         hb_stornd( hb_arrayGetND( pArray, 1 ), 1, -1 );
      #endif
   }

   HB_FUNC( EXTEND_02 )
   {
   #ifndef HB_LONG_LONG_OFF
      hb_stornll( HB_LL(5000000000), 1, -1 );
   #else
      hb_stornd( 5000000000.0, 1, -1 );
   #endif
   }

   HB_FUNC( EXTEND_03 )
   {
   #ifndef HB_LONG_LONG_OFF
      hb_retnll( hb_parnll(1) );
   #else
      hb_retnd( hb_parnd(1) );
   #endif
   }

   HB_FUNC( EXTEND_04 )
   {
   #ifndef HB_LONG_LONG_OFF
      hb_retnlllen( hb_parnll(1), 20 );
   #else
      hb_retndlen( hb_parnd(1), 20, 0 );
   #endif
   }
   #PRAGMA ENDDUMP


