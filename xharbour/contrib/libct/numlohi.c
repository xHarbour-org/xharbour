/*
 * $Id$
 *
 * xHarbour Project source code:
 * CT3 NUMLOW() and NUMHIGH() functions.
 *
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
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
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <stdio.h>

#include "hbapi.h"

/*  $DOC$
 *  $FUNCNAME$
 *      NUMLOW()
 *  $CATEGORY$
 *      CT3 numeric functions
 *  $ONELINER$
 *      Gets the lower value byte of a number
 *  $SYNTAX$
 *      NUMLOW(<xNumber>) --> nByte
 *  $ARGUMENTS$
 *      <xNumber> is either a decimal number or a hexadecimal string of digits.
 *  $RETURNS$
 *      A numeric value which is the lower value byte of the input number.
 *  $DESCRIPTION$
 *      NUMLOW() will return the low byte of the input number.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is numlohi.c, library is libct.
 *  $SEEALSO$
 *      NUMHIGH()
 *  $END$
 */

HB_FUNC( NUMLOW )
{
   unsigned n;
   
   if (ISNUM(1))
     n = hb_parni(1);
   else
     sscanf("%x", hb_parc(1), &n);
   hb_retni(n & 0xFF);
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMHIGH()
 *  $CATEGORY$
 *      CT3 numeric functions
 *  $ONELINER$
 *      Gets the higher value byte of the lower value word of a number
 *  $SYNTAX$
 *      NUMHIGH(<xNumber>) --> nByte
 *  $ARGUMENTS$
 *      <xNumber> is either a decimal number or a hexadecimal string of digits.
 *  $RETURNS$
 *      A numeric value which is the higher value byte of the lower value word
 *      of the input number.
 *  $DESCRIPTION$
 *      NUMHIGH() will return the high byte of the low word of the input number.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is numlohi.c, library is libct.
 *  $SEEALSO$
 *      NUMLOW()
 *  $END$
 */

HB_FUNC( NUMHIGH )
{
   unsigned n;

   if (ISNUM(1))
     n = hb_parni(1);
   else
     sscanf("%x", hb_parc(1), &n);
   hb_retni((n >> 8) & 0xFF);
}
