/*
 *  $Id: util.prg,v 1.2 2004/12/08 00:00:00 modalsist Exp $
 */

/*
 * xHarbour Project source code:
 * LibCT util functions used by another libct functions.
 *
 * Default()
 * IsDir()
 * Occurs()
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modIFy
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
 * along with this software; see the file COPYING.  IF not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  IF you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modIFied files, you must delete
 * this exception notice from them.
 *
 * IF you write modIFications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modIFications.
 * IF you do not wish that, delete this exception notice.
 *
 */
#include "common.ch"


*********************************
FUNCTION Default( cVar , uValue )
*********************************

IF cVar == NIL
   cVar := uValue
ENDIF

RETURN (Nil)

****************************
FUNCTION IsDir( cDirectory )
****************************

   LOCAL lExist,cCurDir

   IF valtype( cDirectory ) != "C"
      RETURN ( .F. )
   ENDIF

   cCurDir := DiskName()+":\"+CurDir()

   lExist := ( DirChange( cDirectory ) == 0  )

   IF lExist
      DirChange( cCurDir )
   ENDIF

RETURN ( lExist )


********************************
FUNCTION Occurs( cStr1 , cStr2 )
********************************

   LOCAL i,nOccurs

   i := 0
   nOccurs := 0

   IF valtype( cStr1 ) != "C" .or. valtype( cStr2 ) != "C"
      RETURN ( nOccurs )
   ENDIF

   IF Len( cStr1 ) = 0 .or. Len( cStr2 ) = 0
      RETURN ( nOccurs )
   ENDIF

   FOR i := 1 to Len( cStr2 ) 
      IF cStr1 == substr( cStr2 , i , Len( cStr1) )
         nOccurs += 1
      ENDIF  
   NEXT

RETURN ( nOccurs )
