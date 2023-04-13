/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *   CT3 string functions:
 *
 * EXPAND(), CHARSPREAD()
 * Copyright 2004 Pavel Tsarenko <tpe2.mail.ru>
 * www - http://www.xharbour.org
 *
 * Philip Chee <> 2005 bugfixes to EXPAND()
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

#include "common.ch"

FUNCTION Expand( cStr, nLength, xChar )

   LOCAL cResult, cChar, ser
   LOCAL nCount

   DEFAULT nLength TO 1

   IF ISCHARACTER( nlength )
      nCount := 1
      XChar := Left( nLength, 1 )
   ELSE
      nCount := nLength
   ENDIF

   IF xChar == nil
      cChar := ' '
   ELSEIF ISNUMBER( xChar )
      cChar := Chr( xChar )
   ELSEIF ISCHARACTER( xChar )
      cChar := Left( xChar, 1 )
   ELSE
      cChar := ' '
   ENDIF

   IF ISCHARACTER( cStr )
      IF Len( cStr ) > 1
         cResult := cStr[1]
         FOR ser := 2 TO Len( cStr )
            cResult += Replicate( cChar, nCount ) + cStr[ser]
         NEXT
      ELSE
         cResult := cStr
      ENDIF
   ELSE
      cResult := ''
   ENDIF

   RETURN cResult

FUNCTION CharSpread( cStr, nLength, xChar )

   LOCAL cResult := cStr, cChar
   LOCAL nTokens, ser, nAt, nAt2, nCount

   IF ISCHARACTER( cStr ) .AND. ISNUMBER( nLength )

      cChar := if( ISNUMBER( xChar ), Chr( xChar ), xChar )

      IF nLength > Len( cStr ) .AND. ( nTokens := NumToken( cStr, cChar ) ) > 0
         nLength -= Len( cStr )
         nCount := Int( nLength / ( nTokens - 1 ) )
         nAt := AtToken( cStr, cChar, 2 )
         cResult := Left( cStr, nAt - 1 )
         FOR ser := 2 TO nTokens
            nAt2 := if( ser == nTokens, Len( cStr ) + 1, AtToken( cStr, cChar, ser + 1 ) )
            cResult += Replicate( cChar, if( ser == nTokens, nLength, nCount ) ) + ;
               SubStr( cStr, nAt, nAt2 - nAt )
            nLength -= nCount
            nAt := nAt2
         NEXT
      ENDIF
   ENDIF

   RETURN cResult
