/*
 * $Id: spread.prg,v 1.1 2004/11/29 23:30:00 ptsarenko Exp $
 */

/*
 * xHarbour Project source code:
 *   CT3 string functions:
 *
 * EXPAND(), CHARSPREAD()
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

#include "common.ch"

Function Expand(cStr, nLength, xChar)
Local cResult, cChar, ser

DEFAULT nLength TO 1

if xChar == nil
   cChar := ' '
elseif ISNUMBER(xChar)
   cChar := Chr(xChar)
else
   cChar := xChar
endif

if ISCHARACTER(cStr) .and. len(cStr) > 0
   cResult := cStr[1]
   for ser := 2 to len(cStr)
      cResult += cChar + cStr[ser]
   next
else
   cResult := ''
endif

Return cResult

Function CharSpread(cStr, nLength, xChar)
Local cResult := cStr, cChar
Local nTokens, ser, nAt, nAt2, nCount

if ISCHARACTER(cStr) .and. ISNUMBER(nLength)

   cChar := if(ISNUMBER(xChar), Chr(xChar), xChar)

   if nLength > len(cStr) .and. (nTokens := NumToken(cStr, cChar)) > 0
      nLength -= len(cStr)
      nCount := Int(nLength / (nTokens - 1))
      nAt := AtToken(cStr, cChar, 2)
      cResult := Left(cStr, nAt - 1)
      for ser := 2 to nTokens
         nAt2 := if(ser==nTokens, len(cStr)+1, AtToken(cStr, cChar, ser+1))
         cResult += Repl(cChar, if(ser==nTokens, nLength, nCount)) + ;
                    Substr(cStr, nAt, nAt2 - nAt)
         nLength -= nCount
         nAt := nAt2
      next
   endif
endif
Return cResult
