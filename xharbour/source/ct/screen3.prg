/*
 * $Id: screen3.prg,v 1.3 2006/09/02 14:14:48 ptsarenko Exp $
 */

/*
 * xHarbour Project source code:
 *   CT3 video functions (screen-like functions):
 *
 * CHARWIN(), CLEARSLOW(), COLORWIN(), COLORREPL(),
 * SAYSPREAD(), SAYMOVIN(), SAYDOWN(),
 * SCREENSTR(), STRSCREEN()
 *
 * Copyright 2004 Pavel Tsarenko <tpe2.mail.ru>
 * www - http://www.xharbour.org
 *
 * CLEAREOL(), CLWIN(), CLEOL()
 * Copyright 2004 Philip Chee <philip@aleytys.pc.my>
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


Function CharWin(nTop, nLeft, nBottom, nRight, xNewChar, xOldChar)
Local cScr, cNewChar, cOldChar, ser
Local nCSize := GetCSize()

DEFAULT nTop    TO 0
DEFAULT nLeft   TO 0
DEFAULT nBottom TO MaxRow()
DEFAULT nRight  TO MaxCol()

if xNewChar == nil
   cNewChar := GetClearB()
elseif ValType(xNewChar) = 'N'
   cNewChar := Chr(xNewChar)
else
   cNewChar := xNewChar
endif
if xOldChar # nil
   cOldChar := if(ValType(xOldChar)='N', Chr(xOldChar), xOldChar)
endif
cScr := SaveScreen(nTop, nLeft, nBottom, nRight)
for ser := 1 to len(cScr) / nCSize
   if cOldChar == nil .or. Substr(cScr, (ser-1)*nCSize+1, 1) == cOldChar
      cScr := Stuff(cScr, (ser-1)*nCSize+1, 1, cNewChar)
   endif
next
RestScreen(nTop, nLeft, nBottom, nRight, cScr)
Return ''


Function ClearSlow(nDelay, nTop, nLeft, nBottom, nRight, xChar)
Local cChar, nt, nl, nb, nr, ser

DEFAULT nTop    TO 0
DEFAULT nLeft   TO 0
DEFAULT nBottom TO MaxRow()
DEFAULT nRight  TO MaxCol()

if xChar == nil
   cChar := GetClearB()
elseif ValType(xChar) = 'N'
   cChar := Chr(xChar)
else
   cChar := xChar
endif
nt := nTop
nl := nLeft
nb := nBottom
nr := nRight
while nt < nb .or. nl < nr
   if nl < nr
      DispOutAt(nt, nl, Replicate(cChar, nr-nl+1))
      DispOutAt(nb, nl, Replicate(cChar, nr-nl+1))
   endif
   if nt < nb
      nt ++
      nb --
   endif
   if nt <= nb
      for ser := nt to nb
         DispOutAt(ser, nl, cChar)
         DispOutAt(ser, nr, cChar)
      next
   endif
   SecondsSleep( nDelay*0.001 )
   if nl < nr
      nl ++
      nr --
   endif
enddo
Return ''


Function ColorWin(nTop, nLeft, nBottom, nRight, xAttr, xOld)
Local cScr, nAttr, cAttr, nOld, ser, np
Local nCSize := GetCSize()

DEFAULT nTop    TO Row()
DEFAULT nLeft   TO Col()
DEFAULT nBottom TO MaxRow()
DEFAULT nRight  TO MaxCol()

if xAttr == nil
   nAttr := GetClearA()
elseif ValType(xAttr) = 'C'
   nAttr := ColorToN(xAttr)
else
   nAttr := xAttr
endif
if xOld # nil
   nOld := if(ValType(xOld) = 'C', ColorToN(xOld), xOld)
endif

#ifdef __PLATFORM__Windows   
cScr := SaveScreen(nTop, nLeft, nBottom, nRight)
for ser := 1 to len(cScr) / nCSize
   np := (ser-1)*nCSize + 2
   cAttr := Substr(cScr, np, 1)
   if nOld == nil .or. nOld == Asc(cAttr)
      cScr := Stuff(cScr, np, 1, Chr(nAttr))
   endif
next
RestScreen(nTop, nLeft, nBottom, nRight, cScr)
#else
SetAttribute(nTop, nLeft, nBottom, nRight, nAttr)
#endif
Return ''


Function ColorRepl(xNewAttr, xOldAttr)
Local cNewAttr, cOldAttr, cScr, ser, np
Local nCSize := GetCSize()

if xNewAttr == nil
   cNewAttr := GetClearA()
elseif ValType(xNewAttr) = 'N'
   cNewAttr := Chr(xNewAttr)
else
   cNewAttr := xNewAttr
endif
if xOldAttr # nil
   cOldAttr := if(ValType(xOldAttr)='N', Chr(xOldAttr), xOldAttr)
endif
cScr := SaveScreen(0, 0, MaxRow(), MaxCol())
for ser := 1 to len(cScr) / nCSize
   np := (ser - 1)*nCSize + 2
   if cOldAttr == nil .or. Substr(cScr, np, 1) == cOldAttr
      cScr := Stuff(cScr, np, 1, cNewAttr)
   endif
next
RestScreen(0, 0, MaxRow(), MaxCol(), cScr)
Return ''


Function SaySpread(cStr, nDelay, nRow, nCol)
Local n1 := Int(len(cStr) / 2), n2

DEFAULT nDelay TO 4
DEFAULT nRow TO Row()
DEFAULT nCol TO Col()

if n1 = 0
   n1 := 1
endif
n2 := n1 + 1

nCol -= n1
SetPos(nRow, nCol)
while .t.
   if n1 > 0
      SetPos(nRow, nCol + n1)
      DispOut(Substr(cStr, n1, 1))
   endif
   if n2 <= len(cStr)
      SetPos(nRow, nCol + n2)
      DispOut(Substr(cStr, n2, 1))
   endif
   SecondsSleep( nDelay * 0.001 )
   n1 --
   n2 ++
   if n1 < 1 .and. n2 > Len(cStr)
      exit
   endif
enddo
SetPos(nRow, nCol)
Return ''


Function SayMoveIn(cStr, nDelay, nRow, nCol, lDir)
Local n1 := 1

DEFAULT nDelay TO 4
DEFAULT lDir TO .f.
DEFAULT nRow TO Row()
DEFAULT nCol TO Col()

while n1 <= len(cStr)
   DispOutAt(nRow, iif(lDir, nCol+len(cStr)-n1, nCol),;
                   iif(lDir, Left(cStr, n1), Right(cStr, n1)))
   SecondsSleep( nDelay * 0.001 )
   n1 ++
enddo
SetPos(nRow, nCol)
Return ''


Function SayDown(cStr, nDelay, nRow, nCol)
Local ser

DEFAULT nDelay TO 4
DEFAULT nRow TO Row()
DEFAULT nCol TO Col()

for ser := 1 to len(cStr)
   DispOutAt(nRow+ser-1, nCol, Substr(cStr, ser, 1))
   SecondsSleep( nDelay * 0.001 )
next
SetPos(nRow, nCol)
Return ''


Function ScreenStr(nRow, nCol, nCount)
Local cStr
Local nCSize := GetCSize()

DEFAULT nRow TO Row()
DEFAULT nCol TO Col()

cStr := SaveScreen(nRow, nCol, MaxRow(), MaxCol())
if ValType(nCount) == 'N' .and. nCount*nCSize < len(cStr)
   cStr := Left(cStr, nCount*nCSize)
endif
Return cStr


Function StrScreen(cStr, nRow, nCol)
Local nCount
Local nCSize := GetCSize()

if IsCharacter(cStr)

   DEFAULT nRow TO Row()
   DEFAULT nCol TO Col()

   while len(cStr) > 0 .and. nRow <= MaxRow()
      nCount := Min( Int(len(cStr) / nCSize), MaxCol() - nCol + 1 )
      RestScreen(nRow, nCol, nRow, nCol+nCount-1, cStr)
      cStr := Substr(cStr, nCount*nCSize + 1)
      nRow ++
      nCol := 0
   enddo

endif
Return ''


FUNCTION CLEAREOL( nRow, nCol, xAttr, xChar )

  DEFAULT nRow  TO  Row()       , ;
          nCol  TO  Col()       , ;
          xAttr TO  GetClearA() , ;
          xChar TO  GetClearB()


  ClearWin( nRow, nCol, nRow, Maxcol(), xAttr, xChar )


RETURN ( "" )

FUNCTION CLWIN( nTop, nLeft, nBottom, nRight )
  LOCAL xAttr   := "W/N"        , ;
        xChar   := ' '

  DEFAULT nTop      TO  Row()  , ;
          nLeft     TO  Col()   , ;
          nBottom   TO  MaxRow(), ;
          nRight    TO  MaxCol()

  ClearWin( nTop, nLeft, nBottom, nRight, xAttr, xChar )

RETURN ( "" )

FUNCTION CLEOL( nRow, nCol )
  LOCAL xAttr   := "W/N"        , ;
        xChar   := ' '

  DEFAULT nRow  TO  Row()   , ;
          nCol  TO  Col()

  ClearEol( nRow, nCol, xAttr, xChar )

RETURN ( "" )

FUNCTION SaveCursor()
RETURN ( Row() << 16 ) | ( SetCursor() << 8 ) | Col()

FUNCTION RestCursor( nSavedCursor )
   SetPos( nSavedCursor >> 16, nSavedCursor & 0x0000FF )
   SetCursor( ( nSavedCursor & 0x00FF00 ) >> 8 )
RETURN ""


#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC_STATIC( GETCSIZE )
{
   UINT uiSize;

   hb_gtRectSize( 1, 1, 1, 1, &uiSize );

   hb_retni( uiSize );
}

//HB_FUNC_STATIC( SETATTRIBUTE )
//{
//   hb_gt_SetAttribute( hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5) );
//}

#pragma ENDDUMP
