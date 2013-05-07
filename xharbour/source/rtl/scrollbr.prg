/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ScrollBar class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "hbclass.ch"
#include "color.ch"
#include "common.ch"
#include "button.ch"

#ifdef HB_COMPAT_C53

CLASS HBScrollBar

   DATA BarLength
   DATA Cargo
   DATA sBlock
   DATA Style
   DATA ClassName INIT "SCROLLBAR"
   DATA ColorSpec

   METHOD DISPLAY()
   METHOD HitTest()
   METHOD Update()
   METHOD New( nStart, nEnd, nOffSet, bsBlock, nOrient )
   ACCESS Current inline ::GetCurrent()
   ASSIGN Current( nCurrent ) inline ::GetCurrent( nCurrent )
   ACCESS End inline ::GetEnd()
   ASSIGN End ( nEnd ) inline ::GetEnd( nEnd )
   ACCESS OffSet inline ::GetOffSet()
   ASSIGN OffSet( nOffSet ) inline ::GetOffSet( nOffSet )
   ACCESS Orient inline ::GetOrient()
   ASSIGN Orient( nOrient ) inline ::GetOrient( nOrient )
   ACCESS Start inline ::GetStart()
   ASSIGN Start( nStart ) inline ::GetStart( nStart )
   ACCESS ThumbPos inline ::GetThumbPos()
   ASSIGN ThumbPos( nPos ) inline ::GetThumbPos( nPos )
   ACCESS TOTAL inline ::GetTotal()
   ASSIGN TOTAL( nTotal ) inline ::GetTotal( nTotal )

   DATA COLOR INIT ''
   DATA nCurrent INIT 0
   DATA nEnd INIT 0
   DATA nOffSet INIT 0
   DATA nOrient INIT 0
   DATA nStart INIT 0
   DATA nThumbPos INIT 1
   DATA nTotal INIT 100

   DATA hb_p_lShow INIT .F.

   METHOD GetCurrent( nCurrent )
   METHOD GetEnd( nEnd )
   METHOD GetStart( nStart )
   METHOD GetThumbPos( nPos )
   METHOD GetTotal( nTotal )
   METHOD GetOffSet( nOffSet )
   METHOD GetOrient( nOrient )

ENDCLASS

METHOD New( nStart, nEnd, nOffSet, bsBlock, nOrient ) CLASS HBScrollBar

   LOCAL cStyle
   LOCAL cColor

   IF nOrient == 1
      cStyle := "°²"
   ELSEIF nOrient == 2
      cStyle := "°²" + Chr( 26 )
   ENDIF

   ::Barlength := nEnd - nStart - 1
   ::Current   := 1
   ::Cargo     := Nil
   cColor      := SetColor()
   ::ColorSpec := __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
      __GUIColor( cColor, CLR_ENHANCED + 1 )
   ::End       := nEnd
   ::OffSet    := nOffSet
   ::Orient    := nOrient
   ::sBlock    := bsBlock
   ::Start     := nStart
   ::Style     := cStyle
   ::Thumbpos  := 1
   ::total     := 1

   RETURN Self

METHOD DISPLAY() CLASS HBScrollBar

   LOCAL nCurRow
   LOCAL nCurCol
   LOCAL cCurColor
   LOCAL cStyle
   LOCAL cOffSet
   LOCAL cColor2
   LOCAL cColor1
   LOCAL nStart
   LOCAL nEnd
   LOCAL nPos
   LOCAL lDisplay  := .F.

   cCurColor := SetColor()
   nCurRow   := Row()
   nCurCol   := Col()

   IF ThumbPos( Self )
      lDisplay := .T.
      cStyle   := ::Style
      cOffSet  := ::OffSet

      DispBegin()
      cColor1 := __GUIColor( ::ColorSpec, 1 )
      cColor2 := __GUIColor( ::ColorSpec, 2 )

      IF ::Orient == 1
         nStart := ::Start
         nEnd   := ::End - 1

         SET COLOR TO ( cColor1 )
         FOR nPos := nStart + 1 TO nEnd
            DispOutAt( nPos, cOffSet, cStyle[ 2 ] )
         NEXT

         SET COLOR TO ( cColor2 )
         DispOutAt( nStart, cOffSet, cStyle[ 1 ] )
         DispOutAt( nStart + ::ThumbPos, cOffSet, cStyle[ 3 ] )
         DispOutAt( nEnd + 1, cOffSet, cStyle[ 4 ] )

      ELSE

         nStart := ::Start
         nEnd   := ::End - 1

         DispOutAt( cOffSet, nStart + 1, Replicate( cStyle[ 2 ], nEnd - nStart ), cColor1 )

         SET COLOR TO ( cColor2 )
         DispOutAt( cOffSet, nStart, cStyle[ 1 ] )
         DispOutAt( cOffSet, nStart + ::ThumbPos, cStyle[ 3 ] )
         DispOutAt( cOffSet, nEnd + 1, cStyle[ 4 ] )

      ENDIF

      DispEnd()

      SET COLOR TO ( cCurColor )
      SetPos( nCurRow, nCurCol )
   ENDIF

   RETURN lDisplay

METHOD HitTest( nRow, nCol ) CLASS HBScrollBar

   IF ::Orient == 1

      DO CASE
      CASE nCol != ::OffSet
      CASE nRow < ::Start
      CASE nRow > ::End
      CASE nRow == ::Start
         RETURN HTSCROLLUNITDEC
      CASE nRow == ::End
         RETURN HTSCROLLUNITINC
      CASE nRow < ::ThumbPos + ::Start
         RETURN HTSCROLLBLOCKDEC
      CASE nRow > ::ThumbPos + ::Start
         RETURN HTSCROLLBLOCKINC
      CASE nRow == ::ThumbPos + ::Start
         RETURN HTSCROLLTHUMBDRAG
      ENDCASE

      IF nCol == ::OffSet + 1 .OR. nCol == ::OffSet

         DO CASE
         CASE nCol != ::OffSet .AND. nCol != ::OffSet + 1
         CASE nRow < ::Start
         CASE nRow > ::End
         CASE nRow == ::Start
            RETURN HTSCROLLUNITDEC
         CASE nRow == ::End
            RETURN HTSCROLLUNITINC
         CASE nRow < ::ThumbPos + ::Start
            RETURN HTSCROLLBLOCKDEC
         CASE nRow > ::ThumbPos + ::Start
            RETURN HTSCROLLBLOCKINC
         CASE nRow == ::ThumbPos + ::Start
            RETURN HTSCROLLTHUMBDRAG
         ENDCASE

      ENDIF

   ELSEIF ::Orient == 2

      DO CASE
      CASE nRow != ::OffSet
      CASE nCol < ::Start
      CASE nCol > ::End
      CASE nCol == ::Start
         RETURN HTSCROLLUNITDEC
      CASE nCol == ::End
         RETURN HTSCROLLUNITINC
      CASE nCol < ::ThumbPos + ::Start
         RETURN HTSCROLLBLOCKDEC
      CASE nCol > ::ThumbPos + ::Start
         RETURN HTSCROLLBLOCKINC
      CASE nCol == ::ThumbPos + ::Start
         RETURN HTSCROLLTHUMBDRAG
      ENDCASE

   ENDIF

   RETURN HTNOWHERE

METHOD Update() CLASS HBScrollBar

   LOCAL nCurRow
   LOCAL nCurCol
   LOCAL lUpdated  := .F.
   LOCAL nThumbPos := ::ThumbPos

   IF !ThumbPos( Self )
   ELSEIF nThumbPos != ::ThumbPos
      lUpdated  := .T.
      nCurRow   := Row()
      nCurCol   := Col()

      DispBegin()

      IF ::Orient == 1
         DispOutAt( ::Start + nThumbPos, ::OffSet, ::Style[ 2 ], __GUIColor( ::ColorSpec, 1 ) )
         DispOutAt( ::Start + ::ThumbPos, ::OffSet, ::Style[ 3 ], __GUIColor( ::ColorSpec, 2 ) )
      ELSE
         DispOutAt( ::OffSet, ::Start + nThumbPos, ::Style[ 2 ], __GUIColor( ::ColorSpec, 1 ) )
         DispOutAt( ::OffSet, ::Start + ::ThumbPos, ::Style[ 3 ], __GUIColor( ::ColorSpec, 2 ) )
      ENDIF

      DispEnd()

      SetPos( nCurRow, nCurCol )
   ENDIF

   RETURN lUpdated

/*
METHOD GetColor(xColor) CLASS HBScrollBar

   if ! ISCHARACTER( xColor )
   elseif Empty(__guicolor(xColor, 2))
   elseif Empty(__guicolor(xColor, 3))
      ::Color := xColor
   endif
return ::Color
*/

METHOD GetCurrent( nCurrent ) CLASS HBScrollBar

   IF ! ISNUMBER( nCurrent )
   ELSEIF nCurrent > ::nTotal
   ELSEIF nCurrent != ::nCurrent
      ::nCurrent := nCurrent
   ENDIF

   RETURN ::nCurrent

METHOD GetEnd( nEnd ) CLASS HBScrollBar

   IF !ISNUMBER( nEnd )
   ELSEIF nEnd < ::nStart
   ELSEIF nEnd != ::nEnd
      ::nEnd      := nEnd
      ::barlength := nEnd - ::nStart - 1
   ENDIF

   RETURN ::nEnd

METHOD GetOffSet( nOffSet ) CLASS HBScrollBar

   IF ! ISNUMBER( nOffSet )
   ELSEIF nOffSet != ::nOffSet
      ::nOffSet := nOffSet
   ENDIF

   RETURN ::nOffSet

METHOD GetOrient( nOrient ) CLASS HBScrollBar

   IF ! ISNUMBER( nOrient )
   ELSEIF nOrient == 1 .OR. nOrient == 2
      ::nOrient := nOrient
   ENDIF

   RETURN ::nOrient

METHOD GetStart( nStart ) CLASS HBScrollBar

   IF ! ISNUMBER( nStart )
   ELSEIF nStart > ::End
   ELSEIF nStart != ::nStart
      ::nStart    := nStart
      ::barlength := ::nEnd - nStart - 1
   ENDIF

   RETURN ::nStart

METHOD GetThumbPos( nPos ) CLASS HBScrollBar

   IF ISNUMBER( nPos )
      IF nPos < 1
         ::nThumbPos := 1
      ELSEIF nPos >= ::barlength
         ::nThumbPos := ::barlength

      ELSEIF nPos >= ::barlength - 1
         ::nThumbPos := nPos
      ELSE
         ::nThumbPos := nPos
      ENDIF

      IF nPos == 0
         ::hb_p_lShow := .F.
      ELSE
         ::hb_p_lShow := .T.
      ENDIF

   ENDIF

   RETURN ::nThumbPos

METHOD GetTotal( nTotal ) CLASS HBScrollBar

   IF ! ISNUMBER( nTotal )
   ELSEIF nTotal < 2
   ELSEIF nTotal != ::nTotal
      ::nTotal := nTotal
   ENDIF

   RETURN ::nTotal

STATIC FUNCTION ThumbPos( oScroll )

   LOCAL nPos
   LOCAL nCurrent
   LOCAL nBarLength
   LOCAL nTotal

   IF oScroll:barlength < 2
      RETURN .F.
   ENDIF

   IF oScroll:total < 2
      RETURN .F.
   ENDIF

/*
   IF oScroll:hb_p_lShow
      RETURN .T.
   ENDIF
*/

   nCurrent   := oScroll:Current
   nBarLength := oScroll:BarLength
   nTotal     := oScroll:Total
/*
   nPos      := ( ( nBarLength - 1 ) * nCurrent + nTotal - 2 * nBarLength + 1 ) / ;
*/
   ( nTotal - nBarLength )
   IF nCurrent > 1
      nPos := Max( 1, Min( nBarLength, Round( (nCurrent/nTotal ) * nBarLength,0 ) ) )
   ELSE
      nPos := 1
   ENDIF

   oScroll:ThumbPos := nPos

   RETURN .T.

FUNCTION ScrollBar( nStart, nEnd, nOffSet, bsBlock, nOrient )

   LOCAL oScroll
   //LOCAL cStyle

   IF !( ISNUMBER( nStart ) ) .OR. !( ISNUMBER( nEnd ) ) .OR. !( ISNUMBER( nOffSet ) )
      RETURN Nil
   ENDIF

   IF nOrient == NIL
      nOrient := 1
   ENDIF

   IF nOrient == 1
      //cStyle := "°²"
   ELSEIF nOrient == 2
      //cStyle := "°²" + Chr( 26 )
   ELSE
      RETURN Nil
   ENDIF

   oScroll           := HBScrollBar():New( nStart, nEnd, nOffSet, bsBlock, nOrient )
   oScroll:Barlength := nEnd - nStart - 1
   oScroll:Cargo     := NIL
   oScroll:end       := nEnd
   oScroll:OffSet    := nOffSet
   oScroll:orient    := nOrient
   oScroll:sBlock    := bsBlock
   oScroll:Start     := nStart

   RETURN oScroll

#endif
