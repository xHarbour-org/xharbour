/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Listbox class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
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

#include 'hbclass.ch'
#include "color.ch"
#include 'common.ch'
#include "box.ch"
#include "button.ch"
#include "inkey.ch"

#ifdef HB_COMPAT_C53

CLASS HBListBox

   METHOD New( nTop, nLeft, nBottom, nRight, lDrop )

   MESSAGE SELECT( nPos ) METHOD SELECTS( nPos )
   METHOD AddItem( cText, xValue )
   METHOD CLOSE()
   METHOD DelItem( nPos )
   METHOD DISPLAY()
   METHOD FindText( cText, nPos, lCaseSensitive, lExact )
   METHOD FindData( cText, nPos, lCaseSensitive, lExact )
   METHOD GetData( xItem )
   METHOD GetItem( nPos )
   METHOD GetText( nPos )
   METHOD HitTest( n, p )
   METHOD InsItem( nPos, cText, xVal )
   METHOD KillFocus()
   METHOD NextItem()
   METHOD Open()
   METHOD PrevItem()
   MESSAGE Scroll( n ) METHOD _Scroll( n )

   METHOD SetData( nPos, xValue )
   METHOD SetFocus()
   METHOD SetItem( nPos, aitem )
   METHOD SetText( nPos, xValue )
   DATA ClassName Init "LISTBOX"
   DATA Buffer
   DATA CapCol
   DATA CapRow
   DATA Cargo Init NIL
   DATA HasFocus Init .T.
   DATA ItemCount Init 0
   DATA Left Init 0
   DATA MESSAGE Init ''
   DATA TextValue Init ''
   DATA Style Init ""
   DATA sBlock Init NIL
   DATA fBlock Init Nil
   DATA hotbox Init ""
   DATA ColorSpec Init ""
   DATA coldBox
   DATA ISOPEN Init .F.
   DATA aItems Init {}
   DATA vScrolls

   DATA Value Init 0
   DATA TOP Init 0
   DATA right Init 0
   DATA BOTTOM Init 0
   DATA TopItem Init 1
   DATA dropdown Init .F.
   ACCESS nTop inline ::SetTop()
   ASSIGN nTop( xData ) inline ::SetTop( xData )
   ACCESS vScroll inline ::vScrolls
   ASSIGN vScroll( xData ) inline ::SetScroll( xData )
   ACCESS NRight inline ::SetRight()
   ASSIGN nRight( xData ) inline ::SetRight( xData )
   ACCESS lDropDown inline ::SetDropDown()
   ASSIGN lDropDown( xData ) inline ::SetDropDown( xData )
   ACCESS caption inline ::SetCaption()
   ASSIGN Caption( xData ) inline ::SetCaption( xData )
   ACCESS nBottom inline ::SetBottom()
   ASSIGN nBottom( xData ) inline ::SetBottom( xData )
   ACCESS nTopItem inline ::SetTopItem()
   ASSIGN nTopItem( xTop ) inline ::SetTopItem( xTop )
   ACCESS TypeOut inline ::itemCount == 0
   ASSIGN TypeOut( x ) inline iif( x != nil, x, ::itemCount == 0 )

   Hidden:

   METHOD SetScroll( xData )
   DATA xTop Init 0
   METHOD SetTop( xData )
   DATA xRight Init 0
   METHOD SetRight( xData )
   DATA xDropDown Init .F.
   METHOD SetDropDown( xData )
   DATA cCaption Init ''
   METHOD SetCaption( xData )
   DATA xBottom Init 0
   METHOD SetBottom( xData )
   DATA aScreen Init NIL
   DATA nCursor Init 0
   DATA xtopItem Init 0
   METHOD SetTopItem( xTop )

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, lDrop )

   LOCAL cColor

   ::ClassName := 'LISTBOX'
   ::Bottom    := nBottom
   ::nBottom   := nBottom
   ::right     := nRight
   ::nright    := nRight
   ::Top       := nTop
   ::ntop      := nTop
   ::left      := nleft
   ::Buffer    := Nil
   ::Caption   := ""
   ::CapCol    := nleft
   ::CapRow    := nTop
   ::Cargo     := Nil
   ::ColdBox   := B_SINGLE

   IF IsDefcolor()
      ::Colorspec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N,W/N"
   ELSE
      cColor      := SetColor()
      ::Colorspec := __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
         __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
         __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
         __GUIColor( cColor, CLR_ENHANCED   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_BORDER     + 1 ) + "," + ;
         __GUIColor( cColor, CLR_STANDARD   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_BACKGROUND + 1 )
   ENDIF

   ::isopen    := !lDrop
   ::aItems    := {}
   ::dropdown  := lDrop
   ::ldropdown := lDrop
   ::fBlock    := Nil
   ::hasfocus  := .F.

   ::hotbox    := B_DOUBLE
   ::itemCount := 0

   ::message   := ""

   ::ascreen   := Str( nTop + 1, 2 ) + ;
      Str( nleft, 2 ) + ;
      Str( nBottom, 2 ) + ;
      Str( nRight, 2 ) + ;
      SaveScreen( nTop + 1, nleft, nBottom, nRight )

   ::sBlock    := Nil
   ::nCursor   := Nil
#ifndef __PLATFORM__Windows
   ::Style     := Chr( 240 )
#endif
   ::TextValue := ""

   ::Topitem   := 0
   ::nTopItem  := 0
   ::vScroll   := Nil
   ::Value     := 0

   RETURN SELF

   /**** Get/Set Datas ****/

METHOD SetScroll( xData ) CLASS HBListBox

   IF ISOBJECT( xData ) /*.and. xData:Classname=="SCROLLBAR" .and. xData:orient==1)*/
      ::vScrolls  := xData
      xData:total := ::iTemCount
   ENDIF

   RETURN ::vScrolls

METHOD SetTop( xData ) CLASS HBListBox

   IF !( ISNIL( xData ) .AND. ISNUMBER( xData ) ) .AND. ;
         ISNUMBER( ::xTop := xData ) .AND. ;
         ISOBJECT( ::vScroll )
      ::vScroll:start := xData + 1
   ENDIF

   RETURN ::xTop

METHOD SetRight( xData ) CLASS HBListBox

   IF !( ISNIL( xData ) ) .AND. ISOBJECT( ( ::xRight := xData, ::vScroll ) )
      ::vScroll:offset := xData
   ENDIF

   RETURN ::xRight

METHOD SetDropDown( xData ) CLASS HBListBox

   IF !( ISNIL( xData ) ) .AND. ISLOGICAL( xData )
      ::xDropDown := xData

      IF xData
      ELSEIF !::isOpen
         ::isOpen := .T.
      ENDIF
      ::display()
   ENDIF

   RETURN ::xDropDown

METHOD SetCaption( xData ) CLASS HBListBox

   IF ISCHARACTER( xData ) //.and. ISNIL( ::Capcol )
      ::cCaption := xData
      ::Caprow   := ::top
      ::Capcol   := if( ISNIL( ::CapCol ), ::left - Len( xData ), ::CapCol )
   ENDIF

   RETURN ::cCaption

METHOD SetBottom( xData ) CLASS HBListBox

   IF !( ISNIL( xData ) .AND. ISNUMBER( xData ) ) .AND. ;
         ISNUMBER( ( ::xBottom := xData ) ) .AND. ISOBJECT( ( ::vScroll ) )
      ::vScroll:end := xData - 1
   ENDIF

   RETURN ::xBottom

   /*** Class Methods ***/

METHOD ADDITEM( cText, xValue ) CLASS HBListBox

   IF ! ISCHARACTER( cText )
   ELSEIF ValType( xValue ) IN "CU"
      AAdd( ::aItems, { cText, xValue } )
      ::iTemCount ++

      IF ::iTemCount == 1 .AND. ;
            ISOBJECT( ( ::Topitem := 1, ::nTopItem := 1, ::vScroll ) )
         ::vScroll:total := ( ::iTemCount - ( ::bottom - ::top - 2 ) )
      ENDIF

   ENDIF

   RETURN SELF

METHOD CLOSE() CLASS HBListBox

   IF ::isOpen

      RestScreen( Val( SubStr( ::aScreen, 1, 2 ) ), ;
         Val( SubStr( ::aScreen, 3, 2 ) ), ;
         Val( SubStr( ::aScreen, 5, 2 ) ), ;
         Val( SubStr( ::aScreen, 7, 2 ) ), SubStr( ::aScreen, 9 ) )
      ::isOpen  := .F.
      ::aScreen := Nil

   ENDIF

   RETURN SELF

METHOD DELITEM( xitem )

   IF xitem < 1
   ELSEIF xitem <= ::iTemCount
      ADel( ::aItems[ xitem ], .T. )
      //        Asize( ::aItems, -- ::iTemCount )
      ::iTemCount--

      IF ::Value > ::iTemCount
         ::Value := ::iTemCount

         IF ::Value == 0
            ::TextValue := ""
         ELSE
            ::TextValue := _Getdata( ::aItems[ ::iTemCount ] )
         ENDIF

         IF ISNIL( ::Buffer )
         ELSEIF ISNUMBER( ::Buffer )
            ::Buffer := ::iTemCount
         ELSEIF ::Value > 0
            ::Buffer := ::TextValue
         ENDIF

      ENDIF

      IF ::Topitem > ::iTemCount
         ::Topitem  := ::iTemCount
         ::nTopitem := ::iTemCount
      ENDIF

      IF ISOBJECT( ::vScroll )
         ::vScroll:total := ::iTemCount - ( ::Bottom - ::top - 2 )
      ENDIF

   ENDIF

   RETURN SELF

METHOD Getdata( xData ) CLASS HBListBox

   LOCAL xRet := Nil

   IF xData < 1
   ELSEIF xData <= ::itemCount
      xRet := ::aitems[ xData, 2 ]
   ENDIF

   RETURN xRet

METHOD FindData( cText, nPos, lCaseSensitive, lExact ) CLASS HBListBox

   LOCAL nPosFound
   LOCAL lOldExact
   LOCAL nStart
   LOCAL nEnd
   LOCAL nSize

   IF ISLOGICAL( lExact )
      lOldExact := Set( _SET_EXACT, lExact )
   ENDIF

   nEnd := 1

   IF ISNUMBER( nPos )
      nEnd ++
   ELSE
      nPos := 1
   ENDIF

   nSize := Len( ::aitems ) - nPos + 1

   IF ! ISLOGICAL( lCaseSensitive )
      lCaseSensitive := .T.
   ENDIF

   FOR nStart := 1 TO nEnd

      IF lCaseSensitive

         IF SET( _SET_EXACT )
            nPosFound := AScan( ::aitems, ;
               { | _1 | _Getdata( _1 ) == cText }, nPos, nSize )
         ELSE
            nPosFound := AScan( ::aitems, ;
               { | _1 | _Getdata( _1 ) = cText }, nPos, nSize )
         ENDIF

      ELSEIF SET( _SET_EXACT )
         nPosFound := AScan( ::aitems, ;
            { | _1 | Lower( _Getdata( _1 ) ) == Lower( cText ) }, ;
            nPos, nSize )
      ELSE
         nPosFound := AScan( ::aitems, ;
            { | _1 | Lower( _Getdata( _1 ) ) == Lower( cText ) }, ;
            nPos, nSize )
      ENDIF

      IF nPosFound > 0
         EXIT
      ENDIF

      nSize := nPos - 1
      nPos  := 1
   NEXT

   IF ! ISNIL( lOldExact )
      SET Exact ( lOldExact )
   ENDIF

   RETURN nPosFound

METHOD FindText( cText, nPos, lCaseSensitive, lExact ) CLASS HBListBox

   LOCAL nPosFound
   LOCAL lOldExact
   LOCAL nStart
   LOCAL nEnd
   LOCAL nSize

   IF ISLOGICAL( lExact )
      lOldExact := Set( _SET_EXACT, lExact )
   ENDIF

   nEnd := 1

   IF ISNUMBER( nPos )
      nEnd ++
   ELSE
      nPos := 1
   ENDIF

   nSize := Len( ::aitems ) - nPos + 1

   IF ! ISLOGICAL( lCaseSensitive )
      lCaseSensitive := .T.
   ENDIF

   FOR nStart := 1 TO nEnd
      IF lCaseSensitive

         IF SET( _SET_EXACT )
            nPosFound := AScan( ::aitems, ;
               { | _1 | _1[ 1 ] == cText }, nPos, nSize )

         ELSE
            nPosFound := AScan( ::aitems, ;
               { | _1 | _1[ 1 ] == cText }, nPos, nSize )
         ENDIF

      ELSEIF SET( _SET_EXACT )
         nPosFound := AScan( ::aitems, ;
            { | _1 | Lower( _1[ 1 ] ) == Lower( cText ) }, ;
            nPos, nSize )
      ELSE
         nPosFound := AScan( ::aitems, ;
            { | _1 | Lower( _1[ 1 ] ) = Lower( cText ) }, ;
            nPos, nSize )
      ENDIF

      IF nPosFound > 0
         EXIT
      ENDIF

      nSize := nPos - 1
      nPos  := 1
   NEXT

   IF ! ISNIL( lOldExact )
      SET Exact ( lOldExact )
   ENDIF

   RETURN nPosFound

METHOD NEXTITEM() CLASS HBListBox

   LOCAL nCurValue
   LOCAL nValue

   IF ! ::hasfocus
   ELSEIF ::itemCount > 0

      IF ( nCurValue := ::value ) == ::itemCount
         nValue := nCurValue
      ELSE
         nValue := nCurValue + 1
      ENDIF

      changeitem( SELF, nCurValue, nValue )

   ENDIF

   RETURN SELF

METHOD PREVITEM() CLASS HBListBox

   LOCAL nCurValue
   LOCAL nValue

   IF ! ::hasfocus
   ELSEIF ::itemCount > 0

      IF ( nCurValue := ::value ) == 0
         nValue := 1
      ELSEIF nCurValue == 1
         nValue := nCurValue
      ELSE
         nValue := nCurValue - 1
      ENDIF

      changeitem( SELF, nCurValue, nValue )

   ENDIF

   RETURN SELF

METHOD _SCROLL( nMethod ) CLASS HBListBox

   LOCAL nPos
   LOCAL nTopItem
   LOCAL nCount
   LOCAL nThumbPos
   LOCAL nCurrent
   LOCAL nBarLength
   LOCAL nTotal
   LOCAL nSize
   LOCAL nMouRow
   LOCAL nMouseRow
   LOCAL nKey
   LOCAL nStart

   Switch nMethod
   CASE HTSCROLLTHUMBDRAG
      nMouseRow := MRow()
      DO WHILE ( ( nKey := Inkey( 0 ) ) != 1003 )
         IF nKey == K_MOUSEMOVE
            nMouRow := MRow()
            IF nMouRow <= ::vScroll:start()
               nMouRow := ::vScroll:start() + 1
            ENDIF
            IF nMouRow >= ::vScroll:end()
               nMouRow := ::vScroll:end() - 1
            ENDIF
            IF nMouRow != nMouseRow
               nThumbPos  := ::vScroll:thumbpos() + ( nMouRow - nMouseRow )
               nBarLength := ::vScroll:barlength()
               nTotal     := ::vScroll:total()
               nSize      := ( nThumbPos * ( nTotal - nBarLength - 2 ) + 2 * ;
                  nBarLength + 1 - nTotal ) / ( nBarLength - 1 )
               IF nSize < 1
                  nSize := 1
               ENDIF
               IF nSize > nTotal
                  nSize := nTotal
               ENDIF
               nCurrent := ::vScroll:current()
               IF nSize - nCurrent > 0
                  FOR nStart := 1 TO nSize - nCurrent
                     SELF:Scroll( HTSCROLLUNITINC )
                  NEXT
               ELSE
                  FOR nStart := 1 TO nCurrent - nSize
                     SELF:Scroll( HTSCROLLUNITDEC )
                  NEXT
               ENDIF
               nMouseRow := nMouRow
            ENDIF
         ENDIF
      ENDDO
      EXIT

   CASE HTSCROLLUNITDEC
      IF ::topitem > 1
         ::topitem --
         ::vScroll:current := lbadjustcu( SELF )
         SELF:display()
      ENDIF
      EXIT

   CASE HTSCROLLUNITINC
      IF ( ::topitem + ::bottom - ::top ) <= ::itemCount + 1
         ::topitem ++
         ::vScroll:current( lbadjustcu( SELF ) )
         SELF:display()
      ENDIF
      EXIT

   CASE HTSCROLLBLOCKDEC
      nPos     := ::bottom - ::top - iif( ::bitmap, 2, 1 )
      nTopItem := ::topitem - nPos
      IF ::topitem > 1
         IF nTopItem < 1
            nTopItem := 1
         ENDIF
         ::topitem  := nTopItem
         ::ntopitem := nTopItem
         ::vScroll:current( lbadjustcu( SELF ) )
         SELF:display()
      ENDIF
      EXIT

   CASE HTSCROLLBLOCKINC
      nPos     := ::bottom - ::top - 1
      nCount   := ::itemCount
      nTopItem := ::topitem + nPos
      IF ::topitem < nCount - nPos + 1
         IF nTopItem + nPos - 1 > nCount
            nTopItem := nCount - nPos + 1
         ENDIF
         ::topitem  := nTopItem
         ::ntopitem := nTopItem
         ::vScroll:current( lbadjustcu( SELF ) )
         SELF:display()
      ENDIF
      EXIT

   End

   RETURN SELF

METHOD SELECTS( nPosition ) CLASS HBListBox

   LOCAL nValue
   LOCAL nPos
   LOCAL xType := ValType( nPosition )

   DO CASE
   CASE xType == "C"
      nPos := SELF:finddata( nPosition )
      IF !( ValType( ::buffer ) IN "CU" )
         ::buffer := nPos
      ELSEIF ::value == 0
         ::buffer := nPosition
      ELSE
         ::buffer := _Getdata( ::aitems[ nPos ] )
      ENDIF
   CASE !( xType == "N" )
      RETURN ::value
   CASE nPosition < 1
      RETURN ::value
   CASE nPosition > ::itemCount
      RETURN ::value
   CASE nPosition == ::value
      RETURN ::value
   OTHERWISE
      nPos := nPosition
      IF ValType( ::buffer ) IN "NU"
         ::buffer := nPos
      ELSEIF nPos == 0
         ::buffer := ""
      ELSE
         ::buffer := _Getdata( ::aitems[ nPos ] )
      ENDIF
   ENDCASE
   ::value := nPos

   IF nPos == 0
      ::textvalue := ""
   ELSE
      ::textvalue := _Getdata( ::aitems[ nPos ] )
   ENDIF

   IF Empty( ::hotbox + ::coldbox )
      nPos := 0
   ELSE
      nPos := 2
   ENDIF

   nValue := ::value - ( ::bottom - ::top - nPos )
   IF ::topitem <= nValue
      ::topitem  := nValue
      ::ntopitem := nValue
      IF ISOBJECT( ::vScroll )
         ::vScroll:current := lbadjustcu( SELF )
      ENDIF
   ELSEIF ::value == 0
   ELSEIF ::topitem > ::value .AND. ISOBJECT( ( ;
         ::topitem := ::value, ::ntopitem := ::value, ::vScroll ) )
      ::vScroll:current := lbadjustcu( SELF )
   ENDIF
   SELF:display()
   IF ISBLOCK( ::sBlock )
      Eval( ::sBlock )
   ENDIF

   RETURN ::value

METHOD SetTOPITEM( xData ) CLASS HBListBox

   LOCAL nSize
   LOCAL nPos

   IF !( ISNIL( xData ) ) .AND. xData > 0 .AND. xData <= ::itemCount

      IF Empty( ::hotbox + ::coldbox )
         nPos := 0
      ELSE
         nPos := 2
      ENDIF
      nSize := ::itemCount - ( ::bottom - ::top - nPos )
      IF xData > nSize
         xData := nSize
      ENDIF
      IF ::topitem != xData
         ::xtopitem := xData
         IF ISOBJECT( ::vScroll )
            ::vScroll:current := lbadjustcu( SELF )
         ENDIF
         SELF:display()
      ENDIF
   ENDIF

   RETURN ::xtopitem

METHOD DISPLAY() CLASS HBListBox

   LOCAL nCurRow       := Row()
   LOCAL nCurCol       := Col()
   LOCAL cCurrentColor := SetColor()
   LOCAL nStart
   LOCAL nEnd
   LOCAL cColor4
   LOCAL cColor3
   LOCAL nTop          := ::top
   LOCAL nLeft         := ::left
   LOCAL nSize
   LOCAL cHotBox
   LOCAL cCaption
   LOCAL nAmpPos
   LOCAL cColorAny

   nSize := ::right - nLeft + 1

   IF ::hasfocus
      cHotBox := ::hotbox
      cColor3 := __GUIColor( ::colorspec, 3 )
      cColor4 := __GUIColor( ::colorspec, 4 )

      IF ::isopen
         cColorAny := __GUIColor( ::colorspec, 2 )
      ELSE
         cColorAny := __GUIColor( ::colorspec, 4 )
      ENDIF

   ELSE
      cHotBox   := ::coldbox
      cColor3   := __GUIColor( ::colorspec, 1 )
      cColor4   := __GUIColor( ::colorspec, 2 )
      cColorAny := __GUIColor( ::colorspec, 2 )

   ENDIF

   DispBegin()
   nEnd := ::topitem + ::bottom - ::top

   IF ::dropdown
      SET COLOR TO ( cColorAny )
      SetPos( nTop ++, nLeft )

      IF ::value == 0
         ?? Space( nSize - 1 )
      ELSE
         ?? PadR( ::aitems[ ::value, 1 ], nSize - 1 )
      ENDIF

      SET COLOR TO ( __GUIColor( ::colorspec, 8 ) )
      ?? Left( ::style, 1 )
      nEnd --

   ENDIF

   IF ::isopen
      IF !Empty( cHotBox )

         SET COLOR TO ( __GUIColor( ::colorspec, 5 ) )
         @ nTop, nLeft CLEAR TO ::bottom, ::right
         @ nTop, nLeft, ::bottom, ::right BOX cHotBox

         IF ISOBJECT( ::vScroll )
            ::vScroll:display()
         ENDIF

         nTop ++
         nLeft ++
         nSize -= 2
         nEnd  -= 2

      ENDIF

      IF nEnd > ::itemCount
         nEnd := ::itemCount
      ENDIF

      FOR nStart := ::topitem TO nEnd

         IF nStart == ::value
            SET COLOR TO ( cColor4 )
         ELSE
            SET COLOR TO ( cColor3 )
         ENDIF

         SetPos( nTop ++, nLeft )
         ?? PadR( ::aitems[ nStart, 1 ], nSize )

      NEXT

   ENDIF

   IF !Empty( cCaption := ::caption )

      IF ( nAmpPos := At( "&", cCaption ) ) == 0
      ELSEIF nAmpPos == Len( cCaption )
         nAmpPos := 0
      ELSE
         cCaption := Stuff( cCaption, nAmpPos, 1, "" )
      ENDIF

      SET COLOR TO ( __GUIColor( ::colorspec, 6 ) )
      SetPos( ::caprow, ::capcol - 1 )
      ?? cCaption

      IF nAmpPos != 0
         SET COLOR TO ( __GUIColor( ::colorspec, 7 ) )
         SetPos( ::caprow, ::capcol + nAmpPos - 2 )
         ?? SubStr( cCaption, nAmpPos, 1 )
      ENDIF

   ENDIF

   DispEnd()

   SET COLOR TO ( cCurrentColor )
   SetPos( nCurRow, nCurCol )

   RETURN SELF

METHOD GetItem( xItem ) CLASS HBListBox

   LOCAL xRet := Nil

   IF xItem < 1
   ELSEIF xItem <= ::itemCount
      xRet := ::aitems[ xItem ]
   ENDIF

   RETURN xRet

METHOD GetText( xItem ) CLASS HBListBox

   LOCAL xRet := Nil

   IF xItem < 1
   ELSEIF xItem <= ::itemCount
      xRet := ::aitems[ xItem, 1 ]
   ENDIF

   RETURN xRet

METHOD InsItem( nPosition, cText, xExp )

   IF ! ISCHARACTER( cText )
   ELSEIF ! ISNUMBER( nPosition )
   ELSEIF nPosition < ::itemCount
      //        Asize( ::aitems, ++ ::itemCount )
      ::itemCount++
      AIns( ::aitems, nPosition, { cText, xExp }, .T. )
      //        ::aitems[ nPosition ] := { cText, xExp }

      IF ::itemCount == 1
         ::topitem  := 1
         ::ntopitem := 1
      ENDIF

      IF ISOBJECT( ::vScroll )
         ::vScroll:total := ::itemCount - ( ::bottom - ::top - 2 )
      ENDIF

   ENDIF

   RETURN SELF

METHOD HitTest( nMouseRow, nMouseCol ) CLASS HBListBox

   LOCAL Local1
   LOCAL cColor

   IF ! ::isopen
   ELSEIF ! ISOBJECT( ::vScroll )
   ELSEIF ( Local1 := ::vScroll:HitTest( nMouseRow, nMouseCol ) ) != 0
      RETURN Local1
   ENDIF

   IF ! ::isopen .OR. Empty( ::hotbox + ::coldbox )
      Local1 := 0
   ELSE
      cColor := ::top
      IF ::DropDown
         cColor ++
      ENDIF

      DO CASE
      CASE nMouseRow == cColor
         IF nMouseCol == ::left
            RETURN HTTOPLEFT
         ELSEIF nMouseCol == ::right
            RETURN HTTOPRIGHT
         ELSEIF nMouseCol >= ::left .AND. nMouseCol <= ::right
            RETURN HTTOP
         ENDIF
      CASE nMouseRow == ::bottom
         IF nMouseCol == ::left
            RETURN HTBOTTOMLEFT
         ELSEIF nMouseCol == ::right
            RETURN HTBOTTOMRIGHT
         ELSEIF nMouseCol >= ::left .AND. nMouseCol <= ::right
            RETURN HTBOTTOM
         ENDIF
      CASE nMouseCol == ::left
         IF nMouseRow >= ::TOP .AND. nMouseRow <= ::bottom
            RETURN HTLEFT
         ELSE
            RETURN HTNOWHERE
         ENDIF
      CASE nMouseCol == ::right
         IF nMouseRow >= ::TOP .AND. nMouseRow <= ::bottom
            RETURN HTRIGHT
         ELSE
            RETURN HTNOWHERE
         ENDIF
      ENDCASE
      Local1 := 1
   ENDIF

   DO CASE
   CASE ! ::isopen
   CASE nMouseRow < cColor + Local1
   CASE nMouseRow > ::bottom - Local1
   CASE nMouseCol < ::left + Local1
   CASE nMouseCol <= ::right - Local1
      RETURN ::topitem + nMouseRow - ( cColor + Local1 )
   ENDCASE

   DO CASE
   CASE ! ::dropdown
   CASE nMouseRow != ::top
   CASE nMouseCol < ::left
   CASE nMouseCol < ::right
      RETURN HTCLIENT
   CASE nMouseCol == ::right
      RETURN HTDROPBUTTON
   ENDCASE

   DO CASE
   CASE Empty( ::caption )
   CASE nMouseRow != ::caprow
   CASE nMouseCol < ::capcol
   CASE nMouseCol < ::capcol + __CapLength( ::caption )
      RETURN HTCAPTION
   ENDCASE

   RETURN 0

METHOD KillFocus() CLASS HBListBox

   IF ::hasfocus
      ::hasfocus := .F.

      IF ISBLOCK( ::fblock )
         Eval( ::fblock )
      ENDIF

      DispBegin()

      IF ::dropdown .AND. ::isopen
         ::close()
      ENDIF

      ::display()
      DispEnd()

      SetCursor( ::nCursor )

   ENDIF

   RETURN SELF

METHOD Open() CLASS HBListBox

   IF ! ::isopen

      ::ascreen := Str( ::top + 1, 2 ) + ;
         Str( ::left, 2 ) + ;
         Str( ::bottom, 2 ) + ;
         Str( ::right, 2 ) + ;
         SaveScreen( ::top + 1, ::left, ::bottom, ::right )
      ::isopen := .T.
      SELF:display()

   ENDIF

   RETURN SELF

METHOD SetText( nPos, cText ) CLASS HBListBox

   IF nPos < 1
   ELSEIF nPos <= ::itemCount
      ::aitems[ nPos, 1 ] := cText
   ENDIF

   RETURN SELF

METHOD SetItem( nPos, cText ) CLASS HBListBox

   DO CASE
   CASE nPos < 1
   CASE nPos > ::itemCount
   CASE Len( cText ) != 2
   CASE ISCHARACTER( cText[ 1 ] )
      ::aitems[ nPos ] := cText
   ENDCASE

   RETURN SELF

METHOD SetFocus() CLASS HBListBox

   IF ! ::hasfocus
      ::nCursor  := SetCursor( 0 )
      ::hasfocus := .T.
      DispBegin()
      ::display()
      DispEnd()

      IF ISBLOCK( ::fblock )
         Eval( ::fblock )
      ENDIF

   ENDIF

   RETURN SELF

METHOD SetData( nPos, xData ) CLASS HBListBox

   IF nPos < 1
   ELSEIF nPos <= ::itemCount
      ::aitems[ nPos, 2 ] := xData
   ENDIF

   RETURN SELF

STATIC FUNCTION CHANGEITEM( oList, nPos, nItem )

   LOCAL Local1
   LOCAL Local2

   IF nPos != nItem
      oList:value := nItem

      IF oList:value == 0
         oList:Textvalue := ""
      ELSE
         oList:Textvalue := _Getdata( oList:aItems[ oList:value ] )
      ENDIF

      IF ISNIL( oList:Buffer )
      ELSEIF ISNUMBER( oList:Buffer )
         oList:Buffer := oList:value
      ELSEIF oList:value > 0
         oList:Buffer := oList:Textvalue
      ENDIF

      IF Empty( oList:hotbox + oList:coldbox )
         Local2 := 0
      ELSE
         Local2 := 2
      ENDIF

      IF oList:Dropdown
         Local2 ++
      ENDIF

      Local1 := oList:value - ( oList:Bottom - oList:top - Local2 )

      IF oList:Topitem > oList:value
         oList:topitem := oList:value

         IF ISOBJECT( oList:vScroll )
            oList:vScroll:current := lbadjustcu( oList )
         ENDIF

      ELSEIF oList:topitem <= Local1 .AND. ;
            ISOBJECT( ( oList:topitem := Local1, oList:vScroll ) )
         oList:vScroll:current := lbadjustcu( oList )
      ENDIF

      oList:display()

      IF ISBLOCK( oList:sBlock )
         Eval( oList:sBlock )
      ENDIF

   ENDIF

   RETURN oList

STATIC FUNCTION LBADJUSTCU( oList )

   LOCAL nSize
   LOCAL nCount
   LOCAL nLength
   LOCAL nTopItem
   LOCAL nNewSize

   nSize    := oList:Bottom - oList:top - iif( oList:dropdown, 2, 1 )
   nCount   := oList:itemCount
   nLength  := oList:vScroll:barlength
   nTopItem := oList:Topitem
   nNewSize := ( ( nCount - nLength ) * nTopItem + nLength - nSize ) / ;
      ( nCount - nSize )

   RETURN nNewSize

FUNCTION ListBox( nTop, nLeft, nBottom, nRight, lDrop )

   DEFAULT lDrop TO .F.

   IF ISNUMBER( nTop ) .AND. ;
         ISNUMBER( nleft ) .AND. ;
         ISNUMBER( nBottom ) .AND. ;
         ISNUMBER( nRight )

      RETURN HBListBox():New( nTop, nLeft, nBottom, nRight, lDrop )

   ENDIF

   RETURN nil

STATIC FUNCTION _Getdata( xItem )

   IF ISNIL( xItem[ 2 ] )
      RETURN xItem[ 1 ]
   ENDIF

   RETURN xItem[ 2 ]

FUNCTION _LISTBOX_( Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, ;
      Arg8, Arg9, Arg10, Arg11, Arg12, Arg13 )

   LOCAL oScroll
   LOCAL nPos
   LOCAL nLen
   LOCAL nCurPos

   DEFAULT arg5 TO 1
   DEFAULT arg12 TO .F.
   DEFAULT arg13 TO .F.
   DEFAULT arg7 TO ''

   oScroll := ListBox( Arg1, Arg2, Arg3, Arg4, Arg12 )

   IF ! ISNIL( oScroll )

      IF ISCHARACTER( Arg7 )
         oScroll:capcol  := NIL
         oScroll:caption := Arg7
         oScroll:capcol  := Arg2 - __CapLength( Arg7 )
      ENDIF

      IF arg9 != nil
         oScroll:colorspec := Arg9
      ENDIF

      oScroll:message := Arg8
      oScroll:fblock  := Arg10
      oScroll:sblock  := Arg11

      nLen            := Len( Arg6 )

      FOR nPos := 1 TO nLen
         nCurPos := Arg6[ nPos ]

         IF ! ISARRAY( nCurPos )
            oScroll:additem( nCurPos )
         ELSEIF Len( nCurPos ) == 1
            oScroll:additem( nCurPos[ 1 ] )
         ELSE
            oScroll:additem( nCurPos[ 1 ], nCurPos[ 2 ] )
         ENDIF

      NEXT

      IF ISLOGICAL( Arg13 ) .AND. Arg13

         IF ! ISLOGICAL( Arg12 )
         ELSEIF Arg12
            Arg1 ++
         ENDIF

         oScroll:vscroll := ScrollBar( Arg1 + 1, Arg3 - 1, Arg4, , 1 )

      ENDIF

      oScroll:Select( Arg5 )

   ENDIF

   RETURN oScroll

FUNCTION __CapLength( Arg1 )

   LOCAL Local1
   LOCAL Local2

   DEFAULT Arg1 TO ""

   Local1 := Len( Arg1 )

   IF ( Local2 := At( "&", Arg1 ) ) == 0
   ELSEIF Local2 < Local1
      Local1 --
   ENDIF

   RETURN Local1

#endif

//+ EOF: LISTBOX.PRG
