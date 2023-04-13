/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * POPUP menu class
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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
#include "box.ch"
#include "button.ch"
#include "color.ch"
#include "common.ch"

#ifdef HB_COMPAT_C53

/* TOFIX: Harbour doesn't check if the colorSpec instance
          var has always six pairs of colors. It should
          do so and throw an error. [jlalin]
*/

/* NOTE: In the Get* methods we are breaking the "only one return" rule. I
         know this isn't a good practice however we are eliminating a variable,
         an exit statement and two assigments which is good for speed critical
         and small functions. [jlalin]
*/

//--------------------------------------------------------------------------//

FUNCTION Popup( nTop, nLeft, nBottom, nRight )

   LOCAL oPopUp := PopUpMenu():New( nTop, nLeft, nBottom, nRight )

   RETURN oPopUp

//--------------------------------------------------------------------------//

CLASS PopUpMenu

   DATA ClassName       init    "POPUPMENU"
   DATA aItems          init    {}
   DATA border          init    B_SINGLE + SEPARATOR_SINGLE
   DATA BOTTOM
   DATA cargo
   DATA colorSpec       init    "N/W,W/N,W+/W,W+/N,N+/W,W/N"
   DATA current         init    0
   DATA itemCount       init    0
   DATA left
   DATA opened          init    FALSE PROTECTED
   DATA right
   DATA saveScr         init    ""    PROTECTED
   DATA TOP
   DATA width           init    0

#ifdef HB_EXTENSION
   DATA shadowed        init    FALSE
#endif

   METHOD New( nTop, nLeft, nBottom, nRight )
   METHOD AddItem( oItem )
   METHOD CLOSE( lClose )
   METHOD DelItem( nPos )
   METHOD DISPLAY()
   METHOD GetAccel( nKey )
   METHOD GetFirst()
   METHOD GetItem( nPos )
   METHOD GetLast()
   METHOD GetNext()
   METHOD GetPrev()
   METHOD GetShortct( nKey )
   METHOD HitTest( nRow, nCol )
   METHOD InsItem( nPos, oItem )
   METHOD IsOpen()
   METHOD Open()
   MESSAGE SELECT( nPos )       METHOD _Select( nPos )
   METHOD SetItem( nPos, oItem )

   /* NOTE: This method is new in Harbour */
#ifdef HB_EXTENSION
   METHOD SetCoors( nRow, nCol, lTop )
#endif

   METHOD IsShortcut( nKey, nID )
   METHOD IsQuick( nKey, nID )

ENDCLASS

//--------------------------------------------------------------------------//

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS PopUpMenu

   /* NOTE: When a PopUp is created and attached to a TopBar object, its
            coords are initialized to -1, so the TopBar can update them
            accordingly to its own position on to the screen. [jlalin]
   */

   DEFAULT nTop    TO - 1
   DEFAULT nLeft   TO - 1
   DEFAULT nBottom TO  0
   DEFAULT nRight  TO  0

   ::aItems    := {}
   ::border    := B_SINGLE + SEPARATOR_SINGLE
   ::bottom    := nBottom
   ::colorSpec := "N/W,W/N,W+/W,W+/N,N+/W,W/N"
   ::current   := 0
   ::itemCount := 0
   ::left      := nLeft
   ::opened    := FALSE
   ::right     := nRight
   ::saveScr   := ""
   ::top       := nTop
   ::width     := 0

#ifdef HB_EXTENSION
   ::shadowed  := FALSE
#endif

   RETURN Self

//--------------------------------------------------------------------------//

METHOD AddItem( oItem ) CLASS PopUpMenu

   LOCAL nLen

   AAdd( ::aItems, oItem )
   ::itemCount++

   nLen := Len( StrTran( oItem:caption, "&", "" ) )
   ::width := Max( nLen + 4, ::width ) // 4 is for box margins

   RETURN Self

//--------------------------------------------------------------------------//

METHOD CLOSE( lClose ) CLASS PopUpMenu

   DEFAULT lClose TO TRUE

   if ::opened
      IF lClose
         if ::current > 0
            if ::aItems[ ::current ]:isPopUp()
               ::aItems[ ::current ]:data:Close( lClose )
            ENDIF
         ENDIF
      ENDIF
      ::current := 0
      ::opened := FALSE
      RestScreen( ::top, ::left, ::bottom, ::right, ::saveScr )
      ::saveScr := nil
   ENDIF

   RETURN Self

//--------------------------------------------------------------------------//

METHOD DelItem( nPos ) CLASS PopUpMenu

   IF nPos > 0 .AND. nPos <= ::itemCount
      ADel( ::aItems, nPos, .T. )
      ::itemCount--

      AEval( ::aItems, ;
         {|oItem| ::width := Max( Len( StrTran( oItem:caption, "&", "" ) ) + 4, ::width ) } )

   ENDIF

   RETURN Self

//--------------------------------------------------------------------------//
/* NOTE: This method corrects two bugs in Cl*pper:
         1) when two menuitems have the same key and the
            first item is disabled
         2) when a menuitem is disabled it will ignore the key [jlalin]
*/

METHOD GetAccel( nKey ) CLASS PopUpMenu

   LOCAL nAt
   LOCAL cKey  := Upper( Chr( nKey ) )
   LOCAL oItems

   FOR EACH oItems IN ::aItems
      nAt := At( "&", oItems:caption )
      IF nAt > 0 .AND. oItems:enabled .AND. Upper( SubStr( oItems:caption, nAt + 1, 1 ) ) == cKey
         RETURN HB_EnumIndex()
      ENDIF
   NEXT

   RETURN 0

//--------------------------------------------------------------------------//

METHOD GetFirst() CLASS PopUpMenu

   LOCAL oItems

   FOR EACH oItems IN ::aItems
      IF oItems:caption != MENU_SEPARATOR .AND. oItems:enabled
         RETURN HB_EnumIndex()
      ENDIF
   NEXT

   RETURN 0

//--------------------------------------------------------------------------//

METHOD GetItem( nPos ) CLASS PopUpMenu

   LOCAL oItem

   IF nPos > 0 .AND. nPos <= ::itemCount
      oItem := ::aItems[ nPos ]
   ENDIF

   RETURN oItem

//--------------------------------------------------------------------------//

METHOD GetLast() CLASS PopUpMenu

   LOCAL n

   FOR n := ::itemCount TO 1 step - 1
      if ::aItems[ n ]:caption != MENU_SEPARATOR .AND. ::aItems[ n ]:enabled
         RETURN n
      ENDIF
   NEXT

   RETURN 0

//--------------------------------------------------------------------------//

METHOD GetNext() CLASS PopUpMenu

   LOCAL n

   if ::current < ::itemCount
      FOR n := ::current + 1 to ::itemCount
         if ::aItems[ n ]:caption != MENU_SEPARATOR .AND. ::aItems[ n ]:enabled
            RETURN n
         ENDIF
      NEXT
   ENDIF

   RETURN 0

//--------------------------------------------------------------------------//

METHOD GetPrev() CLASS PopUpMenu

   LOCAL n

   if ::current > 1
      FOR n := ::current - 1 TO 1 step - 1
         if ::aItems[ n ]:caption != MENU_SEPARATOR .AND. ::aItems[ n ]:enabled
            RETURN n
         ENDIF
      NEXT
   ENDIF

   RETURN 0

//--------------------------------------------------------------------------//
/* NOTE: This method corrects a bug in Cl*pper:
         1) when a menuitem is disabled it will ignore the key [jlalin]
*/

METHOD GetShortct( nKey ) CLASS PopUpMenu

   LOCAL oItems

   FOR EACH oItems IN ::aItems
      IF HB_ISSTRING( oItems:shortcut )
         oItems:shortcut := Asc( oItems:shortcut[1] )
      End
      IF oItems:enabled .AND. oItems:shortcut == nKey
         RETURN HB_EnumIndex()
      ENDIF
   NEXT

   RETURN 0

//--------------------------------------------------------------------------//
/* NOTE: This method corrects two bugs in Cl*pper:
         1) when two menuitems have the same key and the first item
            is disabled
         2) when a menuitem is disabled it will ignore the click [jlalin]
*/

METHOD HitTest( nRow, nCol ) CLASS PopUpMenu

   LOCAL nHit  := HTNOWHERE

   DO CASE
   CASE nRow == ::top
      IF nCol == ::left
         nHit := HTTOPLEFT
      ELSEIF nCol == ::right
         nHit := HTTOPRIGHT
      ELSE
         nHit := HTTOP
      ENDIF
   CASE nRow == ::bottom
      IF nCol == ::left
         nHit := HTBOTTOMLEFT
      ELSEIF nCol == ::right
         nHit := HTBOTTOMRIGHT
      ELSE
         nHit := HTBOTTOM
      ENDIF
   CASE nRow > ::TOP .AND. nCol > ::left .AND. nRow < ::BOTTOM .AND. nCol < ::right
      if ::aItems[ nRow - ::top ]:enabled .AND. ::aItems[ nRow - ::top ]:caption != MENU_SEPARATOR
         nHit := nRow - ::top
      ELSE
         nHit := HTSEPARATOR
      ENDIF
   CASE nRow > ::TOP .AND. nRow < ::bottom
      IF nCol == ::left
         nHit := HTLEFT
      ELSEIF nCol == ::right
         nHit := HTRIGHT
      ENDIF
   ENDCASE

   RETURN nHit

//--------------------------------------------------------------------------//

METHOD InsItem( nPos, oItem ) CLASS PopUpMenu

   IF nPos > 0 .AND. nPos <= ::itemCount
      AIns( ::aItems, nPos, oItem, .T. )
      ::itemCount++

      AEval( ::aItems, ;
         {|oItem| ::width := Max( Len( StrTran( oItem:caption, "&", "" ) ) + 4, ::width ) } )

   ENDIF

   RETURN Self

//--------------------------------------------------------------------------//

METHOD IsOpen() CLASS PopUpMenu

   return ::opened

//--------------------------------------------------------------------------//

METHOD Open() CLASS PopUpMenu

   IF !::opened
      ::opened := TRUE
      If ::top >= ::bottom
         ::bottom := ::top + ::itemCount + 1
      End
      If ::left >= ::right
         ::right := ::left + ::width - 1
      End
      ::saveScr := SaveScreen( ::top, ::left, ::bottom, ::right )
      ::Display()
   ENDIF

   RETURN Self

//--------------------------------------------------------------------------//

METHOD _Select( nPos ) CLASS PopUpMenu

   IF ( nPos > 0 .AND. nPos <= ::itemCount ) .AND. ;
         ::current != nPos .AND. ::aItems[ nPos ]:enabled

      if ::opened .AND. ::current > 0
         if ::aItems[ ::current ]:isPopUp()
            ::aItems[ ::current ]:data:Close()
         ENDIF
      ENDIF

      ::current := nPos
   ENDIF

   RETURN Self

//--------------------------------------------------------------------------//

METHOD SetItem( nPos, oItem ) CLASS PopUpMenu

   IF nPos > 0 .AND. nPos <= ::itemCount
      ::aItems[ nPos ] := oItem
      ::width := Max( Len( StrTran( oItem:caption, "&", "" ) ) + 4, ::width )
   ENDIF

   RETURN Self

//--------------------------------------------------------------------------//

METHOD DISPLAY() CLASS PopUpMenu

   LOCAL nTop     := ::top
   LOCAL nAt
   LOCAL lPopup
   LOCAL cPrompt
   LOCAL oItems

   DispBegin()

   DEFAULT ::border  TO Space( 8 )

   DispBox( ::top, ::left, ::bottom, ::right, ;
      SubStr( ::border, 1, 8 ) + " ", ;
      hb_ColorIndex( ::colorSpec, 5 ) )

#ifdef HB_EXTENSION
   if ::shadowed
      hb_Shadow( ::top + 1, ::left + 1, ::bottom + 1, ::right + 1 )
   ENDIF
#endif

   FOR EACH oItems IN ::aItems

      nAt := At( "&", oItems:caption )
      cPrompt := StrTran( oItems:caption, "&", "" )

      IF cPrompt == MENU_SEPARATOR
         DispOutAt( ;
            oItems:row + nTop + HB_EnumIndex(), ::left, ;
            SubStr( ::border, 9, 1 ) + ;
            Replicate( SubStr( ::border, 10, 1 ), ::right - ::left - 1 ) + ;
            SubStr( ::border, 11, 1 ), ;
            hb_ColorIndex( ::colorspec, 5 ) )
      ELSE
         lPopUp := oItems:isPopUp()

         DispOutAt( ;
            oItems:row + nTop + HB_EnumIndex(), ::left + 1, ;
            iif( oItems:checked, SubStr( oItems:style, 1, 1 ), " " ) + ;
            PadR( cPrompt + " ", ::width - 4 ) + ;
            iif( lPopUp, SubStr( oItems:style, 2, 1 ), " " ), ;
            hb_ColorIndex( ::colorSpec, ;
            iif( oItems:enabled, ;
            iif( HB_EnumIndex() == ::current, CLR_ENHANCED, CLR_STANDARD ), ;
            CLR_UNSELECTED ) ) )

         IF nAt > 0
            DispOutAt( ;
               oItems:row + nTop + HB_EnumIndex(), ::left + nAt + 1, ;
               SubStr( cPrompt, nAt, 1 ), ;
               hb_ColorIndex( ::colorSpec, ;
               iif( oItems:enabled, ;
               iif( HB_EnumIndex() == ::current, CLR_BACKGROUND, CLR_BORDER ), ;
               CLR_UNSELECTED ) ) )
         ENDIF
      ENDIF
   NEXT

   DispEnd()

   RETURN Self

#ifdef HB_EXTENSION

//--------------------------------------------------------------------------//

METHOD SetCoors( nRow, nCol, lTop ) CLASS PopUpMenu

   LOCAL oItem, nDif

   if ::top == - 1 .OR. ::left == - 1
      ::top    := nRow
      ::left   := nCol
      ::bottom := ::top + ::itemCount + 1
      ::right  := ::left + ::width - 1

      if ::right > MaxCol()
         nDif    := ::right - MaxCol()
         ::right -= nDif
         ::left  -= nDif
         IF !lTop
            ::top++
            ::bottom++
         ENDIF
      ENDIF

      if ::left < 0
         nDif    := ::left
         ::right -= nDif
         ::left  -= nDif
      ENDIF

      if ::bottom > MaxRow()
         nDif     := ::bottom - MaxRow()
         ::bottom -= nDif
         ::top    -= nDif
      ENDIF

      if ::top < 0
         nDif     := ::top
         ::bottom -= nDif
         ::top    -= nDif
      ENDIF

      FOR EACH oItem in ::aItems
         IF oItem:isPopup()
            oItem:data:SetCoors( nRow + HB_EnumIndex(), ::right + 1, .F. )
         ENDIF
      NEXT


   ENDIF

   RETURN Self

#endif

//--------------------------------------------------------------------------//

   /* The routines below were added by Larry Sevilla <lsevilla@nddc.edu.ph> */
   /* based on the MenuSys.prg of Clipper 5.3b                              */

/***
*
*  IsShortCut( <oMenu>, <nKey>, <nID> ) -> .T. | .F.
*
*  ShortCut processing for initial Get or Menu Item.
*
***/

METHOD IsShortcut( nKey, nID ) CLASS PopUpMenu

   LOCAL nItem, nTotal, nShortCut, oItem, i

   DO CASE
      // Test and assign top menu item shortCut, enabled, and !PopUp:
      // Changed by enclosing assignment before ':Enabled':
   CASE ( ( nShortCut := ::GetShortCt( nKey ) ) > 0 ) .AND. ;
         ( ( oItem := ::GetItem( nShortcut ) ):Enabled ) .AND. ;
         ( !( oItem:IsPopUp() ) )
      ::Select( nShortCut )
      Eval( oItem:Data, oItem )
      nID := oItem:ID

      RETURN .T.

      // Test and assignment for TopBar MenuItem:
   CASE nShortCut == 0
      nTotal := ::ItemCount()
      nItem  := ::Current
      iif( nItem == 0, nItem := 1, )

      // Loop to wrap around through TopMenu from Current Item:
      FOR i := 1 TO nTotal
         IF !( oItem := ::GetItem( nItem ) ):Enabled
         ELSEIF !oItem:IsPopUp()
         ELSEIF oItem:Data:IsQuick( nKey, @nID )
            RETURN .T.
         ENDIF
         iif( ++nItem > nTotal, nItem := 1, )
      NEXT

   ENDCASE

   RETURN .F.

/***
*
*  IsQuick( <oMenu>, <nKey>, <nID> ) --> .T. | .F.
*
*  IsShortCut() for secondary ShortCut processing.
*  Navigates to the next Get or Menu Item from the
*  Current if more than one uses the same ShortCut.
*
***/

METHOD IsQuick( nKey, nID ) CLASS PopUpMenu

   LOCAL nItem, nTotal, nShortCut, oItem // , i

   IF ( nShortCut := ::GetShortCt( nKey ) ) == 0
      nTotal := ::ItemCount

      FOR nItem := 1 TO nTotal
         IF !( oItem := ::GetItem( nItem ) ):Enabled
         ELSEIF ! oItem:IsPopUp()
         ELSEIF oItem:Data:IsQuick( nKey, @nID )
            RETURN .T.
         ENDIF
      NEXT

   ELSEIF !( oItem := ::GetItem( nShortCut ) ):IsPopUp()
      IF oItem:Enabled
         ::Select( nShortCut )
         Eval( oItem:Data, oItem )
         nID := oItem:ID
         RETURN .T.
      ENDIF

   ENDIF

   RETURN .F.

#endif
