/*
 * $Id: tpopup.prg,v 1.5 2003/01/31 21:03:35 walito Exp $
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

#include "box.ch"
#include "button.ch"
#include "color.ch"
#include "common.ch"
#include "hbsetup.ch"
#include "hbclass.ch"

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
function PopUp( nTop, nLeft, nBottom, nRight )

   LOCAL oPopUp := PopUpMenu():New( nTop, nLeft, nBottom, nRight )

return oPopUp

//--------------------------------------------------------------------------//
CLASS PopUpMenu

   DATA ClassName       init    "POPUPMENU"
   DATA aItems          init    {}
   DATA border          init    B_SINGLE + SEPARATOR_SINGLE
   DATA bottom
   DATA cargo
   DATA colorSpec       init    "N/W,W/N,W+/W,W+/N,N+/W,W/N"
   DATA current         init    0
   DATA itemCount       init    0
   DATA left
   DATA opened          init    FALSE PROTECTED
   DATA right
   DATA saveScr         init    ""    PROTECTED
   DATA top
   DATA width           init    0

#ifdef HB_EXTENSION
   DATA shadowed        init    FALSE
#endif

   METHOD New( nTop, nLeft, nBottom, nRight )
   METHOD AddItem( oItem )
   METHOD Close( lClose )
   METHOD DelItem( nPos )
   METHOD Display()
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
   MESSAGE Select( nPos )       METHOD _Select( nPos )
   METHOD SetItem( nPos, oItem )

   /* NOTE: This method is new in Harbour */
#ifdef HB_EXTENSION
   METHOD SetCoors( nRow, nCol, lTop )
#endif

   METHOD IsShortCut( nKey, nID )
   METHOD IsQuick( nKey, nID )

ENDCLASS

//--------------------------------------------------------------------------//
METHOD New( nTop, nLeft, nBottom, nRight ) CLASS PopUpMenu

   /* NOTE: When a PopUp is created and attached to a TopBar object, its
            coords are initialized to -1, so the TopBar can update them
            accordingly to its own position on to the screen. [jlalin]
   */
   DEFAULT nTop    TO -1
   DEFAULT nLeft   TO -1
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

return Self

//--------------------------------------------------------------------------//
METHOD AddItem( oItem ) CLASS PopUpMenu

   LOCAL nLen

   aAdd( ::aItems, oItem )
   ::itemCount++

   nLen := Len( StrTran( oItem:caption, "&", "" ) )
   ::width := Max( nLen + 4, ::width ) // 4 is for box margins

return Self

//--------------------------------------------------------------------------//
METHOD Close( lClose ) CLASS PopUpMenu

   DEFAULT lClose TO TRUE

   if ::opened
      if lClose
         if ::current > 0
            if ::aItems[ ::current ]:isPopUp()
               ::aItems[ ::current ]:data:Close( lClose )
            endif
         endif
      endif
      ::current := 0
      ::opened := FALSE
      RestScreen( ::top, ::left, ::bottom, ::right, ::saveScr )
      ::saveScr := nil
   endif

return Self

//--------------------------------------------------------------------------//
METHOD DelItem( nPos ) CLASS PopUpMenu

   if nPos > 0 .and. nPos <= ::itemCount
      aDel( ::aItems, nPos, .T. )
      ::itemCount--

      aEval( ::aItems, ;
         {|oItem| ::width := Max( Len( StrTran( oItem:caption, "&", "" ) ) + 4, ::width ) } )

   endif

return Self

//--------------------------------------------------------------------------//
/* NOTE: This method corrects two bugs in Cl*pper:
         1) when two menuitems have the same key and the
            first item is disabled
         2) when a menuitem is disabled it will ignore the key [jlalin]
*/
METHOD GetAccel( nKey ) CLASS PopUpMenu

   LOCAL nAt   := 0
   LOCAL cKey  := Upper( Chr( nKey ) )
   LOCAL oItems

   FOR EACH oItems IN ::aItems
      nAt := At( "&", oItems:caption )
      if nAt > 0 .and. oItems:enabled .and. Upper( SubStr( oItems:caption, nAt + 1, 1 ) ) == cKey
         return HB_EnumIndex()
      endif
   NEXT

return 0

//--------------------------------------------------------------------------//
METHOD GetFirst() CLASS PopUpMenu

   LOCAL oItems

   FOR EACH oItems IN ::aItems
      if oItems:caption != MENU_SEPARATOR .and. oItems:enabled
         return HB_EnumIndex()
      endif
   NEXT

return 0

//--------------------------------------------------------------------------//
METHOD GetItem( nPos ) CLASS PopUpMenu

   LOCAL oItem

   if nPos > 0 .and. nPos <= ::itemCount
      oItem := ::aItems[ nPos ]
   endif

return oItem

//--------------------------------------------------------------------------//
METHOD GetLast() CLASS PopUpMenu

   LOCAL n

   for n := ::itemCount to 1 step -1
      if ::aItems[ n ]:caption != MENU_SEPARATOR .and. ::aItems[ n ]:enabled
         return n
      endif
   next

return 0

//--------------------------------------------------------------------------//
METHOD GetNext() CLASS PopUpMenu

   LOCAL n

   if ::current < ::itemCount
      for n := ::current + 1 to ::itemCount
         if ::aItems[ n ]:caption != MENU_SEPARATOR .and. ::aItems[ n ]:enabled
            return n
         endif
      next
  endif

return 0

//--------------------------------------------------------------------------//
METHOD GetPrev() CLASS PopUpMenu

   LOCAL n

   if ::current > 1
      for n := ::current - 1 to 1 step -1
         if ::aItems[ n ]:caption != MENU_SEPARATOR .and. ::aItems[ n ]:enabled
            return n
         endif
      next
  endif

return 0

//--------------------------------------------------------------------------//
/* NOTE: This method corrects a bug in Cl*pper:
         1) when a menuitem is disabled it will ignore the key [jlalin]
*/
METHOD GetShortct( nKey ) CLASS PopUpMenu

   LOCAL oItems

   FOR EACH oItems IN ::aItems
      if oItems:enabled .and. oItems:shortcut == nKey
         return HB_EnumIndex()
      endif
   NEXT

return 0

//--------------------------------------------------------------------------//
/* NOTE: This method corrects two bugs in Cl*pper:
         1) when two menuitems have the same key and the first item
            is disabled
         2) when a menuitem is disabled it will ignore the click [jlalin]
*/
METHOD HitTest( nRow, nCol ) CLASS PopUpMenu

   LOCAL nHit  := HTNOWHERE

   do case
      case nRow == ::top
         if nCol == ::left
            nHit := HTTOPLEFT
         elseif nCol == ::right
            nHit := HTTOPRIGHT
         else
            nHit := HTTOP
         endif
      case nRow == ::bottom
         if nCol == ::left
            nHit := HTBOTTOMLEFT
         elseif nCol == ::right
            nHit := HTBOTTOMRIGHT
         else
            nHit := HTBOTTOM
         endif
      case nRow > ::top .and. nCol > ::left .and. nRow < ::bottom .and. nCol < ::right
         if ::aItems[ nRow - ::top ]:enabled .and. ::aItems[ nRow - ::top ]:caption != MENU_SEPARATOR
            nHit := nRow - ::top
         else
            nHit := HTSEPARATOR
         endif
      case nRow > ::top .and. nRow < ::bottom
         if nCol == ::left
            nHit := HTLEFT
         elseif nCol == ::right
            nHit := HTRIGHT
         endif
   endcase

return nHit

//--------------------------------------------------------------------------//
METHOD InsItem( nPos, oItem ) CLASS PopUpMenu

   if nPos > 0 .and. nPos <= ::itemCount
      aIns( ::aItems, nPos, oItem, .T. )
      ::itemCount++

      aEval( ::aItems, ;
         {|oItem| ::width := Max( Len( StrTran( oItem:caption, "&", "" ) ) + 4, ::width ) } )

   endif

return Self

//--------------------------------------------------------------------------//
METHOD IsOpen() CLASS PopUpMenu

return ::opened

//--------------------------------------------------------------------------//
METHOD Open() CLASS PopUpMenu

   if !::opened
      ::opened := TRUE
      ::saveScr := SaveScreen( ::top, ::left, ::bottom, ::right )
      ::Display()
   endif

return Self

//--------------------------------------------------------------------------//
METHOD _Select( nPos ) CLASS PopUpMenu

   if ( nPos > 0 .and. nPos <= ::itemCount ) .and. ;
         ::current != nPos .and. ::aItems[ nPos ]:enabled

      if ::opened .and. ::current > 0
         if ::aItems[ ::current ]:isPopUp()
            ::aItems[ ::current ]:data:Close()
         endif
      endif

      ::current := nPos
   endif

return Self

//--------------------------------------------------------------------------//
METHOD SetItem( nPos, oItem ) CLASS PopUpMenu

   if nPos > 0 .and. nPos <= ::itemCount
      ::aItems[ nPos ] := oItem
      ::width := Max( Len( StrTran( oItem:caption, "&", "" ) ) + 4, ::width )
   endif

return Self
//--------------------------------------------------------------------------//

METHOD Display() CLASS PopUpMenu

   LOCAL nTop     := ::top
   LOCAL nAt      := 0
   LOCAL lPopup   := FALSE
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
   endif
#endif

   FOR EACH oItems IN ::aItems

      nAt := At( "&", oItems:caption )
      cPrompt := StrTran( oItems:caption, "&", "" )

      if cPrompt == MENU_SEPARATOR
         DispOutAt( ;
            oItems:row + nTop + HB_EnumIndex(), ::left, ;
            SubStr( ::border, 9, 1 ) + ;
            Replicate( SubStr( ::border, 10, 1 ), ::right - ::left - 1 ) + ;
            SubStr( ::border, 11, 1 ), ;
            hb_ColorIndex( ::colorspec, 5 ) )
      else
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

         if nAt > 0
            DispOutAt( ;
               oItems:row + nTop + HB_EnumIndex(), ::left + nAt + 1, ;
               SubStr( cPrompt, nAt, 1 ), ;
               hb_ColorIndex( ::colorSpec, ;
                  iif( oItems:enabled, ;
                     iif( HB_EnumIndex() == ::current, CLR_BACKGROUND, CLR_BORDER ), ;
                     CLR_UNSELECTED ) ) )
         endif
      endif
   next

   DispEnd()

return Self

#ifdef HB_EXTENSION
//--------------------------------------------------------------------------//
METHOD SetCoors( nRow, nCol, lTop ) CLASS PopUpMenu
   Local oItem, nDif

   if ::top == -1 .or. ::left == -1
      ::top    := nRow
      ::left   := nCol
      ::bottom := ::top + ::itemCount + 1
      ::right  := ::left + ::width - 1

      if ::right > maxcol()
         nDif    := ::right - maxcol()
         ::right -= nDif
         ::left  -= nDif
         if !lTop
            ::top++
         endif
      endif

      if ::left < 0
         nDif    := ::left
         ::right -= nDif
         ::left  -= nDif
      endif

      if ::bottom > maxrow()
         nDif     := ::bottom - maxrow()
         ::bottom -= nDif
         ::top    -= nDif
      endif

      if ::top < 0
         nDif     := ::top
         ::bottom -= nDif
         ::top    -= nDif
      endif

      for each oItem in ::aItems
         if oItem:isPopup()
            oItem:data:SetCoors( nRow + HB_EnumIndex(), ::right + 1, .f. )
         endif
      next


   endif

return Self
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
METHOD IsShortCut( nKey, nID ) CLASS PopUpMenu

   LOCAL nItem, nTotal, nShortCut, oItem, i

  do case
   // Test and assign top menu item shortCut, enabled, and !PopUp:
   // Changed by enclosing assignment before ':Enabled':
   case   ( ( nShortCut := ::GetShortCt( nKey ) ) > 0 ) .AND. ;
            ( ( oItem := ::GetItem( nShortcut ) ):Enabled ) .AND. ;
            ( !( oItem:IsPopUp() ) )
      ::Select( nShortCut )
      EVAL( oItem:Data, oItem )
      nID := oItem:ID
      RETURN ( .T. )

   // Test and assignment for TopBar MenuItem:
   case   nShortCut == 0
      nTotal := ::ItemCount()
      nItem  := ::Current
      IIF( nItem == 0, nItem := 1, )

      // Loop to wrap around through TopMenu from Current Item:
      FOR i := 1 TO nTotal
         IF ( !( oItem := ::GetItem( nItem ) ):Enabled )
         ELSEIF ( !oItem:IsPopUp() )
         ELSEIF ( oItem:Data:IsQuick( nKey, @nID ) )
            RETURN ( .T. )
         ENDIF
         IIF( ++nItem > nTotal, nItem := 1, )
      NEXT

   ENDcase

   RETURN ( .F. )

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

   IF ( ( nShortCut := ::GetShortCt( nKey ) ) == 0 )
      nTotal := ::ItemCount

      FOR nItem := 1 TO nTotal
         IF ( !( oItem := ::GetItem( nItem ) ):Enabled )
	 ELSEIF ( !( oItem:IsPopUp() ) )
         ELSEIF ( oItem:Data:IsQuick( nKey, @nID ) )
	    RETURN ( .T. )
	 ENDIF
      NEXT

   ELSEIF ( !( oItem := ::GetItem( nShortCut ) ):IsPopUp() )
      IF oItem:Enabled
         ::Select( nShortCut )
	 EVAL( oItem:Data, oItem )
	 nID := oItem:ID
	 RETURN ( .T. )
      ENDIF

   ENDIF

   RETURN ( .F. )

#endif