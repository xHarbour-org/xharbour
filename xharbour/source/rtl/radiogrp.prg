/*
 * $Id: radiogrp.prg,v 1.5 2003/01/27 03:40:53 walito Exp $
 */

/*
 * Harbour Project source code:
 * RADIOGROUP class
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

#include "common.ch"
#include "hbclass.ch"
#include "hbsetup.ch"

#IFdef HB_COMPAT_C53
CLASS HBRadioGroup

   export:

   METHOD AddItem( xItem )
   METHOD DelItem( xItem )
   METHOD Display()
   METHOD GetAccel( xItem )
   METHOD GetItem( Xitem )
   METHOD HitTest( nRow, nCol )
   METHOD InsItem( nPos, oButtom )
   METHOD KillFocus( )
   METHOD NextItem( )
   METHOD PrevItem( )
   MESSAGE Select( xItem ) METHOD _Select( xItem )
   MESSAGE SetColor( xItem ) METHOD _SetColor( xItem )
   METHOD SetFocus( )
   METHOD SetStyle( xItem )
   METHOD New( nTop, nLeft, nBottom, nRight )
//   METHOD GetColor( xColor )
   DATA Bottom

   DATA Buffer INIT  NIL

   DATA CapCol

   DATA CapRow

   DATA Caption 

   DATA Cargo  INIT  NIL

   DATA ColdBox  INIT  "ÚÄ¿³ÙÄÀ³"

   DATA fBlock  INIT  NIL

   DATA HasFocus  INIT  .F.

   DATA HotBox INIT  "ÉÍ»º¼ÍÈº"

   DATA ItemCount INIT 0

   DATA Left 

   DATA Message INIT ""

   DATA Right
   DATA aItems INIT {}
   DATA lCursor INIT 0

   DATA TextValue INIT ""

   DATA Top
   DATA CLASSName INIT "RADIOGROUP"
   DATA TypeOut INIT .F.

   DATA Value INIT 0
   DATA Color
   Data colorspec INIT ""
//   ASSIGN Colorspec( xColor ) inline IIF( xColor != NIL, ::GetColor( xColor ), )

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS HBRadioGroup

Local cColor

      IF ( isdefcolor() )
          ::ColorSpec := "W/N,W/N,W+/N"
      ELSE
         cColor := SetColor()
         ::ColorSpec :=  __guicolor( cColor, 3 ) + "," + ;
            __guicolor( cColor, 1 ) + "," + __guicolor( cColor, 4 )
      ENDIF

      ::Bottom := nBottom
      ::CapCol := nLeft+2
      ::CapRow := nTop
      ::Left := nLeft
      ::right := nRight
      ::top := nTop
RETURN Self

METHOD  ADDITEM( xItem ) CLASS HBRadioGroup


   IF ( !( ISOBJECT( xItem ) ) )
   ELSEIF ( xItem:classname() == "RADIOBUTTO" )
      Aadd( ::aItems, xItem )
      ::ItemCount++
   ENDIF
   RETURN Self

METHOD  SetStyle( xStyle ) CLASS HBRadioGroup
   
   Local oItems

   FOR EACH oItems IN ::aItems
      oItems:style( xStyle )
   NEXT

   RETURN Self

METHOD  SetFocus() CLASS HBRadioGroup

   Local oItems

   IF  !::HasFocus
      ::lCursor  := setcursor( 0 )
      ::HasFocus := .T.

      dispbegin()

      FOR EACH oItems IN ::aItems
         oItems:SetFocus()
      NEXT

      ::display()

      dispend()

      IF ISBLOCK( ::fBlock )
         Eval( ::fBlock )
      ENDIF

   ENDIF

   RETURN self

METHOD  _SetColor( cColor ) CLASS HBRadioGroup

   Local oItems

   FOR EACH oItems IN ::aItems
      oItems:ColorSpec := cColor
   NEXT

   RETURN Self

METHOD  _Select( xValue ) CLASS HBRadioGroup

   Local nPos //,nLen := ::ItemCount
   Local cType := ValType( xValue )
   Local oItems

   IF cType == "C"

      FOR EACH oItems IN ::aItems

         IF oItems:data == xValue
            Default ::Buffer TO ""
            nPos := HB_EnumIndex()
            changebutt( Self, ::Value, nPos )

            exit
         ENDIF

      NEXT

      IF nPos < 0
         ::Buffer := xValue
      ENDIF

   ELSE
      IF !( cType != "U" .AND. xValue < 1 ) .AND.;
           ( cType != "U" .AND. xValue <= ::ItemCount )
         default ::Buffer to 0
         changebutt( self, ::Value, xValue )
      ENDIF
   ENDIF

   RETURN Self

METHOD  PrevItem()        CLASS HBRadioGroup

   local xValue := ::Value, nPos

   IF ::HasFocus .AND. ::ItemCount > 0

      Switch xValue
      CASE 0
         nPos := 1
         exit
      CASE 1
         nPos := ::ItemCount
         exit
      Default
         nPos := xValue - 1
      End

      changebutt( self, xValue, nPos )

   ENDIF

   RETURN self

METHOD  NextItem()        CLASS HBRadioGroup


   local xValue, nPos

   IF ::HasFocus .AND. ::ItemCount > 0

      IF ( ( xValue := ::Value ) == ::ItemCount )
         nPos := 1
      ELSE
         nPos := xValue + 1
      ENDIF

      changebutt( self, xValue, nPos )

   ENDIF
   RETURN Self

METHOD  KillFocus()       CLASS HBRadioGroup

   local nPos, nCount, aItems
   Local oItems

   IF ::HasFocus

      ::HasFocus := .F.
      IF ( ISBLOCK( ::fBlock ) )
         Eval( ::fBlock )
      ENDIF

      dispbegin()

      FOR EACH oItems IN ::aItems
         oItems:KillFocus()
      NEXT

      ::display()

      dispend()

      setcursor(::lCursor )

   ENDIF

   RETURN self

METHOD  InsItem( nPos, oButtom ) CLASS HBRadioGroup


   IF ( !( ISOBJECT( oButtom ) ) )
   ELSEIF ( !( oButtom:classname() == "RADIOBUTTN" ) )
   ELSEIF ( nPos < ::ItemCount )
//      asize(::aItems, ++::ItemCount )
      ains(::aItems, nPos, oButtom, .T. )
//      ::aItems[ nPos ] := oButtom
   ENDIF
   RETURN ::aItems[ nPos ]

METHOD  HitTest( nRow, nCol )    CLASS HBRadioGroup

   Local nLen, nPosition
   Local oItems

   do CASE
   CASE Empty( ::Coldbox + ::HotBox )
   CASE nRow == ::Top
      IF ( nCol == ::Left )
         RETURN -1
      ELSEIF ( nCol == ::Right )
         RETURN -3
      ELSEIF ( nCol >= ::Left .AND. nCol <= ::Right )
         RETURN -2
      ENDIF
   CASE nRow == ::Bottom
      IF ( nCol == ::Left )
         RETURN -7
      ELSEIF ( nCol == ::Right )
         RETURN -5
      ELSEIF ( nCol >= ::Left .AND. nCol <= ::Right )
         RETURN -6
      ENDIF
   CASE nCol == ::Left
      IF ( nRow >= ::Top .AND. nRow <= ::Bottom )
         RETURN -8
      ELSE
         RETURN 0
      ENDIF
   CASE nCol == ::Right
      IF ( nRow >= ::Top .AND. nRow <= ::Bottom )
         RETURN -4
      ELSE
         RETURN 0
      ENDIF
   ENDCASE

   nLen := Len( ::Caption )

   IF ( ( nPosition := At( "&", ::Caption ) ) == 0 )
   ELSEIF ( nPosition < nLen )
      nLen--
   ENDIF

   do CASE
   CASE Empty( ::Caption )
   CASE nRow != ::CapRow
   CASE nCol < ::CapCol
   CASE nCol < ::CapCol + nLen
      RETURN -1025
   ENDCASE

   do CASE
   CASE nRow < ::Top
   CASE nRow > ::Bottom
   CASE nCol < ::Left
   CASE nCol <= ::Right
//      nPos := 1
      FOR EACH oItems IN ::aItems
         IF oItems:HitTest( nRow, nCol ) != 0
            RETURN HB_EnumIndex()
         ENDIF
//         nPos++
      NEXT
      RETURN -2049
   ENDCASE

   RETURN 0

METHOD  GetItem( xValue )        CLASS HBRadioGroup
   local xReturn := NIL

   IF xValue >= 1 .AND. xValue <= ::ItemCount
      xReturn := ::aItems[ xValue ]
   ENDIF

   RETURN xReturn

METHOD  GetAccel( xValue )       CLASS HBRadioGroup

//   Local nPos
   Local oItems

   Do CASE
   CASE ISNUMBER( xValue )
      xValue := Chr( xValue )
   CASE !ISCHARACTER( xValue )
      RETURN 0
   ENDCASE

   xValue := Lower( xValue )

//   nPos := 1
   FOR EACH oItems IN ::aItems
      IF oItems:IsAccel( xValue )
         RETURN HB_EnumIndex()
      ENDIF
//      nPos++
   NEXT

   RETURN 0

METHOD  Display() CLASS HBRadioGroup

   Local cColor := SetColor(), ;
         nCurRow := Row(), nCurCol := Col(), cSelBox, ;
         cUnSelBox, cCaption, nPosition
   Local oItems

   dispbegin()

   IF ::HasFocus
      cSelBox := ::HotBox
      cUnSelBox := ::Coldbox
   ELSE
      cSelBox := ::Coldbox
      cUnSelBox := ::HotBox
   ENDIF

   set color to ( __guicolor( ::ColorSpec, 1 ) )

   IF !Empty( cSelBox )
      @ ::Top, ::Left, ::Bottom, ::Right ;
         box cSelBox
   ELSEIF ( !Empty( cUnSelBox ) )
      @ ::Top, ::Left, ::Bottom, ::Right ;
         box cUnSelBox
   ENDIF

   IF !Empty( cCaption := ::Caption )

      IF !( ( nPosition := At( "&", cCaption ) ) == 0 )
         IF nPosition == Len( cCaption )
            nPosition := 0
         ELSE
            cCaption := stuff( cCaption, nPosition, 1, "" )
         ENDIF
      ENDIF

      set color to ( __guicolor( ::ColorSpec, 2 ) )
      SetPos( ::CapRow, ::CapCol )
      ?? cCaption

      IF nPosition != 0
         set color to ( __guicolor( ::ColorSpec, 3 ) )
         SetPos( ::CapRow, ::CapCol + nPosition - 1 )
         ?? SubStr( cCaption, nPosition, 1 )
      ENDIF

   ENDIF

   FOR EACH oItems IN ::aItems
      oItems:Display()
   NEXT

   dispend()

   set color to ( cColor )
   SetPos( nCurRow, nCurCol )

   RETURN self

METHOD  DelItem( xItem ) CLASS HBRadioGroup

   IF xItem >= 1 .AND. xItem <= ::ItemCount
      Adel( ::aItems[ xItem ], .T. )
//      asize(::aItems, --::ItemCount )
      ::ItemCount--
   ENDIF

   IF ::HasFocus .AND. ::ItemCount < ::Value

      ::Value := ::ItemCount
      ::TextValue := ::aItems[ ::Value ]:data

      IF ISNUMBER( ::Buffer )
         ::Buffer := ::Value
      ELSE
         ::Buffer := ::TextValue
      ENDIF

   ENDIF

   RETURN self

/*METHOD GetColor(xColor )  CLASS HBRadioGroup
   IF ( !( ISNIL( xColor ) ) )
      ::Color := IIF( Valtype(xColor )=="C" .and. !Empty( __guicolor( xColor, 3 ) ) .AND. ;
      Empty( __guicolor( xColor, 4 ) ), xColor, )

   ENDIF
   RETURN ::Color
*/

static function  ChangeButt( oItems, xVal, nPos )

   IF xVal != nPos

      dispbegin()

      IF xVal > 0
         oItems:aItems[ xVal ]:select( .F. )
         oItems:aItems[ xVal ]:display()
      ENDIF

      IF nPos > 0
         oItems:aItems[ nPos ]:select( .T. )
         oItems:aItems[ nPos ]:display()
      ENDIF

      dispend()

      oItems:Value := nPos
      oItems:TextValue := oItems:aItems[ nPos ]:data

      IF ISNUMBER( oItems:Buffer )
         oItems:Buffer := nPos
      ELSE
         oItems:Buffer := oItems:TextValue
      ENDIF

   ENDIF

   RETURN .T.

// Radio Group Class Constructor Function

function RadioGroup( nTop, nLeft, nBottom, nRight )

   IF ISNUMBER( nTop ) .and. ISNUMBER( nLeft ) .and.;
        ISNUMBER( nBottom ) .and. ISNUMBER( nright )
      RETURN HBRadioGroup():New( nTop, nLeft, nBottom, nRight )
   ENDIF

   RETURN NIL


function _RADIOGRP_( nTop, nLeft, nBottom, nRight, xValue, aItems, cCaption, cMessage, ;
   cColor, bFblock )

   Local oRadioGroup
   Local xItems

   default ccaption to ""

   oRadioGroup := RadioGroup( nTop, nLeft, nBottom, nRight )

   IF ! ISNIL( oRadioGroup )

      oRadioGroup:caption :=  IIF( cCaption != NIL, cCaption, )
      oRadioGroup:colorspec := IIF( cColor != NIL, cColor, )
      oRadioGroup:message := IIF( cMessage != NIL, cMessage, )
      oRadioGroup:fblock := IIF( bFblock != NIL, bFblock, )

      FOR EACH xItems IN aItems
         oRadioGroup:AddItem( xItems )
      NEXT

      oRadioGroup:select( xValue )

   ENDIF

   RETURN oRadioGroup

#ENDIF
