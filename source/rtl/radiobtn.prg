/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RADIOBUTTON class
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

CLASS HBRadioButton

   EXPORT:

   DATA Buffer
   DATA CapRow
   DATA CapCol
   DATA Caption
   DATA Cargo
   DATA Col
   DATA pData
   DATA ColorSpec
   DATA Classname init "RADIOBUTTO"
   DATA fBlock
   DATA HasFocus
   DATA Row
   DATA sBlock
   DATA Style

   METHOD SetData( xData )
   ACCESS DATA inline ::SetData()
   ASSIGN DATA( xData ) inline if( xData != NIL, ::SetData( xData ), )
   METHOD DISPLAY()
   METHOD HitTest( nrow, nCol )
   METHOD IsAccel( xVal )
   METHOD KillFocus()
   MESSAGE SELECT( lVal ) METHOD _Select( LVal )
   METHOD SetFocus()
   METHOD New( nRow, nCol, cCaption, xData )

ENDCLASS

METHOD New( nRow, nCol, cCaption, xData ) CLASS HBRadioButton

   LOCAL cColor

   ::Buffer := .F.
   ::CapRow := nRow
   ::CapCol :=  nCol + 3 + 1
   ::Caption := cCaption
   ::Cargo := NIL
   ::Col := nCol

   IF IsDefcolor()
      ::ColorSpec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N"
   ELSE
      cColor := SetColor()
      ::ColorSpec := __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
         __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
         __GUIColor( cColor, CLR_ENHANCED   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_ENHANCED   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_STANDARD   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_STANDARD   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_BACKGROUND + 1 )
   ENDIF

   ::fBlock := NIL

   ::HasFocus := .F.
   ::Row := nRow
   ::sBlock := nil

   ::Style := "(* )"
   ::Data := xData

   RETURN Self

METHOD SETFOCus()  CLASS HBRadioButton

   IF ( !::hasfocus .AND. ISBLOCK( ( ::hasfocus := .T., ::Display(), ::fblock ) ) )
      Eval( ::fblock )
   ENDIF

   RETURN Self

METHOD _SELECT( lStatus )  CLASS HBRadioButton

   LOCAL lOldBuffer := ::Buffer

   IF ISLOGICAL( lStatus )
      ::Buffer := lStatus
   ELSE
      ::Buffer := !::Buffer
   ENDIF

   IF lOldBuffer == ::Buffer
   ELSEIF ISBLOCK( ::sBlock )
      Eval( ::sBlock )
   ENDIF

   RETURN self

METHOD KillFocus()  CLASS HBRadioButton

   if ::HasFocus
      ::HasFocus := .F.
      IF ISBLOCK( ::fBlock )
         Eval( ::fBlock )
      ENDIF
      ::Display()
   ENDIF

   RETURN Self

METHOD DISPLAY()  CLASS HBRadioButton

   LOCAL cColor := SetColor(), cCurStyle, nCurRow := Row(), nCurCol := Col(), ;
      cPairs, nPos, cOldCaption

   if ::hasfocus
      cPairs := __GUIColor( ::colorspec, 7 )
   ELSE
      cPairs := __GUIColor( ::colorspec, 6 )
   ENDIF

   cCurStyle := ::Style
   DispBegin()

   if ::Buffer
      SET COLOR to ( __GUIColor( ::colorspec, 4 ) )
   ELSE
      SET COLOR to ( __GUIColor( ::colorspec, 2 ) )
   ENDIF

   SetPos( ::Row, ::Col )
   ?? Left( cCurStyle, 1 )

   if ::Buffer
      ?? SubStr( cCurStyle, 2, 1 )
   ELSE
      ?? SubStr( cCurStyle, 3, 1 )
   ENDIF

   ?? Right( cCurStyle, 1 )

   IF !Empty( cOldCaption := ::Caption )
      IF ( nPos := At( "&", cOldCaption ) ) == 0
      ELSEIF nPos == Len( cOldCaption )
         nPos := 0
      ELSE
         cOldCaption := Stuff( cOldCaption, nPos, 1, "" )
      ENDIF
      SET COLOR to ( __GUIColor( ::ColorSpec, 5 ) )
      SetPos( ::CapRow, ::CapCol )
      ?? cOldCaption
      IF nPos != 0
         SET COLOR to ( cPairs )
         SetPos( ::CapRow, ::CapCol + nPos - 1 )
         ?? SubStr( cOldCaption, nPos, 1 )
      ENDIF
   ENDIF
   DispEnd()
   SET COLOR to ( cColor )
   SetPos( nCurRow, nCurCol )

   RETURN Self

METHOD IsAccel( xValue )  CLASS HBRadioButton

   LOCAL nPos, cCaption, xResult

   IF ISNUMBER( xValue )
      xValue := Chr( xValue )
   ELSEIF !ISCHARACTER( xValue )
      RETURN .F.
   ENDIF

   xValue := Lower( xValue )
   cCaption := ::Caption

   IF ( nPos := At( "&", cCaption ) ) == 0
   ELSEIF ( ( xResult := Lower(SubStr(cCaption, nPos + 1, 1 ) ), nPos ;
         < Len( cCaption ) .AND. xResult == xValue ) )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD HitTest( nRow, nCol )  CLASS HBRadioButton

   LOCAL nPos, nLen

   IF nRow != ::Row
   ELSEIF nCol < ::Col
   ELSEIF nCol < ::Col + 3
      RETURN HTCLIENT
   ENDIF

   nLen := Len( ::Caption )

   IF ( nPos := At( "&", ::Caption ) ) == 0
   ELSEIF nPos < nLen
      nLen--
   ENDIF

   IF nRow != ::CapRow
   ELSEIF nCol < ::CapCol
   ELSEIF nCol < ::CapCol + nLen
      RETURN HTCLIENT
   ENDIF

   RETURN HTNOWHERE

METHOD SetData( Arg1 ) CLASS HBRadioButton

   IF PCount() == 0
   ELSEIF ISNIL( Arg1 )
      ::pData := Arg1
   ELSE
      ::pData := if( HB_ISSTRING( Arg1 ), arg1, "" )
   ENDIF

   IF ISNIL( ::pData )
      RETURN __Caption( ::Caption )
   ENDIF

   Return ::pData

FUNCTION RadioButto( nRow, nCol, cCaption, xData )

   DEFAULT cCaption TO ""

   IF ISNUMBER( nRow ) .AND. ISNUMBER( nCol )
      RETURN HBRadioButton():New( nRow, nCol, cCaption, xData )
   ENDIF

   RETURN nil

#ifdef HB_EXTENSION

FUNCTION RadioButton( nRow, nCol, cCaption, xData )

   DEFAULT cCaption TO ""

   IF ISNUMBER( nRow ) .AND. ISNUMBER( nCol )
      RETURN HBRadioButton():New( nRow, nCol, cCaption, xData )
   ENDIF

   RETURN nil

#endif

   /** Return the Caption Letter of an Given Caption String */

FUNCTION __Caption( cCaption )

   LOCAL  nPos

   IF ( nPos := At( "&", cCaption ) ) > 0
      cCaption := Stuff( cCaption, nPos, 1, "" )
   ENDIF

   RETURN cCaption

#endif
