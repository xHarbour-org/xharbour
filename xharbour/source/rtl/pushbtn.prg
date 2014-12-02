/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PUSHBUTTON class
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

#include 'hbclass.ch'
#include "color.ch"
#include "common.ch"
#include "button.ch"

#ifdef HB_COMPAT_C53

CLASS HBPushButton

   EXPORT:

   DATA ClassName INIT "PUSHBUTTON"
   DATA Buffer
   DATA Caption
   DATA Cargo
   DATA Col
   DATA fBlock
   DATA HasFocus
   DATA MESSAGE
   DATA Row
   DATA sBlock
   DATA TypeOut INIT .F.

   METHOD DISPLAY()
   METHOD HitTest( nRow, nCol )
   METHOD KillFocus()
   MESSAGE SELECT() METHOD _Select()
   METHOD SetFocus()
   METHOD New( nRow, nCol, cCaption )
   ACCESS ColorSpec INLINE ::GetColor()
   ASSIGN ColorSpec( xColor ) INLINE iif( xColor != Nil, ::GetColor( xColor ), )
   ACCESS Style INLINE ::GetStyle()
   ASSIGN Style( cStyle ) INLINE iif( cStyle != Nil, ::GetStyle( cStyle ), )

   Hidden:

   DATA CurStyle
   DATA COLOR
   DATA lCursor
   METHOD Getcolor( xColor )
   METHOD GetStyle( xStyle )

ENDCLASS

METHOD GetColor( xColor ) CLASS HBPushButton

   IF ! ISNIL( xColor )
      ::Color := iif( HB_ISSTRING( xColor ) .AND. ;
                      !Empty( __GUIColor( xColor, 4 ) ) .AND. ;
                      Empty( __GUIColor( xColor, 6 ) ), xColor, )

   ENDIF

   RETURN ::Color

METHOD GetStyle( cStyle ) CLASS HBPushButton

   IF ! ISNIL( cStyle )
      ::curStyle := iif( HB_ISSTRING( cStyle ) .AND. ;
                         LTrim( Str( Len( cStyle ) ) ) IN "0�2�8", cStyle, )
   ENDIF

   RETURN ::curStyle

METHOD New( nRow, nCol, cCaption ) CLASS HBPushButton

   LOCAL cColor

   DEFAULT cCaption TO ""

   ::Buffer   := .F.
   ::Caption  := cCaption
   ::Cargo    := Nil
   ::Col      := nCol
   ::fBlock   := Nil
   ::sBlock   := Nil
   ::HasFocus := .F.
   ::Message  := ""
   ::Row      := nRow
   ::lCursor  := Nil
   ::Style    := "<>"

   IF IsDefcolor()
      ::ColorSpec := "W/N,N/W,W+/N,W+/N"
   ELSE
      cColor      := SetColor()
      ::ColorSpec := __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
                     __GUIColor( cColor, CLR_ENHANCED   + 1 ) + "," + ;
                     __GUIColor( cColor, CLR_STANDARD   + 1 ) + "," + ;
                     __GUIColor( cColor, CLR_BACKGROUND + 1 )
   ENDIF

   RETURN Self

METHOD SetFocus() CLASS HBPushButton

   IF ( !::HasFocus .AND. ;
   	ISBLOCK( ( ::lCursor := SetCursor( 0 ), ::HasFocus := .T., ::display(), ::fBlock ) ) )
      Eval( ::fBlock )
   ENDIF

   RETURN Self

METHOD _Select( nPos ) CLASS HBPushButton

   LOCAL nCurPos := nPos

   IF ::HasFocus
      ::Buffer := .T.
      ::display()

      IF ISNUMBER( nPos )

         IF nPos == 32
            Inkey( 0.4 )

            DO WHILE nCurPos == 32
               nCurPos := Inkey( 0.1 )
            ENDDO

         ELSE

            DO WHILE nPos == Inkey( 0 )
            ENDDO

         ENDIF

      ENDIF

      IF ISBLOCK( ::sBlock )
         Eval( ::sBlock )
      ENDIF

      ::Buffer := .F.
      ::display()
   ENDIF

   RETURN Self

METHOD KillFocus() CLASS HBPushButton

   IF ::HasFocus

      ::HasFocus := .F.

      IF ISBLOCK( ::fBlock )
         Eval( ::fBlock )
      ENDIF

      ::display()
      SetCursor( ::lCursor )
   ENDIF

   RETURN Self

METHOD HitTest( nRow, nCol ) CLASS HBPushButton

   LOCAL nCurrentPos := 1
   LOCAL nLen        := Len( ::Caption )
   LOCAL cStyle
   LOCAL nAmpPos

   IF ( nAmpPos := At( "&", ::Caption ) ) == 0
   ELSEIF nAmpPos < nLen
      nLen --
   ENDIF

   IF ( cStyle := Len( ::Style ) ) == 2
      nLen += 2
   ELSEIF cStyle == 8
      nCurrentPos := 3
      nLen        += 2
   ENDIF

   DO CASE
   CASE nRow < ::Row
   CASE nCol < ::Col
   CASE nRow >= ::Row + nCurrentPos
   CASE nCol < ::Col + nLen
      RETURN HTCLIENT
   ENDCASE

   RETURN HTNOWHERE

METHOD DISPLAY() CLASS HBPushButton

   LOCAL cOldColor := SetColor()
   LOCAL cStyle
   LOCAL nCurCol
   LOCAL cCaption
   LOCAL nRow := Row()
   LOCAL nCol := Col()
   LOCAL nCurRow
   LOCAL nAmpPos
   LOCAL cColor4

   cStyle := ::Style

   DispBegin()

   IF ::Buffer
      SET COLOR TO ( __GUIColor( ::ColorSpec, 3 ) )
      cColor4 := __GUIColor( ::ColorSpec, 4 )

   ELSEIF ::HasFocus
      SET COLOR TO ( __GUIColor( ::ColorSpec, 2 ) )
      cColor4 := __GUIColor( ::ColorSpec, 4 )

   ELSE
      SET COLOR TO ( __GUIColor( ::ColorSpec, 1 ) )
      cColor4 := __GUIColor( ::ColorSpec, 4 )
   ENDIF

   nCurRow  := ::Row
   nCurCol  := ::Col
   cCaption := ::Caption

   IF ( nAmpPos := At( "&", cCaption ) ) == 0
   ELSEIF nAmpPos == Len( cCaption )
      nAmpPos := 0
   ELSE
      cCaption := Stuff( cCaption, nAmpPos, 1, "" )
   ENDIF

   IF !Empty( cStyle )
      nCurCol ++

      IF Len( cStyle ) == 2
         SetPos( ::Row, ::Col )
         ?? SubStr( cStyle, 1, 1 )
         SetPos( ::Row, ::Col + Len( cCaption ) + 1 )
         ?? SubStr( cStyle, 2, 1 )
      ELSE
         nCurRow ++
         DispBox( ::Row, ::Col, ::Row + 2, ::Col + Len( cCaption ) + 1, cStyle )
      ENDIF

   ENDIF

   IF !Empty( cCaption )

      SetPos( nCurRow, nCurCol )
      ?? cCaption

      IF nAmpPos != 0
         SET COLOR TO ( cColor4 )
         SetPos( nCurRow, nCurCol + nAmpPos - 1 )
         ?? SubStr( cCaption, nAmpPos, 1 )
      ENDIF

   ENDIF

   DispEnd()

   SET COLOR TO ( cOldColor )
   SetPos( nRow, nCol )

   RETURN Self

FUNCTION PushButton( nRow, nCol, cCaption )

   IF ISNUMBER( nRow ) .AND. ISNUMBER( nCol )
      DEFAULT cCaption TO ""
      RETURN HBPushButton():New( nRow, nCol, cCaption )
   ENDIF

   RETURN Nil

FUNCTION _PUSHBUTT_( cCaption, cMessage, cColor, bFBlock, bSBlock, cStyle )

   LOCAL oPushButton

   DEFAULT cCaption TO ""

   oPushButton := PushButton( Row(), Col(), cCaption )

   IF ! ISNIL( oPushButton )
      oPushButton:Caption   := cCaption
      oPushButton:ColorSpec := cColor
      oPushButton:Message   := cMessage
      oPushButton:Style     := cStyle
      oPushButton:fBlock    := bFBlock
      oPushButton:sBlock    := bSBlock
   ENDIF

   RETURN oPushButton

FUNCTION _GETNUMCOL( Arg1 )

   LOCAL aColors := { { "N+",  8 }, { "B+" ,  9 }, { "G+" , 10 }, { "BG+", 11 }, ;
                      { "R+", 12 }, { "RB+", 13 }, { "GR+", 14 }, { "W+" , 15 }, ;
                      { "BG",  3 }, { "RB" ,  5 }, { "GR" ,  6 }, { "B"  ,  1 }, ;
                      { "G" ,  2 }, { "R"  ,  4 }, { "W"  ,  7 } }
   LOCAL nPos := At( "/", Arg1 )
   LOCAL nReturn

   IF nPos > 1
      Arg1 := SubStr( Arg1, 1, nPos - 1 )
   ELSEIF nPos == 1
      Arg1 := ""
   ENDIF

   nReturn := AScan( aColors, { | a| a[ 1 ] == arg1 } )

   IF nReturn > 0
      RETURN aColors[ nReturn, 2 ]
   ENDIF

   RETURN 0

#endif
