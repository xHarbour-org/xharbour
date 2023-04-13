/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CHECKBOX class
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

CLASS HBCHECKBOX

   DATA Buffer INIT .F.
   DATA Caption
   DATA CapRow
   DATA CapCol
   DATA Cargo
   DATA Col
   DATA colorspec
   DATA FBlock
   DATA HasFocus INIT .F.
   DATA MESSAGE INIT ""
   DATA Row
   DATA SBlock
   DATA Style INIT "[û ]"
   DATA lCursor
   DATA Typeout INIT .F.
   DATA ClassName INIT "CHECKBOX"

   METHOD New( nRow, nCol, cCaption )
   METHOD SetFocus()
   MESSAGE SELECT() METHOD _Select()
   METHOD KillFocus()
   METHOD DISPLAY()
   METHOD HitTest( nRow, nCol )

ENDCLASS

METHOD New( nRow, nCol, cCaption )

   LOCAL cColor

   ::Buffer  := .F.
   ::Caption := cCaption
   ::CapRow  := nRow
   ::CapCol  := nCol + 3 + 1
   ::Col := nCol

   IF IsDefcolor()
      ::ColorSpec := "W/N,W+/N,W/N,W+/N"
   ELSE
      cColor      := SetColor()
      ::ColorSpec := __GUIColor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
         __GUIColor( cColor, CLR_ENHANCED   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_STANDARD   + 1 ) + "," + ;
         __GUIColor( cColor, CLR_BACKGROUND + 1 )
   ENDIF

   ::HasFocus := .F.
   ::Message  := ""
   ::Row      := nRow

   ::Style := "[û ]"

   ::Typeout := .F.

   RETURN Self

METHOD SetFocus() CLASS HBCHECKBOX

   IF ( !::HasFocus .AND. ISBLOCK( ( ::lCursor := SetCursor( 0 ), ;
         ::HasFocus := .T. , ::Display(), ::FBlock ) ) )
      Eval( ::FBlock )
   ENDIF

   RETURN Self

METHOD _Select( lState ) CLASS HBCHECKBOX

   LOCAL lStatus := ::Buffer

   IF ISLOGICAL( lState )
      ::Buffer := lState
   ELSE
      ::Buffer := !::Buffer
   ENDIF

   IF lStatus != ::Buffer .AND. ;
         ISBLOCK( ( ::Display(), ::SBlock ) )
      Eval( ::SBlock )
   ENDIF

   RETURN Self

METHOD KillFocus() CLASS HBCHECKBOX

   IF ::HasFocus
      ::HasFocus := .F.

      IF ISBLOCK( ::FBlock )
         Eval( ::FBlock )
      ENDIF

      qSelf():Display()
      SetCursor( ::lCursor )

   ENDIF

   RETURN Self

METHOD HitTest( Arg1, Arg2 ) CLASS HBCHECKBOX

   LOCAL Local1
   LOCAL Local2

   IF Arg1 != ::Row
   ELSEIF Arg2 < ::Col
   ELSEIF Arg2 < ::Col + 3
      RETURN HTCLIENT
   ENDIF

   IF HB_ISSTRING( ::Caption )
      Local2 :=  Len( ::Caption )
      IF ( Local1 := At( "&", ::Caption ) ) == 0
      ELSEIF Local1 < Local2
         Local2 --
      ENDIF

      IF Arg1 != ::Caprow
      ELSEIF Arg2 < ::CapCol
      ELSEIF Arg2 < ::CapCol + Local2
         RETURN HTCAPTION
      ENDIF
   ENDIF

   RETURN HTNOWHERE

METHOD DISPLAY() CLASS HBCHECKBOX

   LOCAL cColor    := SetColor()
   LOCAL nCurRow   := Row()
   LOCAL nCurCol   := Col()
   LOCAL cOldStyle := ::Style
   LOCAL cCaption
   LOCAL nPos

   DispBegin()

   IF ::HasFocus
      SET COLOR TO ( __GUIColor( ::ColorSpec, 2 ) )
   ELSE
      SET COLOR TO ( __GUIColor( ::ColorSpec, 1 ) )
   ENDIF

   SetPos( ::Row, ::Col + 1 )
   IF ::Buffer
      ?? SubStr( cOldStyle, 2, 1 )
   ELSE
      ?? SubStr( cOldStyle, 3, 1 )
   ENDIF

   SET COLOR TO ( __GUIColor( ::ColorSpec, 3 ) )
   SetPos( ::Row, ::Col )
   ?? Left( cOldStyle, 1 )
   SetPos( ::Row, ::Col + 2 )
   ?? Right( cOldStyle, 1 )

   IF !Empty( cCaption := ::Caption )

      IF ( nPos := At( "&", cCaption ) ) == 0
      ELSEIF nPos == Len( cCaption )
         nPos := 0
      ELSE
         cCaption := Stuff( cCaption, nPos, 1, "" )
      ENDIF

      IF ::HasFocus
         SET COLOR TO ( __GUIColor( ::ColorSpec, 4 ) )
      ENDIF

      SetPos( ::CapRow, ::CapCol )
      ?? cCaption

      IF !::HasFocus .AND. nPos != 0
         SET COLOR TO ( __GUIColor( ::ColorSpec, 4 ) )
         SetPos( ::CapRow, ::CapCol + nPos - 1 )
         ?? SubStr( cCaption, nPos, 1 )
      ENDIF

   ENDIF

   DispEnd()

   SET COLOR TO ( cColor )
   SetPos( nCurRow, nCurCol )

   RETURN Self

FUNCTION _CHECKBOX_( Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7 )

   LOCAL oCheck

   oCheck := HBCheckBox():new( Row(), Col(), Arg2 )
   IF ! ISNIL( oCheck )
      oCheck:Select( Arg1 )
      oCheck:caption := Arg2
      IF arg4 != nil
         oCheck:colorspec := Arg4
      ENDIF
      oCheck:message := Arg3
      IF arg7 != NIL
         oCheck:style := Arg7
      ENDIF
      oCheck:fblock := Arg5
      oCheck:sblock := Arg6

   ENDIF

   RETURN oCheck

FUNCTION CheckBox( nr, ncol, cCaption )

   DEFAULT cCaption TO ""

   RETURN HBCheckBox():new( nr, nCol, cCaption )

#endif
