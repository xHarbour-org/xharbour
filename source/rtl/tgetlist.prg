/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBGetList Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik
 *    Support for Ca-Clipper 5.3 Getsystem
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include "hbclass.ch"
#include "common.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "button.ch"
#include "tbrowse.ch"
#include "hbgtinfo.ch"

#define SCORE_ROW       0
#define SCORE_COL       60

#define _GET_INSERT_ON  7
#define _GET_INSERT_OFF 8
#define _GET_INVD_DATE  9

#define K_UNDO          K_CTRL_U

CLASS HBGetList

   DATA aGetList
   DATA oGet, nPos
   DATA bFormat
   DATA lUpdated
   DATA lKillRead
   DATA lBumpTop, lBumpBot
   DATA nLastExitState
   DATA nLastPos
   DATA oActiveGet
   DATA cReadProcName, nReadProcLine
   DATA cVarName
   DATA lHasFocus
   DATA lInvalid
   DATA nSaveCursor

#ifdef HB_COMPAT_C53
   DATA nHitcode
   DATA nNextGet
   DATA nMenuID
#endif

   METHOD New( GetList )

#ifdef HB_COMPAT_C53
   METHOD Settle( nPos, lInit )
   METHOD Reader( oMenu, oGetMsg )
   METHOD GetApplyKey( nKey, oMenu, oGetMsg )
   METHOD GetPreValidate( oGet, oGetMsg )
   METHOD GetPostValidate( oGet, oGetMsg )
#else
   METHOD Settle( nPos )
   METHOD Reader()
   METHOD GetApplyKey( nKey )
   METHOD GetPreValidate()
   METHOD GetPostValidate()
#endif

   METHOD GetDoSetKey( bKeyBlock )
   METHOD PostActiveGet()
   METHOD GetReadVar()
   METHOD SetFormat( bFormat, lSet )
   METHOD KillRead( lKill )
   METHOD GetActive( oGet )
   METHOD DateMsg()
   METHOD ShowScoreBoard()
   METHOD ReadUpdated( lUpdated )
   METHOD ReadVar( cNewVarName )
   METHOD ReadExit( lNew ) INLINE ( Self ), IF( ISLOGICAL( lNew ), Set( _SET_EXIT, lNew ), Set( _SET_EXIT ) )
   METHOD SetFocus()
   METHOD Updated() INLINE ::lUpdated

#ifdef HB_COMPAT_C53
   METHOD GUIReader( oGet, oMenu, oGetMsg )
   METHOD GUIApplyKey( oGUI, nKey, oMenu, oGetMsg )
   METHOD GUIPreValidate( oGUI, oGetMsg )
   METHOD GUIPostValidate( oGUI, oGetMsg )
   METHOD TBReader( oGet, oMenu, oGetMsg )
   METHOD TBApplyKey( oGet, oTB, nKey, oMenu, oGetMsg )
   METHOD Accelerator( nKey, oGetMsg ) // Removed STATIC
   METHOD HitTest( nMouseRow, nMouseColumn, oGetMsg ) // Removed STATIC
#endif

ENDCLASS

METHOD New( GetList ) CLASS HBGetList

   ::aGetList       := GetList
   ::lKillRead      := .F.
   ::lBumpTop       := .F.
   ::lBumpBot       := .F.
   ::nLastExitState := 0
   ::nLastPos       := 0
   ::cReadProcName  := ""
   ::lUpdated       := .F.
   ::nPos           := 1
   ::oGet           := iif( ISARRAY( GetList ) .AND. Len( GetList ) >= 1, GetList[ 1 ], NIL )
   ::lHasFocus      := .F.
   ::lInvalid       := .F.

#ifdef HB_COMPAT_C53
   ::nHitCode       := 0
   ::nNextGet       := 0
   ::nMenuID        := 0
#endif

   RETURN Self

METHOD SetFocus() CLASS HBGetList

   __GetListSetActive( Self )
   __GetListLast( Self )
   ::aGetList[ ::nPos ]:SetFocus()

   RETURN Self

#ifdef HB_COMPAT_C53

METHOD Reader( oMenu, oGetMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet, nKey, nRow, nCol, nCursor

   ::lInValid := .F. // Flag to reset Get when postblock return .F.

   if ::nLastExitState == GE_SHORTCUT .OR. ;
         ::nLastExitState == GE_MOUSEHIT .OR. ;
         ::GetPreValidate( oGet, oGetMsg )

      oGetMsg:Show( oGet )

      ::nHitCode       := 0
      ::nLastExitState := 0
      oGet:SetFocus()

      WHILE oGet:ExitState == GE_NOEXIT .AND. !::lKillRead
         IF oGet:Buffer == ""
            oGet:ExitState := GE_ENTER
         ENDIF

         IF oGet:typeOut
            oGet:ExitState := GE_ENTER
         ENDIF

         IF oGet:buffer == NIL
            oGet:ExitState := GE_ENTER
         ENDIF

         WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            SetCursor( iif( ::nSaveCursor == SC_NONE, SC_NORMAL, ::nSaveCursor ) )
            nKey := Inkey( 0 )
            SetCursor( SC_NONE )
            ::GetApplyKey( nKey, oMenu, oGetMsg )
            oGetMsg:Show( oGet )
         end

         if       ::nLastExitState == GE_SHORTCUT
         elseif   ::nLastExitState == GE_MOUSEHIT
         ELSEIF ! ::GetPostValidate( oGet, oGetMsg )
            oGet:ExitState := GE_NOEXIT
            // postblock returns .F., set flag to reset get
         ENDIF
      end

      nRow    := Row()
      nCol    := Col()
      nCursor := SetCursor()
      oGet:killFocus()
      SetCursor( nCursor )
      SetPos( nRow, nCol )
      oGetMsg:Erase()

   ENDIF

   RETURN Self

#else   // C52

METHOD Reader() CLASS HBGetList

   LOCAL oGet := ::oGet

   ::lInValid := .F.

   if ::GetPreValidate()

      oGet:SetFocus()

      WHILE oGet:ExitState == GE_NOEXIT
         IF oGet:Buffer == ""
            oGet:ExitState := GE_ENTER
         ENDIF

         IF oGet:typeOut
            oGet:ExitState := GE_ENTER
         ENDIF

         IF oGet:buffer == NIL
            oGet:ExitState := GE_ENTER
         ENDIF

         WHILE oGet:exitState == GE_NOEXIT
            ::GetApplyKey( Inkey( 0 ) )
         end

         IF ! ::GetPostValidate()
            oGet:ExitState := GE_NOEXIT
         ENDIF
      end

      oGet:killFocus()
   ENDIF

   RETURN Self

#endif  // HB_COMPAT_C53

#ifdef HB_COMPAT_C53

METHOD GetApplyKey( nKey, oMenu, oGetMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL cKey
   LOCAL bKeyBlock
   LOCAL nMouseRow, nMouseColumn
   LOCAL nButton
   LOCAL nHotItem
   LOCAL lPassword
#ifdef HB_EXT_INKEY
   LOCAL cToPaste, nI, nLen
#endif

   IF ! ( ( bKeyBlock := SetKey( nKey ) ) == NIL )
      if ::GetDoSetKey( bKeyBlock )
         RETURN Self
      ENDIF
   ENDIF

   IF ( !( ::aGetList == NIL ) .AND. ;
         ( ( nHotItem := ::Accelerator( nKey, oGetMsg ) ) != 0 ) )

      oGet:ExitState   := GE_SHORTCUT
      ::nNextGet       := nHotItem
      ::nLastExitState := GE_SHORTCUT

   ELSEIF !HB_ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:GetAccel( nKey ) ) != 0
      ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
      nKey := 0

   ELSEIF oMenu:IsShortcut( nKey )
      nKey := 0

   ENDIF

#else

METHOD GetApplyKey( nKey ) CLASS HBGetList

   LOCAL cKey, bKeyBlock, oGet := ::oGet, lPassword

#ifdef HB_EXT_INKEY
   LOCAL cToPaste, nI, nLen
#endif

   IF ! ( ( bKeyBlock := SetKey( nKey ) ) == NIL )
      ::GetDoSetKey( bKeyBlock )
      RETURN Self
   ENDIF

#endif
   lPassword := oGet:Password

   Switch nKey
   CASE K_UP
      IF ! lPassword
         oGet:ExitState := GE_UP
      ENDIF
      EXIT

   CASE K_SH_TAB
      IF ! lPassword
         oGet:ExitState := GE_UP
      ENDIF
      EXIT

   CASE K_DOWN
      IF ! lPassword
         oGet:ExitState := GE_DOWN
      ENDIF
      EXIT

   CASE K_TAB
      IF ! lPassword
         oGet:ExitState := GE_DOWN
      ENDIF
      EXIT

   CASE K_ENTER
      oGet:ExitState := GE_ENTER
      EXIT

   CASE K_ESC
      IF SET( _SET_ESCAPE )
         oGet:UnDo()
         oGet:ExitState := GE_ESCAPE
      ENDIF
      EXIT

   CASE K_PGUP
      IF ! lPassword
         oGet:ExitState := GE_WRITE
      ENDIF
      EXIT

   CASE K_PGDN
      IF ! lPassword
         oGet:ExitState := GE_WRITE
      ENDIF
      EXIT

   CASE K_CTRL_HOME
      IF ! lPassword
         oGet:ExitState := GE_TOP
      ENDIF
      EXIT

#ifdef CTRL_END_SPECIAL
   CASE K_CTRL_END
      IF ! lPassword
         oGet:ExitState := GE_BOTTOM
      ENDIF
      EXIT
#else
   CASE K_CTRL_W
      IF ! lPassword
         oGet:ExitState := GE_WRITE
      ENDIF
      EXIT
#endif

   CASE K_INS
      //if ! lPassword
      SET( _SET_INSERT, ! Set( _SET_INSERT ) )
      ::ShowScoreboard()
      //endif
         /* 2007/SEP/24 - EF - Toggle cursor shape at insert mode on/off
          *               Uncomment it, if you want this behaviour.
          *if ::nSaveCursor != SC_NONE
          *   ::nSaveCursor := if( Set(_SET_INSERT), SC_INSERT, SC_NORMAL )
          *endif
          */
      EXIT
#ifdef HB_COMPAT_C53
   CASE K_LBUTTONDOWN
   CASE K_LDBLCLK
      IF ! lPassword

         nMouseRow    := MRow()
         nMouseColumn := MCol()

         IF !HB_ISOBJECT( oMenu )
            nButton := 0

         ELSEIF !( oMenu:ClassName() == "TOPBARMENU" )
            nButton := 0

         ELSEIF ( nButton := oMenu:HitTest( nMouseRow, nMouseColumn ) ) != 0
            ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
            nButton := 1

         ENDIF


         IF ( nButton != 0 )

         ELSEIF ( nButton := oGet:HitTest( nMouseRow, nMouseColumn ) ) == HTCLIENT

            WHILE  oGet:Col + oGet:Pos - 1 > nMouseColumn
               oGet:Left()

               // Handle editing buffer if first character is non-editable:
               IF oGet:typeOut
                  // reset typeout:
                  oGet:Home()
                  EXIT
               ENDIF

            end

            WHILE  oGet:Col + oGet:Pos - 1 < nMouseColumn
               oGet:Right()

               // Handle editing buffer if last character is non-editable:
               IF oGet:typeOut
                  // reset typeout:
                  oGet:End()
                  EXIT
               ENDIF

            end

         ELSEIF !( nButton == HTNOWHERE )

         ELSEIF !( ::aGetList == NIL ) .AND. ;
               ::HitTest( nMouseRow, nMouseColumn, oGetMsg ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT

         ELSE
            oGet:ExitState := GE_NOEXIT

         ENDIF
      ENDIF
      EXIT
#endif

   CASE K_UNDO
      oGet:UnDo()
      EXIT

   CASE K_HOME
      IF ! lPassword
         oGet:Home()
      ENDIF
      EXIT

   CASE K_END
      IF ! lPassword
         oGet:End()
      ENDIF
      EXIT

   CASE K_RIGHT
      IF ! lPassword
         oGet:Right()
      ENDIF
      EXIT

   CASE K_LEFT
      IF ! lPassword
         oGet:Left()
      ENDIF
      EXIT

   CASE K_CTRL_RIGHT
      IF ! lPassword
         oGet:WordRight()
      ENDIF
      EXIT

   CASE K_CTRL_LEFT
      IF ! lPassword
         oGet:WordLeft()
      ENDIF
      EXIT

   CASE K_BS
      oGet:BackSpace()
      IF oGet:NToL
         oGet:NumToLeft()
      ENDIF
      EXIT

   CASE K_DEL
      IF ! lPassword
         IF oGet:NToL
            IF oGet:DecPos != nil .AND. oGet:Pos > oGet:DecPos
               oGet:Delete()
            ELSE
               oGet:BackSpace()
               oGet:NumToLeft()
            ENDIF
         ELSE
            oGet:Delete()
         ENDIF
      ENDIF
      EXIT

   CASE K_CTRL_T
      IF ! lPassword
         oGet:DelWordRight()
      ENDIF
      EXIT

   CASE K_CTRL_Y
      IF ! lPassword
         oGet:DelEnd()
         IF oGet:NToL
            oGet:NumToLeft()
         ENDIF
      ENDIF
      EXIT

   CASE K_CTRL_BS
      IF ! lPassword
         oGet:DelWordLeft()
      ENDIF
      EXIT

#ifdef HB_EXT_INKEY

   CASE K_CTRL_C
      IF ! lPassword
         oGet:Assign()
         hb_gtInfo( GTI_CLIPBOARDDATA, CStr( oGet:VarGet() ) )
      ENDIF
      EXIT

   CASE K_CTRL_X
      IF ! lPassword
         oGet:Assign()
         hb_gtInfo( GTI_CLIPBOARDDATA, CStr( oGet:VarGet() ) )
         oGet:DelEnd()
      ENDIF
      EXIT

   CASE K_CTRL_V
      IF ! lPassword
         cToPaste := hb_gtInfo( GTI_CLIPBOARDDATA )
         nLen := Len( cToPaste )
         FOR nI := 1 TO nLen
            oGet:Insert( cToPaste[ nI ] )
         NEXT nI
      ENDIF
      EXIT

#endif

      DEFAULT

      IF nKey >= 32 .AND. nKey <= 255

         if ::lInValid
               /*  2007/JUL/06 - E.F. Disabled oGet:DelEnd() for Clipper compatibility.
                *  clear buffer and get window when postblock returns .F.
                *  oGet:DelEnd() */

            /* 2007/SEP/24 - EF - Adjust buffer content. */
            IF oGet:Type == "N"
               oGet:DelEnd()
               IF oGet:Untransform() == 0
                  oGet:Clear := .T.
               ENDIF
            ENDIF
         ENDIF

         cKey := Chr( nKey )

         IF oGet:type == "N" .AND. ( cKey == "." .OR. cKey == "," )
            oGet:changed := .T.  // 2006/DEC/22 - E.F. Fixed by Marco Bernardi.
            oGet:ToDecPos()

            /* 2007/SEP/25 -EF - Deny type minus sign more than one time.  */
         ELSEIF oGet:type == "N" .AND. cKey == "-" .AND. oGet:Minus
         ELSE
            IF SET( _SET_INSERT )
               oGet:Insert( cKey )
            ELSE
               oGet:OverStrike( cKey )
            ENDIF
            IF oGet:TypeOut
               IF SET( _SET_BELL )
                  ?? Chr( 7 )
               ENDIF
               IF ! Set( _SET_CONFIRM )
                  oGet:ExitState := GE_ENTER
               ENDIF
               /* 2007/SEP/24 - EF - Adjust buffer content. */
               IF oGet:Type == "N" .AND. oGet:Untransform() == 0
                  oGet:Minus := .F.
                  oGet:VarPut( 0 )
                  oGet:Assign()
                  oGet:UpdateBuffer()
               ENDIF
            ENDIF
         ENDIF
         IF oGet:NToL
            oGet:NumToLeft()
         ENDIF
      ENDIF
   end

   /* 2007/SEP/24 - EF - Adjust buffer content. */
   IF oGet:ExitState != NIL .AND. oGet:ExitState > GE_NOEXIT .AND. oGet:Type == "N"
      IF oGet:Untransform() == 0
         oGet:Minus := .F.
         oGet:VarPut( 0 )
         oGet:Buffer := StrTran( oGet:Buffer, "-", " " )
      ENDIF
      oGet:Assign()
      oGet:UpdateBuffer()
   ENDIF

   RETURN Self

#ifdef HB_COMPAT_C53

METHOD GetPreValidate( oGet, oGetMsg ) CLASS HBGetList

   LOCAL lUpdated, lWhen := .T., aMsg := { FALSE }
   LOCAL xValue

   IF ISNIL( oGet )
      oGet := ::oGet
   ENDIF

   IF ISOBJECT( oGetMsg )
      aMsg := oGetMsg:aMsg
   ENDIF
#else

METHOD GetPreValidate() CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL lUpdated, lWhen := .T.
   LOCAL xValue
#endif

   IF oGet:PreBlock != NIL
      xValue    := oGet:VarGet()
      oGet:type := ValType( xValue )
      lUpdated  := ::lUpdated

#ifdef HB_COMPAT_C53
      lWhen := Eval( oGet:PreBlock, oGet, aMsg )
      IF !HB_ISOBJECT( oGet:Control ) .AND. !lWhen
         oGet:Display()
         //      elseif ValType( xValue ) != ValType( oGet:VarGet() ) .or. ;
         //           oGet:VarGet() != xValue
         //         oGet:VarPut( oGet:VarGet() )
         //      else
         //         oGet:Display()
      ENDIF
#else
      lWhen := Eval( oGet:PreBlock, oGet )
      IF ValType( xValue ) != ValType( oGet:VarGet() ) .OR. ;
            oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      ELSE
         oGet:Display()
      ENDIF
#endif

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      if !( __GetListActive() == Self )
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )
   ENDIF

   if ::lKillRead
      lWhen := .F.
      oGet:ExitState := GE_ESCAPE
   ELSEIF ! lWhen
      oGet:ExitState := GE_WHEN
   ELSE
      oGet:ExitState := GE_NOEXIT
   end

   RETURN lWhen

#ifdef HB_COMPAT_C53

METHOD GetPostValidate( oGet, oGetMsg ) CLASS HBGetList

   LOCAL lUpdated, lValid := .T., aMsg := { FALSE }, nCursor
   LOCAL xValue

   IF ISNIL( oGet )
      oGet := ::oGet
   ENDIF

   IF ISOBJECT( oGetMsg )
      aMsg := oGetMsg:aMsg
   ENDIF
#else

METHOD GetPostValidate() CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL lUpdated, lValid := .T.
   LOCAL xValue
#endif

   IF oGet:ExitState == GE_ESCAPE
      RETURN .T.
   ENDIF

   IF oGet:BadDate()
      //      oGet:SetFocus()
      oGet:TypeOut := .F.
      ::DateMsg()
      ::ShowScoreboard()
      RETURN .F.
   ENDIF

   IF oGet:Changed
      oGet:Assign()
      ::lUpdated := .T.
   ENDIF

#ifdef HB_COMPAT_C53
   nCursor := SetCursor()
   oGet:reset()
   SetCursor( nCursor )
#else
   oGet:Reset():Display()
#endif

   IF oGet:PostBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated
      SetPos( oGet:Row, oGet:Col + iif( oGet:Buffer == NIL, 0, Len( oGet:Buffer ) ) )

#ifdef HB_COMPAT_C53
      lValid := Eval( oGet:PostBlock, oGet, aMsg )
#else
      lValid := Eval( oGet:PostBlock, oGet )
#endif

      IF !HB_ISLOGICAL( lValid )
         lValid := .F.
      ENDIF

      SetPos( oGet:Row, oGet:Col )

      IF ValType( xValue ) != ValType( oGet:VarGet() ) .OR. ;
            oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      ENDIF
      oGet:UpdateBuffer()

      ::ShowScoreBoard()

#ifdef HB_COMPAT_C53
      ::lUpdated := iif( oGet:changed, .T., lUpdated )
#else
      ::lUpdated := lUpdated
#endif

/*
      if !( __GetListActive() == Self )
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )

      if ::lKillRead
         oGet:ExitState := GE_ESCAPE
         lValid := .T.
      ENDIF
   ENDIF

   ::lInvalid := ! ( lValid )

   RETURN lValid

METHOD GetDoSetKey( bKeyBlock ) CLASS HBGetList

   LOCAL oGet := ::oGet, lUpdated, lSetKey

   IF oGet:Changed
      oGet:Assign()
      ::lUpdated := .T.
   ENDIF

   lUpdated := ::lUpdated

   lSetKey := Eval( bKeyBlock, ::cReadProcName, ::nReadProcLine, ::ReadVar() )

   IF !HB_ISLOGICAL( lSetKey )
      lSetKey := .T.              // Will cause Return from ApplyKey(), C53
   ENDIF                          // no effect on C52

   ::ShowScoreboard()
   oGet:UpdateBuffer()

   ::lUpdated := lUpdated

/*
   if !( __GetListActive() == Self )
      __GetListSetActive( Self )
   endif
*/
   __GetListLast( Self )

   if ::lKillRead
      oGet:ExitState := GE_ESCAPE
   ENDIF

   RETURN lSetKey

#ifdef HB_COMPAT_C53

METHOD Settle( nPos, lInit ) CLASS HBGetList

#else

METHOD Settle( nPos ) CLASS HBGetList

#endif

   LOCAL nExitState

   IF nPos == NIL
      nPos := ::nPos
   ENDIF

   IF nPos == 0
      nExitState := GE_DOWN
#ifdef HB_COMPAT_C53
   ELSEIF ( nPos > 0 .AND. lInit )
      nExitState := GE_NOEXIT
#endif
   ELSE
      nExitState := ::aGetList[ nPos ]:ExitState
   ENDIF

   IF nExitState == GE_ESCAPE .OR. nExitState == GE_WRITE
      RETURN 0
   ENDIF

   IF nExitState != GE_WHEN
      ::nLastPos := nPos
      ::lBumpTop := .F.
      ::lBumpBot := .F.
   ELSE
#ifdef HB_COMPAT_C53
      if ::nLastExitState != 0
         nExitState := ::nLastExitState
      elseif ::nNextGet < ::nLastPos
         nExitState := GE_UP
      ELSE
         nExitState := GE_DOWN
      ENDIF
#else
      nExitState := ::nLastExitState
#endif
   ENDIF

   DEFAULT nExitState TO 0

   Switch nExitState
   CASE GE_UP
      nPos--
      EXIT

   CASE GE_DOWN
      nPos++
      EXIT

   CASE GE_TOP
      nPos := 1
      ::lBumpTop := .T.
      nExitState := GE_DOWN
      EXIT

   CASE GE_BOTTOM
      nPos := Len( ::aGetList )
      ::lBumpBot := .T.
      nExitState := GE_UP
      EXIT

   CASE GE_ENTER
      nPos++
      EXIT

#ifdef HB_COMPAT_C53
   CASE GE_SHORTCUT
      return ::nNextGet

   CASE GE_MOUSEHIT
      return ::nNextGet
#endif

   end

   IF nPos == 0
      IF ! ::ReadExit() .AND. ! ::lBumpBot
         ::lBumpTop := .T.
         nPos       := ::nLastPos
         nExitState := GE_DOWN
      ENDIF

   ELSEIF nPos == Len( ::aGetList ) + 1
      IF ! ::ReadExit() .AND. nExitState != GE_ENTER .AND. ! ::lBumpTop
         ::lBumpBot := .T.
         nPos       := ::nLastPos
         nExitState := GE_UP
      ELSE
         nPos := 0
      ENDIF
   ENDIF

   ::nLastExitState := nExitState

   IF nPos != 0
      ::aGetList[ nPos ]:ExitState := nExitState
   ENDIF

   RETURN nPos

METHOD PostActiveGet() CLASS HBGetList

   ::GetActive( ::oGet )
   ::ReadVar( ::GetReadVar() )
   ::ShowScoreBoard()

   RETURN Self

METHOD GetReadVar() CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL cName := Upper( oGet:Name )
   LOCAL n

   IF oGet:Subscript != NIL
      FOR n := 1 TO Len( oGet:Subscript )
         /* cName += "[" + LTrim( Str( oGet:Subscript[ n ] ) ) + "]" */
         cName += "[" + LTrim( ValToPrg( oGet:Subscript[ n ] ) ) + "]"
      NEXT
   end

   RETURN cName

METHOD SetFormat( bFormat, lSet ) CLASS HBGetList

   LOCAL bSavFormat := ::bFormat

   IF ! ISNIL( bFormat ) .OR. lSet
      ::bFormat := bFormat
   ENDIF

   RETURN bSavFormat

METHOD KillRead( lKill ) CLASS HBGetList

   LOCAL lSavKill := ::lKillRead

   IF PCount() > 0
      ::lKillRead := lKill
   ENDIF

   RETURN lSavKill

METHOD GetActive( oGet ) CLASS HBGetList

   LOCAL oOldGet := ::oActiveGet

   IF PCount() > 0
      ::oActiveGet := oGet
   ENDIF

   RETURN oOldGet

METHOD ShowScoreboard() CLASS HBGetList

   LOCAL nRow, nCol, nOldCursor

   IF SET( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      nOldCursor := SetCursor( SC_NONE )

      DispOutAt( SCORE_ROW, SCORE_COL, iif( Set( _SET_INSERT ), NationMsg( _GET_INSERT_ON ), NationMsg( _GET_INSERT_OFF ) ) )

      SetCursor( nOldCursor )
      SetPos( nRow, nCol )

   ENDIF

   RETURN Self

METHOD DateMsg() CLASS HBGetList

   LOCAL nRow, nCol, nOldCursor

   IF SET( _SET_SCOREBOARD )

      nOldCursor := Max( 1, SetCursor( SC_NONE ) )
      nRow := ::oGet:Row
      nCol := ::oGet:Col

      DispOutAt( SCORE_ROW, SCORE_COL, NationMsg( _GET_INVD_DATE ) )

      ::oGet:Home()
      SetPos( nRow, nCol )
      SetCursor( nOldCursor )

      WHILE NextKey() == 0
      end

      DispOutAt( SCORE_ROW, SCORE_COL, Space( Len( NationMsg( _GET_INVD_DATE ) ) ) )

      SetPos( nRow, nCol )
      SetCursor( nOldCursor )

   ENDIF

   RETURN Self

METHOD ReadVar( cNewVarName ) CLASS HBGetList

   LOCAL cOldName := ::cVarName

   IF ISCHARACTER( cNewVarName )
      ::cVarName := cNewVarName
   ENDIF

   RETURN cOldName

METHOD ReadUpdated( lUpdated ) CLASS HBGetList

   LOCAL lSavUpdated := ::lUpdated

   IF PCount() > 0
      ::lUpdated := lUpdated
   ENDIF

   RETURN lSavUpdated

#ifdef HB_COMPAT_C53

METHOD GUIReader( oGet, oMenu, oGetMsg ) CLASS HBGetList

//Local oGet := ::oGet
   LOCAL oGui

   IF HB_ISOBJECT( oGet:Control ) .AND. ;     // Moved up 2 lines.
      ::nLastExitState == GE_SHORTCUT .OR. ;  // Added.
      ::nLastExitState == GE_MOUSEHIT .OR. ;  // Added.
      ::GetPreValidate( oGet, oGetMsg )

      oGetMsg:Show( oGet )
      ::nLastExitState := 0

      // Activate the GET for reading
      oGUI := oGet:Control
      oGUI:Select( oGet:VarGet() )
      oGUI:setFocus()

      IF oGet:exitState == GE_NOEXIT  // Added.

         IF ( ::nHitCode > 0 )
            oGUI:Select( ::nHitCode )

         ELSEIF ( ::nHitCode == HTCAPTION )
            oGUI:Select()

         ELSEIF ( ::nHitCode == HTCLIENT )
            oGUI:Select( K_LBUTTONDOWN )

         ELSEIF ( ::nHitCode == HTDROPBUTTON )
            oGUI:Open()

         ELSEIF ( ( ::nHitCode >= HTSCROLLFIRST ) .AND. ;
               ( ::nHitCode <= HTSCROLLLAST ) )
            oGUI:Scroll( ::nHitCode )
         ENDIF

      ENDIF  // Added.

      ::nHitCode := 0

      WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Check for initial typeout (no editable positions)
         IF ( oGui:typeOut )
            oGet:exitState := GE_ENTER
         ENDIF

         // Apply keystrokes until exit
         WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            ::GUIApplyKey( oGUI, Inkey( 0 ), oMenu, oGetMsg )
            oGetMsg:Show( oGet )
         end

         // Disallow exit if the VALID condition is not satisfied

         if     ::nLastExitState == GE_SHORTCUT  // Added.
         elseif ::nLastExitState == GE_MOUSEHIT  // Added.
         ELSEIF !HB_ISOBJECT( oGet:Control ) .AND. !::GetPostValidate( oGet, oGetMsg )
            oGet:exitState := GE_NOEXIT
         ELSEIF HB_ISOBJECT( oGet:Control ) .AND. !::GUIPostValidate( oGUI, oGetMsg )
            oGet:exitState := GE_NOEXIT
         ENDIF
      end

      // De-activate the GET
      IF ( oGUI:ClassName() $ "LISTBOX_HBRADIOGROUP" ) .AND. ;
         HB_ISNUMERIC( oGet:VarGet() )
         // Need to test the Value here:
         oGet:VarPut( oGUI:Value )
      ELSE
         oGet:VarPut( oGUI:Buffer )
      ENDIF
      oGUI:killFocus()

      oGetMsg:Erase()

      IF ( ! oGUI:ClassName() == "LISTBOX" )
      ELSEIF ( ! oGUI:DropDown )
      ELSEIF ( oGUI:IsOpen )
         oGUI:Close()
      ENDIF

   ENDIF

   RETURN Self

METHOD GUIApplyKey( oGUI, nKey, oMenu, oGetMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL bKeyBlock
   LOCAL nMouseRow, nMouseColumn
   LOCAL nButton
   LOCAL oTheClass
   LOCAL nHotItem
   LOCAL lClose

// Check for SET KEY first
   IF !( bKeyBlock := SetKey( nKey ) ) == NIL
      if ::GetDoSetKey( bKeyBlock )
         RETURN Self
      ENDIF
   ENDIF

   IF ( nHotItem := ::Accelerator( nKey, oGetMsg ) ) != 0
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet := nHotItem

   ELSEIF !HB_ISOBJECT( oMenu )

   ELSEIF ( nHotItem := oMenu:GetAccel( nKey ) ) != 0
      ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
      nKey := 0

   ELSEIF oMenu:IsShortcut( nKey )
      nKey := 0

   ENDIF
//tracelog(oGUI:ClassName )
   IF nKey == 0
   ELSEIF ( oTheClass := oGUI:ClassName() ) == "HBRADIOGROUP"
      //tracelog( nKey )
      IF nKey == K_UP
         oGUI:PrevItem()
         nKey := 0

      ELSEIF nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0

      ELSEIF ( nHotItem := oGUI:GetAccel( nKey ) ) != 0
         oGUI:Select( nHotItem )

      ENDIF

      IF HB_ISNUMERIC( oGet:VarGet() )
         oGet:VarPut( oGUI:Value )
      ENDIF

   ELSEIF oTheClass == "CHECKBOX"
      IF nKey == K_SPACE
         oGUI:Select()
      ENDIF

   ELSEIF oTheClass == "PUSHBUTTON"
      IF nKey == K_SPACE
         oGUI:Select( K_SPACE )

      ELSEIF nKey == K_ENTER
         oGUI:Select()
         nKey := 0

      ENDIF

   ELSEIF oTheClass == "LISTBOX"

      IF nKey == K_UP
         oGUI:PrevItem()
         nKey := 0
      ELSEIF nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0
      ELSEIF nKey == K_SPACE
         IF ! oGUI:DropDown
         ELSEIF ! oGUI:IsOpen
            oGUI:Open()
            nKey := 0
         ENDIF
      ELSEIF ( nButton := oGUI:FindText( Chr(nKey ), oGUI:Value + 1, .F., .F. ) ) != 0
         oGUI:Select( nButton )
      ENDIF

      IF HB_ISNUMERIC( oGet:VarGet() )
         oGet:VarPut( oGui:Value )
      ENDIF

   ENDIF

   Switch nKey
   CASE K_UP
      oGet:ExitState := GE_UP
      EXIT

   CASE K_SH_TAB
      oGet:ExitState := GE_UP
      EXIT

   CASE K_DOWN
      oGet:ExitState := GE_DOWN
      EXIT

   CASE K_TAB
      oGet:ExitState := GE_DOWN
      EXIT

   CASE K_ENTER
      oGet:ExitState := GE_ENTER
      EXIT

   CASE K_ESC
      IF SET( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      ENDIF
      EXIT

   CASE K_PGUP
      oGet:ExitState := GE_WRITE
      EXIT

   CASE K_PGDN
      oGet:ExitState := GE_WRITE
      EXIT

   CASE K_CTRL_HOME
      oGet:ExitState := GE_TOP
      EXIT

#ifdef CTRL_END_SPECIAL

      // Both ^W and ^End go to the last GET
   CASE K_CTRL_END
      oGet:ExitState := GE_BOTTOM
      EXIT

#else

      // Both ^W and ^End terminate the READ (the default)
   CASE K_CTRL_W
      oGet:ExitState := GE_WRITE
      EXIT

#endif

   CASE K_LBUTTONDOWN
   CASE K_LDBLCLK
      nMouseRow    := MRow()
      nMouseColumn := MCol()

      IF !HB_ISOBJECT( oMenu )
         nButton := 0

      ELSEIF ( !( oMenu:ClassName() == "TOPBARMENU" ) )
         nButton := 0

      ELSEIF ( ( nButton := oMenu:HitTest( nMouseRow, nMouseColumn ) ) != 0 )
         ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
         nButton := 1

      ENDIF

      lClose := .T.

      IF ( nButton != 0 )
      ELSEIF ( nButton := oGUI:HitTest( nMouseRow, nMouseColumn ) ) == HTNOWHERE
         // Changed test:
         if ::HitTest( nMouseRow, nMouseColumn, oGetMsg ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT  // Added.
         ELSE
            oGet:ExitState := GE_NOEXIT
         ENDIF

      ELSEIF nButton >= HTCLIENT
         oGUI:Select( nButton )

      ELSEIF nButton == HTDROPBUTTON
         IF !oGUI:IsOpen
            oGUI:Open()
            lClose := .F.
         ENDIF

      ELSEIF nButton >= HTSCROLLFIRST .AND. nButton <= HTSCROLLLAST
         oGUI:Scroll( nButton )
         lClose := .F.

      ENDIF

      IF ! lClose
      ELSEIF ! oTheClass == "LISTBOX"
      ELSEIF ! oGUI:DropDown
      ELSEIF oGUI:IsOpen
         oGUI:Close()
         oGUI:Display()
      ENDIF
      EXIT

   end

   RETURN Self

METHOD GUIPreValidate( oGUI, oGetMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL lUpdated
   LOCAL lWhen := .T.
   LOCAL xValue

   IF !( oGet:preBlock == NIL )
      xValue      := oGet:VarGet()
      oGet:type   := ValType( xValue )
      lUpdated := ::lUpdated

      lWhen := Eval( oGet:preBlock, oGet, oGetMsg:aMsg )

      IF ValType( xValue ) != ValType( oGet:VarGet() ) .OR. ;
            oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      ELSEIF ( !( oGUI:ClassName() == "TBROWSE" ) )
         oGet:Display()
      ENDIF

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      if !( __GetListActive() == Self )
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )
   ENDIF

   if ::lKillRead
      lWhen := .F.
      oGet:ExitState := GE_ESCAPE

   ELSEIF !lWhen
      oGet:ExitState := GE_WHEN

   ELSE
      oGet:ExitState := GE_NOEXIT

   ENDIF

   RETURN lWhen

METHOD GUIPostValidate( oGUI, oGetMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL lUpdated
   LOCAL lValid := .T.
   LOCAL uOldData, uNewData
   LOCAL xValue

   IF oGet:exitState == GE_ESCAPE
      RETURN .T.                   // NOTE
   ENDIF

   IF oGet:BadDate()
      //      oGet:SetFocus()
      oGet:TypeOut := .F.
      ::DateMsg()
      ::ShowScoreboard()
      RETURN .F.
   ENDIF

   IF oGet:Changed
      oGet:Assign()
      ::lUpdated := .T.
   ENDIF

   oGet:Reset():Display()

   IF ( !( oGUI:ClassName() == "TBROWSE" ) )
      uOldData := oGet:VarGet()

      IF oGUI:ClassName() $ "LISTBOX_HBRADIOGROUP" .AND. ;
         HB_ISNUMERIC( oGet:VarGet() )
         uNewData := oGUI:Value
      ELSE
         uNewData := oGUI:Buffer
      ENDIF

   ENDIF

// If editing occurred, assign the new value to the variable
   IF !( uOldData == uNewData )
      oGet:VarPut( uNewData )
      ::lUpdated := .T.
   ENDIF

// Check VALID condition if specified
   IF !( oGet:postBlock == NIL )

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      lValid := Eval( oGet:postBlock, oGet, oGetMsg:aMsg )

      // Reset S'87 compatibility cursor position
      SetPos( oGet:Row, oGet:Col )

      IF ValType( xValue ) != ValType( oGet:VarGet() ) .OR. ;
            oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      ENDIF
      oGet:UpdateBuffer()

      ::ShowScoreBoard()
      IF ! ( oGUI:ClassName == "TBROWSE" )
         oGUI:Select( oGet:VarGet() )
      ENDIF

      ::lUpdated := lUpdated

/*
      if !( __GetListActive() == Self )
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )

      if ::lKillRead
         oGet:ExitState := GE_ESCAPE      // Provokes ReadModal() exit
         lValid := .T.
      ENDIF

   ENDIF

   RETURN  lValid

METHOD TBReader( oGet, oMenu,  oGetMsg ) CLASS HBGETLIST

   LOCAL oTB, nKey, lAutoLite, nCursor, nProcessed

//   Local nRow, nCol
//   Local oGui := oGet:control

// Read the GET if the WHEN condition is satisfied
   IF HB_ISOBJECT( oGet:control ) .AND.    ;  // Moved up 2 lines.
      ::nLastExitState == GE_SHORTCUT .OR. ;  // Added.
      ::nLastExitState == GE_MOUSEHIT .OR. ;  // Added.
      ::GetPreValidate( oGet, oGetMsg )

      oGetMsg:Show( oGet )
      ::nLastExitState := 0  // Added.

      nCursor := SetCursor( SC_NONE )

      // Activate the GET for reading
      oTB := oGet:Control

      lAutoLite := oTB:Autolite
      oTB:Autolite := .T.
      oTB:Hilite()

      IF oGet:exitState == GE_NOEXIT  // Added.
         if ::nHitcode == HTCELL
            //TraceLog( 'hitcode ',::nHitcode )
            // Replaces call to TBMouse( oTB, MRow(), MCol() ):
            oTB:RowPos := oTb:mRowPos
            oTB:ColPos := oTb:mColPos
            oTB:Invalidate()
         ENDIF
      ENDIF  // Added.

      ::nHitcode := 0

      WHILE  oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Apply keystrokes until exit
         WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            nKey := 0

            WHILE !oTB:Stabilize() .AND. nKey == 0
               nKey := Inkey()
            end

            IF nKey == 0
               nKey := Inkey( 0 )
            ENDIF

            nProcessed := oTB:ApplyKey( nKey )
            IF nProcessed == TBR_EXIT
               oGet:exitState := GE_ESCAPE
               EXIT

            ELSEIF nProcessed == TBR_EXCEPTION
               ::TBApplyKey( oGet, oTB, nKey, oMenu, oGetMsg )

               // nRow := ROW()  // Commented out.
               // nCol := COL()  // Commented out.
               oGetMsg:Show( oGet )
               // DevPos( nRow, nCol )  // Commented out.

            ENDIF

         end

         // Disallow exit if the VALID condition is not satisfied
         if ::nLastExitState == GE_SHORTCUT       // Added.
         elseif ::nLastExitState == GE_MOUSEHIT   // Added.
         ELSEIF !::GetPostValidate( oGet, oGetMsg )  // Changed.
            // if !::GUIPostValidate( oGUI, oGetMsg ) // Old test.
            oGet:ExitState := GE_NOEXIT
         ENDIF

      end

      // De-activate the GET
      oTB:Autolite := lAutoLite
      oTB:DeHilite()

      oGetMsg:Erase()

      SetCursor( nCursor )
   ENDIF

   RETURN Self

METHOD TBApplyKey( oGet, oTB, nKey, oMenu, oGetMsg ) CLASS HBGETLIST

   LOCAL bKeyBlock
   LOCAL nMouseRow, nMouseColumn
   LOCAL nButton
   LOCAL nHotItem

// Check for SET KEY first
   IF !( ( bKeyBlock := SetKey( nKey ) ) == NIL )
      if ::GetDoSetKey( bKeyBlock )
         RETURN Self
      ENDIF
   ENDIF

   IF ( nHotItem := ::Accelerator( nKey, oGetMsg ) ) != 0
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet := nHotItem

   ELSEIF !HB_ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:GetAccel( nKey ) ) != 0
      ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
      nKey := 0

   ELSEIF ( oMenu:IsShortcut( nKey ) )
      nKey := 0

   ENDIF

   Switch nKey
   CASE K_TAB
      oGet:ExitState := GE_DOWN
      EXIT

   CASE K_SH_TAB
      oGet:ExitState := GE_UP
      EXIT

   CASE K_ENTER
      IF !oTb:Stable()
         oTb:ForceStable()
      ENDIF
      oGet:ExitState := GE_ENTER
      EXIT

   CASE K_ESC
      IF SET( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      ENDIF
      EXIT

#ifdef CTRL_END_SPECIAL

      // Both ^W and ^End go to the last GET
   CASE K_CTRL_END
      oGet:ExitState := GE_BOTTOM
      EXIT

#else

      // Both ^W and ^End terminate the READ (the default)
   CASE K_CTRL_W
      oGet:ExitState := GE_WRITE
      EXIT

#endif

   CASE K_LBUTTONDOWN
   CASE K_LDBLCLK
      nMouseRow    := MRow()
      nMouseColumn := MCol()

      IF !HB_ISOBJECT( oMenu )
         nButton := 0

      ELSEIF !( oMenu:ClassName() == "TOPBARMENU" )
         nButton := 0

      ELSEIF ( nButton := oMenu:HitTest( nMouseRow, nMouseColumn ) ) != 0
         ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
         nButton := 1

      ENDIF

      IF nButton != 0
      ELSEIF oTB:HitTest( nMouseRow, nMouseColumn ) == HTNOWHERE // Changed test:
         if ::HitTest( nMouseRow, nMouseColumn, oGetMsg ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT
         ELSE
            oGet:ExitState := GE_NOEXIT

         ENDIF
      ENDIF
      EXIT

   end

   RETURN self

METHOD  HitTest( nMouseRow, nMouseCol, oGetMsg ) CLASS HBGETLIST

   LOCAL oGet, lGUI

   ::nNextGet := 0

   FOR EACH oGet in ::aGetList
      IF ( ::nHitCode := oGet:HitTest( nMouseRow, nMouseCol ) ) != HTNOWHERE
         ::nNextGet := HB_EnumIndex()
         EXIT
      ENDIF
   NEXT

   IF !( ::nNextGet == 0 )  // Changed.

      // Test the current GUI-GET or Get PostValidation:
      lGUI := HB_ISOBJECT( ::aGetList[ ::nPos ]:Control )

      IF lGUI .AND. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control, oGetMsg )

         ::nNextGet := 0
         RETURN 0

      ELSEIF !lGUI .AND. !::GetPostValidate( ::aGetList[ ::nPos ], oGetMsg )

         ::nNextGet := 0
         RETURN 0

      ENDIF

      // Test the next GUI-GET or Get PreValidation:
      lGUI := HB_ISOBJECT( ::aGetList[ ::nNextGet ]:Control )

      IF lGUI .AND. !::GUIPreValidate( ::aGetList[ ::nNextGet ]:Control, oGetMsg )

         ::nNextGet := 0
         return ::nNextGet

      ELSEIF !lGUI .AND. !::GetPreValidate( ::aGetList[ ::nNextGet ], oGetMsg )

         ::nNextGet := 0
         return ::nNextGet

      ENDIF

      return ::nNextGet

   ENDIF

   RETURN 0

METHOD Accelerator( nKey, oGetMsg ) CLASS HBGETLIST  // Removed STATIC

   LOCAL nGet, oGet, nHotPos, cKey, cCaption, nStart, nEnd
   LOCAL nIteration, lGUI

   IF nKey >= K_ALT_Q .AND. nKey <= K_ALT_P
      cKey := SubStr( "qwertyuiop", nKey - K_ALT_Q + 1, 1 )

   ELSEIF nKey >= K_ALT_A .AND. nKey <= K_ALT_L
      cKey := SubStr( "asdfghjkl", nKey - K_ALT_A + 1, 1 )

   ELSEIF nKey >= K_ALT_Z .AND. nKey <= K_ALT_M
      cKey := SubStr( "zxcvbnm", nKey - K_ALT_Z + 1, 1 )

   ELSEIF nKey >= K_ALT_1 .AND. nKey <= K_ALT_0
      cKey := SubStr( "1234567890", nKey - K_ALT_1 + 1, 1 )

   ELSE
      RETURN 0

   ENDIF

   nStart := ::nPos + 1
   nEnd   := Len( ::aGetList )

   FOR nIteration := 1 TO 2
      FOR nGet := nStart TO nEnd

         oGet  := ::aGetList[ nGet ]

         IF HB_ISOBJECT( oGet:Control ) .AND. ;
               oGet:Control:ClassName() != "TBROWSE"
            cCaption := oGet:Control:Caption

         ELSE
            cCaption := oGet:Caption

         ENDIF

         IF ( nHotPos := At( "&", cCaption ) ) == 0

         ELSEIF nHotPos == Len( cCaption )

         ELSEIF Lower( SubStr( cCaption, nHotPos + 1, 1 ) ) == cKey

            // Test the current GUI-GET or Get PostValidation:
            lGUI := HB_ISOBJECT( ::aGetList[ ::nPos ]:Control )

            IF lGUI .AND. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control, oGetMsg )
               ::nNextGet := 0
               RETURN 0

            ELSEIF !lGUI .AND. !::GetPostValidate( ::aGetList[ ::nPos ], oGetMsg )
               RETURN 0

            ENDIF

            // Test the next GUI-GET or Get PreValidation:
            lGUI := HB_ISOBJECT( oGet:Control )

            IF lGUI .AND. !::GUIPreValidate( oGet:Control, oGetMsg )
               // return 0  // Commented out.
               RETURN nGet  // Changed.

            ELSEIF !lGUI .AND. !::GetPreValidate( oGet, oGetMsg )
               // return 0  // Commented out.
               RETURN nGet  // Changed.

            ENDIF

            RETURN ( nGet )
         ENDIF
      NEXT

      nStart := 1
      nEnd   := ::nPos - 1

   NEXT

   RETURN  0

#endif
