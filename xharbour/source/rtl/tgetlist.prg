/*
 * $Id: tgetlist.prg,v 1.19 2003/09/13 20:37:30 walito Exp $
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

   #ifdef HB_COMPAT_C53
   Data nHitcode
   Data nNextGet
   Data nMenuID
   Data nSaveCursor
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
   METHOD ReadExit( lNew ) INLINE IF( ISLOGICAL(lNew), Set( _SET_EXIT, lNew ), Set( _SET_EXIT ) )
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
   ::lKillRead      := .f.
   ::lBumpTop       := .f.
   ::lBumpBot       := .f.
   ::nLastExitState := 0
   ::nLastPos       := 0
   ::cReadProcName  := ""
   ::lUpdated       := .f.
   ::nPos           := 1
   ::oGet           := iif( ISARRAY( GetList ) .AND. Len( GetList ) >= 1, GetList[ 1 ], NIL )
   ::lHasFocus      := .F.

   #ifdef HB_COMPAT_C53
   ::nHitCode       := 0
   ::nNextGet       := 0
   ::nMenuID        := 0
   #endif

return Self

METHOD SetFocus() CLASS HBGetList

   __GetListSetActive( Self )
   __GetListLast( Self )
   ::aGetList[ ::nPos ]:SetFocus()

   return Self

#ifdef HB_COMPAT_C53
METHOD Reader( oMenu, oGetMsg ) CLASS HBGetList

   local oGet := ::oGet, nKey, nRow, nCol, nCursor
   local lDelEnd := .F. // Flag to reset Get when postblock return .F.

   if ::nLastExitState == GE_SHORTCUT .OR.;
      ::nLastExitState == GE_MOUSEHIT .OR.;
      ::GetPreValidate( oGet, oGetMsg )

      oGetMsg:Show( oGet )

      ::nHitCode       := 0
      ::nLastExitState := 0
      oGet:SetFocus()

      while oGet:ExitState == GE_NOEXIT .and. !::lKillRead
         IF oGet:Buffer == ""
            oGet:ExitState := GE_ENTER
         ENDIF

         if oGet:typeOut
            oGet:ExitState := GE_ENTER
         endif

         if oGet:buffer == NIL
            oGet:ExitState := GE_ENTER
         endif

         while oGet:exitState == GE_NOEXIT .and. !::lKillRead
            setCursor( iif( ::nSaveCursor == SC_NONE, SC_NORMAL, ::nSaveCursor ) )
            nKey := INKEY( 0 )
            setCursor( SC_NONE )
            ::GetApplyKey( nKey, oMenu, oGetMsg, lDelEnd )
            oGetMsg:Show( oGet )
         end

         if       ::nLastExitState == GE_SHORTCUT
         elseif   ::nLastExitState == GE_MOUSEHIT
         elseif ! ::GetPostValidate( oGet, oGetMsg )
            oGet:ExitState := GE_NOEXIT
            // postblock returns .F., set flag to reset get
            lDelEnd := .T.
         endif
      end

      nRow    := ROW()
      nCol    := COL()
      nCursor := SETCURSOR()
      oGet:killFocus()
      SETCURSOR( nCursor )
      SETPOS( nRow, nCol )
      oGetMsg:Erase()

   endif

return Self

#else   // C52

METHOD Reader() CLASS HBGetList

   local oGet := ::oGet
   local lDelEnd := .F.

   if ::GetPreValidate()

      oGet:SetFocus()

      while oGet:ExitState == GE_NOEXIT
         IF oGet:Buffer == ""
            oGet:ExitState := GE_ENTER
         ENDIF

         if oGet:typeOut
            oGet:ExitState := GE_ENTER
         endif

         if oGet:buffer == NIL
            oGet:ExitState := GE_ENTER
         endif

         while oGet:exitState == GE_NOEXIT
            ::GetApplyKey( Inkey( 0 ), lDelEnd )
         end

         if ! ::GetPostValidate()
            lDelEnd := .T.
            oGet:ExitState := GE_NOEXIT
         endif
      end

      oGet:killFocus()
   endif

return Self
#endif  // HB_COMPAT_C53


#ifdef HB_COMPAT_C53
METHOD GetApplyKey( nKey, oMenu, oGetMsg, lDelEnd ) CLASS HBGetList

   local oGet := ::oGet
   local cKey
   local bKeyBlock
   local nMouseRow, nMouseColumn
   local nButton
   local nHotItem

   if ! ( ( bKeyBlock := Setkey( nKey ) ) == NIL )
      if ::GetDoSetKey( bKeyBlock )
        return Self
      endif
   endif

   if ( !( ::aGetList == NIL ) .AND. ;
    ( ( nHotItem := ::Accelerator( nKey, oGetMsg ) ) != 0 ) )

      oGet:ExitState   := GE_SHORTCUT
      ::nNextGet       := nHotItem
      ::nLastExitState := GE_SHORTCUT

   elseif ( !( VALTYPE( oMenu ) == "O" ) )
   elseif ( ( nHotItem := oMenu:GetAccel( nKey ) ) != 0 )
      ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
      nKey := 0

   ELSEIF ( oMenu:IsShortCut( nKey )  )
      nKey := 0

   endif

#else

METHOD GetApplyKey( nKey, lDelEnd ) CLASS HBGetList

   local cKey, bKeyBlock, oGet := ::oGet

   if ! ( ( bKeyBlock := Setkey( nKey ) ) == NIL )
      ::GetDoSetKey( bKeyBlock )
      return Self
   endif

#endif

   Switch nKey
      case K_UP
         oGet:ExitState := GE_UP
         exit

      case K_SH_TAB
         oGet:ExitState := GE_UP
         exit

      case K_DOWN
         oGet:ExitState := GE_DOWN
         exit

      case K_TAB
         oGet:ExitState := GE_DOWN
         exit

      case K_ENTER
         oGet:ExitState := GE_ENTER
         exit

      case K_ESC
         if Set( _SET_ESCAPE )
            oGet:UnDo()
            oGet:ExitState := GE_ESCAPE
         endif
         exit

      case K_PGUP
         oGet:ExitState := GE_WRITE
         exit

      case K_PGDN
         oGet:ExitState := GE_WRITE
         exit

      case K_CTRL_HOME
         oGet:ExitState := GE_TOP
         exit

   #ifdef CTRL_END_SPECIAL
      case K_CTRL_END
         oGet:ExitState := GE_BOTTOM
         exit
   #else
      case K_CTRL_W
         oGet:ExitState := GE_WRITE
         exit
   #endif

      case K_INS
         Set( _SET_INSERT, ! Set( _SET_INSERT ) )
         ::ShowScoreboard()
         exit

#ifdef HB_COMPAT_C53
      case K_LBUTTONDOWN
      case K_LDBLCLK
         nMouseRow    := mROW()
         nMouseColumn := mCOL()

         IF ( !( VALTYPE( oMenu ) == "O" ) )
            nButton := 0

         ELSEIF ( !( oMenu:ClassName() == "TOPBARMENU" ) )
            nButton := 0

         ELSEIF ( ( nButton := oMenu:HitTest( nMouseRow, nMouseColumn ) ) != 0 )
            ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
            nButton := 1

         ENDIF


         IF ( nButton != 0 )

         elseif (nButton := oGet:HitTest( nMouseRow, nMouseColumn )) == HTCLIENT

            while  oGet:Col+oGet:Pos-1 > nMouseColumn
               oGet:Left()

               // Handle editing buffer if first character is non-editable:
               if oGet:typeOut
                  // reset typeout:
                  oGet:Home()
                  exit
               endif

            end

            while  oGet:Col+oGet:Pos-1 < nMouseColumn
               oGet:Right()

               // Handle editing buffer if last character is non-editable:
               if oGet:typeOut
                  // reset typeout:
                  oGet:End()
                  exit
               endif

            end

         elseif !( nButton == HTNOWHERE )

         elseif !( ::aGetList == NIL ) .AND.;
            ::HitTest( nMouseRow, nMouseColumn, oGetMsg ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT

         else
            oGet:ExitState := GE_NOEXIT

         endif
         exit
#endif

      case K_UNDO
         oGet:UnDo()
         exit

      case K_HOME
         oGet:Home()
         exit

      case K_END
         oGet:End()
         exit

      case K_RIGHT
         oGet:Right()
         exit

      case K_LEFT
         oGet:Left()
         exit

      case K_CTRL_RIGHT
         oGet:WordRight()
         exit

      case K_CTRL_LEFT
         oGet:WordLeft()
         exit

      case K_BS
         oGet:BackSpace()
         exit

      case K_DEL
         oGet:Delete()
         exit

      case K_CTRL_T
         oGet:DelWordRight()
         exit

      case K_CTRL_Y
         oGet:DelEnd()
         exit

      case K_CTRL_BS
         oGet:DelWordLeft()
         exit

      Default

         if nKey >= 32 .and. nKey <= 255
            // clear buffer and get window when postblock returns .F.
            if lDelEnd
               oGet:DelEnd()
            endif
            cKey := Chr( nKey )
            if oGet:type == "N" .and. ( cKey == "." .or. cKey == "," )
               oGet:ToDecPos()
            else
               if Set( _SET_INSERT )
                  oGet:Insert( cKey )
               else
                  oGet:OverStrike( cKey )
               endif

               if oGet:TypeOut
                  if Set( _SET_BELL )
                     ?? Chr( 7 )
                  endif
                  if ! Set( _SET_CONFIRM )
                     oGet:ExitState := GE_ENTER
                  endif
               endif
            endif
         endif
      end

return Self

#ifdef HB_COMPAT_C53
METHOD GetPreValidate( oGet, oGetMsg ) CLASS HBGetList
   local lUpdated, lWhen := .t., aMsg := { FALSE }
   local xValue

   if ISNIL(oGet)
     oGet := ::oGet
   endif

   if ISOBJECT(oGetMsg)
     aMsg := oGetMsg:aMsg
   endif
#else
METHOD GetPreValidate() CLASS HBGetList
   local oGet := ::oGet
   local lUpdated, lWhen := .t.
   local xValue
#endif

   if oGet:PreBlock != NIL
      xValue    := oGet:VarGet()
      oGet:type := ValType( xValue )
      lUpdated  := ::lUpdated

#ifdef HB_COMPAT_C53
      lWhen := Eval( oGet:PreBlock, oGet, aMsg )
      if !( VALTYPE( oGet:Control ) == "O" ) .AND. !lWhen
        oGet:Display()
      elseif ValType( xValue ) != ValType( oGet:VarGet() ) .or.;
           oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      else
         oGet:Display()
      endif
#else
      lWhen := Eval( oGet:PreBlock, oGet )
      if ValType( xValue ) != ValType( oGet:VarGet() ) .or.;
           oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      else
         oGet:Display()
      endif
#endif

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      if !( __GetListActive() == Self )
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )
   endif

   if ::lKillRead
      lWhen := .f.
      oGet:ExitState := GE_ESCAPE
   elseif ! lWhen
      oGet:ExitState := GE_WHEN
   else
      oGet:ExitState := GE_NOEXIT
   end

return lWhen


#ifdef HB_COMPAT_C53
METHOD GetPostValidate( oGet, oGetMsg ) CLASS HBGetList
   local lUpdated, lValid := .t., aMsg := { FALSE }, nCursor
   local xValue

   if ISNIL(oGet)
     oGet := ::oGet
   endif

   if ISOBJECT(oGetMsg)
     aMsg := oGetMsg:aMsg
   endif
#else
METHOD GetPostValidate() CLASS HBGetList
   local oGet := ::oGet
   local lUpdated, lValid := .t.
   local xValue
#endif

   if oGet:ExitState == GE_ESCAPE
      return .t.
   endif

   if oGet:BadDate()
//      oGet:SetFocus()
      oGet:TypeOut := .f.
      ::DateMsg()
      ::ShowScoreboard()
      return .f.
   endif

   if oGet:Changed
      oGet:Assign()
      ::lUpdated := .t.
   endif

#ifdef HB_COMPAT_C53
   nCursor := SETCURSOR()
   oGet:reset()
   SETCURSOR( nCursor )
#else
   oGet:Reset():Display()
#endif

   if oGet:PostBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated
      SetPos( oGet:Row, oGet:Col + IIF( oGet:Buffer == NIL, 0, Len( oGet:Buffer ) ) )

#ifdef HB_COMPAT_C53
      lValid := Eval( oGet:PostBlock, oGet, aMsg )
#else
      lValid := Eval( oGet:PostBlock, oGet )
#endif

      SetPos( oGet:Row, oGet:Col )

      if ValType( xValue ) != ValType( oGet:VarGet() ) .or.;
           oGet:VarGet() != xValue
           oGet:VarPut( oGet:VarGet() )
      endif
      oGet:UpdateBuffer()

      ::ShowScoreBoard()

#ifdef HB_COMPAT_C53
      ::lUpdated := IIF( oGet:changed, .T., lUpdated )
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
         lValid := .t.
      endif
   endif

return lValid

METHOD GetDoSetKey( bKeyBlock ) CLASS HBGetList

   local oGet := ::oGet, lUpdated, lSetKey

   if oGet:Changed
      oGet:Assign()
      ::lUpdated := .t.
   endif

   lUpdated := ::lUpdated

   lSetKey := Eval( bKeyBlock, ::cReadProcName, ::nReadProcLine, ::ReadVar() )

   IF ( !( VALTYPE( lSetKey ) == "L" ) )
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
   endif

return lSetKey

#ifdef HB_COMPAT_C53
METHOD Settle( nPos, lInit ) CLASS HBGetList
#else
METHOD Settle( nPos ) CLASS HBGetList
#endif
   local nExitState

   if nPos == NIL
      nPos := ::nPos
   endif

   if nPos == 0
      nExitState := GE_DOWN
   #ifdef HB_COMPAT_C53
   ELSEIF ( nPos > 0 .AND. lInit)
      nExitState := GE_NOEXIT
   #endif
   else
      nExitState := ::aGetList[ nPos ]:ExitState
   endif

   if nExitState == GE_ESCAPE .or. nExitState == GE_WRITE
      return 0
   endif

   if nExitState != GE_WHEN
      ::nLastPos := nPos
      ::lBumpTop := .f.
      ::lBumpBot := .f.
   else
#ifdef HB_COMPAT_C53
      if ::nLastExitState != 0
         nExitState := ::nLastExitState
      elseif ::nNextGet < ::nLastPos
         nExitState := GE_UP
      else
         nExitState := GE_DOWN
      endif
#else
      nExitState := ::nLastExitState
#endif
   endif

   Switch nExitState
      case GE_UP
         nPos--
         exit

      case GE_DOWN
         nPos++
         exit

      case GE_TOP
         nPos := 1
         ::lBumpTop := .T.
         nExitState := GE_DOWN
         exit

      case GE_BOTTOM
         nPos := Len( ::aGetList )
         ::lBumpBot := .t.
         nExitState := GE_UP
         exit

      case GE_ENTER
         nPos++
         exit

#ifdef HB_COMPAT_C53
      case GE_SHORTCUT
         return ::nNextGet

      case GE_MOUSEHIT
         return ::nNextGet
#endif

   end

   if nPos == 0
      if ! ::ReadExit() .and. ! ::lBumpBot
         ::lBumpTop := .t.
         nPos       := ::nLastPos
         nExitState := GE_DOWN
      endif

   elseif nPos == Len( ::aGetList ) + 1
      if ! ::ReadExit() .and. nExitState != GE_ENTER .and. ! ::lBumpTop
         ::lBumpBot := .t.
         nPos       := ::nLastPos
         nExitState := GE_UP
      else
         nPos := 0
      endif
   endif

   ::nLastExitState := nExitState

   if nPos != 0
      ::aGetList[ nPos ]:ExitState := nExitState
   endif

return nPos

METHOD PostActiveGet() CLASS HBGetList

   ::GetActive( ::oGet )
   ::ReadVar( ::GetReadVar() )
   ::ShowScoreBoard()

return Self

METHOD GetReadVar() CLASS HBGetList

   local oGet := ::oGet
   local cName := Upper( oGet:Name )
   local n

   if oGet:Subscript != NIL
      for n := 1 TO Len( oGet:Subscript )
         cName += "[" + LTrim( Str( oGet:Subscript[ n ] ) ) + "]"
      next
   end

return cName

METHOD SetFormat( bFormat, lSet ) CLASS HBGetList

   local bSavFormat := ::bFormat

   if ! ISNIL(bFormat) .or. lSet
     ::bFormat := bFormat
   endif

return bSavFormat

METHOD KillRead( lKill ) CLASS HBGetList

   local lSavKill := ::lKillRead

   if PCount() > 0
      ::lKillRead := lKill
   endif

return lSavKill

METHOD GetActive( oGet ) CLASS HBGetList

   local oOldGet := ::oActiveGet

   if PCount() > 0
      ::oActiveGet := oGet
   endif

return oOldGet

METHOD ShowScoreboard() CLASS HBGetList

   local nRow, nCol, nOldCursor

   if Set( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      nOldCursor := SetCursor( SC_NONE )

      DispOutAt( SCORE_ROW, SCORE_COL, iif( Set( _SET_INSERT ), NationMsg( _GET_INSERT_ON ), NationMsg( _GET_INSERT_OFF ) ) )
      SetPos( nRow, nCol )

      SetCursor( nOldCursor )

   endif

return Self

METHOD DateMsg() CLASS HBGetList

   local nRow
   local nCol

   if Set( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      DispOutAt( SCORE_ROW, SCORE_COL, NationMsg( _GET_INVD_DATE ) )
      SetPos( nRow, nCol )

      while NextKey() == 0
      end

      DispOutAt( SCORE_ROW, SCORE_COL, Space( Len( NationMsg( _GET_INVD_DATE ) ) ) )
      SetPos( nRow, nCol )

   endif

return Self

METHOD ReadVar( cNewVarName ) CLASS HBGetList

   local cOldName := ::cVarName

   if ISCHARACTER( cNewVarName )
      ::cVarName := cNewVarName
   endif

return cOldName

METHOD ReadUpdated( lUpdated ) CLASS HBGetList

   local lSavUpdated := ::lUpdated

   if PCount() > 0
      ::lUpdated := lUpdated
   endif

return lSavUpdated

#ifdef HB_COMPAT_C53

METHOD GuiReader( oGet, oMenu, oGetMsg ) CLASS HBGetList

  //Local oGet := ::oGet
   Local oGui

   if ( ValType( oGet:Control ) == "O" ) .AND. ;  // Moved up 2 lines.
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

      while oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Check for initial typeout (no editable positions)
         if ( oGui:typeOut )
            oGet:exitState := GE_ENTER
         endif

         // Apply keystrokes until exit
         while oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            ::GUIApplyKey( oGUI, inkey(0), oMenu, oGetMsg )
            oGetMsg:Show( oGet )
         end

         // Disallow exit if the VALID condition is not satisfied

         if     ::nLastExitState == GE_SHORTCUT  // Added.
         elseif ::nLastExitState == GE_MOUSEHIT  // Added.
         elseif !::GetPostValidate( oGet, oGetMsg )
         // IF ( !::GUIPostValidate( oGet, oGUI, oGetMsg ) ) // Old test.
            oGet:exitState := GE_NOEXIT
         endif
      end

         // De-activate the GET
      IF ( ( oGUI:ClassName() $ "LISTBOX_RADIOGROUP" ) .AND. ;
         VALTYPE( oGet:VarGet() ) == "N" )
         // Need to test the Value here:
         oGet:VarPut( oGUI:Value )
      ELSE
         oGet:VarPut( oGUI:Buffer )
      ENDIF
      oGUI:killFocus()

      oGetMsg:Erase()

      if ( ! oGUI:ClassName() == "LISTBOX" )
      elseif ( ! oGUI:DropDown )
      elseif ( oGUI:IsOpen )
         oGUI:Close()
      endif

   endif

   return Self

METHOD GUIApplyKey(  oGUI, nKey, oMenu, oGetMsg ) CLASS HBGetList
   Local oGet := ::oGet
   Local bKeyBlock
   Local nMouseRow, nMouseColumn
   Local nButton
   Local oTheClass
   Local nHotItem
   Local lClose

   // Check for SET KEY first
   if !( bKeyBlock := setkey( nKey ) ) == NIL
      if ::GetDoSetKey( bKeyBlock )
        return Self
      endif
   endif

   IF ( nHotItem := ::Accelerator( nKey, oGetMsg ) ) != 0
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet := nHotItem

   ELSEIF ( !( VALTYPE( oMenu ) == "O" ) )

   ELSEIF ( ( nHotItem := oMenu:GetAccel( nKey ) ) != 0 )
      ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
      nKey := 0

   ELSEIF ( oMenu:IsShortCut( nKey )  )
      nKey := 0

   endif

   if nKey == 0
   elseif ( oTheClass := oGUI:ClassName() ) == "RADIOGROUP"
      if nKey == K_UP
         oGUI:PrevItem()
         nKey := 0

      elseif nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0

      elseif ( nHotItem := oGUI:GetAccel( nKey ) ) != 0
         oGUI:Select( nHotItem )

      endif

      if valtype( oGet:VarGet() ) == "N"
         oGet:VarPut( oGUI:Value )
      endif

   elseif oTheClass == "CHECKBOX"
      if nKey == K_SPACE
         oGUI:Select()
      endif

   elseif oTheClass == "PUSHBUTTON"
      if nKey == K_SPACE
         oGUI:Select( K_SPACE )

      elseif nKey == K_ENTER
         oGUI:Select()
         nKey := 0

      endif

   elseif oTheClass == "LISTBOX"

      if nKey == K_UP
         oGUI:PrevItem()
         nKey := 0
      elseif nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0
      elseif nKey == K_SPACE
         if ! oGUI:DropDown
         elseif ! oGUI:IsOpen
            oGUI:Open()
            nKey := 0
         endif
      elseif ( nButton := oGUI:FindText( chr(nKey), oGUI:Value+1, .f., .f. )) != 0
         oGUI:Select( nButton )
      endif

      if valtype( oGet:VarGet() ) == "N"
         oGet:VarPut( oGui:Value )
      endif

   endif

   Switch nKey
   case K_UP
      oGet:ExitState := GE_UP
      exit

   case K_SH_TAB
      oGet:ExitState := GE_UP
      exit

   case K_DOWN
      oGet:ExitState := GE_DOWN
      exit

   case K_TAB
      oGet:ExitState := GE_DOWN
      exit

   case K_ENTER
      oGet:ExitState := GE_ENTER
      exit

   case K_ESC
      if set( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      endif
      exit

   case K_PGUP
      oGet:ExitState := GE_WRITE
      exit

   case K_PGDN
      oGet:ExitState := GE_WRITE
      exit

   case K_CTRL_HOME
      oGet:ExitState := GE_TOP
      exit

#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   case K_CTRL_END
      oGet:ExitState := GE_BOTTOM
      exit

#else

   // Both ^W and ^End terminate the READ (the default)
   case K_CTRL_W
      oGet:ExitState := GE_WRITE
      exit

#endif

   case K_LBUTTONDOWN
   case K_LDBLCLK
      nMouseRow    := mROW()
      nMouseColumn := mCOL()

      IF ( !( VALTYPE( oMenu ) == "O" ) )
         nButton := 0

      ELSEIF ( !( oMenu:ClassName() == "TOPBARMENU" ) )
         nButton := 0

      ELSEIF ( ( nButton := oMenu:HitTest( nMouseRow, nMouseColumn ) ) != 0 )
         ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
         nButton := 1

      ENDIF

      lClose := .T.

      if ( nButton != 0 )
      elseif ( nButton := oGUI:HitTest( nMouseRow, nMouseColumn )) == HTNOWHERE
         // Changed test:
         if ::HitTest( nMouseRow, nMouseColumn, oGetMsg  ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT  // Added.
         else
            oGet:ExitState := GE_NOEXIT
         endif

      elseif nButton >= HTCLIENT
         oGUI:Select( nButton )

      elseif nButton == HTDROPBUTTON
         if !oGUI:IsOpen
            oGUI:Open()
            lClose := .F.
         endif

      elseif nButton >= HTSCROLLFIRST .and. nButton <= HTSCROLLLAST
         oGUI:Scroll( nButton )
         lClose := .F.

      endif

      if ! lClose
      elseif ! oTheClass == "LISTBOX"
      elseif ! oGUI:DropDown
      elseif oGUI:IsOpen
         oGUI:Close()
         oGUI:Display()
      endif
      exit

   end

   return Self

METHOD GUIPreValidate( oGUI, oGetMsg ) CLASS HBGetList
   Local oGet := ::oGet
   Local lUpdated
   Local lWhen := .T.
   Local xValue

   if !( oGet:preBlock == NIL )
      xValue      := oGet:VarGet()
      oGet:type   := ValType( xValue )
      lUpdated := ::lUpdated

      lWhen := eval( oGet:preBlock, oGet, oGetMsg:aMsg )

      if ValType( xValue ) != ValType( oGet:VarGet() ) .or.;
           oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      elseif ( !( oGUI:ClassName() == "TBROWSE" ) )
         oGet:Display()
      endif

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      if !( __GetListActive() == Self )
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )
   endif

   if ::lKillRead
      lWhen := .F.
      oGet:ExitState := GE_ESCAPE

   elseif !lWhen
      oGet:ExitState := GE_WHEN

   else
      oGet:ExitState := GE_NOEXIT

   endif

   return lWhen

METHOD GUIPostValidate( oGUI, oGetMsg ) CLASS HBGetList
   Local oGet := ::oGet
   Local lUpdated
   Local lValid := .T.
   Local uOldData, uNewData
   Local xValue

   if oGet:exitState == GE_ESCAPE
      return .t.                   // NOTE
   endif

   if oGet:BadDate()
//      oGet:SetFocus()
      oGet:TypeOut := .f.
      ::DateMsg()
      ::ShowScoreboard()
      return .f.
   endif

   if oGet:Changed
      oGet:Assign()
      ::lUpdated := .t.
   endif

   oGet:Reset():Display()

   IF ( !( oGUI:ClassName() == "TBROWSE" ) )
      uOldData := oGet:VarGet()

      IF ( oGUI:ClassName() $ "LISTBOX_RADIOGROUP" .AND. ;
         VALTYPE( oGet:VarGet() ) == "N" )
         uNewData := oGUI:Value
      ELSE
         uNewData := oGUI:Buffer
      ENDIF

   ENDIF

   // If editing occurred, assign the new value to the variable
   if !( uOldData == uNewData )
      oGet:VarPut( uNewData )
      ::lUpdated := .T.
   endif

   // Check VALID condition if specified
   if !( oGet:postBlock == NIL )

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      lValid := eval( oGet:postBlock, oGet, oGetMsg:aMsg )

      // Reset S'87 compatibility cursor position
      setpos( oGet:Row, oGet:Col )

      if ValType( xValue ) != ValType( oGet:VarGet() ) .or.;
           oGet:VarGet() != xValue
         oGet:VarPut( oGet:VarGet() )
      endif
      oGet:UpdateBuffer()

      ::ShowScoreBoard()
      if ! ( oGUI:ClassName == "TBROWSE" )
         oGUI:Select( oGet:VarGet() )
      endif

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
      endif

   endif

   return  lValid

METHOD TBReader( oGet, oMenu,  oGetMsg ) Class HBGETLIST
   Local oTB, nKey, lAutoLite, nSaveCursor, nProcessed
//   Local nRow, nCol
//   Local oGui := oGet:control

   // Read the GET if the WHEN condition is satisfied
   if VALTYPE( oGet:control ) == "O" .AND. ;    // Moved up 2 lines.
        ::nLastExitState == GE_SHORTCUT .OR. ;  // Added.
        ::nLastExitState == GE_MOUSEHIT .OR. ;  // Added.
        ::GetPreValidate( oGet, oGetMsg )

      oGetMsg:Show( oGet )
      ::nLastExitState := 0  // Added.

      nSaveCursor := SetCursor( SC_NONE )

      // Activate the GET for reading
      oTB := oGet:Control

      lAutoLite := oTB:Autolite
      oTB:Autolite := .T.
      oTB:Hilite()

      if oGet:exitState == GE_NOEXIT  // Added.
         if ::nHitcode == HTCELL
            //TraceLog( 'hitcode ',::nHitcode )
            // Replaces call to TBMouse( oTB, mROW(), mCOL() ):
            oTB:RowPos := oTb:mRowPos
            oTB:ColPos := oTb:mColPos
            oTB:Invalidate()
         endif
      endif  // Added.

      ::nHitcode := 0

      while  oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Apply keystrokes until exit
         while oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            nKey := 0

            while !oTB:Stabilize() .and. nKey == 0
               nKey := Inkey()
            end

            if nKey == 0
               nKey := Inkey(0)
            endif

            nProcessed := oTB:ApplyKey( nKey )
            if nProcessed == TBR_EXIT
               oGet:exitState := GE_ESCAPE
               exit

            elseif nProcessed == TBR_EXCEPTION
               ::TBApplyKey( oGet, oTB, nKey, oMenu, oGetMsg )

               // nRow := ROW()  // Commented out.
               // nCol := COL()  // Commented out.
               oGetMsg:Show( oGet )
               // DevPos( nRow, nCol )  // Commented out.

            endif

         end

         // Disallow exit if the VALID condition is not satisfied
         if ::nLastExitState == GE_SHORTCUT       // Added.
         elseif ::nLastExitState == GE_MOUSEHIT   // Added.
         elseif !::GetPostValidate( oGet, oGetMsg )  // Changed.
         // if !::GUIPostValidate( oGUI, oGetMsg ) // Old test.
            oGet:ExitState := GE_NOEXIT
         endif

      end

      // De-activate the GET
      oTB:Autolite := lAutoLite
      oTB:DeHilite()

      oGetMsg:Erase()

      SetCursor( nSaveCursor )
   endif

   return Self

METHOD TBApplyKey( oGet, oTB, nKey, oMenu, oGetMsg ) CLASS HBGETLIST

   Local bKeyBlock
   Local nMouseRow, nMouseColumn
   Local nButton
   Local nHotItem

   // Check for SET KEY first
   if !(( bKeyBlock := SETKEY( nKey )) == NIL )
      if ::GetDoSetKey( bKeyBlock )
         return Self
      endif
   endif

   IF ( nHotItem := ::Accelerator( nKey, oGetMsg ) ) != 0
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet := nHotItem

   ELSEIF ( !( VALTYPE( oMenu ) == "O" ) )
   ELSEIF ( ( nHotItem := oMenu:GetAccel( nKey ) ) != 0 )
      ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
      nKey := 0

   ELSEIF ( oMenu:IsShortCut( nKey )  )
      nKey := 0

   ENDIF

   Switch nKey
   case K_TAB
      oGet:ExitState := GE_DOWN
      exit

   case K_SH_TAB
      oGet:ExitState := GE_UP
      exit

   case K_ENTER
      if !oTb:Stable()
         oTb:ForceStable()
      endif
      oGet:ExitState := GE_ENTER
      exit

   case K_ESC
      if set( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      endif
      exit

#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   case K_CTRL_END
      oGet:ExitState := GE_BOTTOM
      exit

#else

   // Both ^W and ^End terminate the READ (the default)
   case K_CTRL_W
      oGet:ExitState := GE_WRITE
      exit

#endif

   case K_LBUTTONDOWN
   case K_LDBLCLK
      nMouseRow    := mROW()
      nMouseColumn := mCOL()

      IF ( !( VALTYPE( oMenu ) == "O" ) )
         nButton := 0

      ELSEIF ( !( oMenu:ClassName() == "TOPBARMENU" ) )
         nButton := 0

      ELSEIF ( ( nButton := oMenu:HitTest( nMouseRow, nMouseColumn ) ) != 0 )
         ::nMenuID := oMenu:ModalGet( nHotItem, oGetMsg )
         nButton := 1

      ENDIF

      IF ( nButton != 0 )
      elseif (nButton := oTB:HitTest( nMouseRow, nMouseColumn ) ) == HTNOWHERE // Changed test:
         if ::HitTest(  nMouseRow, nMouseColumn, oGetMsg ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitStat := GE_MOUSEHIT
         else
            oGet:ExitState := GE_NOEXIT

         endif
      endif
      exit

   end

   return self


METHOD  HitTest( nMouseRow, nMouseCol, oGetMsg ) CLASS HBGETLIST
   Local oGet, lGUI

   ::nNextGet := 0

   for each oGet in ::aGetList
      if ( ::nHitCode := oGet:HitTest( nMouseRow, nMouseCol ) ) != HTNOWHERE
         ::nNextGet := HB_EnumIndex()
         exit
      endif
   next

   if !( ::nNextGet == 0 )  // Changed.

      // Test the current GUI-GET or Get PostValidation:
      lGUI := valtype( ::aGetList[ ::nPos ]:Control ) == "O"

      if lGUI .and. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control, oGetMsg )

         ::nNextGet := 0
         return 0

      elseif !lGUI .and. !::GetPostValidate( ::aGetList[ ::nPos ], oGetMsg )

         ::nNextGet := 0
         return 0

      endif

      // Test the next GUI-GET or Get PreValidation:
      lGUI := valtype( ::aGetList[ ::nNextGet ]:Control ) == "O"

      if lGUI .and. !::GUIPreValidate( ::aGetList[ ::nNextGet ]:Control, oGetMsg )

         ::nNextGet := 0
         return ::nNextGet

      elseif !lGUI .and. !::GetPreValidate( ::aGetList[ ::nNextGet ], oGetMsg )

         ::nNextGet := 0
         return ::nNextGet

      endif

      return ::nNextGet

   endif

   return 0


METHOD Accelerator( nKey, oGetMsg ) CLASS HBGETLIST  // Removed STATIC

   Local nGet, oGet, nHotPos, cKey, cCaption, nStart, nEnd
   Local nIteration, lGUI

   if nKey >= K_ALT_Q .and. nKey <= K_ALT_P
      cKey := substr( "qwertyuiop", nKey - K_ALT_Q + 1, 1 )

   elseif nKey >= K_ALT_A .and. nKey <= K_ALT_L
      cKey := substr( "asdfghjkl", nKey - K_ALT_A + 1, 1 )

   elseif nKey >= K_ALT_Z .and. nKey <= K_ALT_M
      cKey := substr( "zxcvbnm", nKey - K_ALT_Z + 1, 1 )

   elseif nKey >= K_ALT_1 .and. nKey <= K_ALT_0
      cKey := substr( "1234567890", nKey - K_ALT_1 + 1, 1 )

   else
      return 0

   endif

   nStart := ::nPos + 1
   nEnd   := len( ::aGetList )

   for nIteration := 1 to 2
       for nGet := nStart to nEnd

          oGet  := ::aGetList[ nGet ]

          if valtype( oGet:Control ) == "O" .and. ;
                      oGet:Control:ClassName() != "TBROWSE"
             cCaption := oGet:Control:Caption

          else
             cCaption := oGet:Caption

          endif

          if ( nHotPos := at( "&", cCaption ) ) == 0

          elseif nHotPos == len( cCaption )

          elseif lower( substr( cCaption, nHotPos + 1, 1 ) ) == cKey

             // Test the current GUI-GET or Get PostValidation:
             lGUI := valtype( ::aGetList[ ::nPos ]:Control ) == "O"

             if lGUI .and. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control, oGetMsg )
                ::nNextGet := 0
                return 0

             elseif !lGUI .and. !::GetPostValidate( ::aGetList[ ::nPos ], oGetMsg )
                return 0

             endif

             // Test the next GUI-GET or Get PreValidation:
             lGUI := valtype( oGet:Control ) == "O"

             if lGUI .and. !::GUIPreValidate( oGet:Control, oGetMsg )
                // return 0  // Commented out.
                return nGet  // Changed.

             elseif !lGUI .and. !::GetPreValidate( oGet, oGetMsg )
                // return 0  // Commented out.
                return nGet  // Changed.

             endif

             return ( nGet )
          endif
       next

       nStart := 1
       nEnd   := ::nPos - 1

   next

   return  0

#endif


