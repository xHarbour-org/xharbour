/*
 * $Id: memoedit.prg,v 1.21 2004/05/15 17:15:00 modalsist Exp $
 */

/*
 * Harbour Project source code:
 * MemoEdit() function
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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
//-------------------------------------------------------------------//

/*
 * Eduardo Fernandes <eduardo@modalsistemas.com.br>
 *
 * v.1.19
 * Revision to proper working with tab columns, CTRL_T behaviour and other things.
 * See teditor.prg and ttextlin.prg to more details.
 *
 * v.1.20
 * Fixed bug in read only mode when call an user function and press Tab key, reported by 
 * Stephan Hennekens. 
 * 
 * v.1.21 - 2004/05/15
 * Fixed word-wrap control in memoedit.prg through <nLineLength> parameter.
 * 
 * if nLineLength < 0 then word-wrap = false like Clipper.
 * if nLineLength = 0 or null, then word-wrap = true and wordwrapcol = nRight - nLeft + 1.
 * if nLineLength > 0 then word-wrap = true and wordwrapcol = nLineLength
 *
 */

#include "common.ch"
#include "hbclass.ch"
#include "memoedit.ch"
#include "inkey.ch"

//-------------------------------------------------------------------//
//
// A specialized HBEditor which can simulate MemoEdit() behaviour
//
CLASS TMemoEditor FROM HBEditor

   DATA  xUserFunction                    // User Function called to change default MemoEdit() behaviour

   METHOD MemoInit( cUserFunction )       // This method is called after ::New() returns to perform ME_INIT actions
   METHOD Edit()                          // Calls super:Edit(nKey) but is needed to handle configurable keys
   METHOD KeyboardHook( nKey )            // Gets called every time there is a key not handled directly by HBEditor
   METHOD IdleHook()                      // Gets called every time there are no more keys to hanlde

   METHOD HandleUserKey( nKey,nUserKey )  // Handles keys returned to MemoEdit() by user function
   METHOD xDo( nStatus )                  // Calls xUserFunction saving and restoring cursor position and shape
ENDCLASS

//-------------------------------------------------------------------//

METHOD MemoInit( cUserFunction ) CLASS TMemoEditor

   local nKey

   default cUserFunction to nil

   // Save/Init object internal representation of user function
   //
   ::xUserFunction := cUserFunction

   if ISCHARACTER( ::xUserFunction )
      // Keep calling user function until it returns 0
      //
      while ( nKey := ::xDo( ME_INIT ) ) <> ME_DEFAULT

         // At this time there is no input from user of MemoEdit() only handling
         // of values returned by ::xUserFunction, so I pass these value on both
         // parameters of ::HandleUserKey()
         //
         ::HandleUserKey( nKey, nKey )

      enddo

   endif

return Self

//-------------------------------------------------------------------//

METHOD Edit() CLASS TMemoEditor

   local nKey, nUserKey

   // NOTE: K_ALT_W is not compatible with clipper exit memo and save key, but I cannot discriminate
   //       K_CTRL_W and K_CTRL_END from harbour code.
   //
   local aConfigurableKeys := { K_CTRL_Y, K_CTRL_T, K_CTRL_B, K_CTRL_V, K_ALT_W, K_ESC }
   local bKeyBlock

   // If I have an user function I need to trap configurable keys and ask to
   // user function if handle them the standard way or not
   //
   if ::lEditAllow .AND. ISCHARACTER( ::xUserFunction )

      while !( ::lExitEdit )

         // I need to test this condition here since I never block inside HBEditor:Edit()
         // if there is an user function
         //
         if NextKey() == 0
            ::IdleHook()
         endif

         nKey := Inkey( 0 )

         if ( bKeyBlock := Setkey( nKey ) ) <> NIL
            Eval( bKeyBlock, Self )                // 7/01/2004 12:47p.m. Pass Self as parameter
            Loop
         endif

         // Is it a configurable key ?
         //
         if nKey IN aConfigurableKeys
            nUserKey := ::xDo( iif( ::lDirty, ME_UNKEYX, ME_UNKEY ) )
            ::HandleUserKey( nKey, nUserKey )

         else
            super:Edit( nKey )

         endif

      enddo

   else
      // If I can't edit text buffer or there is not a user function enter standard HBEditor
      // ::Edit() method which is able to handle everything
      //
      super:Edit()

   endif

return Self

//-------------------------------------------------------------------//
//
// I come here if I have an unknown key and it is not a configurable key
// if there is an user function I leave to it its handling
//
METHOD KeyboardHook( nKey ) CLASS TMemoEditor

   local nUserKey

   if ISCHARACTER( ::xUserFunction ) .and. ::lEditAllow

      nUserKey := ::xDo( iif( ::lDirty, ME_UNKEYX, ME_UNKEY ) )
      ::HandleUserKey( nKey, nUserKey )

   endif

return Self

//-------------------------------------------------------------------//

METHOD IdleHook() CLASS TMemoEditor

   if ISCHARACTER( ::xUserFunction )
      ::xDo( ME_IDLE )

   endif

return Self

//-------------------------------------------------------------------//

METHOD HandleUserKey( nKey, nUserKey ) CLASS TMemoEditor

   // HBEditor does not handle these keys and would call ::KeyboardHook() causing infinite loop
   //
   Local aUnHandledKeys := { K_CTRL_J,K_CTRL_K,K_CTRL_L,K_CTRL_N,K_CTRL_O,K_CTRL_P,K_CTRL_Q,K_CTRL_U,; // K_CTRL_T is activated.
                             K_F1,K_F2,K_F3,K_F4,K_F5,K_F6,K_F7,K_F8,K_F9,K_F10,K_F11,K_F12,;
                             K_SH_F1,K_SH_F2,K_SH_F3,K_SH_F4,K_SH_F5,K_SH_F6,K_SH_F7,K_SH_F8,K_SH_F9,K_SH_F10,K_SH_F11,K_SH_F12,;
                             K_CTRL_F1,K_CTRL_F2,K_CTRL_F3,K_CTRL_F4,K_CTRL_F5,K_CTRL_F6,K_CTRL_F7,K_CTRL_F8,K_CTRL_F9,K_CTRL_F10,K_CTRL_F11,K_CTRL_F12,;
                             K_ALT_F1,K_ALT_F2,K_ALT_F3,K_ALT_F4,K_ALT_F5,K_ALT_F6,K_ALT_F7,K_ALT_F8,K_ALT_F9,K_ALT_F10,K_ALT_F11,K_ALT_F12 }
                           

   if nUserKey <> nil
      Switch nUserKey
         // I won't reach this point during ME_INIT since ME_DEFAULT ends initialization phase of MemoEdit()
         //
         case ME_DEFAULT
            // HBEditor is not able to handle keys with a value higher than 256
            //
            if ( nKey <= 256 .or. nKey == K_ALT_W .or. nKey == K_CTRL_W ) .AND. !( nKey IN aUnHandledKeys )
               super:Edit( nKey )
            endif
            exit

         // TOFIX: Not clipper compatible, see teditor.prg
         //
         case 1
         case 2
         case 3
         case 4
         case 5
         case 6
         case 7
         case 8
         case 9
         case 10
         case 11
         case 12
         case 13
         case 14
         case 15
         case 16
         case 17 
         case 18
         case 19
         case 20
         case 21
         case 22
         case 23  
         case 24
         case 25
         case 26
         case 27
         case 28
         case 29
         case 30
         case 31
         case K_ALT_W // Not Clipper compatible. Save but don´t exit.
            //if !( nUserKey IN aUnHandledKeys )
            //   super:Edit( nUserKey )
            //endif
            //exit
         case K_CTRL_Q
              if !( nUserKey IN aUnHandledKeys )
                 super:Edit( nUserKey )
              endif
              exit

         case K_CTRL_W
              if !( nUserKey IN aUnHandledKeys )
                 super:Edit( nUserKey )
              endif
              exit

         case ME_DATA
            if nKey <= 255 .AND. !( nKey IN aUnHandledKeys ) // char 255 is used to virtual tab spaces.
               super:Edit( nKey )
            endif
            exit

         case ME_TOGGLEWRAP
            ::lWordWrap := !( ::lWordWrap )
            exit

         case ME_TOGGLESCROLL
            // TODO: HBEditor does not support vertical scrolling of text inside window without moving cursor position
            exit

         case ME_WORDRIGHT
            ::WordRight()  // MoveCursor(K_CTRL_RIGHT)
            exit

         case ME_BOTTOMRIGHT
            ::Bottom()     // MoveCursor(K_CTRL_END)
            exit

         default
            // Do nothing

      end
   endif

return Self

//-------------------------------------------------------------------//

METHOD xDo( nStatus ) CLASS TMemoEditor

   LOCAL nCurRow := ::Row()
   LOCAL nCurCol := ::Col()
   LOCAL nCurCur := SetCursor()
   LOCAL xRes

   xRes := Do( ::xUserFunction, nStatus, ::nRow, ::nCol - 1 )

   //::SetPos( nCurRow, nCurCol )
   SetPos( nCurRow, nCurCol )
   SetCursor( nCurCur )

return xRes

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                  Prg Level Call of MemoEdit()
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

FUNCTION MemoEdit(cString,;         // same as Clipper
                  nTop, nLeft,;     // idem
                  nBottom, nRight,; // idem
                  lEditMode,;       // idem
                  cUserFunction,;   // idem
                  nLineLength,;     // idem
                  nTabSize,;        // idem
                  nTextBuffRow,;    // idem
                  nTextBuffColumn,; // idem
                  nWindowRow,;      // idem
                  nWindowColumn,;   // idem
                  lToggleExitSave)  // new parameter, xHarbour option.
                                    // this change CTRL-W by CTRL-Q to finish edit with save. 
                                    // Default is CTRL-W as Clipper, but in xHarbour CTRL-W 
                                    // have same behaviour as CTRL-END.

   LOCAL oEd 
   
   DEFAULT nTop            TO 0
   DEFAULT nLeft           TO 0
   DEFAULT nBottom         TO MaxRow()
   DEFAULT nRight          TO MaxCol()
   DEFAULT lEditMode       TO .T.
   DEFAULT nLineLength     TO 0 
   DEFAULT nTabSize        TO 4
   DEFAULT nTextBuffRow    TO 1
   DEFAULT nTextBuffColumn TO 0
   DEFAULT nWindowRow      TO 0
   DEFAULT nWindowColumn   TO nTextBuffColumn
   DEFAULT cString         TO ""
   DEFAULT lToggleExitSave TO .F.  // Toogle betewn CTRL-W/CTRL-Q. Default is CTRL-W
   
   if !ISLOGICAL( cUserFunction ) .AND. Empty( cUserFunction )
      cUserFunction = nil
   endif

   // if nLineLength < 0  Word Wrap will be seted to false in HBEditor, otherwise true.
   IF nLineLength == 0
      nLineLength := nRight - nLeft + 1 
   ENDIF   


// Original MemoEdit() converts Tabs into spaces;
//   oEd := TMemoEditor():New( StrTran( cString, Chr( K_TAB ), Space( 1 ) ), nTop, nLeft, nBottom, nRight, ;
//              lEditMode, nLineLength, nTabSize, nTextBuffRow, nTextBuffColumn, nWindowRow, nWindowColumn )

// Now Tabs is trated properly in HBEditor
   oEd := TMemoEditor():New( cString ,;
                             nTop, nLeft,;
                             nBottom, nRight, ;
                             lEditMode,;
                             nLineLength,;
                             nTabSize,;
                             nTextBuffRow,;
                             nTextBuffColumn,;
                             nWindowRow,;
                             nWindowColumn,;
                             lToggleExitSave )

   oEd:MemoInit( cUserFunction )
   oEd:RefreshWindow()

   if ! ISLOGICAL( cUserFunction ) .OR. cUserFunction == .T.
      oEd:Edit()
      if oEd:lSaved
         cString := oEd:GetText()
      endif
   endif

RETURN cString

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

