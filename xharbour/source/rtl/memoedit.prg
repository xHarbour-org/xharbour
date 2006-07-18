/*
 * $Id: memoedit.prg,v 1.37 2006/02/25 17:32:15 lf_sfnet Exp $
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
   METHOD Edit()                          // Calls ::Super:Edit(nKey) but is needed to handle configurable keys
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
      /* Keep calling user function until it returns 0
         05/08/2004 - <maurilio.longo@libero.it>
                      Clipper 5.2 memoedit() treats a NIL as ME_DEFAULT
      */
      while ! ( nKey := ::xDo( ME_INIT ) ) IN { ME_DEFAULT, NIL }

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
   #ifdef HB_EXT_INKEY
      local aConfigurableKeys := { K_CTRL_Y, K_CTRL_T, K_CTRL_B, K_ALT_W, K_ESC }
   #else
      local aConfigurableKeys := { K_CTRL_Y, K_CTRL_T, K_CTRL_B, K_CTRL_V, K_ALT_W, K_ESC }
   #endif

   local bKeyBlock

   // If I have an user function I need to trap configurable keys and ask to
   // user function if handle them the standard way or not
   //
   if ISCHARACTER( ::xUserFunction )

      while ! ::lExitEdit

         // I need to test this condition here since I never block inside HBEditor:Edit()
         // if there is an user function
         if NextKey() == 0
            ::IdleHook()
         endif

         nKey := Inkey( 0 )

         if ( bKeyBlock := Setkey( nKey ) ) <> NIL
            Eval( bKeyBlock, ::ProcName, ::ProcLine, ReadVar() )
            Loop
         endif

         /* 24/10/2005 - <maurilio.longo@libero.it>
                         Taken from clipper norton guide:

                           The user function: <cUserFunction>, a user-defined function
                           specified as an argument, handles key exceptions and reconfigures
                           special keys.  The user function is called at various times by
                           MEMOEDIT(), most often in response to keys it does not recognize.
                           Keys that instigate a key exception are all available control keys,
                           function keys, and Alt keys.  Since these keys are not processed by
                           MEMOEDIT(), they can be reconfigured.  Some of these keys have a
                           default action assigned to them.  In the user function, you perform
                           various actions, depending on the current MEMOEDIT() mode, then
                           RETURN a value telling MEMOEDIT() what to do next.

                           When the user function argument is specified, MEMOEDIT() defines two
                           classes of keys: nonconfigurable and key exceptions.  When a
                           nonconfigurable key is pressed, MEMOEDIT() executes it, otherwise a
                           key exception is generated and the user function is called.  When
                           there are no keys left in the keyboard buffer for MEMOEDIT() to
                           process, the user function is called once again.
         */

         // Is it a configurable key ?
         if nKey IN aConfigurableKeys
            nUserKey := ::xDo( iif( ::lDirty, ME_UNKEYX, ME_UNKEY ) )
            // Don't pass nUserKey by reference since ::HandleUserKey() defaults it to zero
            ::HandleUserKey( nKey, nUserKey )

         else
            // Key exceptions go to TEditor which, being not able to handle them (think about K_Fn)
            // calls ::KeyboardHook() which calls user-function if available
            ::Super:Edit( nKey )

         endif

      enddo

   else
      if ::lEditAllow
         // If There is not a user function enter standard HBEditor
         // ::Edit() method which is able to handle everything
         ::Super:Edit( nKey )

      else
         /* 24/10/2005 - <maurilio.longo@libero.it>
                         Partly supported for read-only memoedits when there is no user-function
                         Not completely clipper compatible, but, maybe, better than nothing
         */
         while ! ::lExitEdit

            if NextKey() == 0
               ::IdleHook()
            endif

            nKey := InKey( 0 )

            if ( bKeyBlock := Setkey( nKey ) ) <> NIL
               Eval( bKeyBlock, ::ProcName, ::ProcLine, ReadVar() )
               Loop
            endif

            switch nKey
               case K_UP
                  if ::nFirstRow > 1
                     ::nFirstRow--
                     ::nRow--
                     ::RefreshWindow()
                  endif
                  exit

               case K_DOWN
                  if ::nFirstRow < Len( ::aText )
                     ::nFirstRow++
                     ::nRow++
                     ::RefreshWindow()
                  endif
                  exit

               default
                  // Just handle this key and then return
                  ::Super:Edit( nKey )
               end
         enddo
      endif
   endif

return Self


//-------------------------------------------------------------------//
//
// I come here if I have an unknown key and it is not a configurable key
// if there is an user function I leave to it its handling
//
METHOD KeyboardHook( nKey ) CLASS TMemoEditor

   local nUserKey

   if ISCHARACTER( ::xUserFunction )

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
   #ifdef HB_EXT_INKEY
      // I Have to add the values that K_CTRL_x keys have when HB_EXT_INKEY is not defined since those values cause
      // infinite loop
      static aUnHandledKeys := { 10,       11,       12,       14,       15,       16,       17,       20,       21 }
   #else
      static aUnHandledKeys := { K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_N, K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_T, K_CTRL_U }
   #endif

   static aMouseKeys := { K_LBUTTONUP, K_MWFORWARD, K_MWBACKWARD }

   /* 05/08/2004 - <maurilio.longo@libero.it>
                   A little trick to be able to handle a nUserKey with value of NIL
                   like it had a value of ME_DEFAULT
   */
   default nUserKey to ME_DEFAULT

   switch nUserKey
      // I won't reach this point during ME_INIT since ME_DEFAULT ends initialization phase of MemoEdit()
      //
      case ME_DEFAULT
         // HBEditor is not able to handle keys with a value higher than 256 or lower than 1
         //
         if ( nKey > 0 .AND. nKey <= 256 .OR. nKey IN { K_ALT_W, K_CTRL_W } .OR. nKey IN aMouseKeys ) .AND.;
            ! ( nKey IN aUnHandledKeys )

            ::Super:Edit( nKey )

         endif
         exit

      // TOFIX: Not clipper compatible, see teditor.prg
      //
      case K_CTRL_A
      case K_CTRL_B
      case K_CTRL_C
      case K_CTRL_D
      case K_CTRL_E
      case K_CTRL_F
      case K_CTRL_G
      case K_CTRL_H
      case K_CTRL_I
      case K_CTRL_J
      case K_CTRL_K
      case K_CTRL_L
      case K_CTRL_M
      case K_CTRL_N
      case K_CTRL_O
      case K_CTRL_P
      case K_CTRL_Q
      case K_CTRL_R
      case K_CTRL_S
      case K_CTRL_T
      case K_CTRL_U
      case K_CTRL_V
      case K_CTRL_W
      case K_CTRL_X
      case K_CTRL_Y
      case K_CTRL_Z
      case K_ESC
      case K_F1
      case K_CTRL_HOME
      case K_CTRL_PGDN
      case K_CTRL_PGUP
      case K_ALT_W
      case K_CTRL_W
         if ! ( nUserKey IN aUnHandledKeys )

            ::Super:Edit( nUserKey )

         endif
         exit

      case ME_DATA
         if nKey > 0 .AND. nKey <= 256 .AND. ! ( nKey IN aUnHandledKeys )

            ::Super:Edit( nKey )

         endif
         exit

      case ME_TOGGLEWRAP
         ::lWordWrap := ! ::lWordWrap
         exit

      case ME_TOGGLESCROLL
         // TODO: HBEditor does not support vertical scrolling of text inside window without moving cursor position
         /* 24/10/2005 - <maurilio.longo@libero.it>
                         Partly supported for read-only memoedits when there is no user-function
         */
         exit

      case ME_WORDRIGHT
         ::WordRight()
         exit

      case ME_BOTTOMRIGHT
         ::Bottom()
         exit

      default
         // Do nothing

   END

return Self

//-------------------------------------------------------------------//

METHOD xDo( nStatus ) CLASS TMemoEditor

   LOCAL nCurRow := ::Row()
   LOCAL nCurCol := ::Col()
   LOCAL nCurCur := SetCursor()
   LOCAL xRes

   // Latest parameter, Self, is an xHarbour extension, maybe should be guarded as such with some ifdef
   xRes := Do( ::xUserFunction, nStatus, ::nRow, ::nCol - 1, Self )

   ::SetPos( nCurRow, nCurCol )
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

FUNCTION MemoEdit(cString,;
                  nTop, nLeft,;
                  nBottom, nRight,;
                  lEditMode,;
                  cUserFunction,;
                  nLineLength,;
                  nTabSize,;
                  nTextBuffRow,;
                  nTextBuffColumn,;
                  nWindowRow,;
                  nWindowColumn)

   LOCAL oEd

   DEFAULT nTop            TO 0
   DEFAULT nLeft           TO 0
   DEFAULT nBottom         TO MaxRow()
   DEFAULT nRight          TO MaxCol()
   DEFAULT lEditMode       TO .T.
   DEFAULT nLineLength     TO nRight - nLeft
   /* 24/10/2005 - <maurilio.longo@libero.it>
                   NG says 4, but clipper 5.2e inserts 3 spaces when pressing K_TAB
   */
   DEFAULT nTabSize        TO 3
   DEFAULT nTextBuffRow    TO 1
   DEFAULT nTextBuffColumn TO 0
   DEFAULT nWindowRow      TO 0
   DEFAULT nWindowColumn   TO nTextBuffColumn
   DEFAULT cString         TO ""

   if !ISLOGICAL( cUserFunction ) .AND. Empty( cUserFunction )
      cUserFunction = nil
   endif

   /* 24/10/2005 - <maurilio.longo@libero.it>
                   Clipper MemoEdit() converts Tabs into spaces
   */
   oEd := TMemoEditor():New( StrTran( cString, Chr( K_TAB ), Space( nTabSize ) ),;
                             nTop, nLeft, nBottom, nRight,;
                             lEditMode,;
                             nLineLength,;
                             nTabSize,;
                             nTextBuffRow,;
                             nTextBuffColumn,;
                             nWindowRow,;
                             nWindowColumn )

   oEd:ProcName := ProcName( 1 )
   oEd:ProcLine := ProcLine( 1 )

   oEd:MemoInit( cUserFunction )
   oEd:RefreshWindow()

   if ! ISLOGICAL( cUserFunction ) .OR. cUserFunction == .T.
      oEd:Edit()
      if oEd:lSaved
         cString := oEd:GetText( .T. )  // Clipper inserts Soft CR
      endif
   endif

RETURN cString

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

