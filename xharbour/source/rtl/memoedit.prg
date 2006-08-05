/*
 * $Id: memoedit.prg,v 1.39 2006/08/02 13:50:46 modalsist Exp $
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

   DATA  xUserFunction   // User Function called to change default MemoEdit() behaviour

   DATA  aEditKeys
   DATA  aAsciiKeys
   DATA  aConfigurableKeys
   DATA  aMouseKeys
   DATA  aUnHandledKeys

   METHOD  MemoInit( xUDF )                // This method is called after ::New() returns to perform ME_INIT actions
   METHOD  Edit()                          // Calls ::Super:Edit(nKey) but is needed to handle configurable keys
   METHOD  KeyboardHook( nKey )            // Gets called every time there is a key not handled directly by HBEditor

   METHOD  ExistUdf() INLINE ( HB_IsString( ::xUserFunction ) )
   METHOD  HandleUdf( nKey, nUdfReturn )  // Handles requests returned to MemoEdit() by udf
   METHOD  CallUdf( nMode )                // Call user function. ( old xDo )


ENDCLASS

//-------------------------------------------------------------------//

METHOD MemoInit( xUDF ) CLASS TMemoEditor

   local nUdfReturn,i

   DEFAULT xUDF TO NIL


   ::aEditKeys := { K_DOWN,;
                    K_CTRL_E,;
                    K_UP,;
                    K_CTRL_X,;
                    K_LEFT,;
                    K_CTRL_S,;
                    K_RIGHT,;
                    K_CTRL_D,;
                    K_CTRL_LEFT,;
                    K_CTRL_A,;
                    K_CTRL_RIGHT,;
                    K_CTRL_F,;
                    K_HOME,;
                    K_END,;
                    K_CTRL_HOME,;
                    K_CTRL_END,;
                    K_PGUP,;
                    K_PGDN,;
                    K_CTRL_PGUP,;
                    K_CTRL_PGDN,;
                    K_RETURN,;
                    K_ENTER,;
                    K_CTRL_M,;
                    K_DEL,;
                    K_BS,;
                    K_TAB,;
                    K_CTRL_Y,;
                    K_CTRL_T,;
                    K_CTRL_B,;
                    K_CTRL_V,;
                    K_INS,;
                    K_CTRL_W,;
                    K_ESC         }
                     

   ::aAsciiKeys := {}
   FOR i := 32 TO 255
       AAdd( ::aAsciiKeys, i )
   NEXT

   // Save/Init object internal representation of user function
   //
   ::xUserFunction := xUDF

/*
*  // NOTE: K_ALT_W is not compatible with clipper exit memo and save key,
*  //       but I cannot discriminate K_CTRL_W and K_CTRL_END from harbour
*  //       code.
*  //
*  #ifdef HB_EXT_INKEY
*     ::aConfigurableKeys := { K_CTRL_Y, K_CTRL_T, K_CTRL_B, K_ALT_W, K_ESC }
*  #else
*     ::aConfigurableKeys := { K_CTRL_Y, K_CTRL_T, K_CTRL_B, K_CTRL_V, K_ALT_W, K_ESC }
*  #endif
*/
   // HBEditor does not handle these keys and would call ::KeyboardHook()
   // causing infinite loop
   #ifdef HB_EXT_INKEY
      // I Have to add the values that K_CTRL_x keys have when HB_EXT_INKEY
      // is not defined since those values cause infinite loop
      ::aUnHandledKeys := { 10,       11,       12,       14,       15,       16,       17,       20,       21 }
   #else
      // 2006/JUL/24 - E.F. - <CTRL-T> is working now.
      //
      // ::aUnHandledKeys := { K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_N, K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_T, K_CTRL_U }
      ::aUnHandledKeys := { K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_N, K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_U }
   #endif



   ::aMouseKeys := { K_LBUTTONUP, K_MWFORWARD, K_MWBACKWARD }

   ::aConfigurableKeys := { K_F1,;
                            K_F2,;
                            K_F3,;
                            K_F4,;
                            K_F5,;
                            K_F6,;
                            K_F7,;
                            K_F8,;
                            K_F9,;
                            K_F10,;
                            K_F11,;
                            K_F12 }


   if ::ExistUdf()
      /* Keep calling user function until it returns 0
         05/08/2004 - <maurilio.longo@libero.it>
                      Clipper 5.2 memoedit() treats a NIL as ME_DEFAULT
      */
      while ! ( nUdfReturn := ::CallUdf( ME_INIT ) ) IN { ME_DEFAULT, NIL }

         // At this time there is no input from user of MemoEdit() only handling
         // of values returned by ::xUserFunction, so I pass these value on both
         // parameters of ::HandleUdf()
         //
         ::HandleUdf( nUdfReturn, nUdfReturn, .F. )

      enddo

   endif

Return Self

//-------------------------------------------------------------------//

METHOD Edit() CLASS TMemoEditor

   Local nKey, nUdfReturn, lIdle
   
   // If I have an user function I need to trap configurable keys and ask to
   // user function if handle them the standard way or not
   //

   if NextKey()==0 .AND. ::ExistUdf()
      ::CallUdf( ME_IDLE )
   endif

   WHILE !::lExitEdit 

         nKey := Inkey( 0 )

         if ( ::bKeyBlock := Setkey( nKey ) ) <> NIL

            Eval( ::bKeyBlock, ::ProcName, ::ProcLine, ReadVar() )

            // 2006/JUL/29 - E.F. - After set key is called, I need trap
            //                      nextkey, if any.
            if NextKey() != 0
               inkey()
            endif

            // 2006/JUL/29 - E.F.- The execution should be continue to allow
            //                     memoedit process the nKey.
            // Loop

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

         IF ::bKeyBlock == NIL 
            IF !(nKey IN ::aConfigurableKeys) .AND. !(nKey IN ::aUnHandledKeys) .AND.;
               ( (nKey IN ::aEditKeys) .OR. (nKey IN ::aAsciiKeys) )
               ::Super:Edit( nKey )
            ELSEIF (nKey IN ::aConfigurableKeys) .OR. (nKey IN ::aUnHandledKeys) .OR.;
               nKey > 255 .OR. nKey < 0
               ::KeyboardHook(nKey)
            ENDIF
         ENDIF

         IF ::ExistUdf()

            IF !(nKey IN ::aConfigurableKeys) .AND. !(nKey IN ::aUnhandledKeys) .AND.;
               ((nKey IN ::aEditKeys) .OR. (nKey IN ::aAsciiKeys) )

               IF NextKey()==0
                  nUdfReturn := ::CallUdf( ME_IDLE )
               ELSE
                  nUdfReturn := ::CallUdf( iif(::lChanged, ME_UNKEYX,ME_UNKEY) )
               ENDIF

               ::HandleUdf( nKey, nUdfReturn, ::bKeyBlock==NIL )

            ENDIF

         ENDIF

   ENDDO

Return Self


//-------------------------------------------------------------------//
//
// I come here if I have an unknown key and it is not a configurable key
// if there is an user function I leave to it its handling
//
METHOD KeyboardHook( nKey ) CLASS TMemoEditor

   Local nUdfReturn

   IF ::ExistUdf()
      nUdfReturn := ::CallUdf( iif( ::lChanged, ME_UNKEYX, ME_UNKEY ) )
      ::HandleUdf( nKey, nUdfReturn, .F. )
   ENDIF

Return Self

//-------------------------------------------------------------------//

METHOD HandleUdf( nKey, nUdfReturn, lEdited ) CLASS TMemoEditor


   /* 05/08/2004 - <maurilio.longo@libero.it>
                   A little trick to be able to handle a nUdfReturn with value of NIL
                   like it had a value of ME_DEFAULT
   */
   DEFAULT nUdfReturn TO ME_DEFAULT
   DEFAULT lEdited TO .F.

   // I won't reach this point during ME_INIT since ME_DEFAULT ends
   // initialization phase of MemoEdit()
   //

   SWITCH nUdfReturn

      CASE ME_DEFAULT

         // HBEditor is not able to handle keys with a value higher than 256 or lower than 1
         //
         if !lEdited .AND. ( nKey IN ::aAsciiKeys .OR.;
            nKey IN { K_ALT_W, K_CTRL_W } .OR.;
            nKey IN ::aMouseKeys ) .AND.;
            ! ( nKey IN ::aUnHandledKeys )
            ::Super:Edit( nKey )
         endif
         exit

      CASE ME_IGNORE
         // do nothing

      CASE ME_DATA

         if !lEdited .AND. nKey IN ::aAsciiKeys .AND.;
            ! ( nKey IN ::aUnHandledKeys )
            ::Super:Edit( nKey )
         endif
         exit

      CASE ME_TOGGLEWRAP
         ::lWordWrap := ! ::lWordWrap
         exit

      CASE ME_TOGGLESCROLL
         ::lVerticalScroll := ! ::lVerticalScroll
         exit

      CASE ME_WORDRIGHT
         ::WordRight()
         exit

      CASE ME_BOTTOMRIGHT
         ::Bottom()
         ::End()
         exit

      DEFAULT   // ME_UNKEY 1-31

        /* 2006/AUG/02 - E.F. - (NG) Process requested action corresponding to
         *                      key value.
         */
        nKey := nUdfReturn

        IF nKey >=1 .AND. nKey <= 31 .AND.;
           ! ( nKey IN ::aUnHandledKeys ) 
           
           ::Super:Edit( nKey )
        ENDIF
        exit

   END

return Self

//-------------------------------------------------------------------//

METHOD CallUdf( nMode ) CLASS TMemoEditor

   LOCAL nCurRow := ::Row()
   LOCAL nCurCol := ::Col()
   LOCAL nCurCur := SetCursor()
   LOCAL xResult

   IF ::ExistUdf()

      // Latest parameter, Self, is an xHarbour extension, maybe
      // should be guarded as such with some ifdef
      xResult := Do( ::xUserFunction, nMode, ::nRow, ::nCol - 1, Self )

      ::SetPos( nCurRow, nCurCol )
      SetCursor( nCurCur )

   ENDIF

Return xResult

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
                  xUDF,;
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
   DEFAULT nLineLength     TO NIL
   /* 24/10/2005 - <maurilio.longo@libero.it>
                   NG says 4, but clipper 5.2e inserts 3 spaces when pressing K_TAB
   */
   DEFAULT nTabSize        TO 3
   DEFAULT nTextBuffRow    TO 1
   DEFAULT nTextBuffColumn TO 0
   DEFAULT nWindowRow      TO 0
   DEFAULT nWindowColumn   TO nTextBuffColumn
   DEFAULT cString         TO ""

   // 2006/JUL/22 - E.F. Check argument types.
   //
   IF !HB_IsNil( cString ) .AND. !HB_IsString( cString ) .AND. !HB_IsMemo( cString )
      Throw( ErrorNew( "BASE", 0, 1127,  "<cString> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nTop ) .AND. !HB_IsNumeric( nTop )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nTop> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nLeft ) .AND. !HB_IsNumeric( nLeft )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nLeft> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nRight ) .AND. !HB_IsNumeric( nRight )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nRight> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nBottom ) .AND. !HB_IsNumeric( nBottom )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nBottom> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( lEditMode ) .AND. !HB_IsLogical( lEditMode )
      Throw( ErrorNew( "BASE", 0, 1127,  "<lEditMode> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( xUDF ) .AND.  ( !HB_IsString( xUDF ) .AND. !HB_IsLogical( xUDF ) )
      Throw( ErrorNew( "BASE", 0, 1127,  "<cUserFunction> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nLineLength ) .AND. !HB_IsNumeric( nLineLength )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nLineLength> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nTabSize ) .AND. !HB_IsNumeric( nTabSize )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nTabSize> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nTextBuffRow ) .AND. !HB_IsNumeric( nTextBuffRow )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nTextBuffRow> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nTextBuffColumn ) .AND. !HB_IsNumeric( nTextBuffColumn )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nTextBuffColumn> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nWindowRow ) .AND. !HB_IsNumeric( nWindowRow )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nWindowRow> Argument type error" , Procname() ) )
   ENDIF
   IF !HB_IsNil( nWindowColumn ) .AND. !HB_IsNumeric( nWindowColumn )
      Throw( ErrorNew( "BASE", 0, 1127,  "<nWindowColumn> Argument type error" , Procname() ) )
   ENDIF


   // 2006/JUL/22 - E.F. To avoid run time error.
   IF nTop >= nBottom .OR.;
      nLeft >= nRight

      Throw( ErrorNew( "BASE", 0, 1127, "<nTop,nLeft,nRight,nBottom> Argument error" , Procname() ) )
      
   ENDIF


   IF HB_IsString( xUDF ) .AND. Empty( xUDF )
      xUDF := NIL
   ENDIF

   // 2006/JUL/21 - E.F. Line len is total editable columns into screen
   IF nLineLength == NIL
      nLineLength := nRight - nLeft + 1
   ENDIF

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

   oEd:MemoInit( xUDF )
   oEd:RefreshWindow()


   IF !Hb_IsLogical( xUDF )

      oEd:Edit()

      IF oEd:lSaved
         cString := oEd:GetText( .T. )  // If <CTRL-W> pressed, return memoedit text buffer.
      ENDIF

   ELSE
      // 2006/JUL/24 - E.F. - xUDF in .F. only diplay memo content and exit,
      //                      so we have to repos the cursor at bottom of 
      //                      memoedit screen after that.
      SetPos( Min(nBottom,MaxRow()),0)
   ENDIF

RETURN ( cString )

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

