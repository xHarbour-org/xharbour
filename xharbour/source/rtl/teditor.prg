/*
* Teditor Fix: teditorx.prg  -- V 2.0 2003/11/17
* Copyright 2003 Lance Owens <servant@gnosis.org>
*
* This Revised Version has a completely rewritten edit method key commands, with dynamic line & paragraqph reformatting.
* Includes a fix for the bugs in Teditor key processing that previously caused array errors
*
* Note: --If using the paste function to enter text, increase size of keyboard buffer to 2048 or 4096!
*         Otherwise buffer will overrun -- it takes some processor time to do all the dynamic reformatting
*   --SetCursor() is used to change cursor between insert and overwrite. Modify if desired....
*         This will need to be cleared to return to original cursor within Memoedit()!!
*       --K_LEFT is set to exit Memoedit() in read-only mode, in addition to the standard exit keys ESC.
*       --CHR(141)+CHR(10) "soft CR" inserted by Clipper memoedit() is automatically removed when encountered in text
*       --Color persistence problems in previous version corrected by taking setcolor() at Method New file call.
*
* Modifications are based upon the following source file:
*/

/* $Id: teditor.prg,v 1.20 2004/02/19 23:46:37 jonnymind Exp $
 * Harbour Project source code:
 * Editor Class (base for Memoedit(), debugger, etc.)
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
 *               Pritpal Bedi <pritpal@vouchcac.com>
 *                            28Feb2004
 *
 *   Suppor for Clipper's MemoEdit( ..., nTextBufferRow, nTextBufferCol, nWindowRow, nWindowCol )
 *   Rearrangement of code in logical sections.
 *   Reformatting of code to be more readable.
 *   Navigation code broken into small methods for easy mainainability on lines with TBrowse()
 *
 */
//-------------------------------------------------------------------//

#include "common.ch"
#include "hbclass.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "color.ch"

//-------------------------------------------------------------------//

CLASS HBEditor

   DATA  cFile          INIT ""     // name of file being edited

   DATA  aText          INIT {}     // array with lines of text being edited
   DATA  naTextLen      INIT 0      // number of lines of text inside aText.

   DATA  nTop                       // boundaries of editor window, without box around
   DATA  nLeft
   DATA  nBottom
   DATA  nRight

   DATA  nFirstCol      INIT 1      // FirstCol/Row of current text visible inside editor window
   DATA  nFirstRow      INIT 1
   DATA  nRow           INIT 1      // Cursor position inside aText (nRow) and inside current line of text (nCol)
   DATA  nCol           INIT 1

   DATA  nPhysRow       INIT 0      // Hardware cursor position, I cannot rely on Row()/Col() because I could be inside another
   DATA  nPhysCol       INIT 0      // application/object and this one could be moving real cursor. If I'm running full
                                    // screen nPhysRow will always have the same value as Row() and nPhysCol as Col()

   DATA  nNumCols       INIT 1      // How many columns / rows can be displayed inside editor window
   DATA  nNumRows       INIT 1

   DATA  lInsert        INIT .T.    // Is editor in Insert mode or in Overstrike one?
   DATA  nTabWidth      INIT 5      // Size of Tab chars
   DATA  lEditAllow     INIT .T.    // Are changes to text allowed?
   DATA  lSaved         INIT .F.    // True if user exited editor with K_CTRL_W
   DATA  lWordWrap      INIT .F.    // True if word wrapping is active
   DATA  nWordWrapCol   INIT 0      // At which column word wrapping occurs
   DATA  lDirty                     // .T. if there are changes not saved
   DATA  lExitEdit      INIT .F.    // .T. if user requested to end Edit() method

   DATA  cColorSpec     INIT SetColor()     // Color string used for screen writes

   DATA  lRightScroll   INIT .T.    // MARKER TO SET LINE SCROLLING OF R_KEY
   DATA  nMarkPos                   // Mark proper new position of cursor when wrapping and splitting lines
   DATA  nMarkLen
   DATA  nOrigCursor    INIT SetCursor()  // Save to restore original cursor format on exit

   METHOD  New( cString, nTop, nLeft, nBottom,;             // Converts a string to an array of strings splitting input string at EOL boundaries
               nRight, lEditMode, nLineLength, nTabSize, nTextRow, nTextCol, nWndRow, nWndCol )

   METHOD  LoadFile( cFileName )                            // Load cFileName into active editor
   METHOD  LoadText( cString )                              // Load cString into active editor
   METHOD  SaveFile()                                       // Save active file ( not for MemoEdit() emulation )

   METHOD  AddLine( cLine, lSoftCR )                        // Add a new Line of text at end of current text
   METHOD  InsertLine( cLine, lSoftCR, nRow )               // Insert a line of text at a defined row
   METHOD  RemoveLine( nRow )                               // Remove a line of text
   METHOD  GetLine( nRow )                                  // Return line n of text
   METHOD  LineLen( nRow ) INLINE Len( ::aText[ nRow ]:cText )  // Return text length of line n
   METHOD  SplitLine( nRow )                                // If a line of text is longer than nWordWrapCol divides it into multiple lines
   METHOD  GotoLine( nRow )                                 // Put line nRow at cursor position

   METHOD  GetText()                                        // Returns aText as a string ( for MemoEdit() )

   METHOD  RefreshWindow()                                  // Redraw a window
   METHOD  RefreshLine()                                    // Redraw a line
   METHOD  RefreshColumn()                                  // Redraw a column of text

   METHOD  LineColor( nRow ) INLINE ::cColorSpec            // Returns color string to use to draw nRow ( current line if nRow is empty )

   METHOD  MoveCursor( nKey )                               // Move cursor inside text / window ( needs a movement key )
   METHOD  InsertState( lInsState )                         // Changes lInsert value and insertion / overstrike mode of editor
   METHOD  Edit( nPassedKey )                               // Handles input ( can receive a key in which case handles only this key and then exits )

   METHOD  KeyboardHook( nKey )                             // Gets called every time there is a key not handled directly by HBEditor
   METHOD  IdleHook()                                       // Gets called every time there are no more keys to hanlde just before HBEditor blocks itself waiting for a char

   METHOD  Resize( nTop, nLeft, nBottom, nRight )           // Redefines editor window size and refreshes it
   METHOD  SetColor( cColorString )                         // Sets/retrieves color used for screen writes
   METHOD  Hilite()                                         // Start Hilighting swapping first two color definitions inside cColorSpec
   METHOD  DeHilite()                                       // Stop Hilighting

   METHOD  SetPos( nRow, nCol )                             // Updates ::nPhysRow, ::nPhysCol and then calls SetPos() to move hardware cursor
   METHOD  Row() INLINE ::nPhysRow                          // Same as clipper ones, returns ::nPhysRow value
   METHOD  Col() INLINE ::nPhysCol                          // Same as clipper ones, returns ::nPhysCol value

   METHOD  Down()
   METHOD  PageDown()
   METHOD  Bottom()
   METHOD  GoBottom()
   METHOD  Up()
   METHOD  PageUp()
   METHOD  Top()
   METHOD  GoTop()
   MESSAGE Right() METHOD _Right()
   METHOD  WordRight()
   MESSAGE End()   METHOD _End()
   MESSAGE Left()  METHOD _Left()
   METHOD  WordLeft()
   METHOD  Home()
   METHOD  K_Ascii()
   METHOD  K_Return()
   METHOD  K_Del()
   METHOD  K_Bs()
   METHOD  K_Tab()
   METHOD  K_Space()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( cString, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize, nTextRow, nTextCol, nWndRow, nWndCol ) CLASS HBEditor
   Local acsn, nRow, nCol

   default  cString     to ""
   default  nTop        to 0
   default  nLeft       to 0
   default  nBottom     to MaxRow()
   default  nRight      to MaxCol()
   default  lEditMode   to .T.
   default  nLineLength to nil
   default  nTabSize    to nil
   default  nTextRow    to 1
   default  nTextCol    to 0
   default  nWndRow     to 0
   default  nWndCol     to 0

   IF HB_IsNumeric( nLineLength ) .AND. nLineLength <= 0
      nLineLength := NIL
   ENDIF

   // fix setcolor() to value at New() call
   ::cColorSpec := setcolor()

   // Note original cursor to restore after editing
   ::nOrigCursor := SetCursor()

   // If memofield was created with Clipper, it needs to have chr( 141 )+chr( 10 ) stripped
   if chr( 141 ) $ cString
       acsn := chr( 32 ) + chr( 141 ) + chr( 10 )
       cString := STRTRAN( cString,acsn, " " )

       acsn := chr( 141 ) + chr( 10 )
       cString := STRTRAN( cString, acsn, " " )
   endif

   ::aText := Text2Array( cString, nLineLength )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   endif

   // editor window boundaries
   ::nTop    := nTop
   ::nLeft   := nLeft
   ::nBottom := nBottom
   ::nRight  := nRight

   // How many cols and rows are available
   ::nNumCols := nRight - nLeft + 1
   ::nNumRows := nBottom - nTop + 1

   if lEditMode != NIL
      ::lEditAllow := lEditMode
   endif

   // set correct insert state
   if ::lEditAllow
      ::InsertState( ::lInsert )
      if ::lInsert
           SetCursor( 2 )      // change style for insert/overwrite modes
      else
      SetCursor( 3 )
      endif
   endif

   // No need to save
   ::lDirty := .F.

   // is word wrap required?
   if nLineLength != NIL
      ::lWordWrap := .T.
      ::nWordWrapCol := nLineLength
   endif

   // how many spaces for each tab?
   if nTabSize != NIL
      ::nTabWidth := nTabSize
   endif

   ::nFirstRow := max( 1, nTextRow - nWndRow )
   ::nFirstCol := max( 1, nTextCol - nWndCol )
   ::nRow      := nTextRow
   ::nCol      := nTextCol + 1

   // Empty area of screen which will hold editor window
   Scroll( nTop, nLeft, nBottom, nRight )

   // Set cursor position
   ::SetPos( ::nTop + nWndRow, ::nLeft + nWndCol )

return Self

//-------------------------------------------------------------------//
//
// Redefines editor window size and refreshes it
//
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBEditor

   // don't change coordinates not given
   default nTop    to ::nTop
   default nLeft   to ::nLeft
   default nBottom to ::nBottom
   default nRight  to ::nRight

   ::nTop      := nTop
   ::nLeft     := nLeft
   ::nBottom   := nBottom
   ::nRight    := nRight

   // How many cols and rows are available
   ::nNumCols  := ::nRight - ::nLeft + 1
   ::nNumRows  := ::nBottom - ::nTop + 1

   // FirstCol/Row of current text visible inside editor window
   ::nFirstCol := 1
   ::nFirstRow := 1
   // Cursor position inside aText ( nRow ) and inside current line of text ( nCol )
   ::nRow := 1
   ::nCol := 1

   // Set cursor upper left corner
   ::SetPos( ::nTop, ::nLeft )

   ::RefreshWindow()

return Self

//-------------------------------------------------------------------//
//
//                            Screen Output
//
//-------------------------------------------------------------------//
//
// Redraws a screenfull of text
//
METHOD RefreshWindow() CLASS HBEditor

   LOCAL i
   LOCAL nOCol
   LOCAL nORow
   LOCAL nOCur

   nOCol := ::Col()
   nORow := ::Row()
   nOCur := SetCursor( SC_NONE )

   for i := 0 to Min( ::nNumRows - 1, ::naTextLen - 1 )
      DispOutAt( ::nTop + i, ::nLeft, PadR( SubStr( ::GetLine( ::nFirstRow + i ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nFirstRow + i ) )
   next

   // Clear rest of editor window ( needed when deleting lines of text )
   if ::naTextLen < ::nNumRows
      Scroll( ::nTop + ::naTextLen, ::nLeft, ::nBottom, ::nRight )
   endif

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

return Self

//-------------------------------------------------------------------//
//
// Redraws current screen line
//
METHOD RefreshLine() CLASS HBEditor

   LOCAL nOCol
   LOCAL nORow

   nOCol := ::Col()
   nORow := ::Row()

   DispOutAt( ::Row(), ::nLeft, PadR( SubStr( ::GetLine( ::nRow ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nRow ) )

   ::SetPos( nORow, nOCol )

return Self

//-------------------------------------------------------------------//
//
// Refreshes only one screen column of text ( for Left() and Right() movements )
//
METHOD RefreshColumn() CLASS HBEditor

   LOCAL i
   LOCAL nOCol
   LOCAL nORow
   LOCAL nOCur

   nOCol := ::Col()
   nORow := ::Row()
   nOCur := SetCursor( SC_NONE )

   for i := 0 to Min( ::nNumRows - 1, ::naTextLen - 1 )
      DispOutAt( ::nTop + i, nOCol, SubStr( ::GetLine( ::nFirstRow + i ), ::nCol, 1 ), ::LineColor( ::nFirstRow + i ) )
   next

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

return Self

//-------------------------------------------------------------------//
//
//                          Vertical Navigation
//
//-------------------------------------------------------------------//

METHOD Down() CLASS HBEditor

   if !::lEditAllow
      while ::Row() < ::nBottom .AND. ::nRow < ::naTextLen
         ::nRow++
         ::SetPos( ::Row() + 1, ::Col() )
      enddo
   endif

   if ::Row() == ::nBottom
      if ::nRow < ::naTextLen
         Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, 1 )
         ::nFirstRow++
         ::nRow++
         ::RefreshLine()
      endif
   else
      if ::nRow < ::naTextLen
         ::nRow++
         ::SetPos( ::Row() + 1, ::Col() )
      endif
   endif

   // Modified = keep at max of end of line
   if ::nCol > ::LineLen( ::nRow )
      ::End()
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD PageDown() CLASS HBEditor

   if ::nRow + ::nNumRows < ::naTextLen
      ::nRow += ::nNumRows
      ::nFirstRow += ::nNumRows
      if ::nFirstRow + ::nNumRows > ::naTextLen
         ::nFirstRow -= ( ( ::nFirstRow + ::nNumRows ) - ::naTextLen ) + 1
      endif
   else
      ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
      ::nRow := ::naTextLen
      ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), ::Col() )
   endif
   ::RefreshWindow()

   // Modified = keep at max of end of line
   if ::nCol > ::LineLen( ::nRow )
      ::End()
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD Bottom() CLASS HBEditor

   ::nRow += ::nBottom - ::Row()
   if ::nRow >  ::naTextLen
      ::nRow := ::naTextLen
   endif
   ::nCol      := Max( ::LineLen( ::nRow ), 1 )
   ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
   ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )

   ::RefreshWindow()

   Return Self

//-------------------------------------------------------------------//

METHOD GoBottom() CLASS HBEditor

   ::nRow      := ::naTextLen
   ::nCol      := Max( ::LineLen( ::nRow ), 1 )
   ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
   ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
   ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )

   ::RefreshWindow()

   Return Self

//-------------------------------------------------------------------//

METHOD Up() CLASS HBEditor

   if ! ::lEditAllow
      while ::Row() > ::nTop .AND. ::nRow > 1
         ::nRow--
         ::SetPos( ::Row() - 1, ::Col() )
      enddo
   endif

   if ::Row() == ::nTop
      if ::nRow > 1
         Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, -1 )
         ::nFirstRow--
         ::nRow--
         ::RefreshLine()
      endif
   else
      ::nRow--
      ::SetPos( ::Row() - 1, ::Col() )
   endif

   // Modified = keep at max of end of line
   if ::nCol > ::LineLen( ::nRow )
      ::End()
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD PageUp() CLASS HBEditor

   if ( ::nRow - ::nNumRows ) > 1
      ::nRow -= ::nNumRows
      ::nFirstRow -= ::nNumRows
      if ::nFirstRow < 1
         ::nFirstRow := 1
      endif
   else
      ::nFirstRow := 1
      ::nRow := 1
      ::SetPos( ::nTop, ::Col() )
   endif

   ::RefreshWindow()

   // Modified = keep at max of end of line
   if ::nCol > ::LineLen( ::nRow )
      ::End()
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD Top() CLASS HBEditor

   ::nCol      := 1
   ::nFirstCol := 1
   ::nRow      -= ( ::Row() - ::nTop )
   ::SetPos( ::nTop, ::nLeft )

   ::RefreshWindow()

   Return Self

//-------------------------------------------------------------------//

METHOD GoTop() CLASS HBEditor

   ::nRow      := 1
   ::nCol      := 1
   ::nFirstCol := 1
   ::nFirstRow := 1
   ::SetPos( ::nTop, ::nLeft )

   ::RefreshWindow()

   Return Self

//-------------------------------------------------------------------//
//
//                       Horizontal Navigation
//
//-------------------------------------------------------------------//

METHOD _Right() CLASS HBEditor

   if ::Col() == ::nRight
      if ::nCol <= iif( ::lWordWrap, ::nWordWrapCol, ::LineLen( ::nRow ) )
         Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, 1 )
         ::nFirstCol++
         ::nCol++
         ::RefreshColumn()
      endif
   else
      ::nCol++
      ::SetPos( ::Row(), ::Col() + 1 )
   endif

   //mod = move to next line
   //
   If ::lRightScroll
      if ::nCol > ::LineLen( ::nRow ) .and. ::nRow < ::naTextLen
         ::Down()
         ::Home()
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD WordRight() CLASS HBEditor

   // NOTE: should be faster without call to ::GetLine()
   while ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) <> " "
      ::Right()
   enddo
   while ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
      ::Right()
   enddo

   // mod = move to next line
   If ::lRightScroll
      if ::nCol > ::LineLen( ::nRow ) .and. ::nRow < ::naTextLen
         ::Down()
         ::Home()
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD _End() CLASS HBEditor

   // Empty lines have 0 len
   ::nCol      := Max( ::LineLen( ::nRow ) + 1, 1 )
   ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
   ::SetPos( ::Row(), Min( ::nLeft + ::nCol - 1, ::nRight ) )

   ::RefreshWindow()

   Return Self

//-------------------------------------------------------------------//

METHOD _Left() CLASS HBEditor

   if ::Col() == ::nLeft
      if ::nCol > 1
         Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, -1 )
         ::nFirstCol--
         ::nCol--
         ::RefreshColumn()

      // Modified = keep at max of end of line
      else
         if ::nRow>1
            ::Up()
            ::End()
         endif
      endif
   else
      ::nCol--
      ::SetPos( ::Row(), ::Col() - 1 )
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD WordLeft() CLASS HBEditor

   // splitline() does not use this function
   // modifed to wrap lines and position at first letter of word, not word end
   //
   if ::nCol == 1  .and. ::nrow>1
      ::Up()
      ::End()
   endif

   while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) <> " "
      ::Left()
   enddo
   while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
      ::Left()
   enddo
   while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol-1, 1 ) <> " "  // move to front of word
      ::Left()
   enddo

   Return Self

//-------------------------------------------------------------------//

METHOD Home() CLASS HBEditor

   ::nCol      := 1
   ::nFirstCol := 1
   ::SetPos( ::Row(), ::nLeft )

   ::RefreshWindow()

   Return Self

//-------------------------------------------------------------------//

METHOD MoveCursor( nKey ) CLASS HBEditor
   // Modified to handle cursor movements inside text array without crashing!
   // Modified to allow line wrapping, and to track cursor to line ends.
   //
   Switch nKey

      case K_DOWN
         ::Down()
         exit

      case K_PGDN
         ::PageDown()
         exit

      case K_CTRL_PGDN
         ::GoBottom()
         exit

      case K_UP
         ::Up()
         exit

      case K_PGUP
         ::PageUp()
         exit

      case K_CTRL_PGUP
         ::GoTop()
         exit

      case K_RIGHT
         ::Right()
         exit

      case K_CTRL_RIGHT
         ::WordRight()
         exit

      case K_LEFT
         ::Left()
         exit

      case K_CTRL_LEFT
         ::WordLeft()
         exit

      case K_HOME
         ::Home()
         exit

      case K_CTRL_HOME
         ::Top()
         exit

      case K_END
         ::End()
         exit

      case K_CTRL_END
         ::Bottom()
         exit

      default
         RETURN .F.

   end

return .T.

//-------------------------------------------------------------------//
//
//                             Editing
//
//-------------------------------------------------------------------//
//
// Edits text
//
METHOD Edit( nPassedKey ) CLASS HBEditor
   // Many of the complex key handling situations here could be avoided with a
   // complete rewrite of SplitLine() method. This is where the many recurrent problems occur.
   // But alas, that is not what I did....

   LOCAL nKey
   LOCAL lOldInsert
   LOCAL lDelAppend
   LOCAL bKeyBlock
   LOCAL lSingleKeyProcess := .F.   // .T. if I have to process passed key and then exit

   //  [ ::lDirty := .T. ] marks memoedit() return as "file change made."

   if ! ::lEditAllow
      tracelog( nPassedKey )
      BrowseText( Self,nPassedKey )

   else
      // If user pressed an exiting key ( K_ESC or K_ALT_W ) or I've received a key to handle and then exit
      while ! ::lExitEdit .AND. ! lSingleKeyProcess

         // If I haven't been called with a key already preset, evaluate this key and then exit
         if nPassedKey == NIL

            if NextKey() == 0
               ::IdleHook()
            endif

            nKey := InKey( 0 )
         else
            lSingleKeyProcess := .T.
            nKey := nPassedKey

         endif

         if ( bKeyBlock := Setkey( nKey ) ) <> NIL
            Eval( bKeyBlock, Self )  // 7/01/2004 12:47p.m. Pass Self as parameter
            Loop
         endif

         Switch nKey
            case K_ALT_W
            case K_CTRL_W
               // TOFIX: Not clipper compatible
               ::lSaved := .T.
               ::lExitEdit := .T.
               SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
               exit

            case K_CTRL_Y
               ::lDirty := .T.
               if ::naTextLen > 1 .AND. ::nRow < ::naTextLen

                  ::RemoveLine( ::nRow )
                  ::RefreshWindow()
                  ::Home()
                  ::RefreshLine()
               else
                  ::aText[ ::nRow ]:cText := ""
                  ::RefreshLine()
                  ::Home()
                  ::RefreshLine()
               endif
               exit

            case K_DOWN
            case K_PGDN
            case K_CTRL_PGDN
            case K_UP
            case K_PGUP
            case K_CTRL_PGUP
            case K_RIGHT
            case K_CTRL_RIGHT
            case K_LEFT
            case K_CTRL_LEFT
            case K_HOME
            case K_CTRL_HOME
            case K_END
               ::MoveCursor( nKey )
               exit

            case K_ESC
               ::lSaved    := .F.
               ::lExitEdit := .T.
               SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
               exit

            case K_RETURN
               ::K_Return()
               exit

            case K_INS
               ::InsertState( !::lInsert )
               exit

            case K_DEL
               ::K_Del()
               exit

            case K_TAB
               ::K_Tab()
               exit

            case K_BS
               ::K_Bs()
               exit

            case K_CTRL_BS         // block chr( 127 ), a printable character in windows
               exit

            case K_CTRL_END        // Block - Code exits system at present!
               exit

            case K_SPACE
               ::K_Space( nKey )
               exit

            default      // many modifications were beeded to avoid array errors with text entry and line wraps
               if nKey > K_SPACE .AND. nKey < 256
                  ::K_Ascii( nKey )
               else
                  // NOTE: if you call ::Edit() with a key that is passed to ::KeyboardHook() and then
                  // ::KeyboardHook() calls ::Edit() with the same key you end up with an endless loop
                  ::KeyboardHook( nKey )
               endif
         end
      enddo   // finish edit key processing
   endif

return Self

//-------------------------------------------------------------------//

METHOD K_Ascii( nKey ) CLASS HBEditor
   LOCAL lHardCR := .f., nLastLine

   // nKey := ASC( HB_ANSITOOEM( CHR( nKey ) ) )    // convert from windows

   ::lDirty := .T.
   ::nMarkPos := 0

   // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
   if ::nCol > ::LineLen( ::nRow )         // At end of line, add room
      ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
   endif

   // insert char if in insert mode or at end of current line
   if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
   else
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
   endif

   if len( ::aText[ ::nRow ]:cText ) <= ::nWordWrapCol   // New line doesn't neet to wrap
      ::RefreshLine()                               // so just refresh
      ::lRightScroll := .F.
      ::Right()
      ::lRightScroll := .T.
   else                     // Else: it needs to wrap
      //>
      if ::nRow = ::naTextLen .or. !::aText[ ::nRow ]:lSoftCR // It is the last line or hard CR
         nLastLine :=  ::nRow
         ::aText[ ::nRow ]:lSoftCR := .F.

         ::SplitLine( ::nRow )
         ::RefreshLine()

         if nLastLine =  ::nRow .or. ::lInsert
            ::lRightScroll := .F.
            ::Right()
            ::lRightScroll := .T.
         endif
      else
         lHardCR := .F.         // should already by .F., but just to be safe, and it is a tiny line of code...

         if !::aText[ ::nRow+1 ]:lSoftCR     // the next line has a hard return, keep it
            lHardCR := .T.
         endif

         if ::nRow = ::naTextLen-1         // if next to last line of array, last line MUST have HR
            lHardCR := .T.
         endif

         ::nMarkPos := 0         // word wrap marker
         nLastLine :=  ::nRow      // last line marker

         ::nMarkPos :=  ::nCol - RAt( " ", RTrim( ::aText[ ::nRow ]:cText ) )   // if wraps to new line, set new cursor marker
         ::nMarklen := ::LineLen( ::nRow )     // set up markers to check for word wrap with the delete

         ::aText[ ::nRow ]:cText +=  ::GetLine( ::nRow + 1 )
         ::RemoveLine( ::nRow + 1 )

         ::aText[ ::nRow ]:lSoftCR  = !lHardCR     // .T. if lHardCR = .F.

         ::SplitLine( ::nRow )
         ::RefreshLine()

         if ::nCol > ::LineLen( ::nRow ) .and. nLastLine = ::nRow  // past end of line, words has wrapped, move cursor
            ::Down()
            ::Home()

            while ::nMarkPos>0
               ::Right()
               ::nMarkPos--
            enddo
         else
            if nLastLine < ::nRow    // if the line has wrapped from previous, no Kdown/khome needed
               while ::nMarkPos>0
                  ::Right()
                  ::nMarkPos--
               enddo
            else            // no wrap, just move cursor over
               //::lRightScroll := .F.
               ::Right()
               //::lRightScroll := .T.
            endif
         endif
      endif
      //>
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD K_Space( nKey ) CLASS HBEditor
   LOCAL lHardCR := .f., nLastLine

   // Space is a very tricky key, and requires its own routine to avoid array errors in Getparagraph()

   ::lDirty := .T.
   nLastLine := ::nRow

   if !( ::nRow=::naTextLen .or. ::aText[ ::nRow ]:lSoftCR = .F. )  // not the last line, not a HardCR
      // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
      if ::nCol > ::LineLen( ::nRow )
         ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
      endif
      // insert char if in insert mode or at end of current line
      if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
      else
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
      endif

      ::RefreshLine()
      ::SplitLine( ::nRow )
      if nLastLine =  ::nRow .or. ::lInsert
         ::lRightScroll := .F.
         ::Right()
         ::lRightScroll := .T.
      endif

   else                     // The last line or a HardCR line
      if ::nCol >= ::nWordWrapCol            // if at wordwrap of Hard CR line
         ::aText[ ::nRow ]:lSoftCR = .T.      // wrap by adding a line and move down, merge CR status
         ::InsertLine( "", .F. , ::nRow +1 )
         ::Down()
         ::RefreshLine()

      else                  // Not at wordwrap of line
         if Empty( substr( ::aText[ ::nRow ]:cText, ::nCol ) )  // if spaces already exist, move over them to EOL
            ::lRightScroll := .F.
            ::Right()
            ::lRightScroll := .T.
         else
            if ::nCol > ::LineLen( ::nRow )
               ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
            endif
            // insert char if in insert mode or at end of current line
            if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
            else
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
            endif

            ::RefreshLine()
            ::SplitLine( ::nRow )

            ::lRightScroll := .F.
            ::Right()
            ::lRightScroll := .T.
         endif
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD K_Bs() CLASS HBEditor
   LOCAL lHardCR := .f.

   // Heavily modified, with line wrapping and respect of :SoftCR status added
   // Handling this destructive key properly is quite complex, especially at line end

   ::lDirty := .T.
   if ::nCol == 1
      if ::nRow>1
         ::Up()
         ::End()
      endif

      if ::LineLen( ::nRow ) == 0 .and. ::naTextLen>1
         ::RemoveLine( ::nRow )
         ::RefreshWindow()
      else            // if hardcr above, make soft and wordwrap and refresh merged paragraph
         if ::LineLen( ::nRow ) > 0 .and. !::aText[ ::nRow ]:lSoftCR
            ::aText[ ::nRow ]:lSoftCR := .T.
            ::aText[ ::nRow ]:cText :=  substr( ::aText[ ::nRow ]:cText,1,::nCol ) // clears the extra space at line end
            ::RefreshLine()
            // Linewrap the new paragraph
            if ::nRow < ::naTextLen
               lHardCR := .F.            // should already by .F., but just to be safe, and it is a tiny line of code...

               if ::aText[ ::nRow ]:lSoftCR
                  if !::aText[ ::nRow+1 ]:lSoftCR  // if the next line has a hard return, keep it
                     lHardCR := .T.
                  endif

                  if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
                     lHardCR := .T.
                  endif

                  ::aText[ ::nRow ]:cText +=  ::GetLine( ::nRow + 1 )
                  ::RemoveLine( ::nRow + 1 )

                  ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR = .F.

                  ::SplitLine( ::nRow )
                  ::RefreshWindow()
               endif
            endif
         endif
      endif

   else
      // delete previous character
      if ::nCol >= ::LineLen( ::nRow )
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, --::nCol, 1, " " )
      else
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, --::nCol, 1, "" )
      endif

      // correct column position for next call to MoveCursor()
      ::nCol++
      ::Left()
      // refresh screen, respecting CR status

      if ::aText[ ::nRow ]:lSoftCR = .F.  .or. ::nRow = ::naTextLen // the line has a HR, keep it.
         ::RefreshLine()
      else
         if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
            lHardCR := .T.
         endif

         if ::nRow = ::naTextLen-1       // if next to last line, last line MUST have HR
            lHardCR := .T.
         endif

         ::aText[ ::nRow ]:cText += ::GetLine( ::nRow + 1 )
         ::RemoveLine( ::nRow + 1 )

         ::aText[ ::nRow ]:lSoftCR  = !lHardCR  // .T. if lHardCR = .F.

         ::SplitLine( ::nRow )
         ::RefreshWindow()

      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD K_Tab() CLASS HBEditor
   LOCAL lHardCR := .f., i

   // insert char if in insert mode or at end of current line
   ::lDirty := .T.
   if ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
      if ::lInsert .OR. ( ::nCol == ::LineLen( ::nRow ) )
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Space( ::nTabWidth ) )
      endif

      ::lRightScroll := .F.         //prevent auto linewrap
      for i := 1 to ::nTabWidth
         if ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
            ::Right()
            ::RefreshLine()
         else
            i := ::nTabWidth         // end of line, stop it!
         endif
      next
      ::lRightScroll :=.T.
      // wrap lines
      if ::LineLen( ::nRow )> ::nWordWrapCol
         lHardCR := .F.            // should already by .F., but just to be safe, and it is a tiny line of code...

         if ::aText[ ::nRow ]:lSoftCR
            if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
               lHardCR := .T.
            endif

            if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
               lHardCR := .T.
            endif

            ::aText[ ::nRow ]:cText =  ::aText[ ::nRow ]:cText  + ::GetLine( ::nRow + 1 )
            ::RemoveLine( ::nRow + 1 )
            ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR = .F.

            ::SplitLine( ::nRow )
            ::RefreshWindow()
         endif
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD K_Del() CLASS HBEditor
   LOCAL lHardCR := .f., nLastLine

   // Heavily modified, with line wrapping and respect of :SoftCR status
   // Repairs a major bug that resulted in crash of system with array overrun in GetParagraph()!
   // Handling this destructive key properly is quite complex, especially at line end
   // Figuring out the errors in logic overlooked by the old code was quite a job!

   ::lDirty := .T.
   if ::LineLen( ::nRow ) == 0                // an empty row
      if ::naTextLen > 1 .AND. ::nRow < ::naTextLen
         ::RemoveLine( ::nRow )
         ::RefreshWindow()
      else
         ::aText[ ::nRow ]:cText := ""
         ::RefreshLine()
      endif

   else
      // wrap line if too near WordWrapCol ( -1 is choosen value )  to fit another word on the same line
      if ::lWordWrap .AND. ::nCol >= ::nWordWrapCol -1  .and. ::nCol >= ::LineLen( ::nRow )
         If ::nRow != ::naTextLen
            ::Down()
            ::Home()
         endif
      else
         // If on last char of a line and there are more lines, append next line to current one
         // Modified: ...BUT ALSO correct for lSoftCR of lines to avoid array overflow in GetParagraph()!

         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, "" )

         lHardCR := .F.         // should already by .F., but just to be safe, and it is a tiny line of code...
         nLastLine := ::nRow

         if ::nRow = ::naTextLen .or. ( !::aText[ ::nRow ]:lSoftCR .and. ::nCol <= len( ::aText[ ::nRow ]:cText ) ) // the last line, or hard return line not at end.
            ::RefreshLine()
         else
            if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
               lHardCR := .T.
            endif

            if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
               lHardCR := .T.
            endif

            ::nMarklen := ::LineLen( ::nRow )  // set up markers to check for word wrap with the delete
            ::nMarkPos := 0

            if ::nCol > ::LineLen( ::nRow ) .AND.  At( " ", ::aText[ ::nRow+1 ]:cText ) + ::nCol >= ::nWordWrapCol
               ::nMarkPos :=  ::nCol - RAt( " ", RTrim( ::aText[ ::nRow ]:cText ) ) - 1  // if wraps to new line, set new cursor marker
            endif

            ::aText[ ::nRow ]:cText +=  ::GetLine( ::nRow + 1 )
            ::RemoveLine( ::nRow + 1 )

            ::aText[ ::nRow ]:lSoftCR  = !lHardCR  // .T. if lHardCR = .F.

            ::SplitLine( ::nRow )
            ::RefreshWindow()

            // this MAY ( ? ) still have an occasional wrap misposition around line ends
            if ::LineLen( ::nRow )< ::nMarklen  // reposition cursor for wrapped word ( line length was decrease by wrap )
               if nLastLine =  ::nRow         // cursor not yet on wrapped line
                  ::Down()
                  ::Home()
               endif
               while ::nMarkPos>0      // wrap to character where cursor was before wrap
                  ::Right()
                  ::nMarkPos--
               enddo
            else
               if ::nCol > ::LineLen( ::nRow ) .AND. ::nRow < ::naTextLen  //cursor stuck at end of line, unable to wrap
                  ::Down()
                  ::Home()
               endif
            endif
         endif
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD K_Return() CLASS HBEditor
   LOCAL lHardCR := .f.

   ::lDirty := .T.

   if ::lInsert .OR. ::nRow == ::naTextLen
      if ::LineLen( ::nRow ) > 0 .AND. ::lInsert
         // Split current line at cursor position
         ::InsertLine( Right( ::aText[ ::nRow ]:cText, ::LineLen( ::nRow ) - ::nCol + 1 ), ::aText[ ::nRow ]:lSoftCR, ::nRow + 1 )
         ::aText[ ::nRow ]:cText := Left( ::aText[ ::nRow ]:cText, ::nCol - 1 )
         if ::lWordWrap
            ::aText[ ::nRow ]:lSoftCR := .F.
         endif
      else
         if ::lInsert
            ::InsertLine( "", .F., ::nRow + 1 )
         else
            ::AddLine( "", .F. )
         endif
      endif
   endif

   ::Down()
   ::Home()

   // Linewrap the new paragraph
   if ::nRow < ::naTextLen
      lHardCR := .F.         // should already by .F., but just to be safe, and it is a tiny line of code...

      if ::aText[ ::nRow ]:lSoftCR
         if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
            lHardCR := .T.
         endif

         if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
            lHardCR := .T.
         endif

         ::aText[ ::nRow ]:cText =  ::aText[ ::nRow ]:cText  + ::GetLine( ::nRow + 1 )
         ::RemoveLine( ::nRow + 1 )

         ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR = .F.

         ::SplitLine( ::nRow )
         ::RefreshWindow()
      endif
  endif

  Return Self

//-------------------------------------------------------------------//
//
//                   Data Retrieval Methods
//
//-------------------------------------------------------------------//
//
// Add a new Line of text at end of current text
//
METHOD AddLine( cLine, lSoftCR ) CLASS HBEditor

   AAdd( ::aText, HBTextLine():New( cLine, lSoftCR ) )
   ::naTextLen++

return Self

//-------------------------------------------------------------------//
//
// Insert a line of text at a defined row
//
METHOD InsertLine( cLine, lSoftCR, nRow ) CLASS HBEditor

//   ::AddLine()
   ::naTextLen++
   AIns( ::aText, nRow, HBTextLine():New( cLine, lSoftCR ), .T. )
//   ::aText[ nRow ] := HBTextLine():New( cLine, lSoftCR )

return Self

//-------------------------------------------------------------------//
//
// Remove a line of text
//
METHOD RemoveLine( nRow ) CLASS HBEditor

    ADel( ::aText, nRow, .T. )
   ::naTextLen--

return Self

//-------------------------------------------------------------------//
//
// Return line n of text
//
METHOD GetLine( nRow ) CLASS HBEditor

   if nRow <= ::naTextLen .AND. nRow > 0
      return ::aText[ nRow ]:cText
   else
      return ""
   endif

return Self

//-------------------------------------------------------------------//

METHOD GotoLine( nRow ) CLASS HBEditor

   if nRow <= ::naTextLen .AND. nRow > 0

      // Back one line
      if ::nRow - nRow == 1
         ::Up()

      elseif ::nRow - nRow == -1
         ::Down()

      else
         // I need to move cursor if is past requested line number and if requested line is
         // inside first screen of text otherwise ::nFirstRow would be wrong
         if nRow < ::nNumRows .AND. ( ::nTop + nRow ) < ::Row()
            ::SetPos( ::nTop + nRow, ::Col() )
         endif

         ::nRow := nRow
         ::nFirstRow := Max( 1, nRow - ( ::Row() - ::nTop ) )

         ::RefreshWindow()
      endif
   endif

return Self

//-------------------------------------------------------------------//
//
// Rebuild a long line from multiple short ones ( wrapped at soft CR )
//
STATIC function GetParagraph( oSelf, nRow )

   LOCAL cLine := ""

   while oSelf:aText[ nRow ]:lSoftCR
      cLine = cline + oSelf:aText[ nRow ]:cText
      // I don't need to increment nRow since I'm removing lines, ie line n is
      // a different line each time I add it to cLine
      oSelf:RemoveLine( nRow )
   enddo

   // Last line, or only one line
      cLine += oSelf:aText[ nRow ]:cText
      oSelf:RemoveLine( nRow )   // this is where array error occurs IF final line of text is allowed to have :lSoftCR

return cLine


//-------------------------------------------------------------------//
//
// If a line of text is longer than nWordWrapCol divides it into multiple lines,
// Used during text editing to reflow a paragraph
//
METHOD SplitLine( nRow ) CLASS HBEditor

   LOCAL nFirstSpace
   LOCAL cLine
   LOCAL cSplittedLine
   LOCAL nStartRow
   LOCAL nOCol
   LOCAL nORow
   LOCAL lMoveToNextLine
   LOCAL nPosInWord
   LOCAL nI

   // Do something only if Word Wrapping is on
   if ::lWordWrap .AND. ( ::LineLen( nRow ) > ::nWordWrapCol )
      ::lRightScroll := .F.                       // must be .F. within this Method
      nOCol := ::Col()
      nORow := ::Row()

      // Move cursor to next line if you will move the word which I'm over to next line
      // ie, since word wrapping happens at spaces if first space is behind cursor

      lMoveToNextLine := RAt( " ", RTrim( ::GetLine( nRow ) ) ) < ::nCol
      nPosInWord := Len( ::GetLine( nRow ) ) - ::nCol

      nStartRow := nRow
      cLine := GetParagraph( Self, nRow )

      while !Empty( cLine )

         if Len( cLine ) > ::nWordWrapCol
            nFirstSpace := ::nWordWrapCol

            // Split line at fist space before current position
            while SubStr( cLine, --nFirstSpace, 1 ) <> " " .AND. nFirstSpace > 1
            enddo

            // If there is a space before beginning of line split there
            if nFirstSpace > 1
               cSplittedLine := Left( cLine, nFirstSpace )
            else
               // Changed -- now splits line at the nWordWrapCol when no space!  The cursor position is not reliable!
               // This avoids error if the line has NO SPACES! Without this modif. code enters infinite loop on wrap
               // Note that cursor postioning when wrapping lines that have NO space is funky due to MovetoNextLine() problems

               cSplittedLine := Left( cLine, ::nWordWrapCol )

                  // Old method was: else split at current cursor position
                  // cSplittedLine := Left( cLine, ::nCol - 1 )
            endif

            ::InsertLine( cSplittedLine, .T., nStartRow++ )

         else
            // remainder of line
            cSplittedLine := cLine
            ::InsertLine( cSplittedLine, .F., nStartRow++ )
         endif

         cLine := Right( cLine, Len( cLine ) - Len( cSplittedLine ) )
      enddo

      if lMoveToNextLine
         ::Down()
         ::Home()
         ::WordRight()  // ::MoveCursor( K_CTRL_RIGHT )
         if nPosInWord > 0
            // from 0 since I have to take into account previous K_CTRL_RIGHT which moves me past end of word
            for nI := 0 to nPosInWord
               ::Left()
            next
         endif
      else
         ::SetPos( nORow, nOCol )
      endif
      ::RefreshWindow()
      ::lRightScroll := .T.          // set at beginning of if/endif -- must be .F. in SplitScreen()
   endif

return Self

//-------------------------------------------------------------------//
//
//                         Utility Methods
//
//-------------------------------------------------------------------//
//
// This in an empty method which can be used by classes subclassing HBEditor to be able
// to handle particular keys.
//
METHOD KeyboardHook( nKey )  CLASS HBEditor

return Self

//-------------------------------------------------------------------//
//
// There are no more keys to handle. Can I do something for you?
//
METHOD IdleHook()  CLASS HBEditor

return Self

//-------------------------------------------------------------------//

METHOD SetColor( cColorString ) CLASS HBEditor

    local cOldColor := ::cColorSpec

    if cColorString <> nil
       ::cColorSpec := cColorString
    endif

return cOldColor

//-------------------------------------------------------------------//

METHOD Hilite() CLASS HBEditor

   local cEnhanced := ""

   // Swap CLR_STANDARD and CLR_ENHANCED
   cEnhanced += __StrToken( ::cColorSpec, 2, "," ) +  ","
   cEnhanced += __StrToken( ::cColorSpec, 1, "," )

   ::SetColor( cEnhanced + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cEnhanced ) ) )

return Self

//-------------------------------------------------------------------//

METHOD DeHilite() CLASS HBEditor

   local cStandard := ""

   // Swap CLR_STANDARD and CLR_ENHANCED back to their original position inside cColorSpec
   cStandard += __StrToken( ::cColorSpec, 2, "," ) +  ","
   cStandard += __StrToken( ::cColorSpec, 1, "," )

   ::SetColor( cStandard + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cStandard ) ) )

return Self

//-------------------------------------------------------------------//

METHOD SetPos( nRow, nCol ) CLASS HBEditor

   default nRow to ::nPhysRow
   default nCol to ::nPhysCol

   ::nPhysRow := nRow
   ::nPhysCol := nCol

   SetPos( ::nPhysRow, ::nPhysCol )

return ::nPhysRow

//-------------------------------------------------------------------//
//
// Changes lInsert value and insertion / overstrike mode of editor
//
METHOD InsertState( lInsState ) CLASS HBEditor

   IF ISLOGICAL( lInsState )
      ::lInsert := lInsState
      SET( _SET_INSERT, lInsState )
      if ::lInsert
           Setcursor( 2 )
      else
      Setcursor( 3 )
      endif
   ENDIF

return Self

//-------------------------------------------------------------------//
//
// Converts an array of text lines to a String
//
METHOD GetText() CLASS HBEditor

   LOCAL cString := ""
   LOCAL cEOL := HB_OSNewLine()

   if ::lWordWrap
      AEval( ::aText, {|cItem| cString += cItem:cText + iif( cItem:lSoftCR, "", cEOL )},, ::naTextLen - 1 )
   else
      AEval( ::aText, {|cItem| cString += cItem:cText + cEOL},, ::naTextLen - 1 )
   endif

   // Last line does not need a cEOL delimiter
   cString += ::aText[ ::naTextLen ]:cText

return cString

//-------------------------------------------------------------------//

METHOD LoadText( cString ) CLASS HBEditor

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, nil ) )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   endif

   ::lDirty := .F.
   ::GoTop()

return Self

//-------------------------------------------------------------------//

METHOD LoadFile( cFileName ) CLASS HBEditor

   local cString := ""

   if File( cFileName )
      ::cFile := cFileName
      cString := MemoRead( cFileName )
   endif

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, nil ) )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   endif

   ::lDirty := .F.
   ::GoTop()

return Self

//-------------------------------------------------------------------//
//
// Saves file being edited, if there is no file name does nothing, returns .T. if OK
//
METHOD SaveFile() CLASS HBEditor

   local cString

   if !Empty( ::cFile )
      cString := ::GetText()
      ::lDirty := !MemoWrit( ::cFile, cString )
      return !::lDirty

   endif

return .F.

//-------------------------------------------------------------------//
//
//                         Utility Functions
//
//-------------------------------------------------------------------//
//
// Returns EOL char ( be it either CR or LF or both )
//
STATIC function WhichEOL( cString )

   LOCAL nCRPos := At( Chr( 13 ), cString )
   LOCAL nLFPos := At( Chr( 10 ), cString )

   if nCRPos > 0 .AND. nLFPos == 0
      return Chr( 13 )

   elseif nCRPos == 0 .AND. nLFPos >  0
      return Chr( 10 )

   elseif nCRPos > 0 .AND. nLFPos == nCRPos + 1
      return Chr( 13 ) + Chr( 10 )

   endif

return HB_OSNewLine()

//-------------------------------------------------------------------//
//
// Converts a string to an array of strings splitting input string at EOL boundaries
//
STATIC function Text2Array( cString, nWordWrapCol )

   LOCAL cLine
   LOCAL nTokNum
   LOCAL aArray
   LOCAL cEOL
   LOCAL nEOLLen
   LOCAL nRetLen
   LOCAL ncSLen
   LOCAL nFirstSpace
   LOCAL cSplittedLine
   LOCAL nTokPos := 0

   nTokNum := 1
   aArray := {}

   cEOL := WhichEOL( cString )
   nEOLLen := Len( cEOL )

   // __StrTkPtr() needs that string to be tokenized be terminated with a token delimiter
   if Rat( cEOL, cString ) <> Len( cString ) - nEOLLen + 1
      cString += cEOL
   endif

   nRetLen := 0
   ncSLen := Len( cString )

   // If cString starts with an EOL delimiter I have to add an empty line since __StrTkPtr
   // gives back _next_ token and would skip this first EOL delimiter
   if Left( cString, nEOLLen ) == cEOL
      AAdd( aArray, HBTextLine():New( cLine, .F. ) )
      nTokPos += nEOLLen
      nRetLen += nEOLLen
   endif

   while nRetLen < ncSLen
      /* TOFIX: Note that __StrToken is not able to cope with delimiters longer than one char */
      // Dos - OS/2 - Windows have CRLF as EOL
      if nEOLLen > 1
         cLine := StrTran( __StrTkPtr( @cString, @nTokPos, cEOL ), SubStr( cEOL, 2 ), "" )
      else
         cLine := __StrTkPtr( @cString, @nTokPos, cEOL )
      endif
      nRetLen += Len( cLine ) + nEOLLen

      if nWordWrapCol != NIL .AND. Len( cLine ) > nWordWrapCol
         while !Empty( cLine )
            // Split line at nWordWrapCol boundary
            if Len( cLine ) > nWordWrapCol

               nFirstSpace := nWordWrapCol
               while SubStr( cLine, --nFirstSpace, 1 ) <> " " .AND. nFirstSpace > 1
               enddo

               if nFirstSpace > 1
                  cSplittedLine := Left( cLine, nFirstSpace  )
               else
                  cSplittedLine := Left( cLine, nWordWrapCol )
               endif

               AAdd( aArray, HBTextLine():New( cSplittedLine, .T. ) )
            else
               // remainder of line is shorter than split point
               AAdd( aArray, HBTextLine():New( cLine, .F. ) )

               // Done.
               EXIT
            endif

            cLine := Right( cLine, Len( cLine ) - Len( cSplittedLine ) )
         enddo
      else
         AAdd( aArray, HBTextLine():New( cLine, .F. ) )
      endif

   enddo

return aArray

//-------------------------------------------------------------------//
//
// if editing isn't allowed we enter this loop which
// handles only movement keys and discards all the others
//
STATIC procedure BrowseText( oSelf, nPassedKey )

   LOCAL nKey,bKeyBlock

   while ! oSelf:lExitEdit

      // If I haven't been called with a key already preset, evaluate this key and then exit
      if nPassedKey == NIL

         if NextKey() == 0
            oSelf:IdleHook()
         endif

         nKey := InKey( 0 )
      else
         nKey = nPassedKey
      endif

      if ( bKeyBlock := Setkey( nKey ) ) <> NIL
         Eval( bKeyBlock, oSelf )  // 7/01/2004 12:47p.m. Pass oSelf as parameter
         Loop
      endif

      // ******* modified to add exit with K_LEFT when in non-edit mode
      if nKey == K_ESC .or. nkey == K_CTRL_W
         oSelf:lExitEdit := .T.
      else
         if !oSelf:MoveCursor( nKey )
            oSelf:KeyboardHook( nKey )
         endif

      endif

   enddo

return

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*         JUST TO REMOVE OLD CODE AFTER THOROUGH DEBUGGING
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

METHOD MoveCursor( nKey ) CLASS HBEditor
   // Modified to handle cursor movements inside text array without crashing!
   // Modified to allow line wrapping, and to track cursor to line ends.
   Switch nKey
      case K_DOWN
         if !::lEditAllow
            while ::Row() < ::nBottom .AND. ::nRow < ::naTextLen
               ::nRow++
               ::SetPos( ::Row() + 1, ::Col() )
            enddo
         endif
         if ::Row() == ::nBottom
            if ::nRow < ::naTextLen
               Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, 1 )
               ::nFirstRow++
               ::nRow++
               ::RefreshLine()
            endif
         else
            if ::nRow < ::naTextLen
               ::nRow++
               ::SetPos( ::Row() + 1, ::Col() )
            endif
         endif

         // Modified = keep at max of end of line
         if ::nCol > ::LineLen( ::nRow )
            ::MoveCursor( K_END )
         endif
         exit

      case K_PGDN
         if ::nRow + ::nNumRows < ::naTextLen
            ::nRow += ::nNumRows
            ::nFirstRow += ::nNumRows
            if ::nFirstRow + ::nNumRows > ::naTextLen
               ::nFirstRow -= ( ( ::nFirstRow + ::nNumRows ) - ::naTextLen ) + 1
            endif
         else
            ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
            ::nRow := ::naTextLen
            ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), ::Col() )
         endif
         ::RefreshWindow()

         // Modified = keep at max of end of line
         if ::nCol > ::LineLen( ::nRow )
            ::MoveCursor( K_END )
         endif

         exit

      case K_CTRL_PGDN
         ::nRow := ::naTextLen
         ::nCol := Max( ::LineLen( ::nRow ), 1 )
         ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
         ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
         ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )
         ::RefreshWindow()
         exit

      case K_UP
         if ! ::lEditAllow
            while ::Row() > ::nTop .AND. ::nRow > 1
               ::nRow--
               ::SetPos( ::Row() - 1, ::Col() )
            enddo
         endif
         if ::Row() == ::nTop
            if ::nRow > 1
               Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, -1 )
               ::nFirstRow--
               ::nRow--
               ::RefreshLine()
            endif
         else
            ::nRow--
            ::SetPos( ::Row() - 1, ::Col() )
         endif

         // Modified = keep at max of end of line
         if ::nCol > ::LineLen( ::nRow )
            ::MoveCursor( K_END )
         endif

         exit

      case K_PGUP
         if ( ::nRow - ::nNumRows ) > 1
            ::nRow -= ::nNumRows
            ::nFirstRow -= ::nNumRows
            if ::nFirstRow < 1
               ::nFirstRow := 1
            endif
         else
            ::nFirstRow := 1
            ::nRow := 1
            ::SetPos( ::nTop, ::Col() )
         endif
         ::RefreshWindow()

         // Modified = keep at max of end of line
         if ::nCol > ::LineLen( ::nRow )
            ::MoveCursor( K_END )
         endif

         exit

      case K_CTRL_PGUP
         ::nRow := 1
         ::nCol := 1
         ::nFirstCol := 1
         ::nFirstRow := 1
         ::SetPos( ::nTop, ::nLeft )
         ::RefreshWindow()
         exit

      case K_RIGHT
         if ::Col() == ::nRight
            if ::nCol <= iif( ::lWordWrap, ::nWordWrapCol, ::LineLen( ::nRow ) )
               Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, 1 )
               ::nFirstCol++
               ::nCol++
               ::RefreshColumn()
            endif
         else
            ::nCol++
            ::SetPos( ::Row(), ::Col() + 1 )
         endif

         //mod = move to next line


         If ::lRightScroll
           if ::nCol > ::LineLen( ::nRow ) .and. ::nRow < ::naTextLen
            ::MoveCursor( K_DOWN )
            ::MoveCursor( K_HOME )
           endif
         endif
         exit

      case K_CTRL_RIGHT
         // NOTE: should be faster without call to ::GetLine()
         while ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) <> " "
            ::MoveCursor( K_RIGHT )
         enddo
         while ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
            ::MoveCursor( K_RIGHT )
         enddo

         // mod = move to next line
         If  ::lRightScroll
           if ::nCol > ::LineLen( ::nRow ) .and. ::nRow < ::naTextLen
            ::MoveCursor( K_DOWN )
            ::MoveCursor( K_HOME )
           endif
         endif

         exit

      case K_LEFT
          if ::Col() == ::nLeft
            if ::nCol > 1
               Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, -1 )
               ::nFirstCol--
               ::nCol--
               ::RefreshColumn()

           // Modified = keep at max of end of line
            else
               if ::nRow>1
                  ::MoveCursor( K_UP )
                  ::MoveCursor( K_END )
               endif

            endif

          else
            ::nCol--
            ::SetPos( ::Row(), ::Col() - 1 )
          endif

          exit

       case K_CTRL_LEFT
         // splitline() does not use this function
         //modifed to wrap lines and position at first letter of word, not word end
         if ::nCol == 1  .and. ::nrow>1
            ::MoveCursor( K_UP )
            ::MoveCursor( K_END )
         endif

         while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) <> " "
            ::MoveCursor( K_LEFT )
         enddo
         while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
            ::MoveCursor( K_LEFT )
         enddo
         while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol-1, 1 ) <> " "  // move to front of word
            ::MoveCursor( K_LEFT )
         enddo
         exit

      case K_HOME
         ::nCol := 1
         ::nFirstCol := 1
         ::SetPos( ::Row(), ::nLeft )
         ::RefreshWindow()
         exit

      case K_CTRL_HOME
         ::nCol := 1
         ::nFirstCol := 1
         ::nRow -= ( ::Row() - ::nTop )
         ::SetPos( ::nTop, ::nLeft )
         ::RefreshWindow()
         exit

      case K_END
         // Empty lines have 0 len
         ::nCol := Max( ::LineLen( ::nRow )+1, 1 )
         ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
         ::SetPos( ::Row(), Min( ::nLeft + ::nCol - 1, ::nRight ) )
         ::RefreshWindow()
         exit

      case K_CTRL_END
         ::nRow += ::nBottom - ::Row()
         if ::nRow > ::naTextLen
            ::nRow := ::naTextLen
         endif
         ::nCol := Max( ::LineLen( ::nRow ), 1 )
         ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
         ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )
         ::RefreshWindow()
         exit

   Default
      RETURN .F.

   end

return .T.
*/
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
METHOD Edit( nPassedKey ) CLASS HBEditor
   // Many of the complex key handling situations here could be avoided with a
   // complete rewrite of SplitLine() method. This is where the many recurrent problems occur.
   // But alas, that is not what I did....

   LOCAL i
   LOCAL nKey
   LOCAL lOldInsert
   LOCAL lDelAppend
   LOCAL bKeyBlock
   LOCAL lSingleKeyProcess := .F.       // .T. if I have to process passed key and then exit

   LOCAL lHardCR := .F.             // This is a marker to respect :SoftCR status when line are combined
   LOCAL nLastLine

   //  [ ::lDirty := .T. ] marks memoedit() return as "file change made."

   if ! ::lEditAllow
      tracelog( nPassedKey )
      BrowseText( Self,nPassedKey )

   else

      // If user pressed an exiting key ( K_ESC or K_ALT_W ) or I've received a key to handle and then exit
      while ! ::lExitEdit .AND. ! lSingleKeyProcess

         // If I haven't been called with a key already preset, evaluate this key and then exit
         if nPassedKey == NIL

            if NextKey() == 0
               ::IdleHook()
            endif

            nKey := InKey( 0 )
         else
            lSingleKeyProcess := .T.
            nKey := nPassedKey

         endif

      if ( bKeyBlock := Setkey( nKey ) ) <> NIL
         Eval( bKeyBlock, Self )  // 7/01/2004 12:47p.m. Pass Self as parameter
         Loop
      endif

         Switch nKey
            case K_ALT_W
            case K_CTRL_W
               // TOFIX: Not clipper compatible
               ::lSaved := .T.
               ::lExitEdit := .T.
                  SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
               exit

            case K_ESC
               ::lSaved := .F.
               ::lExitEdit := .T.
                  SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
               exit

            case K_RETURN
               ::lDirty := .T.
               if ::lInsert .OR. ::nRow == ::naTextLen
                  if ::LineLen( ::nRow ) > 0 .AND. ::lInsert
                     // Split current line at cursor position
                     ::InsertLine( Right( ::aText[ ::nRow ]:cText, ::LineLen( ::nRow ) - ::nCol + 1 ), ::aText[ ::nRow ]:lSoftCR, ::nRow + 1 )
                     ::aText[ ::nRow ]:cText := Left( ::aText[ ::nRow ]:cText, ::nCol - 1 )
                     if ::lWordWrap
                        ::aText[ ::nRow ]:lSoftCR := .F.
                     endif
                  else
                     if ::lInsert
                         ::InsertLine( "", .F., ::nRow + 1 )
                     else
                      ::AddLine( "", .F. )
                     endif
                  endif
               endif
               ::MoveCursor( K_DOWN )
               ::MoveCursor( K_HOME )

               // Linewrap the new paragraph
               if ::nRow < ::naTextLen
                  lHardCR := .F.         // should already by .F., but just to be safe, and it is a tiny line of code...

                  if ::aText[ ::nRow ]:lSoftCR
                      if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
                         lHardCR := .T.
                      endif

                      if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
                        lHardCR := .T.
                      endif

            ::aText[ ::nRow ]:cText =  ::aText[ ::nRow ]:cText  + ::GetLine( ::nRow + 1 )
                      ::RemoveLine( ::nRow + 1 )

                      ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR = .F.

                      ::SplitLine( ::nRow )
                      ::RefreshWindow()
                  endif
               endif
               exit

            case K_INS
               ::InsertState( !::lInsert )
               exit

            case K_DEL
            // Heavily modified, with line wrapping and respect of :SoftCR status
            // Repairs a major bug that resulted in crash of system with array overrun in GetParagraph()!
            // Handling this destructive key properly is quite complex, especially at line end
            // Figuring out the errors in logic overlooked by the old code was quite a job!

            ::lDirty := .T.
            if ::LineLen( ::nRow ) == 0                // an empty row
               if ::naTextLen > 1 .AND. ::nRow < ::naTextLen
                  ::RemoveLine( ::nRow )
                  ::RefreshWindow()
               else
                  ::aText[ ::nRow ]:cText := ""
                  ::RefreshLine()
               endif

            else
               // wrap line if too near WordWrapCol ( -1 is choosen value )  to fit another word on the same line
               if ::lWordWrap .AND. ::nCol >= ::nWordWrapCol -1  .and. ::nCol >= ::LineLen( ::nRow )
                  If  ::nRow != ::naTextLen
                    ::MoveCursor( K_DOWN )
                    ::MoveCursor( K_HOME )
                  endif
               else
                  // If on last char of a line and there are more lines, append next line to current one
                  // Modified: ...BUT ALSO correct for lSoftCR of lines to avoid array overflow in GetParagraph()!

                  ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, "" )

                  lHardCR := .F.         // should already by .F., but just to be safe, and it is a tiny line of code...
         nLastLine =  ::nRow

                  if ::nRow = ::naTextLen .or. ( !::aText[ ::nRow ]:lSoftCR .and. ::nCol <= len( ::aText[ ::nRow ]:cText ) ) // the last line, or hard return line not at end.
                        ::RefreshLine()
                  else
                      if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
                         lHardCR := .T.
                      endif

                      if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
                        lHardCR := .T.
                      endif

                      ::nMarklen := ::LineLen( ::nRow )  // set up markers to check for word wrap with the delete
                      ::nMarkPos := 0

                      if  ::nCol > ::LineLen( ::nRow ) .AND.  At( " ", ::aText[ ::nRow+1 ]:cText ) + ::nCol >= ::nWordWrapCol

                            ::nMarkPos :=  ::nCol - RAt( " ", RTrim( ::aText[ ::nRow ]:cText ) ) - 1  // if wraps to new line, set new cursor marker
                      endif

            ::aText[ ::nRow ]:cText +=  ::GetLine( ::nRow + 1 )
                      ::RemoveLine( ::nRow + 1 )

                      ::aText[ ::nRow ]:lSoftCR  = !lHardCR  // .T. if lHardCR = .F.

                      ::SplitLine( ::nRow )
                      ::RefreshWindow()

            // this MAY ( ? ) still have an occasional wrap misposition around line ends
                      if ::LineLen( ::nRow )< ::nMarklen  // reposition cursor for wrapped word ( line length was decrease by wrap )
                           if nLastLine =  ::nRow         // cursor not yet on wrapped line
               ::MoveCursor( K_DOWN )
                              ::MoveCursor( K_HOME )
                           endif
             while ::nMarkPos>0      // wrap to character where cursor was before wrap
                              ::MoveCursor( K_RIGHT )
                              ::nMarkPos--
                           enddo
                      else

                           if ::nCol > ::LineLen( ::nRow ) .AND. ::nRow < ::naTextLen  //cursor stuck at end of line, unable to wrap
                             ::MoveCursor( K_DOWN )
                             ::MoveCursor( K_HOME )
                    endif
            endif
                  endif
               endif
            endif

            exit

            case K_TAB
             // insert char if in insert mode or at end of current line
             ::lDirty := .T.
             if ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
               if ::lInsert .OR. ( ::nCol == ::LineLen( ::nRow ) )
                  ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Space( ::nTabWidth ) )
               endif

               ::lRightScroll := .F.         //prevent auto linewrap
               for i := 1 to ::nTabWidth
                  if ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
                       ::MoveCursor( K_RIGHT )
                       ::RefreshLine()
                  else
                       i := ::nTabWidth         // end of line, stop it!
                  endif
               next
               ::lRightScroll :=.T.
               // wrap lines
               if ::LineLen( ::nRow )> ::nWordWrapCol
             lHardCR := .F.            // should already by .F., but just to be safe, and it is a tiny line of code...

                  if ::aText[ ::nRow ]:lSoftCR
                      if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
                         lHardCR := .T.
                      endif

                      if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
                        lHardCR := .T.
                      endif

            ::aText[ ::nRow ]:cText =  ::aText[ ::nRow ]:cText  + ::GetLine( ::nRow + 1 )
                      ::RemoveLine( ::nRow + 1 )

                      ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR = .F.

                      ::SplitLine( ::nRow )
                      ::RefreshWindow()
                  endif
               endif
              endif
              exit

            case K_BS
            // Heavily modified, with line wrapping and respect of :SoftCR status added
            // Handling this destructive key properly is quite complex, especially at line end

             ::lDirty := .T.
             if ::nCol == 1
          if ::nRow>1
                  ::MoveCursor( K_UP )
                  ::MoveCursor( K_END )
               endif

               if ::LineLen( ::nRow ) == 0 .and. ::naTextLen>1
                 ::RemoveLine( ::nRow )
                 ::RefreshWindow()
               else            // if hardcr above, make soft and wordwrap and refresh merged paragraph
                 if ::LineLen( ::nRow ) > 0 .and. !::aText[ ::nRow ]:lSoftCR
                    ::aText[ ::nRow ]:lSoftCR := .T.
                    ::aText[ ::nRow ]:cText :=  substr( ::aText[ ::nRow ]:cText,1,::nCol ) // clears the extra space at line end
                    ::RefreshLine()
                    // Linewrap the new paragraph
                    if ::nRow < ::naTextLen
                       lHardCR := .F.            // should already by .F., but just to be safe, and it is a tiny line of code...

                       if ::aText[ ::nRow ]:lSoftCR
                           if !::aText[ ::nRow+1 ]:lSoftCR  // if the next line has a hard return, keep it
                              lHardCR := .T.
                           endif

                           if ::nRow = ::naTextLen-1      // if next to last line of array, last line MUST have HR
                             lHardCR := .T.
                           endif

                   ::aText[ ::nRow ]:cText +=  ::GetLine( ::nRow + 1 )
                           ::RemoveLine( ::nRow + 1 )

                           ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR = .F.

                           ::SplitLine( ::nRow )
                           ::RefreshWindow()
             endif
                    endif
                 endif
               endif

             else
               // delete previous character
               if ::nCol >= ::LineLen( ::nRow )
                   ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, --::nCol, 1, " " )
               else
                   ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, --::nCol, 1, "" )
               endif

               // correct column position for next call to MoveCursor()
               ::nCol++
               ::MoveCursor( K_LEFT )
               // refresh screen, respecting CR status

                  if ::aText[ ::nRow ]:lSoftCR = .F.  .or. ::nRow = ::naTextLen // the line has a HR, keep it.
                        ::RefreshLine()
                  else

                      if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
                         lHardCR := .T.
                      endif

                      if ::nRow = ::naTextLen-1       // if next to last line, last line MUST have HR
                        lHardCR := .T.
                      endif

                      ::aText[ ::nRow ]:cText += ::GetLine( ::nRow + 1 )
                      ::RemoveLine( ::nRow + 1 )

                      ::aText[ ::nRow ]:lSoftCR  = !lHardCR  // .T. if lHardCR = .F.

                      ::SplitLine( ::nRow )
                      ::RefreshWindow()

                   endif
             endif
             exit

            case K_CTRL_Y
               ::lDirty := .T.
               if ::naTextLen > 1 .AND. ::nRow < ::naTextLen

                  ::RemoveLine( ::nRow )
                  ::RefreshWindow()
                  ::MoveCursor( K_HOME )
                  ::RefreshLine()
               else
                  ::aText[ ::nRow ]:cText := ""
                  ::RefreshLine()
                  ::MoveCursor( K_HOME )
                  ::RefreshLine()
               endif
               exit

            case K_CTRL_BS         // block chr( 127 ), a printable character in windows
               exit
            case K_CTRL_END        // Block - Code exits system at present!
               exit
            case K_DOWN
            case K_PGDN
            case K_CTRL_PGDN
            case K_UP
            case K_PGUP
            case K_CTRL_PGUP
            case K_RIGHT
            case K_CTRL_RIGHT
            case K_LEFT
            case K_CTRL_LEFT
            case K_HOME
            case K_CTRL_HOME
            case K_END
               ::MoveCursor( nKey )
               exit

            case K_SPACE
         // Space is a very tricky key, and requires its own routine to avoid array errors in Getparagraph()

              ::lDirty := .T.
              nLastLine =  ::nRow
              if !( ::nRow=::naTextLen .or. ::aText[ ::nRow ]:lSoftCR = .F. )  // not the last line, not a HardCR
                  // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
                  if ::nCol > ::LineLen( ::nRow )
                     ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
                  endif
                  // insert char if in insert mode or at end of current line
                  if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
                     ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
                  else
                     ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
                  endif

                  ::RefreshLine()
                  ::SplitLine( ::nRow )
                  if nLastLine =  ::nRow .or. ::lInsert
            ::lRightScroll := .F.
                      ::MoveCursor( K_RIGHT )
            ::lRightScroll := .T.
                  endif
              else                     // The last line or a HardCR line
                  if ::nCol >= ::nWordWrapCol            // if at wordwrap of Hard CR line
                      ::aText[ ::nRow ]:lSoftCR = .T.      // wrap by adding a line and move down, merge CR status
                      ::InsertLine( "", .F. , ::nRow +1 )
                      ::MoveCursor( K_DOWN )
                      ::RefreshLine()

                  else                  // Not at wordwrap of line
                     if Empty( substr( ::aText[ ::nRow ]:cText, ::nCol ) )  // if spaces already exist, move over them to EOL
                        ::lRightScroll := .F.
                        ::MoveCursor( K_RIGHT )
                        ::lRightScroll := .T.
                     else

                        if ::nCol > ::LineLen( ::nRow )
                           ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
                        endif
                        // insert char if in insert mode or at end of current line
                        if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
                           ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
                        else
                           ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
                        endif

                        ::RefreshLine()
                        ::SplitLine( ::nRow )

                        ::lRightScroll := .F.
                        ::MoveCursor( K_RIGHT )
                   ::lRightScroll := .T.

                     endif
                  endif

              endif
              exit


            Default      // many modifications were beeded to avoid array errors with text entry and line wraps

               if nKey >  K_SPACE .AND. nKey < 256
                  // nKey := ASC( HB_ANSITOOEM( CHR( nKey ) ) )    // convert from windows

                  ::lDirty := .T.
        ::nMarkPos := 0
                  // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
                  if ::nCol > ::LineLen( ::nRow )         // At end of line, add room
                     ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
                  endif

                  // insert char if in insert mode or at end of current line
                  if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
                     ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
                  else
                     ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
                  endif

                  if len( ::aText[ ::nRow ]:cText ) <= ::nWordWrapCol   // New line doesn't neet to wrap
                  ::RefreshLine()                               // so just refresh
                ::lRightScroll := .F.
                     ::MoveCursor( K_RIGHT )
           ::lRightScroll := .T.
                  else                     // Else: it needs to wrap
                  //>
                  if ::nRow = ::naTextLen .or. !::aText[ ::nRow ]:lSoftCR // It is the last line or hard CR
                        nLastLine :=  ::nRow
                        ::aText[ ::nRow ]:lSoftCR := .F.

                        ::SplitLine( ::nRow )
                        ::RefreshLine()

         if nLastLine =  ::nRow .or. ::lInsert
                 ::lRightScroll := .F.
                           ::MoveCursor( K_RIGHT )
            ::lRightScroll := .T.
                        endif
                  else
            lHardCR := .F.         // should already by .F., but just to be safe, and it is a tiny line of code...

                      if !::aText[ ::nRow+1 ]:lSoftCR     // the next line has a hard return, keep it
                         lHardCR := .T.
                      endif

                      if ::nRow = ::naTextLen-1         // if next to last line of array, last line MUST have HR
                        lHardCR := .T.
                      endif

                      ::nMarkPos := 0         // word wrap marker
                      nLastLine :=  ::nRow      // last line marker

                      ::nMarkPos :=  ::nCol - RAt( " ", RTrim( ::aText[ ::nRow ]:cText ) )   // if wraps to new line, set new cursor marker
                      ::nMarklen := ::LineLen( ::nRow )     // set up markers to check for word wrap with the delete


            ::aText[ ::nRow ]:cText +=  ::GetLine( ::nRow + 1 )
                      ::RemoveLine( ::nRow + 1 )

                      ::aText[ ::nRow ]:lSoftCR  = !lHardCR     // .T. if lHardCR = .F.

                      ::SplitLine( ::nRow )
                      ::RefreshLine()

                      if ::nCol > ::LineLen( ::nRow ) .and. nLastLine = ::nRow  // past end of line, words has wrapped, move cursor
                           ::MoveCursor( K_DOWN )
                           ::MoveCursor( K_HOME )
             while ::nMarkPos>0
                              ::MoveCursor( K_RIGHT )
                              ::nMarkPos--
                           enddo
                      else
              if nLastLine < ::nRow    // if the line has wrapped from previous, no Kdown/khome needed
                   while ::nMarkPos>0
                                 ::MoveCursor( K_RIGHT )
                                 ::nMarkPos--
                              enddo
            else            // no wrap, just move cursor over
                              //::lRightScroll := .F.
                              ::MoveCursor( K_RIGHT )
                          //::lRightScroll := .T.
                           endif
            endif
                  endif
                  //>
                  endif
               else
                  // NOTE: if you call ::Edit() with a key that is passed to ::KeyboardHook() and then
                           ::KeyboardHook() calls ::Edit() with the same key you end up with an endless loop //
                  ::KeyboardHook( nKey )
               endif
         end
      enddo   // finish edit key processing


   endif

return Self
*/
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//





