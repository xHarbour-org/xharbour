/*
 * $Id: teditor.prg,v 1.38 2004/05/15 22:35:32 modalsist Exp $
 *
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

/*
* Teditor Fix: teditorx.prg  -- V 3.0beta 2004/04/17
* Copyright 2004 Giancarlo Niccolai <antispam /at/ niccolai /dot/ ws>
*
* Minimal revision for proper working (expecially with word warping).
* Fixed many funtions
* Added GotoCol() and GotoPos() to goto a logical column or position; they translate this
* movement in a adequate ::SetPos call.
*
* Modifications are based upon the following source file:
*/

/*
* Teditor Fix: teditorx.prg  -- V 2.0 2003/11/17
* Copyright 2003 Lance Owens <servant@gnosis.org>
*
* This Revised Version has a completely rewritten edit method key commands, with dynamic
* line & paragraqph reformatting.
* Includes a fix for the bugs in Teditor key processing that previously caused array
* errors
*
* Note: --If using the paste function to enter text, increase size of keyboard buffer to
*         2048 or 4096!
*         Otherwise buffer will overrun -- it takes some processor time to do all the
*         dynamic reformatting
*       --SetCursor() is used to change cursor between insert and overwrite. Modify if
*         desired....
*         This will need to be cleared to return to original cursor within Memoedit()!!
*       --K_LEFT is set to exit Memoedit() in read-only mode, in addition to the standard
*         exit keys ESC.
*       --CHR(141)+CHR(10) "soft CR" inserted by Clipper memoedit() is automatically
*         removed when encountered in text
*       --Color persistence problems in previous version corrected by taking setcolor()
*         at Method New file call.
*
* Modifications are based upon the following source file:
*/

//-------------------------------------------------------------------//
/*
 *               Pritpal Bedi <pritpal@vouchcac.com>
 *                            28Feb2004
 *
 *   Support for Clipper's MemoEdit( ..., nTextBufferRow, nTextBufferCol, nWindowRow, nWindowCol )
 *   Rearrangement of code in logical sections.
 *   Reformatting of code to be more readable.
 *   Navigation code broken into small methods for easy mainainability on lines with TBrowse()
 *
 */
//-------------------------------------------------------------------//

/*
 * Eduardo Fernandes <eduardo@modalsistemas.com.br>
 *
 * v.1.35 - 2004/05/11
 *
 * Revision to proper working with word-wraping at nLineLength+1 and
 * nTabWidth was fixed to 4 as Clipper default.
 * Tab, HardCR and SoftCR management to make xHarbour memoedit() more Clipper
 * compatible.
 * Scroll to left was fixed too.
 *
 * Removed:
 * DATA lRightScroll was removed from HBEditor class.
 *
 * Changed:
 * Any methods and functions was rewriten, like Text2Array() and GetText().
 *
 * Minor Fix:
 * Down(), Up(), End(), Home(), Right(), Left(), WordRight(), WordLeft(), SplitLine().
 * Setpos(), Gotocol(), GotoPos(),etc.
 *
 * Added methods:
 * ==============
 * DelWordRight()                  CTRL-T as Clipper
 * AddTabCol( nCol, lAddTab )      Virtual TAB management.
 * DelTabCol( nCol )               idem
 * AddVirtualTab( nCol )           idem
 * DelVirtualTab( nCol, nNewCol )  idem
 * TabColPos( nCol )               idem
 * IsTabCol( nCol )                idem
 * RefreshTabCol()                 idem
 * IsSoftCR( nRow )                To control SoftCR, HardCR and null CR
 * ScrollLeft()                    Scroll text to left at wordwrapcol.
 * _ShowPos()                      Show screen and array cursor position. To test only.
 *
 * <insert> message at line 0 column 67 when we press <Ins> key as Clipper.
 * In insert mode the cursor size is 2 and in overstrike mode is original size.
 *
 * Behaviour changes:
 * ==================
 * In Clipper to exit with save we press CTRL-W. In xHarbour CTRL-END is same as CTRL-W,
 * so, I added a new parameter in MemoEdit() to choice betwen save with CTRL-W or CTRL-Q.
 * If the choice is CTRL-Q the CTRL-W/CTRL_END move the cursor to end of current window,
 * otherwise CTRL-END/CTRL-W exit with save. The default is CTRL-W as Clipper.
 *
 * Virtual TAB management
 * ======================
 * To control tab in/out of text, I used the chr(255) to swapp betwen real tab and
 * virtual tab. To do it, the HBTextLine class was changed.
 *
 * See memoedit.prg and ttextlin.prg to more details.
 *
 * v.1.36 - 2004/05/12
 *
 * Better cursor and text scroll management.
 *
 * v.1.37 - 2004/05/13
 *
 * Reformat source code, made by Pritipal Bedi.
 *
 * v.1.38 - 2004/05/15
 *
 * Restored correct cursor size in overstrike/insert mode, made in previous release.
 *
 * Fixed word-wrap control in teditor.prg through <nLineLength> parameter from memoedit.
 * 
 * if nLineLength < 0 then word-wrap = false like Clipper.
 * if nLineLength = 0 or null, then word-wrap = true and wordwrapcol = nRight - nLeft + 1.
 * if nLineLength > 0 then word-wrap = true and wordwrapcol = nLineLength
 * 
 * Changed LineLen(nRow) method name to LLen(nRow) to more clean and easy codification.
 * Removed INLINE declaration to normal method declaration.
 *
 * Minor revision in cursor position management after left scroll, especially in insert 
 * mode. Better behaviour in insert mode, but still have somethings to do. 
 *
 * Fixed bug that was word wrap lines improperly in debugger. Reported by Teo Fonrouge. 
 *
 *
 */

#include "common.ch"
#include "hbclass.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "color.ch"

//-------------------------------------------------------------------//

CLASS HBEditor

   DATA  cFile          INIT ""               // name of file being edited

   DATA  aText          INIT {}               // array with lines of text being edited
   DATA  naTextLen      INIT 0                // number of lines of text inside aText.

   DATA  nTop                                 // boundaries of editor window, without box around
   DATA  nLeft                                // idem
   DATA  nBottom                              // idem
   DATA  nRight                               // idem

   DATA  nFirstCol      INIT 1                // FirstCol of current text visible inside editor window.
   DATA  nFirstRow      INIT 1                // FirstRow of current text visible inside editor window.
   DATA  nRow           INIT 1                // Cursor position inside aText (nRow).
   DATA  nCol           INIT 1                // Cursor position inside aText (nCol).

   DATA  nPhysRow       INIT 0                // Row cursor position inside edit window.
   DATA  nPhysCol       INIT 0                // Col cursor position inside edit window.

   DATA  nNumCols       INIT 1                // How many columns can be displayed inside editor window
   DATA  nNumRows       INIT 1                // How many rows can be displayed inside editor window

   DATA  lInsert        INIT .F.              // Is editor in Insert mode or in Overstrike one? Default : Overstrike - Clipper

   DATA  nTabWidth      INIT 4                // Tab size. Default is 4 as Clipper
   DATA  cTabSpace      INIT replicate( chr(255) , 4 ) // Virtual Tab character  ("FF") that will be used to show Tab spaces.
   DATA  cTabChar       INIT chr( K_TAB )     // Real Tab character that will be used to replace virtual tab character.

   DATA  lEditAllow     INIT .T.              // Are changes to text allowed?
   DATA  lSaved         INIT .F.              // True if user exited editor with K_CTRL_W
   DATA  lWordWrap      INIT .T.              // .f. earlier (Debug use this, see xharbour\source\debug\tbrwtext.prg). 
                                              // True if word wrapping is active like Clipper.
   DATA  nWordWrapCol   INIT 0                // At which column word wrapping occurs. This can be equal, larger or shorter than nNumCols.
   DATA  lDirty                               // .T. if there are changes not saved
   DATA  lExitEdit      INIT .F.              // .T. if user requested to end Edit() method

   DATA  cColorSpec     INIT SetColor()       // Color string used for screen writes

   DATA  nMarkPos                             // Mark proper new position of cursor when wrapping and splitting lines
   DATA  nMarkLen
   DATA  nOrigCursor    INIT SetCursor()      // Save to restore original cursor format on exit
   DATA  nCurrentCursor INIT SetCursor()      // Save to restore current cursor format on exit

   DATA  cScreenArea    INIT ""               // To save/restore message "<insert>" at insert/overstrike toogle

   DATA  cSoftCR        INIT chr(141)+chr(10) // Soft CR character as Clipper.
   DATA  cHardCR        INIT HB_OSNewLine()   // Hard CR character. This characters are operational system dependent.
   DATA  lToggleCTRL_WQ INIT .F.              // this change CTRL-W by CTRL-Q to finish edit with save.
                                              // Default is CTRL-W as Clipper.
   DATA  lIsLeftScrolled INIT NIL             // To save if the screen was left scrolled.
                                              // 3 state: NIL = never scrolled.
                                              //          .T. = left scrolled
                                              //          .F. = unscrolled
   DATA  nLeftScrollVal  INIT 8               // Amount of columns to left scroll.
   DATA  nWordWrapColLeftScroll INIT 0        // Column to word wrap in left scroll mode.
   
   METHOD  New( cString, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize,;
                nTextRow, nTextCol, nWndRow, nWndCol, lToggleKeySave )

   METHOD  LoadFile( cFileName )                            // Load cFileName into active editor. Debug use this.
   METHOD  LoadText( cString )                              // Load cString into active editor
   METHOD  SaveFile()                                       // Save active file ( not for MemoEdit() emulation )

   METHOD  AddLine( cLine, lSoftCR )                        // Add a new Line of text at end of current text
   METHOD  InsertLine( cLine, lSoftCR, nRow )               // Insert a line of text at a defined row
   METHOD  RemoveLine( nRow )                               // Remove a line of text
   METHOD  GetLine( nRow )                                  // Return line n of text
   METHOD  LLen( nRow )                                     // Return text length of line n
   METHOD  SplitLine( nRow )                                // If a line of text is longer than nWordWrapCol divides it into multiple lines

   METHOD  GotoLine( nRow )                                 // Put line nRow at cursor position
   METHOD  GotoCol( nCol )                                  // Put line nCol at cursor position
   METHOD  GotoPos( nRow, nCol, lForceRefresh )
   METHOD  GetText()                                        // Returns aText as a string ( for MemoEdit() )
   METHOD  GetTextIndex()                                   // Return current cursor position in text.

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

   METHOD  Down()                    // move cursor do down position.
   METHOD  PageDown()                // move cursor to page down.
   METHOD  Bottom()                  // move cursor to last row in edit window.
   METHOD  GoBottom()

   METHOD  Up()                      // move cursor to up.
   METHOD  PageUp()                  // move cursor page up.
   METHOD  Top()                     // move cursor to first row in edit window.
   METHOD  GoTop()                   // move cursor to highest row.

   METHOD  Right()                   // move cursor one position to right.
   METHOD  WordRight()               // move cursor to next right word.
   METHOD  End()                     // move cursor to end of current line.

   METHOD  Left()                    // move cursor one position to left.
   METHOD  WordLeft()                // move cursor to previous left word.
   METHOD  Home()                    // move cursor to begin of current line.

   METHOD  K_Ascii()                 // process asc-II chars in line text.
   METHOD  K_Return()                // move cursor do next line.
   METHOD  K_Del()                   // delete current character
   METHOD  K_Bs()                    // delete previous character.
   METHOD  K_Tab()                   // move cursor to tab spaces
   METHOD  K_Mouse()
   METHOD  K_Esc()                   // process ESC key.

   METHOD  DelWordRight()            // delete first cursor word rigth by pressing CTRL-T as Clipper.

   METHOD  AddTabCol( nCol, lAddTab )       // add tab column in tab array and add virtual tab in text line.
   METHOD  DelTabCol( nCol )                // del tab column in tab array
   METHOD  AddVirtualTab( nCol )            // add virtual tab character (::cTabSpace) in text line
   METHOD  DelVirtualTab( nCol, nNewCol )   // del virtual tab character in text line and set the new cursor position.
   METHOD  TabColPos( nCol )                // return the first column that represent the real tab position.
   METHOD  IsTabCol( nCol )                 // return true/false if <nCol> is within tab area.
   METHOD  RefreshTabCol()                  // refresh all tab columns in current line

   METHOD  IsSoftCR( nRow )                 // return true/false if the current line end with soft CR.
   METHOD  ScrollLeft()                     // Scroll text to left if greater than nWordWrapCol.
   METHOD  _ShowPos()                       // Show screen and array cursor position. Should be used only in test mode.

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( cString, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize,;
            nTextRow, nTextCol, nWndRow, nWndCol, lToggleKeySave ) CLASS HBEditor

   default  cString        to ""
   default  nTop           to 0
   default  nLeft          to 0
   default  nBottom        to MaxRow()
   default  nRight         to MaxCol()
   default  lEditMode      to .T.
   default  nLineLength    to nil
   default  nTabSize       to 4
   default  nTextRow       to 1
   default  nTextCol       to 0 // 1   Clipper Documentations says it is 0
   default  nWndRow        to 0 // 1   "
   default  nWndCol        to 0 // 1   "
   default  lToggleKeySave to .F.

   ::lToggleCTRL_WQ := lToggleKeySave

   IF HB_IsNumeric( nLineLength ) .AND. nLineLength <= 0
      nLineLength := NIL
   ENDIF

   // fix setcolor() to value at New() call
   ::cColorSpec := setcolor()

   // Note original cursor to restore after editing
   ::nOrigCursor := SetCursor()

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
      ::InsertState( SET( _SET_INSERT ) )
   endif

   // No need to save
   ::lDirty := .F.

   // Is word wrap required?
   if nLineLength != NIL 
      ::lWordWrap := .T.
      ::nWordWrapCol := nLineLength
   else
      ::lWordWrap := .F.
   endif


   // Word wrap column in left scroll mode.
   if ::lWordWrap 
      if ::nWordWrapCol < ::nNumCols
         ::nWordWrapColLeftScroll := ::nLeft + ::nWordWrapCol - ::nLeftScrollVal
      else
         ::nWordWrapColLeftScroll := ::nRight - ::nLeftScrollVal + 1
      endif
   endif
   
   // how many spaces for each tab?
   // declare this before load string into text-array
   if nTabSize != NIL
      ::nTabWidth := nTabSize
   endif
   ::cTabSpace := Replicate( chr( 255 ) , ::nTabWidth )

   // Load string into text-array. Converts a string to an array of strings
   // splitting input string at EOL boundaries
   ::aText := Text2Array( cString, self )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      aadd( ::aText, HBTextLine():New() )
      ::naTextLen := Len( ::aText )
   endif

   nTextRow    := max( 1, nTextRow )
   nTextCol    := max( 0, nTextCol )
   nWndRow     := max( 0, nWndRow  )
   nWndCol     := max( 0, nWndCol  )

   ::nFirstRow := max( 1, nTextRow - nWndRow )
   ::nFirstCol := max( 1, nTextCol - nWndCol )
   ::nRow      := max( 1, min( nTextRow, ::naTextLen ) )
   ::nCol      := max( 1, min( Len( ::aText[ ::nRow ]:cText ) , nTextCol + 1 ) )

   // extra sanitization over max bounds
   IF ::nFirstRow >  ::naTextLen
      ::nFirstRow := ::naTextLen
   ENDIF

   IF ::nFirstCol >  ::LLen() + 1
      ::nFirstCol := ::LLen() + 1
   ENDIF

   // Set cursor position; also initializes phisical to virtual mapping
   ::SetPos( ::nTop + nWndRow, ::nLeft + nWndCol )

   ::RefreshWindow()

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
   //
   ::nNumCols  := ::nRight - ::nLeft + 1
   ::nNumRows  := ::nBottom - ::nTop + 1

   // FirstCol/Row of current text visible inside editor window
   //
   ::nFirstCol := 1
   ::nFirstRow := 1

   // Cursor position inside aText ( nRow ) and inside current line of text ( nCol )
   //
   ::nRow      := 1
   ::nCol      := 1

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
   LOCAL nCurCol
   LOCAL nCurRow

   nCurCol := ::Col()
   nCurRow := ::Row()

   SetCursor( SC_NONE )

   if ::lIsLeftScrolled == .T.
      ::lIsLeftScrolled := .F.
   endif

   DispBegin()

   // This breaks individual line coloring, so I restored the old version with
   // a small optimization. -- Ph.Krylov
   // CLEAR THE WHOLE WINDOW!!! previous version wished to spare some output, but
   // C is faster than a VM loop!!
   //
   //ScrollFixed( ::nTop, ::nLeft, ::nBottom, ::nRight )


   for i := 0 to Min( ::nNumRows - 1, ::naTextLen - 1 )
      DispOutAt( ::nTop + i, ::nLeft, ;
                 PadR( SubStr( ::GetLine( ::nFirstRow + i ), ::nFirstCol, ::nNumCols ), ::nNumCols ), ;
                 ::LineColor( ::nFirstRow + i ) )
   next

   ScrollFixed( ::nTop + i, ::nLeft, ::nBottom, ::nRight )

   DispEnd()

   SetCursor( ::nCurrentCursor )

   ::SetPos( nCurRow, nCurCol )

return Self

//-------------------------------------------------------------------//
//
// Redraws current screen line
//
METHOD RefreshLine() CLASS HBEditor

   LOCAL nCurCol
   LOCAL nCurRow
   LOCAL nStart


   nCurRow := ::Row()
   nCurCol := ::Col()

   SetCursor( SC_NONE )

   if ::lIsLeftScrolled == .T.
      nStart := ::nFirstCol + ::nLeftScrollVal
   else
      nStart := ::nFirstCol
   endif

   DispOutAt( ::Row(), ::nLeft, PadR( SubStr( ::GetLine( ::nRow ), nStart, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nRow ) )

   SetCursor( ::nCurrentCursor )

   SetPos( nCurRow, nCurCol )

return Self


//-------------------------------------------------------------------//
//
// Refreshes only one screen column of text ( for Left() and Right() movements )
//
METHOD RefreshColumn() CLASS HBEditor

   LOCAL i
   LOCAL nOCol
   LOCAL nORow

   nOCol := ::Col()
   nORow := ::Row()

   SetCursor( SC_NONE )

   for i := 0 to Min( ::nNumRows - 1, ::naTextLen - 1 )
       DispOutAt( ::nTop + i, nOCol, SubStr( ::GetLine( ::nFirstRow + i ), ::nCol, 1 ), ::LineColor( ::nFirstRow + i ) )
   next

   SetCursor( ::nCurrentCursor )

   ::SetPos( nORow, nOCol )

Return Self

//-------------------------------------------------------------------//
//
// Wrapper for Cursor Movement to be used from Outside of This Class
//
//-------------------------------------------------------------------//

METHOD MoveCursor( nKey ) CLASS HBEditor
   // Modified to handle cursor movements inside text array without crashing!
   // Modified to allow line wrapping, and to track cursor to line ends.
   //
   Switch nKey

      // TODO: for optimization, change this with relativie GOTOCOL, GOTOPOS and GOTOROW
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
         return .F.

   end

return .T.

//-------------------------------------------------------------------//
//
//                             Editing
//
//-------------------------------------------------------------------//

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
      BrowseText( Self,nPassedKey )

   else

      // If user pressed an exiting key ( K_ESC or K_ALT_W ) or I've received a key to handle and then exit
      while ! ::lExitEdit .AND. ! lSingleKeyProcess

         //::_ShowPos() // Should be used only in test mode.

         SetCursor( ::nCurrentCursor )

         // If I haven't been called with a key already preset, evaluate this key and then exit
         if nPassedKey == NIL

            if NextKey() == 0
               ::IdleHook()
            endif

            nKey := InKey( 0, INKEY_ALL )
         else
            lSingleKeyProcess := .T.
            nKey := nPassedKey

         endif

         if ( bKeyBlock := Setkey( nKey ) ) <> NIL
            Eval( bKeyBlock, Self )    // 7/01/2004 - Passed Self as parameter
            Loop
         endif

         Switch nKey
            case K_LBUTTONUP
            case K_MWFORWARD
            case K_MWBACKWARD
               ::K_Mouse( nKey )
               exit

            case K_ALT_W

            case K_CTRL_T // Same Clipper function.
                ::DelWordRight()
                exit

            case K_CTRL_Q
                 if (::lToggleCTRL_WQ)  // Finish edit with save. xHarbour option.
                    ::lSaved    := .T.
                    ::lExitEdit := .T.
                    ::nCurrentCursor := ::nOrigCursor
                    SetCursor( ::nCurrentCursor )   // restore original cursor saved at startup
                 endif
                 exit

            case K_CTRL_W
                 if !(::lToggleCTRL_WQ)  // Finish edit with save. In xHarbour CTRL-W behaves as CTRL-END.
                    ::lSaved    := .T.
                    ::lExitEdit := .T.
                    ::nCurrentCursor := ::nOrigCursor
                    SetCursor( ::nCurrentCursor )   // restore original cursor saved at startup
                 else
                    ::Bottom() // same CRTL-END behaviour
                 endif
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

            case K_DOWN // same as CTRL-X
               ::Down()
               exit

            case K_PGDN  // same as CTRL-C
               ::PageDown()
               exit

            case K_CTRL_PGDN  // same as K_CTRL-^
               ::GoBottom()
               exit

            case K_UP   // same as CTRL-E
               ::Up()
               exit

            case K_PGUP  // same as CTRL-R
               ::PageUp()
               exit

            case K_CTRL_PGUP  // same as CTRL-HYPHEN
               ::GoTop()
               exit

            case K_RIGHT  // same as CTRL-D
               ::Right()
               exit

            case K_CTRL_RIGHT   // same as CTRL-B
               ::WordRight()
               exit

            case K_LEFT   // same as CTRL-S
               ::Left()
               exit

            case K_CTRL_LEFT  // same as CTRL-Z
               ::WordLeft()
               exit

            case K_HOME   // same as CTRL-A
               ::Home()
               exit

            case K_CTRL_HOME   // same as CTRL-]
               ::Top()
               exit

            case K_END   // same as CTRL-F
               ::End()
               exit

            case K_ESC   // same as CTRL-[
               ::K_Esc()
               exit

            case K_RETURN   // same as CTRL-M
               ::K_Return()
               exit

            case K_INS   // same as CTRL-V
               ::InsertState( !::lInsert )
               exit

            case K_DEL   // same as CTRL-G
               ::K_Del()
               exit

            case K_TAB   // same as CTRL-I
               ::K_Tab()
               exit

            case K_BS    // same as CTRL-H
               ::K_Bs()
               exit

            case K_CTRL_BS // block chr( 127 ), a printable character in windows
               exit

            case K_CTRL_END // same as CTRL-W in Clipper // Block - Code exits system at present!
               ::Bottom()
               exit

            default
               if nKey >= K_SPACE .AND. nKey < 255 // char(255) is used to simulate virtual tab spaces and
                  ::K_Ascii( nKey )                // can not be used in edit.
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
//
//                          Vertical Navigation
//
//-------------------------------------------------------------------//

METHOD Down() CLASS HBEditor

   if ::nRow < ::naTextLen
      ::GotoLine( ::nRow + 1 )
   endif

RETURN Self

//-------------------------------------------------------------------//

METHOD PageDown() CLASS HBEditor
   LOCAL nJump

   nJump := min( ::nNumRows - 1, ::naTextLen - ::nFirstRow - ( ::nPhysRow - ::nTop ) )

   ::nFirstRow += nJump
   ::nRow      += nJump
   ::RefreshWindow()

Return Self

//-------------------------------------------------------------------//

METHOD Bottom() CLASS HBEditor

   LOCAL nRowTo := min( ::nFirstRow + ::nNumRows-1, ::naTextLen )

   ::GotoLine( nRowTo )
   ::End()

RETURN Self

//-------------------------------------------------------------------//

METHOD GoBottom() CLASS HBEditor

   ::GotoPos( ::naTextLen, ::LLen( ::naTextLen ) + 1, .T. )

Return Self

//-------------------------------------------------------------------//

METHOD Up() CLASS HBEditor

   IF ::nRow > 1
      ::GotoLine( ::nRow - 1 )
   ENDIF

Return Self

//-------------------------------------------------------------------//

METHOD PageUp() CLASS HBEditor
   LOCAL nJump

   nJump := min( ::nNumRows - 1, ::nFirstRow - 1 )

   if nJump == 0
      ::GoToLine( 1 )
   else
      ::nFirstRow -= nJump
      ::nRow      -= nJump
      ::RefreshWindow()
   endif

RETURN Self

//-------------------------------------------------------------------//

METHOD Top() CLASS HBEditor

   ::GotoPos( ::nFirstRow, 1, .T. )

RETURN Self

//-------------------------------------------------------------------//

METHOD GoTop() CLASS HBEditor

   ::GotoPos( 1, 1 )

RETURN Self

//-------------------------------------------------------------------//
//
//                       Horizontal Navigation
//
//-------------------------------------------------------------------//

METHOD Right() CLASS HBEditor

   if ::lWordWrap

      IF ::nCol >= Max( ::LLen(), ::nWordWrapCol ) .and. ::nRow < ::naTextLen

         if ( ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled == .F. )
            ::ScrollLeft()

         else
            if ::nCol+1 <= ::nWordWrapCol
               ::GotoCol( ::nCol + 1 )
            elseif ::nCol+1 > ::nWordWrapCol + 1
               ::GotoPos( ::nRow + 1, 1, .T. )
            elseif ::nCol+1 <= ::nWordWrapCol + 1
               ::GotoCol( ::nCol + 1 )
            endif

         endif

      ELSE

         if ::nCol+1 <= ::nWordWrapCol
            ::GotoCol( ::nCol + 1 )

         else
            if ( ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled == .F. )
               ::ScrollLeft()

            else
               if ::nCol+1 <= ::nWordWrapCol
                  ::GotoCol( ::nCol + 1 )
               elseif ::nCol+1 <= ::nWordWrapCol + 1
                  ::GotoCol( ::nCol + 1 )
               endif

            endif
         endif

      ENDIF

   else

     ::GotoCol( ::nCol + 1 )
     
   endif

RETURN Self

//-------------------------------------------------------------------//

METHOD WordRight() CLASS HBEditor
   LOCAL nColLimit

   if ::lWordWrap .and. ::LLen() > ::nWordWrapCol .and. AT(" ",::GetLine( ::nRow )) == 0
      return self
   endif

   nColLimit := iif( ::lWordWrap, Min( ::nWordWrapCol, ::LLen() ), ::LLen() )

   if ::IsTabCol( ::nCol )
      ::GotoCol( ::TabColPos( ::nCol ) + ::nTabWidth )

   else
      while ::nCol <= nColLimit .and. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) != " "
         ::Right()
         if ::lIsLeftScrolled == .T.
            exit
         endif
      enddo

      while ::nCol <= nColLimit .and. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
         ::Right()
      enddo

      if ::nRow == ::naTextLen
         if ::nCol > ::LLen()
            while ::nCol >= 1 .and. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) != " "
               ::Left()
               if ::nCol <= nColLimit .and. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
                  ::Right()
                  exit
               endif
            enddo
         endif
      endif

      // move to next line
      if ::lWordWrap
         if ( ::nCol >= nColLimit .or. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " ") .and. ::nRow < ::naTextLen
            ::Down()

            while ::LLen() == 0 .and. ::nRow < ::naTextLen
               ::Down()
            enddo
            ::Home()

            while ::nCol <= nColLimit .and.;
                      ( SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " " .or.;
                                   ::IsTabCol( ::nCol ) )
               ::Right()
            enddo

         endif
      endif
   endif

RETURN Self

//-------------------------------------------------------------------//

METHOD End() CLASS HBEditor
   LOCAL i,cText

   if !::lWordWrap
      ::GotoCol( ::LLen() + 1 )
      return self
   endif
   
   cText := ::aText[ ::nRow ]:cText


   if ::LLen() < ::nWordWrapCol

      ::GotoCol( ::LLen() + 1 )

      if RAt( " ", cText ) >= Len( RTrim(cText) ) + 1
          ::GotoCol( ::LLen() )
      endif

   else
      if ::nWordWrapCol <= ::nNumCols

         if SubStr( cText, ::nWordWrapCol, 1 ) != " "

            if ( ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled == .F. )
                if ::nWordWrapCol >= ::nNumCols
                   ::ScrollLeft()
                else
                   ::GotoCol( Min( ::nWordWrapCol+1 , ::LLen()+1) )
                endif
            else
               ::GotoCol( ::nWordWrapCol )
            endif

         else
            ::GotoCol( ::nWordWrapCol )
         endif

      else
         for i := ::nCol to Min( ::nWordWrapCol, ::LLen() )
             ::Right()
         next

      endif
   endif

Return Self

//-------------------------------------------------------------------//

METHOD Left() CLASS HBEditor

   IF ::nCol <= 1

      if ::lWordWrap

         IF ::nRow > 1

            while ::nRow>1 .and. ::LLen(::nRow-1)=0
               ::Up()
               ::End()
            end

            ::GotoPos( ::nRow - 1, ::LLen( ::nRow - 1 ) , .T. )
            ::End()

         ENDIF

      endif

   ELSE

      if ::lIsLeftScrolled == .T.

         if ::nCol > 1 .and. ::nCol <= ::nLeftScrollVal+1
            ::RefreshWindow()
            ::GotoCol( ::nCol - 1 )

         else
            ::nPhysCol := ::Col() - 1
            SetPos( ::Row() , ::nPhysCol )
            if ::nPhysCol <= ::nRight - ::nLeftScrollVal
               if ::lWordWrap .and. ::nWordWrapCol > ::nNumCols
                  ::nCol -= 1
                  if ::nCol <= ::nFirstCol
                     ::RefreshWindow()
                  endif
               else
                  ::nCol := ::nPhysCol - ::nLeft + ::nLeftScrollVal + 1

               endif
            endif
         endif

      else
         ::GotoCol( ::nCol - 1 )

      endif
   ENDIF

RETURN Self

//-------------------------------------------------------------------//

METHOD WordLeft() CLASS HBEditor
   LOCAL cChar

   // splitline() does not use this function
   // modifed to wrap lines and position at first letter of word, not word end
   //
   if ::lWordWrap .and. ::nCol == 1 .and. ::nRow > 1 
      ::Up()
      while ::LLen() = 0 .and. ::nRow > 1
        ::Up()
      enddo
      ::GotoPos( ::nRow , ::LLen() , .T. )
   endif

cChar := ""

if ::nCol > 1

   ::Left()

   if ::IsTabCol( ::nCol )
      if ::lWordWrap .and. ::TabColPos( ::nCol ) == 1 .and. ::nRow > 1
         ::Up()
         while ::LLen() = 0 .and. ::nRow > 1
           ::Up()
         enddo
         ::GotoPos( ::nRow , ::LLen() , .T. )
      else
         ::Right()
      endif
   endif

   cChar := SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 )

   while ::nCol > 1 .and. cChar == " "
      ::Left()
      cChar := SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 )
   enddo

   cChar := SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 )

   while ::nCol > 1 .and. cChar != " " .and. !::IsTabCol( ::nCol )
      ::Left()
      cChar := SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 )
      if cChar == " " .or. ::IsTabCol( ::nCol )
         ::Right()
         exit
      endif
   enddo

endif

Return Self

//-------------------------------------------------------------------//

METHOD Home() CLASS HBEditor

  if ::lIsLeftScrolled != NIL .and. ::lIsLeftScrolled == .T.
      ::RefreshWindow()
      ::lIsLeftScrolled := .F.
  endif

  ::GotoCol( 1 )

RETURN Self

//-------------------------------------------------------------------//

METHOD K_Mouse( nKey ) CLASS HBEditor
   LOCAL nRow, nCol, nJump

   Switch nKey
   case K_LBUTTONUP
      nRow := mRow()
      nCol := mCol()

      if ( nRow >= ::nTop .and. nRow <= ::nRight )
         if nCol >= ::nLeft .and. nCol <= ::nRight
            if ( ::nRow + ( nJump := nRow - ::nPhysRow ) ) <= ::naTextLen
               ::GotoPos( max( 1, ::nRow + nJump ), max( 1, ::nCol + ( nCol - ::nPhysCol ) ), .t. )
            endif
         endif
      endif
      exit
   case K_MWFORWARD
      ::Up()
      exit
   case K_MWBACKWARD
      ::Down()
      exit
   end

RETURN Self

//-------------------------------------------------------------------//
//
//                      Keystroke Handelling
//
//-------------------------------------------------------------------//

METHOD K_Ascii( nKey ) CLASS HBEditor
   LOCAL nTabCol

   // nKey := ASC( HB_ANSITOOEM( CHR( nKey ) ) )    // convert from windows

   if !(::lEditAllow)
      Return Self
   endif

   if ::IsTabCol( ::nCol )
      Return Self
   endif

   ::lDirty   := .T.
   ::nMarkPos := 0

   // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
   // Always remeber the cursor position is always 1 ahead of buffer
   // So adding 1 below - Pritpal Bedi
   //
   if ::nCol > ::LLen() + 1  // Add room at end of line.
      ::aText[ ::nRow ]:cText += Space( ::nCol - ::LLen() )
   endif

   // insert char if in insert mode or at end of current line
   //
   if ::lInsert .or. ::nCol > iif(::lWordWrap, Min( ::LLen(), ::nWordWrapCol ), ::LLen() )

      if ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled == .F.
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
      else
         ::nCol += 1
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
         ::nCol -= 1
      endif

   else

      if ::lIsLeftScrolled = NIL .or. ::lIsLeftScrolled = .F.
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
      else
         if ::lWordWrap .and. ::LLen() <= ::nWordWrapCol
            ::aText[ ::nRow ]:cText += Chr( nKey )
         endif
      endif

   endif

   // eventually wordwrap
   //
   IF ::lWordWrap .and. ::LLen() >= ::nWordWrapCol

      if ::LLen() == ::nWordWrapCol
         ::ScrollLeft()
      else
         ::RefreshLine()
      endif


      if ::LLen() > ::nWordWrapCol

         if ::lIsLeftScrolled == .T.
            ::nCol += 1
         endif

         if nKey > K_SPACE .or. ::nRow == ::naTextLen 
            ::SplitLine( ::nRow )
         endif

         if nKey == K_SPACE .or. iif(::nRow > 1, ::LLen( ::nRow-1 ) == ::nWordWrapCol+1 ,.T.)
            ::Right()
            if !::lInsert .or. ::nWordWrapCol >= ::nNumCols
               ::Home()
            endif
         elseif ::nWordWrapCol < ::nNumCols .and.;
            ::LLen() > 0 .and. ::LLen() < ::nWordWrapCol+1
            ::Right()
         else
            if ::lInsert .and. ::nCol <= ::LLen()
               ::Right()
            endif   
         endif

      else
         if ::LLen() == ::nWordWrapCol .and. ::nCol <= ::nWordWrapCol
            ::GotoCol(::nCol+1)
            ::RefreshLine()
         endif   
      endif

   ELSE
      ::RefreshLine()
      ::Right()
   ENDIF

RETURN Self

//-------------------------------------------------------------------//

METHOD K_Bs() CLASS HBEditor

LOCAL nNewCol

   if !::lEditAllow
      if ::nCol > 1
         ::GotoCol( ::nCol - 1 )
      endif

      Return Self

   endif

   if ::nCol == 1 .and. ::nRow > 1

      ::lDirty := .T.

      if ::nRow == ::naTextLen
         ::RemoveLine( ::nRow )
      endif

      if ::lWordWrap
      
         ::Up()
         ::End()

         if ::nCol == ::nWordWrapCol+1
            ::K_Del()
         endif

      endif
      
   else
      // delete previous character
      if ::nCol > 1

         ::lDirty := .T.

         nNewCol := ::TabColPos( ::nCol-1 )

         if nNewCol != nil
            ::DelVirtualTab( ::nCol-1 , nNewCol  )

         else
            if ::lIsLeftScrolled == .T.

               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, "" )

               ::GotoCol( ::nCol )

               if ::nCol > 1 .and. ::nCol < ::nLeftScrollVal
                  ::GotoCol( ::nCol+1 )
               endif

               ::RefreshWindow()

            else
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol-1, 1, "" )
               ::GotoCol( ::nCol - 1 )

            endif

            ::RefreshLine()

         endif

      endif

   endif

Return Self

//-------------------------------------------------------------------//

METHOD K_Del() CLASS HBEditor

   LOCAL nCurRow,nCurCol
   LOCAL lMerge,nNewCol
   LOCAL cText

   lMerge   := .F.
   ::lDirty := .T.

   nNewCol := ::TabColPos( ::nCol )

   if nNewCol != nil
      ::DelVirtualTab( ::nCol , nNewCol  )
   else

      IF ::nCol > ::LLen() .and. ::nRow < ::naTextLen
         // eventually pad.
         //
         IF ::nCol > ::LLen() + 1
            ::aText[ ::nRow ]:cText := Padr( ::aText[ ::nRow ]:cText, ::nCol - 1)
         ENDIF
         lMerge := .T.

      ELSEIF ::nCol <= ::LLen()
         // stuff the character
         //
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, "" )
         ::RefreshLine()

         if ::lIsLeftScrolled == .T.
            ::RefreshWindow()
         endif

      ENDIF

   endif

   // have we to merge with the next line?
   IF lMerge .and. ::lWordWrap

      nCurRow := ::nRow
      nCurCol := ::nCol

      // copy the other line
      if ::nRow+1 <= ::naTextLen
         ::aText[ ::nRow ]:cText += ::aText[ ::nRow + 1 ]:cText
         ::RefreshTabCol()
         // copy its softcr setting
         ::aText[ ::nRow ]:lSoftCr := ::aText[ ::nRow + 1 ]:lSoftCr
         // remove it.
         ::RemoveLine( ::nRow + 1 )

         if ::LLen() > ::nWordWrapCol
            cText := ::aText[ ::nRow ]:cText
            ::aText[ ::nRow ]:cText := Left( cText, ::nWordWrapCol+1 )
            cText := SubStr( cText , Len( ::aText[ ::nRow ]:cText )+1 )
            if Len( cText ) > 0
               ::InsertLine( cText , NIL , ::nRow+1 )
            endif

         endif

      endif

      ::GotoPos( nCurRow, nCurCol, .T. )
      ::RefreshWindow()

   ENDIF

RETURN Self

//-------------------------------------------------------------------//

METHOD K_Tab() CLASS HBEditor

   LOCAL lHardCR := .F.
   LOCAL i := 0
   LOCAL nTabLimit
   LOCAL lAddTabMark

   nTabLimit := iif(::lWordWrap, Max( ::nWordWrapCol , ::LLen() ) , ::LLen() ) - ::nTabWidth

   if ::nCol <= nTabLimit .or. ::lInsert

        ::lDirty := .T.

        lAddTabMark := ::lInsert .or. ::nCol >= ::LLen() .or. ( ::nCol <= ::LLen() - ::nTabWidth .and.;
                        SubStr(::aText[::nRow]:cText,::nCol,::nTabWidth) != ::cTabSpace  )


        ::AddTabCol( ::nCol , lAddTabMark  )
        ::GotoPos( ::nRow , ::nCol + ::nTabWidth , .T. )

        // wrap line if is longer than ...
        if ::lWordWrap .and. ::LLen() >= ::nWordWrapCol

           if ::nRow+1 <= ::naTextLen

                lHardCR := .F. // should already by .F., but just to be safe, and it is a tiny line of code...

                if ::IsSoftCR( ::nRow )
                    if !::IsSoftCR( ::nRow+1 )  // the next line has a hard return, keep it
                        lHardCR := .T.
                    endif

                    if ::nRow = ::naTextLen-1  // if next to last line of array, last line MUST have HR
                        lHardCR := .T.
                    endif
                endif

                if !empty( ::aText[ ::nRow+1 ]:cText )
                   ::aText[ ::nRow ]:cText += ::GetLine( ::nRow+1 )
                   ::RemoveLine( ::nRow+1 )
                else
                  if !::IsSoftCR( ::nRow+1 )
                     ::InsertLine("",.F.,::nRow+1)
                  endif
                endif

                ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR = .F.

           endif

           ::SplitLine( ::nRow )
           ::RefreshWindow()

        endif

   endif

Return Self

//-------------------------------------------------------------------//

METHOD K_Return() CLASS HBEditor

   IF ::lEditAllow

     IF ::lInsert

      ::lDirty := .T.

      // if last row
      IF ::nRow == ::naTextLen

         ::aText[ ::nRow ]:lSoftCR := .F. // HardCR

         if ::nCol > ::LLen()
            ::AddLine( "", .F. )
         else
            ::InsertLine( Substr( ::aText[ ::nRow ]:cText, ::nCol ), .F., ::nRow ) // HardCR
         endif

       ELSEIF ::IsSoftCR( ::nRow )

         ::aText[ ::nRow + 1 ]:cText := SubStr(::aText[ ::nRow ]:cText, ::nCol ) +;
         ::aText[ ::nRow + 1 ]:cText

         ::aText[ ::nRow ]:lSoftCR := .F.

         if ::lWordWrap .and. ::LLen(::nRow + 1 ) >= ::nWordWrapCol
            ::SplitLine( ::nRow + 1 )
         endif

       ELSE
         ::InsertLine( Substr( ::aText[ ::nRow ]:cText, ::nCol ), .F., ::nRow + 1 )
       ENDIF

       ::aText[ ::nRow ]:cText := Left( ::aText[ ::nRow ]:cText, ::nCol - 1 )

     ELSE

        // if last row
        IF ::nRow == ::naTextLen
           ::lDirty := .T.
           ::AddLine( "", .F. )
        ENDIF

     ENDIF

   ENDIF

   // will also refresh
   //
   IF ::nRow < ::naTextLen
      ::GotoPos( ::nRow + 1, 1, .T. )
   ENDIF

       
RETURN Self

//-------------------------------------------------------------------//

METHOD K_Esc() CLASS HBEditor()
   LOCAL cScreenMsg, nCurRow, nCurCol

   // Added message "Abort Edit? Y/N" like Clipper.
   //
   ::lExitEdit := .T.

   if ::lDirty
      if set( _SET_SCOREBOARD )
         nCurCol    := ::Col()
         nCurRow    := ::Row()
         cScreenMsg := SaveScreen( 0,60,0,77 )

         @ 0,60 say '(Abort Edit? Y/N)'
         inkey( 0 )
         RestScreen( 0, 60, 0, 77, cScreenMsg )
         SetPos( nCurRow,nCurCol )

         if lastkey() != asc( "y" ) .and. lastkey() != asc( "Y" )
            ::lExitEdit := .F.
         endif
      endif
   endif

   if ::lExitEdit
      ::nCurrentCursor := ::nOrigCursor
      SetCursor( ::nCurrentCursor )   // restore original cursor saved at startup
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
   aadd( ::aText, HBTextLine():New( cLine, lSoftCR  ) )
   ::naTextLen := Len( ::aText )
return Self

//-------------------------------------------------------------------//
//
// Insert a line of text at a defined row
//
METHOD InsertLine( cLine, lSoftCR, nRow ) CLASS HBEditor

   IF nRow >= ::naTextLen
      AAdd( ::aText, HBTextLine():New( cLine, lSoftCR ) )
   ELSE
      AIns( ::aText, nRow, HBTextLine():New( cLine, lSoftCR ), .T. )
   ENDIF
   ::naTextLen := Len( ::aText )

return Self

//-------------------------------------------------------------------//
//
// Remove a line of text
//
METHOD RemoveLine( nRow ) CLASS HBEditor

   if nRow <= ::naTextLen
      ADel( ::aText, nRow, .T. ) //  the 3rd parameter (.T.) resize array automatically.
      ::naTextLen := Len( ::aText )
   endif

return Self

//-------------------------------------------------------------------//
//
// Return line of text
//
METHOD GetLine( nRow ) CLASS HBEditor
LOCAL cText,i,cChar

   cText := ""
   // cText := ::aText[ nRow ]:cText

   if nRow > 0 .and. nRow <= ::naTextLen
      cText := StrTran(::aText[ nRow ]:cText,::cSoftCR,"")
      cText := StrTran(cText,::cHardCR,"")
   endif

   // Clean all control characters.
   if !empty( cText )
      for i := 0 to 31
         cChar := chr( i )
         cText := StrTran( cText,cChar,"" )
      next

   endif

RETURN cText

//-------------------------------------------------------------------//
// Return line lenght of a text in nRow
//
METHOD LLen( nRow ) CLASS HBEditor

Local nLen := 0

    default nRow to ::nRow

    if nRow > 0 .and. nRow <= ::naTextLen
       nLen := Len( ::aText[ nRow ]:cText )
    endif
    
Return nLen

//-------------------------------------------------------------------//

METHOD GotoLine( nRow ) CLASS HBEditor

   LOCAL lRefresh := .f.

   IF nRow <= ::naTextLen .and. nRow > 0

      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      if nRow < ::nFirstRow
         ::nFirstRow := nRow
         lRefresh := .t.

      elseif nRow - ::nFirstRow >= ::nNumRows
         ::nFirstRow := Max( 1, nRow - ::nNumRows + 1 )
         lRefresh := .t.

      endif

      ::nRow := nRow

      if !::lWordWrap
         if ::nCol > ::LLen( nRow ) + 1
            ::nCol := ::LLen( nRow ) + 1
         endif
      endif

      if ::nCol < ::nFirstCol
         ::nFirstCol := ::nCol
         lRefresh := .t.
      endif

      if ::nCol < ::nNumCols
         ::nFirstCol := 1
         lRefresh := .t.
      endif

      if lRefresh
         if ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled==.F.
           ::RefreshWindow()
         endif
      endif

      if ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled==.F.
         ::SetPos( ::nTop + nRow - ::nFirstRow, ::nLeft + ::nCol - ::nFirstCol )
      else
         ::SetPos( ::nTop + nRow - ::nFirstRow, ::nLeft + ::nCol - ::nFirstCol + ::nLeftScrollVal )
      endif

   ENDIF

RETURN Self

//-------------------------------------------------------------------//

METHOD GotoCol( nCol ) CLASS HBEditor

   IF nCol >= 1

      if ::lWordWrap

         if nCol > ::nWordWrapCol+1
            nCol := ::nWordWrapCol+1
         endif

         ::nCol := Min( ::nWordWrapCol+1, nCol )

      else

         ::nCol := nCol
      
      endif


      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      if nCol < ::nFirstCol
         ::nFirstCol := nCol
         ::RefreshWindow()

      elseif ( nCol - ::nFirstCol ) >= ::nNumCols
         ::nFirstCol := Max( 1, nCol - ::nNumCols + 1 )
         ::RefreshWindow()

      endif

      if ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled==.F.
         ::SetPos( ::Row(), ::nLeft + nCol - ::nFirstCol )
      else
         ::SetPos( ::Row(), ::nLeft + nCol - ::nFirstCol + ::nLeftScrollVal )
      endif

   ENDIF

RETURN Self

//-------------------------------------------------------------------//

METHOD GotoPos( nRow, nCol, lRefresh ) CLASS HBEditor

   DEFAULT lRefresh TO .F.

   if nRow <= ::naTextLen .AND. nRow > 0
      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      IF nRow < ::nFirstRow
         ::nFirstRow := nRow
         lRefresh := .T.
      ELSEIF ( nRow - ::nFirstRow ) >= ::nNumRows
         ::nFirstRow := Max( 1, nRow - ::nNumRows + 1 )
         lRefresh := .T.
      ENDIF
      ::nRow := nRow

   ENDIF

   IF nCol >= 1
      IF ::lWordWrap .and. nCol > ::nWordWrapCol
         nCol := ::nWordWrapCol+1
      ENDIF
      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      if nCol < ::nFirstCol
         ::nFirstCol := nCol
         lRefresh := .T.
      ELSEIF (nCol - ::nFirstCol) > ::nNumCols
         ::nFirstCol := Max( 1, nCol - ::nNumCols + 1 )
         lRefresh := .T.
      ENDIF
      ::nCol := iif(::lWordWrap, Min( ::nWordWrapCol, nCol ), nCol )
   ENDIF

   IF lRefresh
      ::RefreshWindow()
      ::SetPos( ::nTop + nRow - ::nFirstRow, ::nLeft + nCol - ::nFirstCol )
   ENDIF

RETURN Self

//-------------------------------------------------------------------//
//
// If a line of text is longer than nWordWrapCol divides it into multiple lines,
// Used during text editing to reflow a paragraph
//
//-------------------------------------------------------------------//

METHOD SplitLine( nRow ) CLASS HBEditor

   LOCAL nFirstSpace,nCurSpace
   LOCAL cSplittedLine,cLine
   LOCAL nPosInWord,nStartRow

   // Split something only if Word Wrapping is on
   if !::lWordWrap // .OR. ( ::LLen( nRow ) <= ::nWordWrapCol )
      Return Self
   endif

   ::aText[ nRow ]:lSoftCR := NIL // Cancel SoftCR and HardCR

   cLine := ::GetLine( nRow )

   // Count words up to the word containing the cursor.
   nFirstSpace := 0
   nCurSpace   := At( " ", cLine )

   while nCurSpace <= ::nCol .and. nCurSpace > 0
      nFirstSpace := nCurSpace
      nCurSpace   := At( " ", cLine, nCurSpace + 1 )
   enddo

   // and see at what point in that line the cursor is.
   // remember that nFirstSpace is zero based, and pointing to one space
   // before the current word.
   nPosInWord := iif( ::nCol > nFirstSpace, ::nCol - nFirstSpace, 1 )

   nStartRow := nRow
   cLine := GetParagraph( Self, nRow )

   while Len( cLine ) > ::nWordWrapCol 


      // Split line at first space before current position
      // Added + 1 because it is possible that line ends when there is a space
      // next to nWordWrapCol
      nFirstSpace := ::nWordWrapCol + 1

      while nFirstSpace > 0 .and. SubStr( cLine, nFirstSpace, 1 ) != " "
         nFirstSpace -= 1
      enddo

      // If there is a space before beginning of line split there
      if nFirstSpace > 0
         cSplittedLine := Left( cLine, nFirstSpace )

      else
         if Len( cLine ) > ::nWordWrapCol
            cSplittedLine := Left( cLine, ::nWordWrapCol+1 )
         else
            cSplittedLine := Left( cLine, ::nWordWrapCol )
         endif

      endif

      // We must not trim the line as split occurs next to a space
      // Insert splitted line in next row.
      ::InsertLine( cSplittedLine, .T., nStartRow++ )  // insert line with SoftCR
      cLine := SubStr( cLine, Len( cSplittedLine ) +1 )
      
   enddo

   if !::lInsert
      ::InsertLine( cLine, NIL, nStartRow++ )  // insert line without SoftCR/HardCR
   else
      if ::nCol == ::nWordWrapCol 
         if ::nWordWrapCol <= ::nNumCols
            ::RefreshLine()
         else
            if ::lWordWrap
               ::RefreshLine()
            endif   
         endif   
      else
         ::InsertLine( cLine, NIL, nStartRow++ )  // insert line without SoftCR/HardCR
      endif
   endif


   // re-count words and see where current word has gone.
   cLine := ::GetLine( nRow )

   IF ::nCol > Len( cLine )
      
      nCurSpace := At( " ", cLine )

      // stop when word count has matched OR when nCol is passed (all stay in current line).
      WHILE nCurSpace > 0 .and. nCurSpace <= ::nCol
         nCurSpace := At( " ", cLine, nCurSpace + 1 )
      ENDDO

      // next line?
      IF nCurSpace == 0
         //fake border new.
         ::nFirstCol := 1
         ::GotoPos( nRow+1, nPosInWord, .T. )
      ELSEIF nCurSpace == ::nCol
         ::GotoPos( nRow+1, 1, .T. )
      ELSE
         ::RefreshWindow()
      ENDIF

   ELSE
      ::RefreshWindow()
   ENDIF

RETURN Self

//-------------------------------------------------------------------//
//
//                         Utility Methods
//
//-------------------------------------------------------------------//
//
// This in an empty method which can be used by classes subclassing HBEditor to be able
// to handle particular keys.
//
//-------------------------------------------------------------------//

METHOD KeyboardHook( nKey )  CLASS HBEditor

RETURN Self

//-------------------------------------------------------------------//
//
// There are no more keys to handle. Can I do something for you?
//
METHOD IdleHook()  CLASS HBEditor

return Self

//-------------------------------------------------------------------//

METHOD SetColor( cColorString ) CLASS HBEditor

    LOCAL cOldColor := ::cColorSpec

    if cColorString <> nil
       ::cColorSpec := cColorString
    endif

return cOldColor

//-------------------------------------------------------------------//

METHOD Hilite() CLASS HBEditor

   LOCAL cEnhanced := ""

   // Swap CLR_STANDARD and CLR_ENHANCED
   cEnhanced += __StrToken( ::cColorSpec, 2, "," ) +  ","
   cEnhanced += __StrToken( ::cColorSpec, 1, "," )

   ::SetColor( cEnhanced + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cEnhanced ) ) )

return Self

//-------------------------------------------------------------------//

METHOD DeHilite() CLASS HBEditor

   LOCAL cStandard := ""

   // Swap CLR_STANDARD and CLR_ENHANCED back to their original position inside cColorSpec
   cStandard += __StrToken( ::cColorSpec, 2, "," ) +  ","
   cStandard += __StrToken( ::cColorSpec, 1, "," )

   ::SetColor( cStandard + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cStandard ) ) )

return Self

//-------------------------------------------------------------------//

METHOD SetPos( nRow, nCol ) CLASS HBEditor

   Default nRow to ::nPhysRow
   Default nCol to ::nPhysCol

   ::nPhysRow := nRow

   if ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled == .F.
      ::nPhysCol := nCol
   else
      ::nPhysCol := nCol - ::nLeftScrollVal
   endif

   if ::nPhysCol < ::nLeft
      ::nPhyscol := ::nLeft
   endif

   if ::nPhysCol > ::nRight
      ::nPhyscol := ::nRight
   endif

   SetPos( ::nPhysRow, ::nPhysCol )

Return ::nPhysRow

//-------------------------------------------------------------------//
//
// Changes lInsert value and insertion / overstrike mode of editor
//
METHOD InsertState( lInsState ) CLASS HBEditor
   LOCAL nCurCol,nCurRow

   IF ISLOGICAL( lInsState )
      ::lInsert := lInsState

      SET( _SET_INSERT, lInsState )

      // Please don´t change cursor size mode. 
      // In overstrike mode the cursor size is normal or user previous defined, 
      // like Clipper. In insert mode the cursor size is normal in Clipper, but in xHarbour 
      // I changed to insert size. 
      //::nCurrentCursor := iif( !::lInsert, SC_INSERT, SC_NORMAL ) 
      ::nCurrentCursor := iif( ::lInsert, SC_INSERT, ::nOrigCursor )

      if SET( _SET_SCOREBOARD )
         nCurCol := ::Col()
         nCurRow := ::Row()
         SetCursor( SC_NONE )

         if ::lInsert
            ::cScreenArea := SaveScreen( 0,60,0,67 )
            @ 0,60 say '<insert>'

         else
            if !empty( ::cScreenArea )
               RestScreen( 0,60,0,67,::cScreenArea )
               ::cScreenArea := ""
            endif

         endif
         SetPos( nCurRow,nCurCol )
      endif

      SetCursor( ::nCurrentCursor )

   ENDIF

return Self

//-------------------------------------------------------------------//
//
// Converts an array of text lines to a String
//
// Return String to Memoedit function if exit by pressing CTRL-Q
// This string can be saved in the memo file.
//
METHOD GetText() CLASS HBEditor
   LOCAL cString,cLine
   LOCAL k,m

   cString := cLine := ""

   if ::naTextLen > 1
      if ::lWordWrap
         for k := 1 to ::naTextLen-1
            cLine := ::aText[ k ]:cText + iif( Len(::aText[ k ]:cText) >= ::nWordWrapCol .or. ::IsSoftCR( k ) ,iif(::IsSoftCR( k ),::cSoftCR,::cHardCR),::cHardCR)
            cString += cLine
         next

      else
         for k := 1 to ::naTextLen-1
            cLine := ::aText[ k ]:cText + ::cHardCR
            cString += cLine
         next

      endif
   endif

   // Last line or unique line does not need a EOL delimiter
   cLine := ::aText[ ::naTextLen ]:cText
   cString += cLine

   // swapping virtual tab-char by real tab-char
   m := RAt( ::cTabSpace , cString )
   While m > 0
      cString := Stuff( cString , m , ::nTabWidth , ::cTabChar )
      m := RAt( ::cTabSpace , cString )
   enddo

Return cString

//-------------------------------------------------------------------//

METHOD GetTextIndex() CLASS HBEditor
   LOCAL nPos := 0
   LOCAL oItem, nCount
   LOCAL nEol := Len(HB_OSNewLine())

   // Using outer IF strategy to be more fast
   IF ::lWordWrap
      FOR nCount := 1 TO ::nRow - 1
         oItem := ::aText[ nCount ]
         nPos += iif( oItem:lSoftCR, 0 , nEol ) + Len( oItem:cText )
      NEXT
   ELSE
      FOR nCount := 1 TO ::nRow - 1
         oItem := ::aText[ nCount ]
         nPos += Len( oItem:cText ) + nEol
      NEXT
   ENDIF

   nPos += ::nCol
RETURN nPos

//-------------------------------------------------------------------//

METHOD LoadText( cString ) CLASS HBEditor

   ::aText := Text2Array( cString, self)
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      aadd( ::aText, HBTextLine():New() )
      ::naTextLen := Len( ::aText )
   endif

   ::lDirty := .F.
   ::GoTop()

return Self

//-------------------------------------------------------------------//

METHOD LoadFile( cFileName ) CLASS HBEditor
// This method is called in tbrwtext.prg by Debugger.

   LOCAL cString := ""

   if File( cFileName )
      ::cFile := cFileName
      cString := MemoRead( cFileName )
   endif

   ::aText := Text2Array( cString, self )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen := Len( ::aText )
   endif

   ::lDirty := .F.
   ::GoTop()

return Self

//-------------------------------------------------------------------//
//
// Saves file being edited, if there is no file name does nothing, returns .T. if OK
//
METHOD SaveFile() CLASS HBEditor

   LOCAL cString

   if !Empty( ::cFile )
      cString := ::GetText()
      ::lDirty := !MemoWrit( ::cFile, cString )
      return !::lDirty
   endif

return .F.

//-------------------------------------------------------------------//
//
// Delete next word to right at cursor position. Called by pressing CTRL-T as Clipper.
//
METHOD DelWordRight() CLASS HBEditor

   
   while ::nCol <= iif(::lWordWrap, Min( ::nWordWrapCol, ::LLen() ) , ::LLen() )

      ::K_Del()

      ::lDirty := .T.

      if SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
         exit
      endif

   end

RETURN Self

//--------------------------------------------------------------------//

METHOD AddTabCol( nCol , lAddTab ) CLASS HBEditor
LOCAL aTab

    default lAddTab to .F.

   aTab := ::aText[ ::nRow ]:aTabCol

   if Len( aTab ) == 0 .or. AScan( aTab , nCol ) == 0 .or. ::lInsert

      while ::lInsert .and. AScan( aTab, nCol ) > 0
          nCol +=1
      end

      aadd( ::aText[ ::nRow ]:aTabCol , nCol )

      if lAddTab
         ::AddVirtualTab( nCol )
      endif

   endif

Return self
//-------------------------------------------------------------------//

METHOD DelTabCol( nCol ) CLASS HBEditor
// Delete tab-mark from array-text.
LOCAL aTab

   aTab := ::aText[ ::nRow ]:aTabCol

   if Len(aTab) > 0 .and. AScan( aTab , nCol ) > 0
       ADel( ::aText[ ::nRow ]:aTabCol , nCol , .T. )
   endif

Return self

//-------------------------------------------------------------------//
//
// Insert virtual tab spaces in tab mark column.
//
METHOD AddVirtualTab( nCol ) CLASS HBEditor
LOCAL aTab,i,nTabCol,cText

   aTab  := ::aText[ ::nRow ]:aTabCol
   cText := ::aText[ ::nRow ]:cText

   if Len( aTab ) > 0
      for i := Len( aTab ) to 1 step -1 // Tab mark
          nTabCol := aTab[i]
          if nTabCol == nCol
              if SubStr( cText , nTabCol, Len(::cTabSpace) ) != ::cTabSpace .or. ::lInsert
                cText := Stuff( cText , nTabCol , 0 , ::cTabSpace )
                ::aText[ ::nRow ]:cText := cText
                exit
             endif
          endif
      next
   endif

Return self

//-------------------------------------------------------------------//
//
// Delete tab column from text-array. Delete virtual tab-spaces from text line and
// Set the new position of cursor.
//
METHOD DelVirtualTab( nCol, nTabCol ) CLASS HBEditor
LOCAL cText

   cText := ::aText[ ::nRow ]:cText

   cText := Stuff( cText, nTabCol, ::nTabWidth , "" )
   ::aText[ ::nRow ]:cText := cText

   ::DelTabCol( nCol )
   ::RefreshTabCol( )

   ::GotoPos( ::nRow , nTabCol , .T. )

Return self

//------------------------------------------------------------------//
//
// Return .t. or .f. is end of line is SoftCR
//
METHOD IsSoftCR( nRow ) CLASS HBEditor
LOCAL lRet := .F.

   if ::aText[ nRow ]:lSoftCR != NIL
      lRet := ::aText[ nRow ]:lSoftCR
   endif

Return lRet

//------------------------------------------------------------------//
//
// Return the real tab column if <nCol> is inside of tab area, otherwise return nil
//
METHOD TabColPos( nCol ) CLASS HBEditor
LOCAL aTab,i,nTabCol,nRet

    aTab := ::aText[ ::nRow ]:aTabCol

    if Len( aTab ) > 0

       for i := Len( aTab ) to 1 step -1
          nTabCol := aTab[ i ]
          if nCol >= nTabCol .and. nCol <= ( nTabCol + ::nTabWidth - 1 )
             nRet := nTabCol
             exit
          endif
       next

   endif

Return nRet
//-------------------------------------------------------------------//
//
// Return true/false if <nCol> is within tab area.
//
METHOD IsTabCol( nCol ) CLASS HBEditor
LOCAL aTab,i,nTabCol,lRet

    aTab := ::aText[ ::nRow ]:aTabCol
    lRet := .F.

    if Len( aTab ) > 0
       for i := Len( aTab ) to 1 step -1
          nTabCol := aTab[ i ]
          if nCol >= nTabCol .and. nCol <= ( nTabCol + ::nTabWidth - 1 )
             lRet := .T.
             exit
          endif
       next
    endif

Return lRet

//-------------------------------------------------------------------//

METHOD RefreshTabCol( ) CLASS HBEditor
LOCAL aTab,i,cLine,cChar

   aTab := {}
   cLine := ::aText[ ::nRow ]:cText

   i := RAt( ::cTabSpace , cLine )
   While i > 0
      aadd( aTab , i )
      cLine := Stuff( cLine , i , ::nTabWidth , "" )
      i := RAt( ::cTabSpace , cLine )
   enddo

   ::aText[ ::nRow ]:aTabCol := aTab

Return self

//-------------------------------------------------------------------//

METHOD ScrollLeft() Class HBEditor
LOCAL i,nStart

   if !::lWordWrap
      return self
   endif   

   if ::nWordWrapCol >= ::nNumCols

      nStart := ::nFirstCol + ::nLeftScrollVal

      if ::lIsLeftScrolled == NIL .or. ::lIsLeftScrolled == .F.

         ::lIsLeftScrolled := .T.

         Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, , ::nLeftScrollVal )

         if ::naTextLen > 1

            for i := 0 to Min( ::nNumRows - 1, ::naTextLen - 1 )
                DispOutAt( ::nTop + i, ::nLeft, PadR( SubStr( ::GetLine( ::nFirstRow + i ), nStart, ::nNumCols ), ::nNumCols ), ::LineColor( ::nFirstRow + i ) )
            next

         endif

         ::RefreshLine()

         ::nFirstCol += ::nLeftScrollVal

         if !::lInsert
            ::nCol := Max( ::LLen(), ::nWordWrapCol ) + 1
         else   
            if ::nCol >= ::LLen() .or. ::LLen() >= ::nWordWrapCol
               ::nCol := Max( ::LLen(), ::nWordWrapCol ) + 1
            endif
         endif
         
         if ::nCol > ::nWordWrapCol + 1
            ::nCol := ::nWordWrapCol + 1
         endif

         ::SetPos( ::Row(), ::nLeft + ::nCol - ::nFirstCol + ::nLeftScrollVal )

      endif

   else

      ::lIsLeftScrolled := .F.

      if ::nCol+1 == ::nWordWrapCol+1  // one column before limit
         ::GotoCol( ::nCol + 1 )
         ::RefreshLine()
            
      elseif ::nCol == ::nWordWrapCol+1 // in the limit
         if !::lInsert
            ::K_Return()
            ::GotoPos( ::nRow, ::nCol+1, .T. )
         else
            if ::nCol >= ::LLen()
               ::GotoPos( ::nRow+1, 1, .T. )
            endif   
         endif   

      endif

   endif

Return self

//--------------------------------------------------------------------//
//
// Show cursor position in screen and text arrray.
// Called by Edit method. It should be used only in test mode.
//
Method _ShowPos() Class HBEditor
LOCAL nRow,nCol

   nRow := ::Row()
   nCol := ::Col()

   // Test mode only.
   if ::nBottom < maxrow()-2
      @ maxrow()-2,0 say space( maxcol() )
      @ maxrow()-2,0 say " ::nTop = "+alltrim(str(::nTop))+;
                         " ::nLeft = "+alltrim(str(::nLeft))+;
                         " ::nBottom = "+alltrim(str(::nBottom))+;
                         " ::nRight = "+alltrim(str(::nRight))+;
                         " ::nWordWrapCol = "+alltrim(str(::nWordWrapCol))
      setpos( nRow, nCol )
   endif

   if ::nBottom < maxrow() -1
      @ maxrow()-1,0 say space( maxcol() )
      @ maxrow()-1,0 say " ::nRow = "+alltrim(str(::nRow))+;
                         " ::nCol = "+alltrim(str(::nCol))+;
                         " ::nPhysRow = "+alltrim(str(::nPhysRow))+;
                         " ::nPhysCol = "+alltrim(str(::nPhysCol))+;
                         " ::LLen() = "+alltrim(str(::LLen() ))  
      setpos( nRow, nCol )
   endif

   if ::nBottom < maxrow()
      @ maxrow(),0 say space( maxcol() )
      @ maxrow(),0 say " ::nFirstRow = "+alltrim(str(::nFirstRow))+;
                       " ::nFirstCol = "+alltrim(str(::nFirstCol))+;
                       " ::nWordWrapColLeftScroll = "+alltrim(str(::nWordWrapColLeftScroll))

      setpos( nRow, nCol )
   endif

Return self

//-------------------------------------------------------------------//
//
//                         Utility Functions
//
//-------------------------------------------------------------------//

//-------------------------------------------------------------------//
//
// Rebuild a long line from multiple short ones ( wrapped at soft CR )
//
//-------------------------------------------------------------------//
//
// Called by SplitLine method.
//
STATIC function GetParagraph( oSelf, nRow )

   LOCAL cLine := ""

   while oSelf:IsSoftCR( nRow )

      cLine += oSelf:GetLine( nRow )
      // I don't need to increment nRow since I'm removing lines, ie line n is
      // a different line each time I add it to cLine
      oSelf:RemoveLine( nRow )

     if nRow > oSelf:naTextLen
        exit
     endif

   enddo

   // Last line, or only one line
   //
   if nRow <= oSelf:naTextLen
      cLine += oSelf:GetLine( nRow )
      oSelf:RemoveLine( nRow )   // this is where array error occurs IF final line of text is allowed to have :lSoftCR
   endif

return cLine

//-------------------------------------------------------------------//
//
// Returns EOL char ( be it either CR or LF ,both or SoftCR )
//
//-------------------------------------------------------------------//

STATIC function WhichEOL( cString, oSelf )

   LOCAL nCRPos,nLFPos,nSoftCRPos,nHardCRPos,cEOL

   nSoftCRPos := At( oSelf:cSoftCR , cString )
   nHardCRPos := At( oSelf:cHardCR , cString )
   nCRPos     := At( Chr( 13 ), cString )
   nLFPos     := At( Chr( 10 ), cString )
   cEOL       := ""

   if nSoftCRPos > 0
      cEOL := oSelf:cSoftCR
   elseif nHardCRPos > 0
      cEOL := oSelf:cHardCR
   elseif nCRPos > 0 .AND. nLFPos == 0
      cEOL := Chr( 13 )
   elseif nCRPos == 0 .AND. nLFPos >  0
      cEOL := Chr( 10 )
   elseif nCRPos > 0 .AND. nLFPos == nCRPos + 1
      cEOL := Chr( 13 ) + Chr( 10 )
   else
      cEOL := oSelf:cHardCR
   endif

Return cEOL

//-------------------------------------------------------------------//
//
// Converts a string to an array of strings splitting input string at EOL boundaries
//
//-------------------------------------------------------------------//

STATIC function Text2Array( cString, oSelf )

   LOCAL nFirstSpace
   LOCAL i,j,k
   LOCAL cSplittedLine,cLine,cLine2,cChar,cTab
   LOCAL lHardCR,lSoftCR,lEOL
   LOCAL aArray,aTab,aHardCR,aSoftCR

   cChar   := cLine   := cLine2  := cSplittedLine := ""
   lSoftCR := lHardCR := lEOL    := .F.
   aArray  := aTab    := aHardCR := {}

   i := j:= k := nFirstSpace := 0

   if IsNil( cString ) //.or. Len( cString ) == 0
      return aArray
   endif


   For i := 1 to Len( cString )

      lEOL := .F.
      lHardCR := lSoftCR := .F.

      cLine := ""
      cLine2 += SubStr( cString, i, 1 )

      lHardCR := iif( RAt( oSelf:cHardCR, cLine2 ) > 0 , .T., .F. )
      lSoftCR := iif( RAt( oSelf:cSoftCR, cLine2 ) > 0 , .T., .F. )

      // if end of line
      if (lHardCR .or. lSoftCR)
         cLine := cLine2
         lEOL  := .T.
      elseif i == Len( cString ) // Last line or first and unique line or end of file.
         cLine := cLine2
         lEOL  := .T.
      endif

      if lEOL .and. Len(cLine) > 0

         cLine2 := ""

        // save tab position.
         aTab := {}
         for j := 1 to Len( cLine )
            cChar := SubStr( cLine, j, Len( oSelf:cTabChar ) )
            if cChar == oSelf:cTabChar
               aadd(aTab,j)
            endif
         next

         // replace tab-char by virtual tab-space
         for k := Len(aTab) to 1 step -1
            cLine := Stuff( cLine, aTab[k], 1, oSelf:cTabSpace )
         next

         // save only HardCR position in line. SoftCR always in in last position of the line.
         aHardCR := {}
         for j := 1 to Len( cLine )
            cChar := SubStr( cLine, j, Len( oSelf:cHardCR ) )
            if cChar == oSelf:cHardCR
               aadd( aHardCR,j )
            endif
         next

         // if any Tab position is greater than HardCR, this can occur when
         // nTabWidt is increased by user in memoedit init.
         for j := 1 to Len( aHardCR )
            for k := 1 to Len( aTab )
               if aTab[ k ] > aHardCR[ j ]
                  aTab[ k ] -= Len(oSelf:cHardCR)
               endif
            next
         next

         lHardCR := (At( oSelf:cHardCR, cLine ) > 0)

         // extracting HardCR and SoftCR.
         cLine := StrTran( cLine, oSelf:cHardCR, "")
         cLine := StrTran( cLine, oSelf:cSoftCR, "")


         if oSelf:lWordWrap 
         
         // Split line.
         if Len( cLine ) > oSelf:nWordWrapCol

            while !Empty( cLine )

               // Split line at nWordWrapCol boundary
               if Len( cLine ) > oSelf:nWordWrapCol
                  nFirstSpace := oSelf:nWordWrapCol+1

                  while nFirstSpace > 0 .and. SubStr( cLine, nFirstSpace ,1 ) != " "
                     nFirstSpace -= 1
                  enddo

                  if nFirstSpace > 0
                     cSplittedLine := Left( cLine, nFirstSpace )

                  else
                     if Len( cLine ) > oSelf:nWordWrapCol
                        cSplittedLine := Left( cLine, oSelf:nWordWrapCol+1 )
                     else
                        cSplittedLine := Left( cLine, oSelf:nWordWrapCol )
                     endif

                  endif

                  aadd( aArray, HBTextLine():New( cSplittedLine, .T. , aTab ) )  // SoftCR

                  cLine := LTrim( SubStr( cLine, Len( cSplittedLine )+1 ) )

               else
                  if lHardCR
                     // remainder of line is shorter than split point
                     aadd( aArray, HBTextLine():New( cLine, .F. ) ) // HardCR
                     // Done.
                     exit
                  else
                     cLine2 += cLine
                     cLine := ""
                  endif
               endif
            enddo

         else // not splitline or cLine is smaller than nWordWrapCol.

            if lHardCR
               aadd( aArray, HBTextLine():New( cLine, .F. , aTab ) )
            elseif lSoftCR
               aadd( aArray, HBTextLine():New( cLine, .T. , aTab ) )
            else
               aadd( aArray, HBTextLine():New( cLine, NIL , aTab ) )
            endif

         endif

         else // lWordWrap = false ( Debug use this, see "xharbour\source\debug\tbrwtext.prg" )

            if lHardCR
               aadd( aArray, HBTextLine():New( cLine, .F. , aTab ) )
            elseif lSoftCR
               aadd( aArray, HBTextLine():New( cLine, .T. , aTab ) )
            else
               aadd( aArray, HBTextLine():New( cLine, NIL , aTab ) )
            endif

         endif // if lWordWrap 
         
      endif
   Next

Return aArray

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
      if nKey == K_ESC .or. nkey == iif( oSelf:lToggleCTRL_WQ, K_CTRL_Q, K_CTRL_W )
         oSelf:lExitEdit := .T.

      else
         if !oSelf:MoveCursor( nKey )
            oSelf:KeyboardHook( nKey )

         endif
      endif
   enddo

return

//-------------------------------------------------------------------//

