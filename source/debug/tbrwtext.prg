/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Text file browser class
 *
 * Copyright 2008 Lorenzo Fiorini <lorenzo.fiorini@gmail.com>
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

#include "common.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"


// Color definitions and positions inside ::cColorSpec
#define  CLR_CODE       0        // color of code
#define  CLR_CURSOR     1        // color of highlighted line (the line to be executed)
#define  CLR_BKPT       2        // color of breakpoint line
#define  CLR_HIBKPT     3        // color of highlighted breakpoint line

CREATE CLASS HBBrwText INHERIT HBEditor

   VAR cFileName                                   // the name of the browsed file
   VAR nActiveLine  INIT 1                         // Active line inside Code Window (the line to be executed)
   VAR aBreakPoints INIT {}                        // Array with line numbers of active Break Points
   VAR lLineNumbers                                // If .T. source code lines are preceded by their number

   ACCESS colorSpec         INLINE ::cColorSpec
   ASSIGN colorSpec( cClr ) INLINE ::cColorSpec := cClr

   METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColor, lLineNumbers, nTabWidth )

   METHOD Left() INLINE ::ScrollTo( ::nCol - 1 ) // Scroll window one column left
   METHOD Right() INLINE ::ScrollTo( ::nCol + 1 ) // Scroll window one column left
   METHOD End() INLINE ::ScrollTo( Len( ::GetLine( ::nRow ) ) - ::nNumCols ) // Scroll window to show the end of line
   METHOD Home() INLINE ::ScrollTo( 1 )    // Scroll window to leftmost position
   METHOD RefreshAll()
   METHOD RefreshCurrent()
   METHOD Resize( nTop, nLeft, nBottom, nRight )
   METHOD ScrollTo( nCol )                         // Scroll the window to specified column
   METHOD ForceStable() INLINE NIL
   METHOD GotoLine( n )                            // Moves active line cursor
   METHOD SetActiveLine( n )                       // Sets the line to be executed
   METHOD GetLine( nRow )                          // Redefine HBEditor method to add line number
   METHOD LineColor( nRow )                        // Redefine HBEditor method to handle line coloring
   METHOD ToggleBreakPoint( nRow, lSet )           // if lSet is .T. there is a BreakPoint active at nRow, if lSet is .F. BreakPoint at nRow has to be removed
   METHOD Search( cString, lCaseSensitive, nMode ) // 0 from Begining to end, 1 Forward, 2 Backwards
   METHOD RowPos() INLINE ::nRow
ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColor, lLineNumbers, nTabWidth ) CLASS HBBrwText

   DEFAULT cColor TO SetColor()
   DEFAULT lLineNumbers TO .T.

   ::cFileName := cFileName
   ::lLineNumbers := lLineNumbers

   /* A hack to enforce cursor setting in Giancarlo's HBEditor */
   SetCursor( SC_SPECIAL1 )

   ::Super:New( "", nTop, nLeft, nBottom, nRight, .F., -1, nTabWidth )

   /* A hack to enforce cursor setting in Eduardo's HBEditor */
   ::nCurrentCursor := SC_SPECIAL1

   /* A hack to inhibit word-wrapping in Giancarlo's HBEditor */
   ::lWordWrap := .F.

   ::Super:SetColor( cColor )
   ::Super:LoadFile( cFileName )

   RETURN Self

METHOD RefreshAll() CLASS HBBrwText

   ::RefreshWindow()

   RETURN Self

METHOD RefreshCurrent() CLASS HBBrwText

   ::RefreshLine()

   return Self

METHOD SetActiveLine( n ) CLASS HBBrwText

   ::nActiveLine := n
   ::RefreshWindow()

   RETURN Self

METHOD GotoLine( n ) CLASS HBBrwText

   ::Super:GotoLine( n )

   RETURN Self

METHOD GetLine( nRow ) CLASS HBBrwText
   RETURN iif( ::lLineNumbers, AllTrim( Str( nRow ) ) + ": ", "" ) + ::Super:GetLine( nRow )

METHOD LineColor( nRow ) CLASS HBBrwText

   LOCAL lHilited := ( nRow == ::nActiveLine )
   LOCAL lBreak := AScan( ::aBreakPoints, nRow ) > 0
   LOCAL nIndex := CLR_CODE

   IF lHilited
      nIndex += CLR_CURSOR
   ENDIF
   IF lBreak
      nIndex += CLR_BKPT
   ENDIF

   RETURN hb_ColorIndex( ::cColorSpec, nIndex )

METHOD ToggleBreakPoint( nRow, lSet) CLASS HBBrwText

   LOCAL nAt := AScan( ::aBreakPoints, nRow )

   IF lSet
      // add it only if not present
      IF nAt == 0
         AAdd( ::aBreakPoints, nRow)
      ENDIF
   ELSEIF nAt != 0
#ifdef HB_C52_STRICT
         ADel( ::aBreakPoints, nAt )
         ASize( ::aBreakPoints, Len( ::aBreakPoints ) - 1 )
#else
         ADel( ::aBreakPoints, nAt, .T. )
#endif
      
   ENDIF

   RETURN Self

/* This method is to restore correct cursor position after ::Super:Resize() */
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBBrwText
   LOCAL nRow

   nRow := ::nRow
   ::Super:Resize( nTop, nLeft, nBottom, nRight )
   ::GotoLine( nRow )
   RETURN Self


METHOD ScrollTo( nCol ) CLASS HBBrwText
   IF nCol >= 1
      ::nCol := nCol
      ::nFirstCol := nCol
      ::RefreshWindow()
      ::SetPos( ::Row(), ::nLeft )
   ENDIF
RETURN Self


METHOD Search( cString, lCaseSensitive, nMode ) CLASS HBBrwText

   LOCAL nFrom
   LOCAL nTo
   LOCAL nStep
   LOCAL nFor
   LOCAL lFound := .F.

   DEFAULT lCaseSensitive TO .F.
   DEFAULT nMode          TO 0

   IF !lCaseSensitive
      cString := Upper( cString )
   ENDIF

   DO CASE
   CASE nMode == 0 // From Top
      nFrom := 1
      nTo   := Len( ::aText )
      nStep := 1
   CASE nMode == 1 // Forward
      nFrom := Min( ::nRow + 1, Len( ::aText ) )
      nTo   := Len( ::aText )
      nStep := 1
   CASE nMode == 2 // Backward
      nFrom := Max( ::nRow - 1, 1 )
      nTo   := 1
      nStep := -1
   ENDCASE

   FOR nFor := nFrom TO nTo STEP nStep
      IF cString $ iif( lCaseSensitive, ::GetLine( nFor ), Upper( ::GetLine( nFor ) ) )
         lFound := .T.
         ::GotoLine( nFor )
         EXIT
      ENDIF
   NEXT

   RETURN lFound
