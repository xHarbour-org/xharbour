/*
 * $Id: achoice.prg,v 1.17 2004/02/20 00:31:23 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * ACHOICE() function
 *
 * Released to Public Domain by Peter Townsend <cephas@tpgi.com.au>
 * www - http://www.harbour-project.org
 *
 */

#include "achoice.ch"
#include "color.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define INRANGE( xLo, xVal, xHi )       ( xVal >= xLo .AND. xVal <= xHi )
#define BETWEEN( xLo, xVal, xHi )       Min( Max( xLo, xVal ), xHi )

FUNCTION AChoice( nTop, nLeft, nBottom, nRight, acItems, xSelect, xUserFunc, nPos, nHiLiteRow )

   LOCAL nNumCols                          // Number of columns in the window
   LOCAL nNumRows                          // Number of rows in the window
   LOCAL nRowsClr                          // Number of rows to clear
   LOCAL acCopy    := {}                   // A padded copy of the items
   LOCAL alSelect                          // Select permission
   LOCAL nNewPos   := 0                    // The next item to be selected
   LOCAL lFinished := .F.                  // Is processing finished?
   LOCAL nKey      := 0                    // The keystroke to be processed
   LOCAL nMode     := AC_IDLE              // The current operating mode
   LOCAL nAtTop    := 1                    // The number of the item at the top
   LOCAL nAtBtm    := 1                    // The number of the item at the bottom
   LOCAL nItems    := 0                    // The number of items
   LOCAL nGap      := 0                    // The number of lines between top and current lines
                                           // Block used to search for items
   LOCAL lUserFunc                         // Is a user function to be used?
   LOCAL nUserFunc := 0                    // Return value from user function
   LOCAL nSaveCsr  := SetCursor( SC_NONE )
   LOCAL nFrstItem := 0
   LOCAL nLastItem := 0

   LOCAL nCntr
   LOCAL bAction
   LOCAL cKey

   LOCAL bSelect   := {|x,y| if(ISLOGICAL(x), x, if(!Empty( x ), (y := &( x ),if(ISLOGICAL(y), y, .T.)), .T.)) }

   ColorSelect( CLR_STANDARD )

   lUserFunc := !Empty( xUserFunc ) .AND. ValType( xUserFunc ) IN "CB"


   DEFAULT nTop       TO 0                 // The topmost row of the window
   DEFAULT nLeft      TO 0                 // The leftmost column of the window
   DEFAULT nBottom    TO MaxRow()          // The bottommost row of the window
   DEFAULT nRight     TO MaxCol()          // The rightmost column of the window

   DEFAULT acItems    TO {}                // The items from which to choose
   DEFAULT xSelect    TO .T.               // Array or logical, what is selectable
   DEFAULT nPos       TO 1                 // The number of the selected item
   DEFAULT nHiLiteRow TO 0                 // The row to be highlighted

   IF nRight > MaxCol()
      nRight := MaxCol()
   ENDIF

   IF nBottom > MaxRow()
      nBottom := MaxRow()
   ENDIF

   nNumCols := nRight - nLeft + 1
   nNumRows := nBottom - nTop + 1


   IF VALTYPE( xSelect ) == "A"
      alSelect := xSelect
   ELSE
      alSelect := ARRAY( LEN( acItems ) )
      AFill( alSelect, xSelect )
   ENDIF

   IF Len( acItems ) <> Len( alSelect )
      aSize( alSelect, Len( acItems ) )
   ENDIF

   IF !lFinished

      nMode := Ach_Limits( @nFrstItem, @nLastItem, @nItems, bSelect, alSelect, acItems )
      IF nMode == AC_NOITEM
         nPos := 0
      ENDIF

      // Ensure hilighted item can be selected
      nPos := BETWEEN( nFrstItem, nPos, nLastItem )

      // Force hilighted row to be valid
      nHiLiteRow := BETWEEN( 0, nHiLiteRow, nNumRows - 1 )

      // Force the topmost item to be a valid index of the array
      nAtTop := BETWEEN( 1, Max( 1, nPos - nHiLiteRow ), nItems )

      // Ensure as much of the selection area as possible is covered
      IF ( nAtTop + nNumRows - 1 ) > nItems
         nAtTop := Max( 1, nItems - nNumrows + 1 )
      ENDIF

      DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect, nItems )

   ENDIF

   DO WHILE !lFinished

      SetPos( nTop + ( nPos - nAtTop ), nLeft )

      SWITCH nMode
      CASE AC_IDLE
         nKey  := Inkey( 0 )
         EXIT
      CASE AC_GOTO
      CASE AC_NOITEM
         nKey := 0
         EXIT
      DEFAULT
         nKey  := 0
         nMode := AC_IDLE
      END


      DO CASE
      CASE ( bAction := SetKey( nKey ) ) != NIL

         Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
         IF Empty( NextKey() )
            KEYBOARD Chr( 255 )
            Inkey()
            nKey := 0
         ENDIF

         nRowsClr := Min( nNumRows, nItems )
         nMode := Ach_Limits( @nFrstItem, @nLastItem, @nItems, bSelect, alSelect, acItems )

         IF nMode == AC_NOITEM
            nPos := 0
            nAtTop := Max( 1, nPos - nNumRows + 1 )
         ELSE
            DO WHILE nPos < nLastItem .and. !Eval( bSelect, alSelect[ nPos ] )
               nPos++
            ENDDO

            IF nPos > nLastItem
               nPos := BETWEEN( nFrstItem, nPos, nLastItem )
            ENDIF

            nAtTop := MIN( nAtTop, nPos )

            IF nAtTop + nNumRows - 1 > nItems
               nAtTop := BETWEEN( 1, nPos - nNumRows + 1, nItems - nNumRows + 1 )
            ENDIF

            IF nAtTop < 1
               nAtTop := 1
            ENDIF

         ENDIF

         DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect, nRowsClr )

      CASE ( nKey == K_ESC .OR. nMode == AC_NOITEM ) .AND. !lUserFunc
         IF nPos > 0
            DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
         ENDIF
         nMode     := AC_ABORT
         nPos      := 0
         lFinished := .T.

      CASE nKey == K_UP

         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nAtTop > Max( 1, nPos - nNumRows + 1 )
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ELSE
            nNewPos := nPos - 1
            DO WHILE !Eval( bSelect, alSelect[ nNewPos ] )
               nNewPos --
            ENDDO
            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ELSE
               DispBegin()
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               Scroll( nTop, nLeft, nBottom, nRight, ( nNewPos - ( nAtTop + nNumRows - 1 ) ) )
               nAtTop := nNewPos
               if nNumRows > 1
                  nPos   := Max( nPos, nAtTop + nNumRows - 1 )
                  DO WHILE nPos > nNewPos
                     DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
                     nPos --
                  ENDDO
               else
                  nPos--
               endif
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
               DispEnd()
            ENDIF
         ENDIF

      CASE nKey == K_DOWN

         // Find the next selectable item to display
         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nAtTop < Min( nPos, nItems - nNumRows + 1 )
               nAtTop := Min( nPos, nItems - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ELSE

            nNewPos := nPos + 1

            DO WHILE !Eval( bSelect, alSelect[ nNewPos ] )
               nNewPos ++
            ENDDO

            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ELSE
               DispBegin()
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               Scroll( nTop, nLeft, nBottom, nRight, ( nNewPos - ( nAtTop + nNumRows - 1 ) ) )
               nAtTop := nNewPos - nNumRows + 1
               nPos   := Max( nPos, nAtTop )
               DO WHILE nPos < nNewPos
                  DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
                  nPos ++
               ENDDO
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
               DispEnd()
            ENDIF

         ENDIF

      CASE nKey == K_CTRL_PGUP .OR. ( nKey == K_HOME .AND. !lUserFunc )

         IF nPos == nFrstItem
            IF nAtTop == Max( 1, nPos - nNumRows + 1 )
//               nMode := AC_HITTOP
            ELSE
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nFrstItem, nAtTop + nNumRows - 1 )
               // On same page as nFrstItem
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos   := nFrstItem
               nAtTop := nPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ELSE
               nPos   := nFrstItem
               nAtTop := nPos
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ENDIF

      CASE nKey == K_CTRL_PGDN .OR. ( nKey == K_END .AND. !lUserFunc )

         IF nPos == nLastItem
            IF nAtTop == Min( nLastItem, Max( 1, nItems - nNumRows + 1 ) )
//               nMode := AC_HITBOTTOM
            ELSE
               nAtTop := Min( nLastItem, Max( 1, nItems - nNumRows + 1 ) )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos := nLastItem
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ELSE
               nPos   := nLastItem
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ENDIF

      CASE nKey == K_CTRL_HOME

         IF nPos == nFrstItem
            IF nAtTop == Max( 1, nPos - nNumRows + 1 )
//               nMode := AC_HITTOP
            ELSE
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ELSE
            nNewPos := nAtTop
            DO WHILE !Eval( bSelect, alSelect[ nNewPos ] )
               nNewPos++
            ENDDO
            IF nNewPos != nPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ENDIF
         ENDIF

      CASE nKey == K_CTRL_END

         IF nPos == nLastItem
            IF nAtTop == Min( nPos, Max( 1, nItems - nNumRows + 1 ) )
//               nMode := AC_HITBOTTOM
            ELSE
               nAtTop := Min( nPos, Max( 1, nItems - nNumRows + 1 ) )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ELSE
            nNewPos := Min( nAtTop + nNumRows - 1, nItems )
            DO WHILE !Eval( bSelect, alSelect[ nNewPos ] )
               nNewPos--
            ENDDO
            IF nNewPos != nPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ENDIF
         ENDIF

      CASE nKey == K_PGUP .OR. nKey == K_MWFORWARD

         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nPos > 0
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
            ENDIF
            IF nAtTop > Max( 1, nPos - nNumRows + 1 )
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ELSE
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nFrstItem, nAtTop + nNumRows - 1 )
               // On same page as nFrstItem
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos   := nFrstItem
               nAtTop := Max( nPos - nNumRows + 1, 1 )
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ELSE
               IF ( nPos - nNumRows + 1 ) < nFrstItem
                  nPos   := nFrstItem
                  nAtTop := nFrstItem
               ELSE
                  nPos   := Max( nFrstItem, nPos - nNumRows + 1 )
                  nAtTop := Max( 1, nAtTop - nNumRows + 1 )
                  DO WHILE nPos > nFrstItem .AND. !Eval( bSelect, alSelect[ nPos ] )
                     nPos--
                     nAtTop--
                  ENDDO
                  nAtTop := Max( 1, nAtTop )
               ENDIF
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ENDIF

      CASE nKey == K_PGDN .OR. nKey == K_MWBACKWARD

         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nPos > 0
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
            ENDIF
            IF nAtTop < Min( nPos, Max( 1, nItems - nNumRows + 1 ) )
               nAtTop := Min( nPos, Max( 1, nItems - nNumRows + 1 ) )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ELSE
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               // On the same page as nLastItem
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos := nLastItem
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ELSE
               nGap := nPos - nAtTop
               nPos := Min( nLastItem, nPos + nNumRows - 1 )
               IF ( nPos + nNumRows - 1 ) > nLastItem
                  // On the last page
                  nAtTop := nLastItem - nNumRows + 1
                  nPos   := Min( nLastItem, nAtTop + nGap )
               ELSE
                  // Not on the last page
                  nAtTop := nPos - nGap
               ENDIF
               // Make sure that the item is selectable
               DO WHILE nPos < nLastItem .AND. !Eval( bSelect, alSelect[ nPos ] )
                  nPos++
                  nAtTop++
               ENDDO
               // Don't leave blank space on the page
               DO WHILE ( nAtTop + nNumRows - 1 ) > nItems
                  nAtTop--
               ENDDO
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ENDIF

      CASE nKey == K_ENTER .AND. !lUserFunc
         nMode     := AC_SELECT
         lFinished := .T.

      CASE nKey == K_LBUTTONDOWN .or. (nKey == K_LDBLCLK .AND. !lUserFunc )
         nNewPos := Hittest( nTop, nLeft, nBottom, nRight )

         IF nNewPos >= 0
            nNewPos += nAtTop
            IF Eval( bSelect, alSelect[ nNewPos ] )
               nPos := nNewPos
            ENDIF
         ENDIF

         IF nKey == K_LDBLCLK
            nMode     := AC_SELECT
            lFinished := .T.
         ENDIF

      CASE nKey == K_RIGHT .AND. !lUserFunc
         IF nPos > 0
            DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
         ENDIF
         nPos      := 0
         lFinished := .T.

      CASE nKey == K_LEFT .AND. !lUserFunc
         IF nPos > 0
            DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
         ENDIF
         nPos      := 0
         lFinished := .T.

      CASE INRANGE( 32, nKey, 255 ) .AND. ( !lUserFunc .OR. nMode == AC_GOTO )

         cKey := Upper( Chr( nKey ) )

         // Find next selectable item
         FOR nNewPos := nPos + 1 TO nItems
            IF Eval( bSelect, alSelect[ nNewPos ] ) .AND. Upper( Left( acItems[ nNewPos ], 1 ) ) == cKey
               EXIT
            ENDIF
         NEXT
         IF nNewPos == nItems + 1
            FOR nNewPos := 1 TO nPos - 1
               IF Eval( bSelect, alSelect[ nNewPos ] ) .AND. Upper( Left( acItems[ nNewPos ], 1 ) ) == cKey
                  EXIT
               ENDIF
            NEXT
         ENDIF

         IF nNewPos != nPos
            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               // On same page
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .T., nNumCols )
            ELSE
               // On different page
               nPos   := nNewPos
               nAtTop := BETWEEN( 1, nPos - nNumRows + 1, nItems )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect )
            ENDIF
         ENDIF

         nMode := AC_IDLE

      CASE nMode == AC_GOTO

         // Garbage collect gotos which aren't valid ASCII characters
         nMode := AC_IDLE

      OTHERWISE

         IF nMode != AC_NOITEM
            IF nKey == 0 // No keystroke
               nMode := AC_IDLE
            ELSE
               nMode := AC_EXCEPT
            ENDIF
         ENDIF

      ENDCASE

      IF lUserFunc

         IF nKey == K_LDBLCLK
            nNewPos := Hittest( nTop, nLeft, nBottom, nRight )
            IF nNewPos >= 0
               nNewPos += nAtTop
               IF Eval( bSelect, alSelect[ nNewPos ] )
                  nPos := nNewPos
                  /* EMULATE ENTER */
                  SetLastKey( K_ENTER )
               ENDIF
            ENDIF
         ENDIF

         nUserFunc := Do( xUserFunc, nMode, nPos, nPos - nAtTop )

         DO CASE
         CASE nUserFunc == AC_ABORT .OR. (nMode == AC_NOITEM .AND. ;
              Ach_Limits( @nFrstItem, @nLastItem, @nItems, bSelect, alSelect, acItems ) == AC_NOITEM )
            lFinished := .T.
            IF nPos > 0
               DispLine( acItems[ nPos ], nTop + ( nPos - nAtTop ), nLeft, Eval( bSelect, alSelect[ nPos ]), .F., nNumCols )
            ENDIF
            nPos := 0

         CASE nUserFunc == AC_SELECT
            lFinished := .T.

         CASE nUserFunc == AC_CONT .OR. nUserFunc == AC_REDRAW
            // Do nothing
            nMode := AC_CONT

         CASE nUserFunc == AC_GOTO
            // Do nothing.  The next keystroke won't be read and
            // this keystroke will be processed as a goto.
            nMode := AC_GOTO
         ENDCASE

         IF nPos > 0

            nRowsClr := Min( nNumRows, nItems )
            nMode := Ach_Limits( @nFrstItem, @nLastItem, @nItems, bSelect, alSelect, acItems )

            IF nMode == AC_NOITEM
               nPos := 0
               nAtTop := Max( 1, nPos - nNumRows + 1 )
            ELSE
               DO WHILE nPos < nLastItem .and. !Eval( bSelect, alSelect[ nPos ] )
                  nPos++
               ENDDO

               IF nPos > nLastItem
                  nPos := BETWEEN( nFrstItem, nPos, nLastItem )
               ENDIF

               nAtTop := MIN( nAtTop, nPos )

               IF nAtTop + nNumRows - 1 > nItems
                  nAtTop := BETWEEN( 1, nPos - nNumRows + 1, nItems - nNumRows + 1 )
               ENDIF

               IF nAtTop < 1
                  nAtTop := 1
               ENDIF

            ENDIF

            DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, bSelect, nRowsClr )
         ENDIF
      ENDIF

   ENDDO

   SetCursor( nSaveCsr )

   RETURN nPos

STATIC PROCEDURE DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nArrLen, bSelect, nRowsClr )

   LOCAL nCntr
   LOCAL nRow                              // Screen row
   LOCAL nIndex                            // Array index
   LOCAL nSaveRow := Row()                 // Position at start of routine
   LOCAL nSaveCol := Col()                 // Position at start of routine

   Default nRowsClr to nNumRows

   DispBegin()

   FOR nCntr := 1 TO Min( nNumRows, nRowsClr )

      nRow   := nTop + nCntr - 1
      nIndex := nCntr + nAtTop - 1

      IF INRANGE( 1, nIndex, nArrLen )
         DispLine( acItems[ nIndex ], nRow, nLeft, Eval( bSelect, alSelect[ nIndex ] ), nIndex == nPos, nRight - nLeft + 1 )
      ELSE
         ColorSelect( CLR_STANDARD )
         DispOutAt( nRow, nLeft, Space( nRight - nLeft + 1 ) )
      ENDIF
   NEXT

   DispEnd()

   SetPos( nSaveRow, nSaveCol )

   RETURN

STATIC PROCEDURE DispLine( cLine, nRow, nCol, lSelect, lHiLite, nNumCols )

   ColorSelect( iif( lSelect .AND. ISCHARACTER( cLine ), ;
                iif( lHiLite, CLR_ENHANCED, CLR_STANDARD ), CLR_UNSELECTED ) )

   IF ISCHARACTER( cLine )
      DispOutAt( nRow, nCol, PadR( cLine, nNumCols ) )
   ELSE
      DispOutAt( nRow, nCol, Space( nNumCols ) )
   ENDIF

   ColorSelect( CLR_STANDARD )

   RETURN


STATIC FUNCTION Ach_Limits( nFrstItem, nLastItem, nItems, bSelect, alSelect, acItems )

   LOCAL nMode, cItems

   nItems := Len( acItems )

   FOR EACH cItems IN acItems
/*
      IF ISCHARACTER( cItems )
         nItems++
      ELSE
         EXIT
      ENDIF
*/
      IF !ISCHARACTER( cItems )
         nItems := HB_EnumIndex() - 1
         EXIT
      ENDIF
   NEXT

   nFrstItem := AScan( alSelect, bSelect )  // First valid item

   IF nFrstItem == 0
      nLastItem := 0
   ELSE
      nMode     := AC_IDLE
      nLastItem := nItems               // Last valid item
      DO WHILE nLastItem>0 .and. !Eval( bSelect, alSelect[ nLastItem ] )
         nLastItem--
      ENDDO
   ENDIF

   IF nLastItem <= 0
      nMode     := AC_NOITEM
      nLastItem := nItems
   ENDIF


   RETURN nMode


STATIC FUNCTION Hittest( nTop, nLeft, nBottom, nRight )
   LOCAL nPos
   LOCAL nMouseRow := MRow()
   LOCAL nMouseCol := MCol()

   nPos := iif( ;
         nMouseRow >= nTop   .and. nMouseRow <= nBottom .and.;
         nMouseCol >= nLeft  .and. nMouseCol <= nRight,  ;
         nMouseRow - nTop, -1 )
RETURN nPos
