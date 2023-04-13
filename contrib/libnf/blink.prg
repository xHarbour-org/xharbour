/*
 * $Id$
 */
/*
 * File......: BLINK.PRG
 * Author....: Terry Hackett
 * CIS ID....: 76662,2035
 *
 * This is an original work by Terry Hackett and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:56   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:06   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:46   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_BLINK()
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Display a blinking message on the screen
 *  $SYNTAX$
 *     FT_BLINK( <cMsg>, [ <nRow> ], [ <nCol> ] ) -> NIL
 *  $ARGUMENTS$
 *     <cMsg> is the string to blink.
 *
 *     <nRow> is an optional screen row for @...SAY, default current.
 *
 *     <nCol> is an optional screen col for @...say, default current.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     A quick way to blink a msg on screen in the CURRENT colors.
 *     Restores colors on return.
 *  $EXAMPLES$
 *     FT_BLINK( "WAIT", 5, 10 )   // Blinks "WAIT" in current colors @ 5,10
 *
 *     @5,10 SAY "WAIT - Printing Report"
 *     FT_BLINK( "..." )           //  Blink "..." after wait message...
 *  $END$
 */

STATIC cxMsg, nxRow, nxCol, nxTask := NIL
STATIC tAnterior, lDoit, nxWait
STATIC aFT_Blinks := {}

#ifdef FT_TEST

FUNCTION MAIN()

   FT_BLINK( "WAIT", 5, 10 )

   RETURN ( nil )

#endif

FUNCTION FT_BLINK( cMsg, nRow, nCol )

// Declare color restore var.
   LOCAL cSavColor, lBlinkStatus

// Return if no msg.
   IF ( cMsg == NIL ) ; RETURN NIL; ENDIF

// Set default row and col to current.
   nRow := IF( nRow == NIL, Row(), nRow )
   nCol := IF( nCol == NIL, Col(), nCol )

   cSavColor := SetColor()                // Save colors to restore on exit.
   lBlinkStatus := SetBlink( .T. )        // Save setblink

// IF blink colors not already set, add blink to current foreground color.

   SetColor( IF( ("*" $ Left(cSavColor,4 ) ), cSavColor, "*" + cSavColor ) )

   @ nRow, nCol SAY cMsg                  // Say the dreaded blinking msg.
   SetColor( cSavColor )                  // It's a wrap, restore colors & exit.
   SetBlink( lBlinkStatus )               // Restore setblink status

   RETURN NIL

FUNCTION FT_BLINKW32( cMsg, nRow, nCol, nWait )

// Return if no msg.
   IF ( cMsg == NIL ) ; RETURN NIL; ENDIF

// Set default row and col to current.
   nRow      := iif( nRow == NIL, Row(), nRow )
   nCol      := iif( nCol == NIL, Col(), nCol )

   nxWait    := iif( nWait == NIL, 1, nWait )
   cxMsg     := cMsg
   nxRow     := nRow
   nxCol     := nCol
   tAnterior := NIL
   lDoit     := NIL
   IF ( nxTask == NIL )
      nxTask := hb_idleAdd( {|| FT_BLINKW32IT() } )
      aFT_Blinks := { { nxRow, nxCol, cxMsg } }
   ELSE
      AAdd( aFT_Blinks, { nxRow, nxCol, cxMsg } )
   ENDIF

   RETURN NIL

FUNCTION FT_BLINKW32CANCEL()

   aFT_Blinks := {}
   hb_idleDel( nxTask )
   nxTask := NIL

   RETURN NIL

FUNCTION FT_BLINKW32IT()

   LOCAL cSavColor, lBlinkStatus
   LOCAL i, aBlinks

   tAnterior := iif( tAnterior == NIL, Seconds(), tAnterior )
   lDoit     := iif( lDoit == NIL, .F. , lDoit )
   IF Seconds() < tAnterior    // Hemos pasado la medianoche
      tAnterior := Seconds()
   ENDIF
   IF Seconds() - tAnterior > nxWait
      tAnterior := Seconds()
      cSavColor    := SetColor()
      lBlinkStatus := SetBlink( .T. )
      SetColor( IF( ("*" $ Left(cSavColor,4 ) ), cSavColor, "*" + cSavColor ) )
      IF Len( aFT_BLINKS ) < 1
         RETURN NIL
      ENDIF
      FOR i := 1 TO Len( aFT_Blinks )
         aBlinks := aFT_Blinks[ i ]
         IF lDoit
            @ aBlinks[ 1 ], aBlinks[ 2 ] SAY Space( Len( aBlinks[ 3 ] ) )
         ELSE
            @ aBlinks[ 1 ], aBlinks[ 2 ] SAY aBlinks[ 3 ]
         ENDIF
      NEXT
      IF lDoit
         lDoit := .F.
      ELSE
         lDoit := .T.
      ENDIF
      SetColor( cSavColor )
      SetBlink( lBlinkStatus )
   ENDIF

   RETURN NIL
