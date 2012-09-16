/*
 * $Id$
 */
/*
 * File......: CALENDAR.PRG
 * Author....: Isa Asudeh
 * CIS ID....: 76477,647
 *
 * This is an original work by Isa Asudeh and is placed in the
 * public domain.
 *
 * Modification history
 * --------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   31 May 1991 21:07:26   GLENN
 * Initial revision.
 *
 */



/*  $DOC$
 *  $FUNCNAME$
 *     FT_CALENDAR()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Display date/time calendar, find a date, return calendar data.
 *  $SYNTAX$
 *     FT_CALENDAR ( [ <nRow> ], [ <nCol> ], [ <cColor> ], [ <lShadow> ] ,
 *                   [ <lShowHelp> ] ) -> aRetVal
 *  $ARGUMENTS$
 *
 *     <nRow> is an optional screen row for calendar display,
 *            default row 1.
 *
 *     <nCol> is an optional screen col for calendar display,
 *            default col 63.
 *
 *     <cColor> is an optional color string for displayed messages,
 *              default is bright white text over green background.
 *
 *     <lShadow> is an optional logical variable. If true (.T.),
 *               it uses FT_SHADOW() to add a transparent shadow
 *               to the display, default (.F.).
 *
 *     <lShowHelp> is an optional logical variable. If true, uses
 *                 FT_XBOX to display  a four line help message
 *                 if the F1 key is pressed, default (.F.).
 *
 *  $RETURNS$
 *     aRetVal  is an 8 element array containing date, month, day, year,
 *              month (in character format), day of the week, julian day
 *              and current time.
 *
 *  $DESCRIPTION$
 *     FT_CALENDAR() simply displays today's date, time and julian
 *     day in a two line display with an optional box shadow. Cursor keys may
 *     be used to page through the calendar by day, week, month or year
 *     increments. Returns an 8 element array of calendar data:
 *
 *     Element  Value
 *     [1]      Date in current date format.
 *     [2]      Numeric month number.
 *     [3]      Numeric day number.
 *     [4]      Numeric year number.
 *     [5]      Month in character format.
 *     [6]      Day of the week in character format.
 *     [7]      Numeric Julian day.
 *     [8]      Current time in time format.
 *
 *     WARNING: FT_CALENDAR uses FT_SHADOW and FT_XBOX
 *              from the Nanforum Toolkit!
 *
 *  $EXAMPLES$
 *
 *   LOCAL aRetVal[8]
 *   CLS
 *   aRetVal := FT_CALENDAR (10,40,'W+/RB',.T.,.T.)
 *   ?aRetVal[1]      // Result: 04/20/91
 *   ?aRetVal[2]      // Result:   4
 *   ?aRetVal[3]      // Result:  20
 *   ?aRetVal[4]      // Result:  1991
 *   ?aRetVal[5]      // Result: April
 *   ?aRetVal[6]      // Result: Saturday
 *   ?aRetVal[7]      // Result:        110
 *   ?aRetVal[8]      // Result: 12:45:20
 *
 *  $SEEALSO$
 *     FT_DAYOFYR()
 *
 *  $END$
 */

#ifdef FT_TEST

FUNCTION MAIN()

   LOCAL aRet[8], i

   SetColor ( 'w+/b' )
   cls
   IF ft_numlock()
      ft_numlock( .F. )
   ENDIF
   KEYBOARD Chr ( 28 )
   aRet := ft_calendar ( 10, 40, 'w+/rb', .T. , .T. ) //display calendar, return all.
   @1, 0 SAY 'Date        :' + DToC( aRet[1] )
   @2, 0 SAY 'Month Number:' + Str( aRet[2], 2, 0 )
   @3, 0 SAY 'Day Number  :' + Str( aRet[3], 2, 0 )
   @4, 0 SAY 'Year Number :' + Str( aRet[4], 4, 0 )
   @5, 0 SAY 'Month       :' + aRet[5]
   @6, 0 SAY 'Day         :' + aRet[6]
   @7, 0 SAY 'Julian Day  :' + Str( aRet[7], 3, 0 )
   @8, 0 SAY 'Current Time:' + aRet[8]

   RETURN ( nil )

#endif

#include "inkey.ch"

FUNCTION FT_CALENDAR ( nRow, nCol, cColor, lShadow, lShowHelp )

   LOCAL  nJump := 0, nKey := 0, cSavColor, cSaveScreen, cSaveCursor
   LOCAL  aRetVal[8]
   LOCAL  nHelpRow, cSaveHelp, lHelpIsDisplayed := .F.
   LOCAL  nDay1, nDay2

   nRow    := iif ( nRow <> NIL, nRow, 1 )           //check display row
   nCol    := iif ( nCol <> NIL, nCol, 63 )           //check display col
   cColor  := iif ( cColor <> NIL, cColor, 'W+/G' )  //check display color
   lShadow := iif ( lShadow == NIL , .F. , lShadow )  //check shadow switch
   lShowHelp := iif ( lShowHelp == NIL , .F. , lShowHelp )//check help switch

   nRow := iif ( nRow < 1 .OR. nRow > 21,  1, nRow )   //check row bounds
   nCol := iif ( nCol < 1 .OR. nCol > 63, 63, nCol )   //check col bounds

   cSavColor   := SetColor( cColor )  //save current and set display color
   cSaveScreen := SaveScreen ( nRow - 1, nCol - 1, nRow + 3, nCol + 17 ) //save screen
   cSaveCursor := SetCursor ( 0 )     // save current and turn off cursor

   IF lShadow
      @nRow - 1, nCol - 1 TO nRow + 2, nCol + 15
      hb_Shadow( nRow - 1, nCol - 1, nRow + 2, nCol + 15 )
   ENDIF

   IF lShowHelp
      nHelpRow := iif ( nRow > 10 , nRow - 10 , nRow + 6 )
   ENDIF

   DO WHILE nKey <> K_ESC

      DO CASE
      CASE nKey == K_HOME
         nJump = nJump - 1

      CASE nKey == K_END
         nJump = nJump + 1

      CASE nKey == K_UP
         nDay1  := Day( Date() + nJump )
         nDay2  := Day( Date() + nJump - 30 )
         nDay1  := nDay2 - nDay1
         nJump = nJump - 30 - nDay1

      CASE nKey == K_DOWN
         nDay1  := Day( Date() + nJump )
         nDay2  := Day( Date() + nJump + 30 )
         nDay1  := nDay2 - nDay1
         nJump = nJump + 30 - nDay1

      CASE nKey == K_PGUP
         nDay1  := Day( Date() + nJump )
         nDay2  := Day( Date() + nJump - 365 )
         nDay1  := nDay2 - nDay1
         nJump = nJump - 365 - nDay1

      CASE nKey == K_PGDN
         nDay1  := Day( Date() + nJump )
         nDay2  := Day( Date() + nJump + 365 )
         nDay1  := nDay2 - nDay1
         nJump = nJump + 365 - nDay1

      CASE nKey == K_RIGHT
         nJump = nJump - 7

      CASE nKey == K_LEFT
         nJump = nJump + 7

      CASE nKey == K_INS
         nJump = 0

      CASE nKey == K_F1
         IF lShowHelp .AND. .NOT. lHelpIsDisplayed
            lHelpIsDisplayed := .T.
            cSaveHelp := SaveScreen ( nHelpRow - 1, 1, nHelpRow + 7, 80 )
            FT_XBOX( 'L', , , cColor, cColor, nHelpRow, 1, ;
               "Home, Up_Arrow or PgUp keys page by day, month or year to a past date.", ;
               "End, Dn_Arrow or PgDn keys page by day, month or year to a future date.", ;
               "Left_Arrow or Right_Arrow keys page by week to a past or future date.", ;
               "Hit Ins to reset to today's date, F1 to get this help, ESC to quit." )
         ENDIF

      OTHERWISE
      ENDCASE

      aRetVal[1] :=         Date() + nJump
      aRetVal[2] :=  Month( Date() + nJump )
      aRetVal[3] :=    Day( Date() + nJump )
      aRetVal[4] :=   Year( Date() + nJump )
      aRetVal[5] := CMonth( Date() + nJump )
      aRetVal[6] :=   CDOW( Date() + nJump )
      aRetVal[7] :=   JDOY( aRetVal[4], aRetVal[2], aRetVal[3] )

      @nRow, nCol SAY SubStr( aRetval[6], 1, 3 ) + ' ' + ;
         Str( aRetVal[3], 2, 0 ) + ' ' + ;
         SubStr( aRetVal[5], 1, 3 ) + ' ' + ;
         Str( aRetVal[4], 4, 0 )
      @nRow + 1, nCol SAY   Str( aRetVal[7], 3, 0 )

      nKey := 0
      DO WHILE nKey == 0
         @nRow + 1, nCol + 3 SAY '    ' + Time()
         nKey := Inkey( 1 )
      ENDDO
      aRetVal[8] :=   Time()
   ENDDO

   SetColor ( cSavColor )                 //restore colors.
   SetCursor ( cSaveCursor )              //restore cursor.
   RestScreen ( nRow - 1, nCol - 1, nRow + 3, nCol + 17, cSaveScreen ) //restore screen.
   IF lHelpIsDisplayed
      RestScreen ( nHelpRow - 1, 1, nHelpRow + 7, 80, cSaveHelp )
   ENDIF

   RETURN aRetVal

STATIC FUNCTION JDOY ( nYear, nMonth, nDay )

   LOCAL cString := '000031059090120151181212243273304334'

   RETURN ( VALS( cString,(nMonth - 1 ) * 3 + 1,3 ) + nDay + ;
      iif( nYear % 4 == 0 .AND. nMonth > 2, 1, 0 ) )

STATIC FUNCTION VALS ( cString, nOffset, nChar )

   RETURN ( Val( SubStr(cString,nOffset,nChar ) ) )

// end of calendar.prg

