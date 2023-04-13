/*
 * $Id$
 */
/*
 * File......: ELAPSED.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: ?
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:37:56   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:05:44   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:18   GLENN
 * Nanforum Toolkit
 *
 */


#ifdef FT_TEST

FUNCTION DEMO()

   LOCAL dStart, dEnd, cTimeStart, cTimeEnd, n, aDataTest := {}

   dStart := CToD( '11/28/90' )
   dEnd   := CToD( '11/30/90' )
   cTimeStart := "08:00:00"
   cTimeEnd   := "12:10:30"

   aDataTest := FT_ELAPSED( dStart, dEnd, cTimeStart, cTimeEnd )
   FOR n = 1 TO 4
      ? aDataTest[n,1], Str( aDataTest[n,2], 12, 4 )
      ?? " "
      ?? IF( n == 1, 'Days', IF( n == 2, 'Hours', IF( n == 3, 'Mins.', 'Secs.' ) ) )
   NEXT

   RETURN NIL

#endif

/*  $DOC$
 *  $FUNCNAME$
 *     FT_ELAPSED()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return elapsed time between two days and/or times
 *  $SYNTAX$
 *     FT_ELAPSED([ <dStart> ], [ <dEnd> ], ;
 *                <cTimeStart>, <cTimeEnd>) -> aTimedata
 *  $ARGUMENTS$
 *     <dStart> is any valid date in any date format. Defaults to DATE().
 *
 *     <dEnd> is any valid date in any date format. Defaults to DATE().
 *
 *     <cTimeStart> is a valid Time string of the format 'hh:mm:ss' where
 *     hh is hours in 24-hour format.
 *
 *     <cTimeEnd> is a valid Time string of the format 'hh:mm:ss' where
 *     hh is hours in 24-hour format.
 *  $RETURNS$
 *     A two-dimensional array containing elapsed time data.
 *  $DESCRIPTION$
 *     FT_ELAPSED() calculates the elapsed time between two Date/Time events.
 *
 *     It returns an array which contains the following data:
 *
 *     aRetVal[1,1]  Integer Days      aRetVal[1,2] Total Days    (nn.nnnn)
 *     aRetVal[2,1]  Integer Hours     aRetVal[2,2] Total Hours   (nn.nnnn)
 *     aRetVal[3,1]  Integer Minutes   aRetVal[3,2] Total Minutes (nn.nnnn)
 *     aRetVal[4,1]  Integer Seconds   aRetVal[4,2] Total Seconds (nn)
 *  $EXAMPLES$
 *     FT_ELAPSED(CTOD('11/28/90'), CTOD('11/30/90'), '08:00:00', '12:10:30')
 *     will return:
 *
 *     aRetVal[1,1] ->  2 (Days)        aRetVal[1,2] ->    2.1740  Days
 *     aRetVal[2,1] ->  4 (Hours)       aRetVal[2,2] ->   52.1750  Hours
 *     aRetVal[3,1] -> 10 (Minutes)     aRetVal[3,2] -> 3130.5000  Minutes
 *     aRetVal[4,1] -> 30 (Seconds)     aRetVal[4,2] -> 187830     Seconds
 *  $END$
 */

FUNCTION FT_ELAPSED( dStart, dEnd, cTimeStart, cTimeEnd )

   LOCAL nTotalSec, nCtr, nConstant, nTemp, aRetVal[4,2]

   IF ! ( ValType( dStart ) $ 'DC' )
      dStart := Date()
   ELSEIF ValType( dStart ) == 'C'
      cTimeStart := dStart
      dStart     := Date()
   ENDIF

   IF ! ( ValType( dEnd ) $ 'DC' )
      dEnd := Date()
   ELSEIF ValType( dEnd ) == 'C'
      cTimeEnd := dEnd
      dEnd     := Date()
   ENDIF

   IF( ValType( cTimeStart ) != 'C', cTimeStart := '00:00:00', )
      IF( ValType( cTimeEnd )   != 'C', cTimeEnd   := '00:00:00', )

         nTotalSec  := ( dEnd - dStart ) * 86400                              + ;
            Val( cTimeEnd )   *  3600                              + ;
            Val( SubStr( cTimeEnd,At(':', cTimeEnd ) + 1,2 ) ) * 60     + ;
            IF( RAt( ':', cTimeEnd ) == At( ':', cTimeEnd ), 0,         ;
            Val( SubStr( cTimeEnd,RAt(':', cTimeEnd ) + 1 ) ) )          - ;
            Val( cTimeStart ) * 3600                               - ;
            Val( SubStr( cTimeStart,At(':', cTimeStart ) + 1,2 ) ) * 60 - ;
            IF( RAt( ':', cTimeStart ) == At( ':', cTimeStart ), 0,     ;
            Val( SubStr( cTimeStart,RAt(':', cTimeStart ) + 1 ) ) )

         nTemp := nTotalSec

         FOR nCtr = 1 TO 4
            nConstant := IF( nCtr == 1, 86400, IF( nCtr == 2, 3600, IF( nCtr == 3, 60, 1 ) ) )
            aRetVal[nCtr,1] := Int( nTemp/nConstant )
            aRetval[nCtr,2] := nTotalSec / nConstant
            nTemp -= aRetVal[nCtr,1] * nConstant
         NEXT

         RETURN aRetVal
