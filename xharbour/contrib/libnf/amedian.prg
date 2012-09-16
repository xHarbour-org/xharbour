/*
 * $Id$
 */
/*
 * File......: AMedian.Prg
 * Author....: Ralph Oliver,  TRANSCOM SYSTEMS
 * CIS ID....: 74030,703
 *
 * This is an original work by Ralph Oliver and is placed in the
 * public domain.
 *
 * This program uses the preprocessor #defines and #command in
 * Aavg.prg by David Husnian.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:22   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   07 Jun 1991 23:03:20   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_AMEDIAN()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Find middle value in array, or average of two middle values
 *  $SYNTAX$
 *     FT_AMEDIAN( <aArray> [, <nStart> [, <nEnd> ] ] )
 *                -> nMedian
 *  $ARGUMENTS$
 *     <aArray> is the array containing the elements to be averaged.
 *
 *     <nStart> is the first array element to include,
 *     defaults to first element.
 *
 *     <nEnd> is the last array element to include,
 *     defaults to last element.
 *  $RETURNS$
 *     The median average of the array elements
 *  $DESCRIPTION$
 *     This function sorts the elements of a numeric array and
 *     then returns the value in the middle element of the sorted
 *     array.  If there is no exact middle value, then it returns
 *     the average of the two middle values.  Half of the elements
 *     are > median and half are < median.  A median average may
 *     more reflect a more useful average when there are extreme
 *     values in the set.
 *  $EXAMPLES$
 *     FT_AMEDIAN( aArray )      // Return Median for entire array
 *
 *     FT_AMEDIAN( aArray, 2)    // Return Median for elements from 2 to end
 *
 *     FT_AMEDIAN( aArray, ,9)   // Return Median for 1st 9 elements
 *
 *     FT_AMEDIAN( aArray,8,40 ) // Return Median for elements 8 to 40
 *  $END$
 */

#ifdef FT_TEST

#include "directry.ch"

FUNCTION MAIN()

   LOCAL var0, myarray0 := Directory(), myarray1 := {}

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AMEDIAN"
   ?
   AEval( myarray0, { |x| AAdd( myarray1, x[ F_SIZE ] ) } )
   var0 := FT_AMEDIAN( myarray1 )
   ? PadR( 'FT_AMEDIAN( myarray1 ) ->', 35 )
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, 2 )
   ? PadR( 'FT_AMEDIAN( myarray1, 2 ) ->', 35 )
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, , 9 )
   ? PadR( 'FT_AMEDIAN( myarray1, , 9 ) ->', 35 )
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, 8, 40 )
   ? PadR( 'FT_AMEDIAN( myarray1, 8, 40 ) ->', 35 )
   ?? var0
   ?

   RETURN NIL

#endif

#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
      => ;
      < Param1 > := IF( < Param1 > == NIL, < Def1 > , < Param1 > ) ;
      [; <ParamN> := IF(<ParamN> == NIL,<DefN>,<ParamN>)]

FUNCTION FT_AMEDIAN( aArray, nStart, nEnd )

   LOCAL nTemplen, aTemparray, nMiddle1, nMiddle2, nMedian

   DEFAULT nStart TO 1, ;
      nEnd   TO Len( aArray )

// Make Sure Bounds are in Range
   FORCE_BETWEEN( 1, nEnd,   Len( aArray ) )
   FORCE_BETWEEN( 1, nStart, nEnd )

// Length of aTemparray
   nTemplen := ( nEnd - nStart ) + 1

// Initialize aTemparray
   aTemparray := ACopy( aArray, Array( nTemplen ), nStart, nTemplen )

// Sort aTemparray
   aTemparray := ASort( aTemparray )

// Determine middle value(s)
   IF ( nTemplen % 2 ) == 0
      nMiddle1 := aTemparray[ (nTemplen / 2) ]
      nMiddle2 := aTemparray[ INT(nTemplen / 2) +1 ]
      nMedian :=  Int( ( nMIddle1 + nMiddle2 ) / 2 )
   ELSE
      nMedian := aTemparray[ INT( nTemplen / 2 ) + 1 ]
   ENDIF

   RETURN ( nMedian )


