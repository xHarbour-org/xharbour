
/*
 * $Id$
 */

/*
 * xHarbour Project test code:
 * hb_Decode( <var>, [ <case1,ret1 [,...,caseN,retN] ] [, <def> ]> ) ---> <xRet>
 *
 * Copyright 2006 Francesco Saverio Giudice <info/at/fsgiudice.com>
 * www - http://www.xharbour.org
 *
 */


#include "common.ch"


PROCEDURE Main()
    LOCAL aArray, xVal

    ? "HB_DECODE() FUNCTION TESTS"
    ? "--------------------------"
    ?

    ? 'Single, return empty value'
    ? hb_Decode( 10 )

    ? 'With a default, return it'
    ? hb_Decode( 10, "X" )

    ? 'A short list, searching for a space, returning default "Empty"'
    ? hb_Decode( " ", "F", "Full", "Empty" )

    ? 'A list, searching for value 3, return "C"'
    ? hb_Decode( 3, 1, "A", 2, "B", 3, "C" )

    ? 'A list, searching an inexistent value, return NIL'
    ? hb_Decode( 10, 1, "A", 2, "B", 3, "C" )

    ? 'A list with default'
    ? hb_Decode( 4, 1, "A", 2, "B", 3, "C", "X" )

    ? 'Using an array as list of values to check'
    ? hb_Decode( 2, { 1, "A", 2, "B", 3, "C" } )

    wait

    ? 'Using an array of arrays as list of values to check'
    ? hb_Decode( 2, { { 1, "A" }, { 2, "B" }, { 3, "C" } } )

    ? 'Using an array of arrays as list of values to check plus default'
    ? hb_Decode( 9, { { 1, "A" }, { 2, "B" }, { 3, "C" }, "X" } )

    ? 'Using an array with default as list of values to check'
    ? hb_Decode( 6, { 1, "A", 2, "B", 3, "C", "Y" } )

    ? 'Using an hash as list (I cannot have a default)'
    ? hb_Decode( 4, { 1 => "A", 2 => "B", 3 => "C", 4 => "D" } )

    ? 'Returning a codeblock'
    ? cStr( hb_Decode( 2, 1, {|| 1 }, 2, {|| 2 }, 3, {|| 3 } ) )

    ? 'Checking an array, this is done by reference, so it returns "A"'
    aArray := { 1 }
    ? hb_Decode( aArray, aArray, "A", { 2 }, "B", { 3 }, "C" )

    ? 'Searching a list, with a match, it returns a value'
    xVal := "Francesco"
    ? hb_Decode( xVal, "Ron", "Pinkas", "Przemyslaw", "Czerpak", "Paul", "Tucker", "Francesco", "Giudice" )

    ? 'Searching a list, without a match, it returns NIL'
    xVal := "Francesco"
    ? hb_Decode( xVal, "Ron", "Pinkas", "Przemyslaw", "Czerpak", "Paul", "Tucker" )

    wait

    ?
    ? "HB_DECODEOREMPTY() FUNCTION TESTS"
    ? "---------------------------------"
    ?

    ? 'Searching same list, without a match, using'
    ? 'hb_DecodeOrEmpty() it returns an empty string'
    xVal := "Francesco"
    ? ">" + hb_DecodeOrEmpty( xVal, "Ron", "Pinkas", "Przemyslaw", "Czerpak", "Paul", "Tucker" ) + "<"

    wait


RETURN


