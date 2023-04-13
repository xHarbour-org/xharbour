/*
 * $Id$
 */
/*
 * File......: SaveSets.Prg
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:06   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   12 Apr 1991 00:18:04   GLENN
 * There was a call to SETCENTURY() that should have been FT_SETCENTURY().
 * Another one of those errors that came from testing earlier versions of
 * a routine before FT_ prefix was added to function names.  Lesson learned.
 *
 *    Rev 1.0   01 Apr 1991 01:02:10   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SAVESETS()
 *  $CATEGORY$
 *     Environment
 *  $ONELINER$
 *     Save the status of all the SET command settings
 *  $SYNTAX$
 *     FT_SAVESETS() -> aOldSets
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     An array containing the values of the supported SETs.
 *  $DESCRIPTION$
 *     This function saves the SET Settings, i.e., it copies them into an
 *     array, aOldSets.  The following SETs are not currently supported:
 *     FILTER, FORMAT, FUNCTION, INDEX, KEYS, MODE, ORDER, PROCEDURE,
 *     RELATION, TYPEAHEAD
 *  $EXAMPLES$
 *     aOldSets := FT_SAVESETS()
 *  $INCLUDE$
 *     SET.CH
 *  $SEEALSO$
 *     FT_RESTSETS() FT_SETCENTURY()
 *  $END$
 */


#include "set.ch"

#define FT_EXTRA_SETS    2
#define FT_SET_CENTURY   _SET_COUNT + 1
#define FT_SET_BLINK     _SET_COUNT + 2

#ifdef FT_TEST

FUNCTION MAIN

   LOCAL ASETS := FT_SAVESETS()

   Inkey( 0 )

   RETURN Nil

#endif

FUNCTION  FT_SAVESETS()

   LOCAL aOldSets := Array( _SET_COUNT + FT_EXTRA_SETS )

   AEval( aOldSets, ;
      { | , nElementNo | ;
      aOldSets[nElementNo] := Set( nElementNo ) } )

   aOldSets[FT_SET_CENTURY] := FT_SETCENTURY()
   aOldSets[FT_SET_BLINK]   := SetBlink()

   RETURN ( aOldSets )                    // FT_SaveSets
