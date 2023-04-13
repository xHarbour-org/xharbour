/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Profiler reporting classes
 *
 * Copyright 2001,2002 Dave Pearson <davep@davep.org>
 * http://www.davep.org/
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

/* Rationale:
 *
 * There are three aspects to profiling:
 *
 * 1) Gathering profile information.
 *
 * 2) Taking a snapshot of an application's profile information.
 *
 * 3) Reporting on the data gathered in the snapshot.
 *
 * Point 1 is handled in harbour's virtual machine. This source aims to
 * provide code for performing points 2 and 3. A class is provided to
 * gather, hold and manipulate the profile snapshot and a hierarchy of
 * classes exist for reporting on that snapshot. The reporting classes are
 * designed such that they are easy to inherit from and improve upon.
 *
 * The idea behind all of this is that the the design should provide all the
 * necessary building blocks for writing profiler reporters that suite the
 * user's needs (perhaps as part of an extended debugger or something).
 *
 */

/* Notes:
 *
 * As much as possible, the profiler class and the profile report classes
 * attempt to turn off the profiler to ensure that we don't get some sort of
 * Heisenberg effect. In other words, we don't want the profiler showing up
 * in the profiler.
 *
 * Many of the "Protected:" scope specifiers in the source have been
 * commented out where there's a problem with scope in harbour's class
 * system. Note that those comments will be removed when the bug is fixed.
 *
 */

/* TODO:
 *
 * o Handle any TODO: items in the source.
 * o Document the classes and the class hierarchy.
 *
 */

/* Thanks:
 *
 * Thanks to Antonio and Patrick for agreeing to replace the old profile
 * reporting code with this approach.
 */

#include "hbclass.ch"
#include "fileio.ch"
#include "common.ch"

#ifdef __TEST__

#include "inkey.ch"

FUNCTION Main()

   LOCAL oProfile := HBProfile():new() // New() is a default method in the class system that calls :init() to initialize the object
   LOCAL n

// Turn on profiling.
   __SetProfiler( .T. )

// Make sure we've got something to see timewise.
   DrawScreen( "Doing nothing for a couple of seconds" )
   DoNothingForTwoSeconds()

// Make sure we've got something to see callwise.
   FOR n := 1 TO 500
      CallMe500Times()
   NEXT

// Take a profile snapshot.
   oProfile:gather()

// Report on calls greater than 0
   DrawScreen( "All methods/functions called one or more times" )
   MemoEdit( HBProfileReportToString():new( oProfile:callSort() ):generate( {|o| o:nCalls > 0 } ), 1, , , , .F. )

// Sorted by name
   DrawScreen( "All methods/functions called one or more times, sorted by name" )
   MemoEdit( HBProfileReportToString():new( oProfile:nameSort() ):generate( {|o| o:nCalls > 0 } ), 1, , , , .F. )

// Sorted by time
   DrawScreen( "All methods/functions taking measurable time, sorted by time" )
   MemoEdit( HBProfileReportToString():new( oProfile:timeSort() ):generate( {|o| o:nTicks > 0 } ), 1, , , , .F. )

// TBrowse all calls greater than 0
   DrawScreen( "TBrowse all methods/functions called one or more times" )
   Browser( HBProfileReportToTBrowse():new( oProfile:callSort() ):generate( {|o| o:nCalls > 0 }, 1 ) )

// Some closing stats
   DrawScreen( "Totals" )
   @ 2, 0 SAY "  Total Calls: " + Str( oProfile:totalCalls() )
   @ 3, 0 SAY "  Total Ticks: " + Str( oProfile:totalTicks() )
   @ 4, 0 SAY "Total Seconds: " + Str( oProfile:totalSeconds() )
   @ 6, 0 SAY ""

   RETURN( NIL )

STATIC FUNCTION DrawScreen( cTitle )

   Scroll()

   @ 0, 0 SAY PadR( cTitle, MaxCol() + 1 ) COLOR "n/w"

   RETURN( NIL )

FUNCTION DoNothingForTwoSeconds()

   Inkey( 2 )

   RETURN( NIL )

FUNCTION CallMe500Times()

   RETURN( NIL )

STATIC FUNCTION Browser( oBrowse )

   LOCAL lBrowsing := .T.
   LOCAL nKey

   DO WHILE lBrowsing

      oBrowse:forceStable()

      nKey := Inkey( 0 )

      Switch nKey

      CASE K_ESC
         lBrowsing := .F.
         EXIT

      CASE K_DOWN
         oBrowse:down()
         EXIT

      CASE K_UP
         oBrowse:up()
         EXIT

      CASE K_LEFT
         oBrowse:Left()
         EXIT

      CASE K_RIGHT
         oBrowse:Right()
         EXIT

      CASE K_PGDN
         oBrowse:pageDown()
         EXIT

      CASE K_PGUP
         oBrowse:pageUp()
         EXIT

         // And so on.... (not really necessary for this test)

      End

   ENDDO
   SET CURSOR ON

   RETURN( NIL )

#endif

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileEntity

CREATE CLASS HBProfileEntity

   Exported:

   Var cName    READONLY
   Var nCalls   READONLY
   Var nTicks   READONLY

   Access nSeconds
   Access nMeanTicks
   Access nMeanSeconds

   METHOD init
   METHOD describe

End CLASS

/////

METHOD init( cName, aInfo ) CLASS HBProfileEntity

   ::cName  := cName
   ::nCalls := aInfo[ 1 ]
   ::nTicks := aInfo[ 2 ]

   RETURN( self )

/////

   Access nSeconds CLASS HBProfileEntity

   RETURN( hb_Clocks2Secs( ::nTicks ) )

/////

   Access nMeanTicks CLASS HBProfileEntity

   RETURN( if( ::nCalls == 0, 0, ::nTicks / ::nCalls ) )

/////

   Access nMeanSeconds CLASS HBProfileEntity

   RETURN( if( ::nCalls == 0, 0, ::nSeconds / ::nCalls ) )

/////

METHOD describe CLASS HBProfileEntity

   RETURN( "Base Entity" )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileFunction

CREATE CLASS HBProfileFunction Inherit HBProfileEntity

   Exported:

   METHOD describe

End CLASS

/////

METHOD describe CLASS HBProfileFunction

   RETURN( "Function" )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileMethod

CREATE CLASS HBProfileMethod Inherit HBProfileEntity

   Exported:

   METHOD describe

End CLASS

/////

METHOD describe CLASS HBProfileMethod

   RETURN( "Method" )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileOPCode

CREATE CLASS HBProfileOPCode Inherit HBProfileEntity

   Exported:

   METHOD describe

End CLASS

/////

METHOD describe CLASS HBProfileOPCode

   RETURN( "OPCode" )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfile

CREATE CLASS HBProfile

   Exported:

   Var aProfile

   METHOD init
   METHOD gather
   METHOD forEach
   METHOD SORT
   METHOD nameSort
   METHOD callSort
   METHOD timeSort
   METHOD totalCalls
   METHOD totalTicks
   METHOD totalSeconds

   Protected:

   METHOD gatherFunctions
   METHOD gatherMethods
   METHOD reset
   METHOD ignoreSymbol

End CLASS

/////

METHOD init CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )

   ::reset()

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD reset CLASS HBProfile

   ::aProfile := {}

   RETURN( self )

/////

METHOD ignoreSymbol( cSymbol ) CLASS HBProfile

   LOCAL cProfPrefix := "HBPROFILE"

   RETURN( ( Left( cSymbol, Len( cProfPrefix ) ) == cProfPrefix ) .OR. ( cSymbol == "__SETPROFILER" ) )

/////

METHOD gatherFunctions CLASS HBProfile

   LOCAL lProfile  := __SetProfiler( .F. )
   LOCAL nSymCount := __dynSCount()
   LOCAL cName
   LOCAL n

// For each known symbol.
// TODO: Question: Will the symbol count have changed because
//                 we've created variables?
   FOR n := 1 TO nSymCount

      // Is the symbol a function?
      IF __dynSIsFun( n )

         // If we're not ignoring the symbol...
         IF !::ignoreSymbol( cName := __dynSGetName( n ) )
            // Yes, it is, add it to the profile.
            AAdd( ::aProfile, HBProfileFunction():new( cName, __dynSGetPRF( n ) ) )
         ENDIF

      ENDIF

   NEXT

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD gatherMethods CLASS HBProfile

   LOCAL lProfile  := __SetProfiler( .F. )
   LOCAL n, nClasses := __clsCntClasses()
   LOCAL cClass
   LOCAL xMember

// For each class in the environment...
   FOR n := 1 TO nClasses
      cClass := __className( n )
      // If we're not ignoring the class' methods...
      IF !::ignoreSymbol( cClass )

         // Collect class members.

         FOR EACH xMember IN __classSel( n )

            // If we've got a member name...
            IF !Empty( xMember )
               // Add it to the profile.
               AAdd( ::aProfile, HBProfileMethod():new( cClass + ":" + xMember, __GetMsgPrf( n, xMember ) ) )
            ENDIF

         NEXT

      ENDIF

   NEXT

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD gather CLASS HBProfile

   LOCAL lProfile  := __SetProfiler( .F. )

// Reset the profile.
   ::reset()

// Gather function calls
   ::gatherFunctions()

// Gather method calls
   ::gatherMethods()

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD forEach( b ) CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )

   AEval( ::aProfile, b )

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD SORT( b ) CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )

   ASort( ::aProfile, , , b )

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD nameSort CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )

   ::sort( {|oX, oY| oX:cName < oY:cName } )

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD callSort CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )

   ::sort( {|oX, oY| oX:nCalls > oY:nCalls } )

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD timeSort CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )

   ::sort( {|oX, oY| oX:nTicks > oY:nTicks } )

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD totalCalls CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )
   LOCAL nCalls   := 0

   ::forEach( {|o| nCalls += o:nCalls } )

   __SetProfiler( lProfile )

   RETURN( nCalls )

/////

METHOD totalTicks CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )
   LOCAL nTicks   := 0

   ::forEach( {|o| nTicks += o:nTicks } )

   __SetProfiler( lProfile )

   RETURN( nTicks )

/////

METHOD totalSeconds CLASS HBProfile

   LOCAL lProfile := __SetProfiler( .F. )
   LOCAL nSeconds := 0

   ::forEach( {|o| nSeconds += o:nSeconds } )

   __SetProfiler( lProfile )

   RETURN( nSeconds )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileLowLevel

CREATE CLASS HBProfileLowLevel Inherit HBProfile

   Exported:

   METHOD gather

   Protected:

   METHOD gatherOPCodes

End CLASS

/////

METHOD gather CLASS HBProfileLowLevel

   LOCAL lProfile := __SetProfiler( .F. )

// Gather functions and methods.
   ::super:gather()

// Also gather opcodes.
   ::gatherOPCodes()

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD gatherOPCodes CLASS HBProfileLowLevel

   LOCAL nMax := __opCount()
   LOCAL cName
   LOCAL nOP

// Loop over all the harbour OP codes. Note that they start at 0.
   FOR nOP := 0 To ( nMax - 1 )
      // If we're not ignoring this opcode.
      IF !::ignoreSymbol( cName := "OPCODE( " + PadL( nOP, 3 ) + " )" )
         // Add it to the profile.
         AAdd( ::aProfile, HBProfileOpcode():new( cName, __opGetPrf( nOP ) ) )
      ENDIF
   NEXT

   RETURN( self )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileReport

CREATE CLASS HBProfileReport

// Protected:

   Var oProfile

   METHOD writeLines
   METHOD header
   METHOD emitHeader
   METHOD line
   METHOD emitLine

   Exported:

   METHOD init
   METHOD generate

End CLASS

/////

METHOD init( oProfile ) CLASS HBProfileReport

   LOCAL lProfile := __SetProfiler( .F. )

   ::oProfile := oProfile

   __SetProfiler( lProfile )

   RETURN( self )

/////

METHOD writeLines( aLines ) CLASS HBProfileReport

   AEval( aLines, {|c| QOut( c ) } )

   RETURN( self )

/////

METHOD header CLASS HBProfileReport

   RETURN( { "Name                                Type       Calls    Ticks       Seconds", ;
      "=================================== ========== ======== =========== ===========" } )

/////

METHOD emitHeader CLASS HBProfileReport

   ::writeLines( ::Header() )

   RETURN( self )

/////

METHOD line( oEntity ) CLASS HBProfileReport

   RETURN( { PadR( oEntity:cName,      35 ) + " " + ;
      PadR( oEntity:describe(),  8 ) + " " + ;
      PadL( oEntity:nCalls,     10 ) + " " + ;
      PadL( oEntity:nTicks,     11 ) + " " + ;
      Str( oEntity:nSeconds,    11, 2 ) } )

/////

METHOD emitLine( oEntity ) CLASS HBProfileReport

   ::writeLines( ::line( oEntity ) )

   RETURN( self )

/////

METHOD generate( bFilter ) CLASS HBProfileReport

   LOCAL lProfile := __SetProfiler( .F. )

   DEFAULT bFilter To {|| .T. }

   ::emitHeader():oProfile:forEach( {|o| if( Eval( bFilter, o ), ::emitLine( o ), NIL ) } )

   __SetProfiler( lProfile )

   RETURN( self )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileReportToFile

CREATE CLASS HBProfileReportToFile Inherit HBProfileReport

// Protected:

   Var hFile

   METHOD writeLines

   Exported:

   METHOD generate

End CLASS

/////

METHOD writeLines( aLines ) CLASS HBProfileReportToFile

   If ::hFile != F_ERROR
      AEval( aLines, {|c| FWrite( ::hFile, c + hb_osNewLine() ) } )
   ENDIF

   RETURN( self )

/////

METHOD generate( bFilter, cFile ) CLASS HBProfileReportToFile

   LOCAL lProfile := __SetProfiler( .F. )

   DEFAULT cFile TO "hbprof.txt"

   IF ( ::hFile := FCreate( cFile ) ) != F_ERROR
      ::super:generate( bFilter )
      FClose( ::hFile )
   ELSE
      // TODO: Throw an error
   ENDIF

   __SetProfiler( lProfile )

   RETURN( self )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileReportToArray

CREATE CLASS HBProfileReportToArray Inherit HBProfileReport

// Protected:

   Var aReport

   METHOD writeLines

   Exported:

   METHOD generate

End CLASS

/////

METHOD writeLines( aLines ) CLASS HBProfileReportToArray

   AEval( aLines, {|c| AAdd( ::aReport, c ) } )

   RETURN( self )

/////

METHOD generate( bFilter ) CLASS HBProfileReportToArray

   ::aReport := {}
   ::super:generate( bFilter )

   RETURN( ::aReport )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileReportToString

CREATE CLASS HBProfileReportToString Inherit HBProfileReportToArray

   Exported:

   METHOD generate

End CLASS

/////

METHOD generate( bFilter ) CLASS HBProfileReportToString

   LOCAL cReport := ""

   AEval( ::super:generate( bFilter ), {|c| cReport += c + hb_osNewLine() } )

   RETURN( cReport )

////////////////////////////////////////////////////////////////////////////
// Class: HBProfileReportToTBrowse

CREATE CLASS HBProfileReportToTBrowse Inherit HBProfileReportToArray

// Protected:

   Var nEntity

   METHOD emitHeader
   METHOD emitLine
   METHOD addColumns

   Exported:

   METHOD generate
   METHOD currentEntity

End CLASS

/////

METHOD emitHeader CLASS HBProfileReportToTBrowse

// No header required.

   RETURN( self )

/////

METHOD emitLine( oEntity ) CLASS HBProfileReportToTBrowse

// Don't "emit" anything, simply add the entity to the array.
   AAdd( ::aReport, oEntity )

   RETURN( self )

/////

METHOD generate( bFilter, nTop, nLeft, nBottom, nRight ) CLASS HBProfileReportToTBrowse

   LOCAL lProfile := __SetProfiler( .F. )
   LOCAL oBrowse

// Start with the first entity.
   ::nEntity := 1

// Generate the array.
   ::super:generate( bFilter )

// Build the browse.
   oBrowse := TBRowseNew( nTop, nLeft, nBottom, nRight )

   oBrowse:goTopBlock    := {|| ::nEntity := 1 }
   oBrowse:goBottomBlock := {|| ::nEntity := Len( ::aReport ) }
   oBrowse:skipBlock     := {|nSkip, nPos| nPos := ::nEntity,                                   ;
      ::nEntity := if( nSkip > 0,                          ;
      Min( Len( ::aReport ), ::nEntity + nSkip ), ;
      Max( 1, ::nEntity + nSkip ) ), ::nEntity - nPos }

   ::addColumns( oBrowse )

   __SetProfiler( lProfile )

   RETURN( oBrowse )

/////

METHOD addColumns( oBrowse ) CLASS HBProfileReportToTBrowse

   oBrowse:addColumn( TBColumnNew( "Name",         {|| PadR( ::currentEntity():cName,        35    ) } ) )
   oBrowse:addColumn( TBColumnNew( "Type",         {|| PadR( ::currentEntity():describe(),    8    ) } ) )
   oBrowse:addColumn( TBColumnNew( "Calls",        {|| PadL( ::currentEntity():nCalls,       10    ) } ) )
   oBrowse:addColumn( TBColumnNew( "Ticks",        {|| PadL( ::currentEntity():nTicks,       11    ) } ) )
   oBrowse:addColumn( TBColumnNew( "Seconds",      {|| Str(  ::currentEntity():nSeconds,     11, 2 ) } ) )
   oBrowse:addColumn( TBColumnNew( "Mean;Ticks",   {|| Str(  ::currentEntity():nMeanTicks,   11, 2 ) } ) )
   oBrowse:addColumn( TBColumnNew( "Mean;Seconds", {|| Str(  ::currentEntity():nMeanSeconds, 11, 2 ) } ) )

   RETURN( self )

/////

METHOD currentEntity CLASS HBProfileReportToTBrowse

   RETURN( ::aReport[ ::nEntity ] )

/*
 * profiler.prg ends here.
 */
