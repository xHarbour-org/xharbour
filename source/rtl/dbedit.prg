/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * DBEDIT() function
 *
 * Copyright 2003 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
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
 * NOTE: This is a total rewrite with all features previous dbedit() had
 *       plus a few more.
 *       It works with or w/o 5.3 extensions
 *       + Using 5.3 extensions gives mouse event handling :)
 *       + Features previous dbedit() had are:
 *         - User func can be a codeblock
 *         - No coords = full screen
 *         - No columns = fill with db structure
 *       + New features in this version:
 *         - Any column can be also a codeblock instead of a string
 *         - Heading/footing separator is single line instead of double line
 *           (see below in the code)
 *         - Columns are movable via K_CTRL_UP / K_CTRL_DOWN
 *         - A column can be an array of 2 items
 *           In this case, the second is the codeblock to do coloring :)
 *         - Userfunc is called with a third parameter, the actual TBRowse object
 *           This is very useful, it increases A LOT the power of dbedit()
 *         - UserFunc is also called once with nMode == -1 (initialization)
 *           Prior to begin browsing
 *         - You can pass pre/post blocks for later using in user func
 *           (combinated with the GET system)
 *
 * DBEdit() is no more deprecated :)
 * Have fun
 *                      Mauricio
 *
 */

#include "dbedit.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbsetup.ch"
#include "common.ch"
#include "tbrowse.ch"

/* E.F. 2006/04/22 - The #define DE_APPEND is for Append mode in dbEdit.
 * I have used tbrowse "cargo" to assign true/false for that.
 * (Append mode is undocumented Clipper's dbEdit feature)
 */

#ifndef DE_APPEND
#define DE_APPEND  3
#endif

FUNCTION dbEdit( nTop, ;
      nLeft, ;
      nBottom, ;
      nRight, ;
      axColumns, ;
      xUserFunc, ;
      acColumnSayPictures, ;
      acColumnHeaders, ;
      acHeadingSep, ;
      acColumnSep, ;
      acFootingSep, ;
      acColumnFootings, ;
      bPreBlock, ;
      bPostBlock )

   LOCAL oTBR, ;
      oTBC, ;
      i, ;
      nRet, ;
      nKey, ;
      bFunc, ;
      nCursor, ;
      cHdr, ;
      nIndex, ;
      lAppend, ;
      lExcept

   IF ! Used()
#ifdef HB_C52_STRICT
      dbGoBottom() /* Clipper compliance: call dbgobotom() to forces error message. */
#else
      /* Call Errorsys() with error 2001 if not database in use. */
      Throw( ErrorNew( "DBCMD", 0, 2001, ProcName(), "Workarea not in use" ) )
#endif
   ELSEIF EOF() .AND. LastRec() > 0
      /* DbEdit() moves cursor to the bottom record if eof() is reached at init. */
      dbGoBottom()
   ENDIF

   DEFAULT nTop TO 0
   DEFAULT nLeft TO 0
   DEFAULT nRight TO MaxCol()
   DEFAULT nBottom TO MaxRow()

// NOTE: Heading/footing separator is SINGLE line instead of DOUBLE line
//       this is because most codepages (unicode too) don't have DOUBLE line chars
//       so the output is ugly with them
//
   DEFAULT acHeadingSep TO Chr( 196 ) + Chr( 194 ) + Chr( 196 )
   DEFAULT acColumnSep  TO " " + Chr( 179 ) + " "
   DEFAULT acColumnFootings TO ""


   IF Empty( axColumns ) .OR. ! HB_ISARRAY( axColumns )

      axColumns := Array( FCount() )

      FOR EACH i IN axColumns
         i := FieldName( HB_EnumIndex() )
      NEXT

   END

  /* 17/05/2006 - E.F. - Check parameters type before continue.
    * 1) Clipper avoid argument values if it is invalid.
    *    xHarbour call a run time error. IMHO this is better solution to
         avoid old errors in code and bad practices inherited from Clipper's days.
   * 2) There is no error base reserved to dbEdit function, then I have
   *    assigned the 1127 for this.
   */

  /* Note: The column's type doesn't need to verify. If any column type is
           invalid or empty, then the dbEdit() will ignore it. */

   IF !HB_ISNIL( nTop ) .AND.  !HB_ISNUMERIC( nTop )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(nTop ) + ">", ProcName() + " <nTop>" ) )
   ENDIF
   IF !HB_ISNIL( nLeft ) .AND.  !HB_ISNUMERIC( nLeft )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(nLeft ) + ">", ProcName() + " <nLeft>" ) )
   ENDIF
   IF !HB_ISNIL( nBottom ) .AND.  !HB_ISNUMERIC( nBottom )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(nBottom ) + ">", ProcName() + " <nBottom>" ) )
   ENDIF
   IF !HB_ISNIL( nRight ) .AND.  !HB_ISNUMERIC( nRight )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(nRight ) + ">", ProcName() + " <nRight>" ) )
   ENDIF

   nTop    := Max( 0, nTop )
   nLeft   := Max( 0, nLeft )
   nBottom := Min( MaxRow(), nBottom )
   nRight  := Min( MaxCol(), nRight )

  /* In Clipper the <cUserFunc> paramenter only can be a
   * string or nil, but in xHarbour can be a codeblock also.
   */
   IF !HB_ISNIL( xUserFunc ) .AND. ( !HB_ISSTRING( xUserFunc ) .AND. !HB_ISBLOCK( xUserFunc ) .AND. !HB_ISLOGICAL( xUserFunc ) )
      Throw( ErrorNew( "BASE", 0, 1127,  "Argument type error <" + ValType(xUserFunc ) + ">", ProcName() + " <xUserFunc>" ) )
   ELSE
      IF HB_ISSTRING( xUserFunc ) .AND. Empty( xUserFunc )
         xUserFunc := NIL
      ENDIF
      IF HB_ISLOGICAL( xUserFunc ) .AND. xUserFunc
         xUserFunc := NIL
      ENDIF
   ENDIF

   IF !HB_ISNIL( acColumnSayPictures ) .AND. ( !HB_ISSTRING( acColumnSayPictures ) .AND. !HB_ISARRAY( acColumnSayPictures ) )
      Throw( ErrorNew( "BASE", 0, 1127,  "Argument type error <" + ValType(acColumnSayPictures ) + ">", ProcName() + " <acColumnSayPictures|cColumnSayPicture>" ) )
   ENDIF

   IF !HB_ISNIL( acColumnHeaders ) .AND. ( !HB_ISSTRING( acColumnHeaders ) .AND. !HB_ISARRAY( acColumnHeaders ) )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(acColumnHeaders ) + ">", ProcName() + " <acColumnHeaders|cColumnHeader>" ) )
   ENDIF

   IF !HB_ISNIL( acHeadingSep ) .AND. ( !HB_ISSTRING( acHeadingSep ) .AND. !HB_ISARRAY( acHeadingSep ) )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(acHeadingSep ) + ">", ProcName() + " <acHeadingSeparators|cHeadingSeparator>" ) )
   ENDIF

   IF !HB_ISNIL( acColumnSep ) .AND. ( !HB_ISSTRING( acColumnSep ) .AND. !HB_ISARRAY( acColumnSep ) )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(acColumnSep ) + ">", ProcName() + " <acColumnSeparators|cColumnSeparator>" ) )
   ENDIF

   IF !HB_ISNIL( acFootingSep ) .AND. ( !HB_ISSTRING( acFootingSep ) .AND. !HB_ISARRAY( acFootingSep ) )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(acFootingSep ) + ">", ProcName() + " <acFootingSeparators|cFootingSeparator>" ) )
   ENDIF

   IF !HB_ISNIL( acColumnFootings ) .AND. ( !HB_ISSTRING( acColumnFootings ) .AND. !HB_ISARRAY( acColumnFootings ) )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(acColumnFootings ) + ">", ProcName() + " <acColumnFootings|cColumnFooting>" ) )
   ENDIF

   IF !HB_ISNIL( bPreBlock ) .AND. !HB_ISBLOCK( bPreBlock )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(bPreBlock ) + ">", ProcName() + " <bPreBlockBlock>" ) )
   ENDIF

   IF !HB_ISNIL( bPostBlock ) .AND. !HB_ISBLOCK( bPostBlock )
      Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <" + ValType(bPostBlock ) + ">", ProcName() + " <bPostBlockBlock>" ) )
   ENDIF

   IF HB_ISBLOCK( bPreBlock )
      i := bPreBlock
      bPreBlock := Array( Len( axColumns ) )
      AFill( bPreBlock, i )
   END

   IF HB_ISBLOCK( bPostBlock )
      i := bPostBlock
      bPostBlock := Array( Len( axColumns ) )
      AFill( bPostBlock, i )
   END

// Save previous cursor shape and position.
   nCursor := SetCursor( SC_NONE )

   iif( HB_ISNIL( acFootingSep ) .AND. !Empty( acColumnFootings ), acFootingSep := Chr( 196 ) + Chr( 193 ) + Chr( 196 ), )

   /* 2007/JAN/30 - EF - To avoid dbedit blinking. */
   DispBegin()

   /* Create Tbrowse object */
   oTBR := TBRowseDb( nTop, nLeft, nBottom, nRight )

   /* E.F. 2006/04/22 - Set append mode off by default */
   oTBR:Cargo := .F.

   /* E.F. 2006/04/22 - Use a custom 'skipper' to handle append mode */
   oTBR:SkipBlock := { |x| dbe_Skipper( x, oTBR ) }


   IF HB_ISSTRING( acHeadingSep )
      oTBR:headSep := acHeadingSep
   END

   IF HB_ISSTRING( acFootingSep )
      oTBR:footSep := acFootingSep
   END

   IF HB_ISSTRING( acColumnSep )
      oTBR:colSep := acColumnSep
   END


#ifdef HB_COMPAT_C53
// EXTENSION: Move columns inside dbedit :)
   oTBR:SetKey( K_CTRL_UP, {|| _MoveCol( oTBR, K_CTRL_UP ), 0 } )
   oTBR:SetKey( K_CTRL_DOWN, {|| _MoveCol( oTBR, K_CTRL_DOWN ), 0 } )
#endif

// Build columns
//
   FOR EACH i IN axColumns

      IF !Empty( i )

         nIndex := HB_EnumIndex()

         IF HB_ISARRAY( i )
            bFunc := iif( HB_ISBLOCK( i[1] ), i[1], &( "{||" + i[1] + '}' ) )
         ELSE
            bFunc := iif( HB_ISBLOCK( i ), i, &( "{||" + i + '}' ) )
         End

         IF HB_ISMEMO( Eval( bFunc ) )
            bFunc := {|| "  <Memo>  " }
         End

         cHdr := i

         IF HB_ISSTRING( acColumnHeaders )
            cHdr := acColumnHeaders
         ELSEIF HB_ISARRAY( acColumnHeaders ) .AND. Len( acColumnHeaders ) >= nIndex .AND. acColumnHeaders[ nIndex ] != NIL // handle empty column headers
            cHdr := acColumnHeaders[ nIndex ]
         End

         IF HB_ISBLOCK( cHdr )
            cHdr := "<block>"
         End

         oTBC := TBColumnNew( cHdr, bFunc )

         IF HB_ISARRAY( i )
            oTBC:colorBlock := i[2]
         End

         IF HB_ISARRAY( acColumnSep )
            oTBC:colSep := acColumnSep[ nIndex ]
         End

         IF HB_ISARRAY( acHeadingSep )
            oTBC:headSep := acHeadingSep[ nIndex ]
         End

         IF HB_ISARRAY( acFootingSep )
            oTBC:footSep := acFootingSep[ nIndex ]
         End

         IF HB_ISARRAY( acColumnFootings )
            oTBC:footing := acColumnFootings[ nIndex ]
         ELSEIF HB_ISSTRING( acColumnFootings )
            oTBC:footing := acColumnFootings
         End

         IF HB_ISARRAY( acColumnSayPictures ) .AND. Len( acColumnSayPictures ) >= nIndex
            oTBC:picture := acColumnSayPictures[ nIndex ]
         ELSEIF HB_ISSTRING( acColumnSayPictures )
            oTBC:picture := acColumnSayPictures
         End

         IF HB_ISARRAY( bPreBlock )

            IF HB_ISLOGICAL( bPreBlock[ nIndex ] )
               bPreBlock[ nIndex ] := iif( bPreBlock[ nIndex ], {|| .T. }, {|| .F. } )
            End

            oTBC:preBlock := bPreBlock[ nIndex ]

         End

         IF HB_ISARRAY( bPostBlock )

            IF HB_ISLOGICAL( bPostBlock[ nIndex ] )
               bPostBlock[ nIndex ] := iif( bPostBlock[ nIndex ], {|| .T. }, {|| .F. } )
            End

            oTBC:postBlock := bPostBlock[ nIndex ]

         END

         oTBR:addColumn( oTBC )

      END

   NEXT

   DispEnd()

   IF Len( axColumns ) == 1
      oTBR:SetKey( K_LEFT, Nil )
      oTBR:SetKey( K_RIGHT, Nil )
   ENDIF

   IF Empty( xUserFunc )
      bFunc := {|| iif( HB_ISNUMERIC( nKey ) .AND. ( Chr(LastKey() ) $ Chr(K_ESC ) + Chr(K_ENTER ) ), DE_ABORT, DE_CONT ) }
   ELSEIF !HB_ISLOGICAL( xUserFunc )
      bFunc := iif( HB_ISBLOCK( xUserFunc ), xUserFunc, &( "{|x, y, z|" + xUserFunc + "(x,y,z)}" ) )
      oTBR:SetKey( K_ESC, nil )
   ENDIF

#ifdef HB_EXTENSION
// xHarbour extension: call UDF with DE_INIT mode.
   nRet := dbe_CallUDF( bFunc, DE_INIT, oTBR:colPos, oTBR )
#else
   nRet := DE_CONT
#endif

   oTBR:ForceStable()
   oTBR:DeHilite()

   IF HB_ISLOGICAL( xUserFunc ) .AND. !xUserFunc
      nRet := DE_ABORT
   ENDIF

   nKey := 0
   lAppend := oTBR:Cargo
   lExcept := .F.


/////////////////////
// PROCESSING LOOP //
/////////////////////


   WHILE nRet != DE_ABORT

      IF nRet == DE_CONT

         oTBR:RefreshCurrent()

      ELSEIF nRet == DE_REFRESH

         oTBR:RefreshAll()

         IF lAppend
            lAppend    := .F.
            oTBR:Cargo := .F.
            oTBR:GoBottom()
         ENDIF

         nRet := DE_CONT

      ENDIF

      oTBR:ForceStable()

      IF nRet == DE_CONT

         IF ! lExcept

            IF dbe_emptydb()
               nRet := dbe_CallUDF( bFunc, DE_EMPTY, oTBR:colPos, oTBR )

            ELSEIF oTBR:HitTop
               oTBR:HitTop := .F.
               nRet := dbe_CallUDF( bFunc, DE_HITTOP, oTBR:colPos, oTBR )

            ELSEIF oTBR:HitBottom
               oTBR:HitBottom := .F.
               nRet := dbe_CallUDF( bFunc, DE_HITBOTTOM, oTBR:colPos, oTBR )

            ENDIF

         ELSE

            //         nRet := dbe_CallUDF(bFunc, DE_EXCEPT, oTBR:colPos, oTBR)
            lExcept := .F.
            IF LastKey() == K_ENTER
               oTBR:RefreshCurrent()
            ENDIF
         ENDIF

         // No keystrokes pending...
         IF NextKey() == 0
            dbe_CallUDF( bFunc, DE_IDLE, oTBR:colPos, oTBR )
            // force dbedit DE_CONT state after IDLE mode.
            nRet := DE_CONT
         ENDIF

      ENDIF


      IF nRet == DE_ABORT
         EXIT

      ELSEIF nRet == DE_REFRESH
         LOOP

      ELSEIF nRet == DE_APPEND .AND. ! oTBR:Cargo

         oTBR:Cargo := .T.
         lAppend    := .T.

         IF ! EOF() .OR. ! dbe_emptydb()
            oTBR:Down()
         ENDIF

         oTBR:RefreshCurrent()
         oTBR:ForceStable()
         nRet := DE_CONT

      ENDIF

      oTBR:Hilite()

      IF NextKey() != 0
         nKey := Inkey()
      ELSE
         nKey := Inkey( 0 )
      ENDIF

      IF nKey != 0

         nRet := dbe_CallUDF( bFunc, DE_EXCEPT, oTBR:colPos, oTBR )

         IF nRet == DE_ABORT
            EXIT
         ENDIF

         IF dbe_ProcessKey( nKey, oTBR ) == DE_ABORT
            EXIT
         ENDIF

         IF HB_ISBLOCK( SetKey( nKey ) )
            Eval( SetKey( nKey ), ProcName( 1 ), ProcLine( 1 ), "" )
         ENDIF

         lExcept := ! dbe_cursorkey( nKey )

      ENDIF

   ENDDO

   SetCursor( nCursor )
   SetPos( Row(), 0 )

   /* Clipper's NG says that DBEdit always returns NIL, but doesn't. */

   RETURN .T.

//------------------------------------------------------*

STATIC FUNCTION dbe_CallUDF( bFunc, nMode, nColPos, oTBR )

//------------------------------------------------------*
   LOCAL nRet, nRec, nKey, nLastRec, lDeleted, lChanged


   nRet := DE_CONT

   IF nMode == DE_INIT

      nKey := NextKey()

      IF nKey == K_ENTER .OR. nKey == K_ESC
         Inkey()
         RETURN DE_ABORT
      ENDIF

      WHILE nKey != 0
         Inkey()
         dbe_ProcessKey( nKey, oTBR )
         nRet := dbe_return( Eval( bFunc, DE_EXCEPT, nColPos, oTBR ) )
         IF nRet == DE_ABORT
            EXIT
         ELSEIF nRet == DE_REFRESH
            oTBR:RefreshAll()
            oTBR:ForceStable()
         ELSEIF nRet == DE_CONT
            oTBR:RefreshCurrent()
            oTBR:ForceStable()
         ENDIF
         nKey := NextKey()
      ENDDO

      IF nRet != DE_ABORT
         nRet := dbe_return( Eval( bFunc, DE_INIT, nColPos, oTBR ) )
      ENDIF

      RETURN nRet

   ELSEIF nMode == DE_EXCEPT

      oTBR:DeHilite()
      oTBR:ColorRect( { oTBR:rowpos, oTBR:colpos, oTBR:rowpos, oTBR:colpos }, { 1, 2 } )

   ELSEIF nMode == DE_IDLE .OR. nMode == DE_EMPTY

      KEYBOARD Chr( 0 )
      Inkey()

   ENDIF

   lDeleted := Deleted()
   nRec     := RecNo()
   nLastRec := LastRec()

// Call UDF
   nRet := dbe_return( Eval( bFunc, nMode, nColPos, oTBR ) )


// A change was occurred on UDF (append, delete or skip).
   lChanged := ( nLastRec != LastRec() .OR. ;
      Deleted() != lDeleted .OR. ;
      nRec != RecNo() )


   IF nRet == DE_ABORT .OR. nRet == DE_APPEND
      RETURN nRet
   ENDIF

// The UDF has changed db/record, so dbedit need to be refreshed.
   IF lChanged

      IF LastRec() > nLastRec  // append.

         nKey := NextKey()

         IF ( nKey != 0 .AND. ! dbe_CursorKey( nKey ) ) .OR. ;
               ordKeyNo() < oTBR:RowPos
            oTBR:Gotop()
         ENDIF

      ELSEIF LastRec() < nLastRec  // pack

         oTBR:RowPos := 1

      ELSEIF Deleted() .AND. LastRec() != 0  // deleted

         IF SET( _SET_DELETED )
            WHILE ! EOF() .AND. Deleted()
               dbSkip()
            ENDDO
         ELSE
            dbe_syncpos( oTBR )
         ENDIF

      ELSEIF nRec != RecNo() // moved.

         dbe_syncpos( oTBR )

      ENDIF

      IF EOF() .AND. LastRec() > 0
         dbGoBottom()
      ENDIF

      nRet := DE_REFRESH

   ENDIF

   RETURN nRet


/***
*
*  dbe_Skipper()
*
*  Handle record movement requests from Tbrowse object.
*
*  This is a special "skipper" that handles append mode. It
*  takes two parameters instead of the usual one. The second
*  parameter is a reference to the Tbrowse object itself. The
*  Tbrowse's "cargo" variable contains information on whether
*  append mode is turned on. This function was based from:
*  clipper\source\samples\tbdemo.prg
*/

//-----------------------------------------*

STATIC FUNCTION dbe_Skipper( nSkip, oTb )

//-----------------------------------------*

   LOCAL lAppend := oTb:Cargo
   LOCAL i       := 0

   DO CASE
   CASE nSkip == 0 .OR. LastRec() == 0
      // Skip 0 (significant on a network)
      dbSkip( 0 )
   CASE nSkip > 0 .AND. !EOF()
      WHILE ( i < nSkip )           // Skip Foward
         dbSkip( 1 )
         i++
         IF EOF() .AND. ! lAppend
            dbSkip( - 1 )
            i--
            EXIT
         ENDIF
      ENDDO
   CASE ( nSkip < 0 )
      WHILE ( i > nSkip )           // Skip backward
         dbSkip( - 1 )
         IF BOF()
            EXIT
         ENDIF
         i--
      ENDDO
   ENDCASE

   RETURN i

#ifdef HB_COMPAT_C53

STATIC FUNCTION _MoveCol( oTBR, nKey )

   LOCAL oTBR1, oTBR2

   IF nKey == K_CTRL_DOWN .AND. oTBR:colPos < oTBR:colCount
      oTBR1 := oTBR:getColumn( oTBR:colPos )
      oTBR2 := oTBR:getColumn( oTBR:colPos + 1 )
      oTBR:setColumn( oTBR:colPos, oTBR2 )
      oTBR:SetColumn( oTBR:colPos + 1, oTBR1 )
      oTBR:colPos++
      oTBR:invalidate()
   ELSEIF nKey == K_CTRL_UP .AND. oTBR:colPos > 1
      oTBR1 := oTBR:getColumn( oTBR:colPos )
      oTBR2 := oTBR:getColumn( oTBR:colPos - 1 )
      oTBR:setColumn( oTBR:colPos, oTBR2 )
      oTBR:SetColumn( oTBR:colPos - 1, oTBR1 )
      oTBR:colPos--
      oTBR:invalidate()
   End

   RETURN Nil

#endif

//-------------------------------------*

STATIC FUNCTION dbe_emptydb()

//-------------------------------------*
// Verify if the current dbf is empty.
//-------------------------------------*
   LOCAL lEmpty

   IF LastRec() == 0
      RETURN .T.
   ENDIF

   IF ! Empty( dbFilter() )
      lEmpty := ( EOF() .OR. RecNo() > LastRec() )
   ELSEIF IndexOrd() == 0
      lEmpty := ( ( EOF() .OR. RecNo() > LastRec() ) .AND. BOF() )
   ELSE
      //lEmpty := ( OrdKeyCount() == 0 ) // this code decrease dbedit's speed at large table.
      lEmpty := ( ordKeyNo() == 0 )
   ENDIF

   RETURN lEmpty

//------------------------------------------*

STATIC FUNCTION dbe_processKey( nKey, oTb )

//------------------------------------------*
   LOCAL nRet := DE_CONT

#ifdef HB_COMPAT_C53
   IF oTb:ApplyKey( nKey ) == TBR_EXIT
      nRet := DE_ABORT
   ENDIF
#else
// xHarbour without 5.3 extensions code
   Switch nKey
   CASE K_DOWN;oTb:down();EXIT
   CASE K_UP;oTb:up();EXIT
   CASE K_LEFT;oTb:Left();EXIT
   CASE K_RIGHT;oTb:Right();EXIT
   CASE K_PGDN;oTb:pageDown();EXIT
   CASE K_PGUP;oTb:pageUp();EXIT
   CASE K_CTRL_PGUP;oTb:goTop();EXIT
   CASE K_CTRL_PGDN;oTb:goBottom();EXIT
   CASE K_HOME;oTb:home();EXIT
   CASE K_END;oTb:end();EXIT
   CASE K_CTRL_HOME;oTb:panHome();EXIT
   CASE K_CTRL_END;oTb:panEnd();EXIT
   CASE K_CTRL_LEFT;oTb:panLeft();EXIT
   CASE K_CTRL_RIGHT;oTb:panRight();EXIT
   End
#endif

   RETURN nRet

//----------------------------------*

STATIC FUNCTION dbe_Return( n )

//----------------------------------*
   IF ! HB_ISNUMERIC( n )
      n := DE_CONT
   ELSEIF n < DE_ABORT .OR. n > DE_APPEND
      n := DE_CONT
   ENDIF

   RETURN n

//------------------------------------*

STATIC FUNCTION dbe_cursorkey( nKey )

//------------------------------------*
   LOCAL aKeys := { K_LEFT, ;
      K_RIGHT, ;
      K_CTRL_LEFT, ;
      K_CTRL_RIGHT, ;
      K_UP, ;
      K_DOWN, ;
      K_HOME, ;
      K_END, ;
      K_CTRL_HOME, ;
      K_CTRL_END, ;
      K_PGUP, ;
      K_PGDN, ;
      K_CTRL_PGUP, ;
      K_CTRL_PGDN }

   RETURN ( AScan( aKeys, nKey ) != 0 )

//--------------------------------*

STATIC FUNCTION dbe_syncpos( oTb )

//--------------------------------*
   LOCAL nRec := RecNo()
   LOCAL nKeyNo
   LOCAL nDel := 0
   LOCAL lDeleted

   IF IndexOrd() != 0

      nKeyNo := ordKeyNo()
      dbSkip( - 1 )

      IF BOF()
         oTb:RowPos := 1
      ELSE
         lDeleted := Set( _SET_DELETED, .F. )
         IF ! lDeleted
            dbGoto( nRec )
            oTb:RowPos := nKeyNo
         ELSE
            dbGoTop()
            WHILE ! EOF() .AND. RecNo() != nRec
               IF Deleted()
                  nDel++
               ENDIF
               dbSkip()
            ENDDO
            dbGoto( nRec )
            oTb:RowPos := nKeyNo - nDel
         ENDIF
         SET( _SET_DELETED, lDeleted )
      ENDIF

   ELSE

      IF nRec < oTb:RowCount
         oTb:RowPos := nRec
      ENDIF

   ENDIF

   RETURN NIL
