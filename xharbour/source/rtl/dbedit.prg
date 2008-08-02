/*
 * $Id: dbedit.prg,v 1.46 2008/07/14 15:24:14 modalsist Exp $
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
#include "button.ch"
#include "setcurs.ch"
#include "hbsetup.ch"
#include "common.ch"
#include "tbrowse.ch"
#include "dbinfo.ch"

/* E.F. 2006/04/22 - The #define DE_APPEND is for Append mode in dbEdit.
 * I have used tbrowse "cargo" to assign true/false for that.
 * (Append mode is undocumented Clipper's dbEdit feature)
 */
#ifndef DE_APPEND
#define DE_APPEND  3
#endif

FUNCTION DBEdit(nTop,;                //  1§
                nLeft,;               //  2§
                nBottom,;             //  3§
                nRight,;              //  4§
                axColumns,;           //  5§
                xUserFunc,;           //  6§
                acColumnSayPictures,; //  7§
                acColumnHeaders,;     //  8§
                acHeadingSep,;        //  9§
                acColumnSep,;         // 10§
                acFootingSep,;        // 11§
                acColumnFootings,;    // 12§
                bPreBlock,;           // 13§ - xHarbour extension
                bPostBlock)           // 14§ - idem

LOCAL oTBR,;
      oTBC,;
      i,;
      nRet,;
      nKey,;
      bFunc,;
      nCursor,;
      cHdr,;
      nIndex,;
      lAppend,;
      lExcept


  nRet := DE_CONT
  lAppend := .f.

  // If no database in use we must call Errorsys() with error 2001
  //
  if ! Used()
     Throw( ErrorNew( "DBCMD", 0, 2001, ProcName(), "Workarea not in use" ) )
  endif


  DEFAULT nTop TO 0
  DEFAULT nLeft TO 0
  DEFAULT nRight TO MaxCol()
  DEFAULT nBottom TO MaxRow()


  // NOTE: Heading/footing separator is SINGLE line instead of DOUBLE line
  //       this is because most codepages (unicode too) don't have DOUBLE line chars
  //       so the output is ugly with them
  //
  DEFAULT acHeadingSep TO Chr(196) + Chr(194) + Chr(196)
  DEFAULT acColumnSep  TO " " + Chr(179) + " "
  DEFAULT acColumnFootings TO ""


  IF Empty(axColumns) .OR. !HB_ISARRAY( axColumns )

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

  If !HB_IsNil(nTop) .AND.  !HB_IsNumeric(nTop)
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(nTop)+">", Procname()+" <nTop>" ) )
  Endif
  If !HB_IsNil(nLeft) .AND.  !HB_IsNumeric(nLeft)
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(nLeft)+">", Procname()+" <nLeft>" ) )
  Endif
  If !HB_IsNil(nBottom) .AND.  !HB_IsNumeric(nBottom)
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(nBottom)+">", Procname()+" <nBottom>" ) )
  Endif
  If !HB_IsNil(nRight) .AND.  !HB_IsNumeric(nRight)
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(nRight)+">", Procname()+" <nRight>" ) )
  Endif

  nTop    := Max(0,nTop)
  nLeft   := Max(0,nLeft)
  nBottom := Min(MaxRow(),nBottom)
  nRight  := Min(MaxCol(),nRight)

  /* In Clipper the <cUserFunc> paramenter only can be a
   * string or nil, but in xHarbour can be a codeblock also.
   */
  IF !HB_IsNil(xUserFunc) .AND. ( !HB_IsString( xUserFunc ) .AND. !HB_IsBlock(xUserFunc) .AND. !HB_IsLogical(xUserFunc) )
     Throw( ErrorNew( "BASE", 0, 1127,  "Argument type error <"+valtype(xUserFunc)+">", Procname()+" <xUserFunc>" ) )
  ELSE
      If HB_IsString(xUserFunc) .AND. Empty(xUserFunc)
         xUserFunc := NIL
      Endif
      If HB_IsLogical(xUserFunc) .AND. xUserFunc
         xUserFunc := NIL
      Endif
  ENDIF

  IF !HB_IsNil(acColumnSayPictures) .AND. ( !HB_IsString(acColumnSayPictures) .AND. !HB_IsArray(acColumnSayPictures) )
     Throw( ErrorNew( "BASE", 0, 1127,  "Argument type error <"+valtype(acColumnSayPictures)+">", Procname()+" <acColumnSayPictures|cColumnSayPicture>" ) )
  ENDIF

  IF !HB_IsNil(acColumnHeaders) .AND. ( !HB_IsString(acColumnHeaders) .AND. !HB_IsArray(acColumnHeaders) )
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(acColumnHeaders)+">" , Procname()+" <acColumnHeaders|cColumnHeader>" ) )
  ENDIF

  IF !HB_IsNil(acHeadingSep) .AND. ( !HB_IsString(acHeadingSep) .AND. !HB_IsArray(acHeadingSep) )
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(acHeadingSep)+">", Procname()+" <acHeadingSeparators|cHeadingSeparator>" ) )
  ENDIF

  IF !HB_IsNil(acColumnSep) .AND. ( !HB_IsString(acColumnSep) .AND. !HB_IsArray(acColumnSep) )
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(acColumnSep)+">", Procname()+" <acColumnSeparators|cColumnSeparator>" ) )
  ENDIF

  IF !HB_IsNil(acFootingSep) .AND. ( !HB_IsString(acFootingSep) .AND. !HB_IsArray(acFootingSep) )
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(acFootingSep)+">", Procname()+" <acFootingSeparators|cFootingSeparator>" ) )
  ENDIF

  IF !HB_IsNil(acColumnFootings) .AND. ( !HB_IsString(acColumnFootings) .AND. !HB_IsArray(acColumnFootings) )
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(acColumnFootings)+">" , Procname()+" <acColumnFootings|cColumnFooting>" ) )
  ENDIF

  IF !HB_IsNil(bPreBlock) .AND. !HB_IsBlock(bPreBlock)
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(bPreBlock)+">", Procname()+" <bPreBlockBlock>" ) )
  ENDIF

  IF !HB_IsNil(bPostBlock) .AND. !HB_IsBlock(bPostBlock)
     Throw( ErrorNew( "BASE", 0, 1127, "Argument type error <"+valtype(bPostBlock)+">", Procname()+" <bPostBlockBlock>" ) )
  ENDIF


  IF HB_ISBLOCK( bPreBlock )
     i := bPreBlock
     bPreBlock := Array( Len(axColumns) )
     AFill(bPreBlock, i)
  END

  IF HB_ISBLOCK( bPostBlock )
     i := bPostBlock
     bPostBlock := Array( Len(axColumns) )
     aFill(bPostBlock, i)
  END

  iif(HB_ISNIL(acFootingSep) .AND. !Empty(acColumnFootings), acFootingSep := Chr(196) + Chr(193) + Chr(196), .T.)

  // Save previous cursor format.
  nCursor := SetCursor(SC_NONE)


  /* 2007/JAN/30 - EF - To avoid dbedit blinking. */
  DispBegin()

  /* Create Tbrowse object */
  oTBR := TBrowseDB(nTop, nLeft, nBottom, nRight)

  /* E.F. 2006/04/22 - Set append mode off by default */
  oTBR:Cargo := .F.

  /* E.F. 2006/04/22 - Use a custom 'skipper' to handle append mode */
  oTBR:SkipBlock := { |x| dbe_Skipper( x, oTBR ) }


  IF HB_ISSTRING(acHeadingSep)
     oTBR:headSep := acHeadingSep
  END

  IF HB_ISSTRING(acFootingSep)
     oTBR:footSep := acFootingSep
  END

  IF HB_ISSTRING(acColumnSep)
     oTBR:colSep := acColumnSep
  END


#ifdef HB_COMPAT_C53
  // EXTENSION: Move columns inside dbedit :)
  oTBR:setKey(K_CTRL_UP, {|| _MoveCol(oTBR, K_CTRL_UP), 0})
  oTBR:setKey(K_CTRL_DOWN, {|| _MoveCol(oTBR, K_CTRL_DOWN), 0})
#endif

 // Build columns
 //
 FOR EACH i IN axColumns

    IF !Empty( i )

       nIndex := HB_EnumIndex()

       If HB_ISARRAY(i)
          bFunc := IIf(HB_ISBLOCK(i[1]), i[1], &("{||" + i[1] + '}'))
       Else
          bFunc := IIf(HB_ISBLOCK(i), i, &("{||" + i + '}'))
       End

       If ValType(Eval(bFunc)) == 'M'  // HB_ISMEMO() returns .T. for strings :(
          bFunc := {|| "  <Memo>  "}
       End

       cHdr := i

       If HB_ISSTRING( acColumnHeaders )
          cHdr := acColumnHeaders
       ElseIf HB_ISARRAY( acColumnHeaders ) .and. Len( acColumnHeaders ) >= nIndex .and. acColumnHeaders[ nIndex ] != NIL // handle empty column headers
          cHdr := acColumnHeaders[ nIndex ]
       End

       If HB_ISBLOCK( cHdr )
          cHdr := "<block>"
       End

       oTBC := TBColumnNew( cHdr, bFunc )

       If HB_ISARRAY( i )
          oTBC:colorBlock := i[2]
       End

       If HB_ISARRAY( acColumnSep )
          oTBC:colSep := acColumnSep[ nIndex ]
       End

       If HB_ISARRAY( acHeadingSep )
          oTBC:headSep := acHeadingSep[ nIndex ]
       End

       If HB_ISARRAY( acFootingSep )
          oTBC:footSep := acFootingSep[ nIndex ]
       End

       If HB_ISARRAY( acColumnFootings )
          oTBC:footing := acColumnFootings[ nIndex ]
       ElseIf HB_ISSTRING(acColumnFootings)
          oTBC:footing := acColumnFootings
       End

       If HB_ISARRAY( acColumnSayPictures ) .and. Len( acColumnSayPictures ) >= nIndex
          oTBC:picture := acColumnSayPictures[ nIndex ]
       ElseIf HB_ISSTRING( acColumnSayPictures )
          oTBC:picture := acColumnSayPictures
       End

       If HB_ISARRAY( bPreBlock )

         If HB_ISLOGICAL( bPreBlock[ nIndex ] )
            bPreBlock[ nIndex ] := IIf( bPreBlock[ nIndex ], {|| .T.}, {|| .F.} )
         End

         oTBC:preBlock := bPreBlock[ nIndex ]

       End

       If HB_ISARRAY( bPostBlock )

         If HB_ISLOGICAL( bPostBlock[ nIndex ] )
            bPostBlock[ nIndex ] := IIf( bPostBlock[ nIndex ], {|| .T.}, {|| .F.} )
         End

         oTBC:postBlock := bPostBlock[ nIndex ]

       END

       oTBR:addColumn( oTBC )

    END

 NEXT

 DispEnd()

 if Len(axColumns) = 1
    oTBR:setKey(K_LEFT, Nil)
    oTBR:setKey(K_RIGHT, Nil)
 endif

 If Empty(xUserFunc)
    bFunc := {|| IIf(HB_ISNUMERIC(nKey) .And. (Chr(LastKey()) $ Chr(K_ESC) + Chr(K_ENTER)), DE_ABORT, DE_CONT)}
 ElseIf !HB_IsLogical(xUserFunc)
    bFunc := IIf(HB_ISBLOCK(xUserFunc), xUserFunc, &("{|x, y, z|" + xUserFunc + "(x,y,z)}"))
    oTBR:setKey( K_ESC, nil )
 Endif


#ifdef HB_EXTENSION
  // xHarbour extension: call UDF with DE_INIT mode.
  nRet := dbe_CallUDF(bFunc, DE_INIT, oTBR:colPos, oTBR)
#endif

 oTBR:ForceStable()
 oTBR:DeHilite()

 if hb_IsLogical(xUserFunc) .and. xUserFunc = .F.
    nRet := DE_ABORT
 endif

 if nRet != DE_ABORT .and. Nextkey() = 0
    nRet := dbe_CallUDF(bFunc, DE_IDLE, oTBR:colPos, oTBR)
 endif

 nKey := 0
 lAppend := oTBR:Cargo
 lExcept := .f.

 /////////////////////
 // PROCESSING LOOP //
 /////////////////////

 WHILE nRet != DE_ABORT

    if nRet = DE_CONT

       oTBR:RefreshCurrent()

    elseif nRet = DE_REFRESH

       oTBR:RefreshAll()

       if lAppend
          lAppend := .F.
          oTBR:Cargo := .F.
          oTBR:GoBottom()
       endif

       nRet := DE_CONT

    endif

    oTBR:ForceStable()

    if nRet = DE_CONT 

       if oTBR:HitTop
          nRet := dbe_CallUDF(bFunc, DE_HITTOP, oTBR:colPos, oTBR)

       elseif oTBR:HitBottom
          nRet := dbe_CallUDF(bFunc, DE_HITBOTTOM, oTBR:colPos, oTBR)

       elseif ! oTBR:Cargo .and. dbe_emptydb()
          nRet := dbe_CallUDF(bFunc, DE_EMPTY, oTBR:colPos, oTBR)

       endif

       if nRet == DE_CONT .and. lExcept
          nRet := dbe_CallUDF(bFunc, DE_EXCEPT, oTBR:colPos, oTBR)
          lExcept := .f.

          if nRet = DE_CONT .and. NextKey() = 0
             oTBR:RefreshCurrent()
             nRet := dbe_CallUDF(bFunc, DE_IDLE, oTBR:colPos, oTBR)
          endif
       endif

    endif

    if nRet = DE_ABORT
       EXIT

    elseif nRet = DE_REFRESH
       LOOP

    elseif nRet = DE_APPEND .and. ! oTBR:Cargo

       oTBR:Cargo := .T.
       lAppend := .T.

       if ! eof() .or. ! dbe_emptydb()
          oTBR:Down()
       endif

       oTBR:RefreshCurrent()
       oTBR:ForceStable()
       nRet := DE_CONT

    endif
 
    oTBR:Hilite()

    if Nextkey() != 0
       nKey := Inkey()
    else
       nKey := Inkey(0)
    endif

    if nKey != 0

       if dbe_ProcessKey( nKey, oTBR) = DE_ABORT
          EXIT
       endif

       if ValType( SetKey(nKey) ) == 'B'
          Eval( SetKey(nKey), ProcName(1), ProcLine(1), "")
       endif

       lExcept := .t.

    endif

 ENDDO

 SetCursor( nCursor )
 SetPos(Row(),0)

RETURN .T.



*------------------------------------------------------*
STATIC FUNCTION dbe_CallUDF(bFunc, nMode, nColPos, oTBR)
*------------------------------------------------------*
LOCAL nRet, nRec, nKey, i, j, nLastRec, lDeleted, lChanged

  nRet := DE_CONT

  if nMode = DE_INIT

     nKey := NextKey()

     if nKey == K_ENTER .or. nKey == K_ESC
        inkey()
        Return DE_ABORT
     endif

     while nKey != 0
        inkey()
        dbe_ProcessKey( nKey, oTBR )
        nRet := dbe_return( Eval(bFunc, DE_EXCEPT, nColPos, oTBR) )
        if nRet = DE_ABORT
           EXIT
        elseif nRet = DE_REFRESH
           oTBR:RefreshAll()
           oTBR:ForceStable()
        elseif nRet = DE_CONT
           oTBR:RefreshCurrent()
           oTBR:ForceStable()
        endif
        nKey := NextKey()
     enddo

     if nRet != DE_ABORT
        nRet := dbe_return( Eval(bFunc, DE_INIT, nColPos, oTBR) )
     endif

     Return nRet

  elseif nMode = DE_EXCEPT

     oTBR:DeHilite()
     oTBR:ColorRect({oTBR:rowpos,oTBR:colpos,oTBR:rowpos,oTBR:colpos},{1,2})

  endif

  lDeleted := Deleted()
  nRec     := RecNo()
  nLastRec := LastRec()

  // Call UDF
  nRet := dbe_return( Eval(bFunc, nMode, nColPos, oTBR) )

  // A change was occurred on UDF (append, delete or skip).
  lChanged := ( nLastRec != LastRec() .or.;
                Deleted() != lDeleted .or.;
                nRec != Recno() )


  if nRet = DE_ABORT .or. nRet = DE_APPEND
     Return nRet
  endif

  // The UDF has changed file, so dbedit need to be refreshed.
  if lChanged 
     if LastRec() > nLastRec  // append blank
        oTBR:RowPos := 1
     elseif Deleted() .and. Lastrec() != 0
        if SET(_SET_DELETED)
           dbSkip()
        endif
     endif
     nRet := DE_REFRESH
  endif

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

*-----------------------------------------*
STATIC FUNCTION dbe_Skipper( nSkip, oTb )
*-----------------------------------------*

   LOCAL lAppend := oTb:Cargo
   LOCAL i       := 0

   do case
   case ( nSkip = 0 .or. lastrec() = 0 )
      // Skip 0 (significant on a network)
      dbSkip( 0 )

   case ( nSkip > 0 .and. !eof() )
      while ( i < nSkip )           // Skip Foward

         dbskip( 1 )
         i++

         if eof() .and. ! lAppend
            dbskip( -1 )
            i--
            exit
         endif

      enddo

   case ( nSkip < 0 )
      while ( i > nSkip )           // Skip backward

         dbskip( -1 )

         if bof()
            exit

         endif

         i--

      enddo

   endcase

RETURN i


#ifdef HB_COMPAT_C53
Static Function _MoveCol(oTBR, nKey)
Local oTBR1, oTBR2

  If nKey = K_CTRL_DOWN .And. oTBR:colPos < oTBR:colCount
    oTBR1 := oTBR:getColumn(oTBR:colPos)
    oTBR2 := oTBR:getColumn(oTBR:colPos + 1)
    oTBR:setColumn(oTBR:colPos, oTBR2)
    oTBR:SetColumn(oTBR:colPos + 1, oTBR1)
    oTBR:colPos++
    oTBR:invalidate()
  ElseIf nKey = K_CTRL_UP .And. oTBR:colPos > 1
    oTBR1 := oTBR:getColumn(oTBR:colPos)
    oTBR2 := oTBR:getColumn(oTBR:colPos - 1)
    oTBR:setColumn(oTBR:colPos, oTBR2)
    oTBR:SetColumn(oTBR:colPos - 1, oTBR1)
    oTBR:colPos--
    oTBR:invalidate()
  End
Return Nil
#endif


/****
 *
 *  dbe_emptydb()
 *
 *  Verify if the dbf in the current workarea is empty.
 */

*-------------------------------------*
STATIC FUNCTION dbe_emptydb()
*-------------------------------------*
Local lEmpty

 IF !Empty( dbFilter() )
    lEmpty := eof()
 ELSEIF IndexOrd() = 0
    lEmpty := ( ( Eof() .OR. RecNo() = LastRec() + 1) .AND. Bof() )
 ELSE
    lEmpty := ( OrdKeyCount() = 0  )
 ENDIF

RETURN lEmpty

*------------------------------------------*
STATIC FUNCTION dbe_processKey( nKey, oTb )
*------------------------------------------*
Local nRet := DE_CONT

#ifdef HB_COMPAT_C53

    if oTb:ApplyKey( nKey ) = TBR_EXIT
       nRet := DE_ABORT
    endif

#else

    // xHarbour without 5.3 extensions code
    Switch nKey
      Case K_DOWN
        oTb:down()
        Exit
      Case K_UP
        oTb:up()
        Exit
      Case K_LEFT
        oTb:left()
        Exit
      Case K_RIGHT
        oTb:right()
        Exit
      Case K_PGDN
        oTb:pageDown()
        Exit
      Case K_PGUP
        oTb:pageUp()
        Exit
      Case K_CTRL_PGUP
        oTb:goTop()
        Exit
      Case K_CTRL_PGDN
        oTb:goBottom()
        Exit
      Case K_HOME
        oTb:home()
        Exit
      Case K_END
        oTb:end()
        Exit
      Case K_CTRL_HOME
        oTb:panHome()
        Exit
      Case K_CTRL_END
        oTb:panEnd()
        Exit
      Case K_CTRL_LEFT
        oTb:panLeft()
        Exit
      Case K_CTRL_RIGHT
        oTb:panRight()
        Exit
    End

#endif

Return nRet

*----------------------------------*
STATIC FUNCTION dbe_Return( n )
*----------------------------------*
if ! hb_isnumeric( n )
   n := DE_CONT
elseif n < DE_ABORT .or. n > DE_APPEND
   n := DE_CONT
endif

Return n


