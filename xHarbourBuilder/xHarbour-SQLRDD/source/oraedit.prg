request ADS
#include "ads.ch"
#ifndef __XHARBOUR__
   #define DE_INIT -1
   #include "hbcompat.ch"
   #include "xhbcls.ch"
#endif

Static hHashData := {=>}

Static TRACE_STRUCT   := { ;
                              { "USUARIO",    "C", 10, 0 },;
                              { "DATA",       "D", 08, 0 },;
                              { "HORA",       "C", 08, 0 },;
                              { "CONTADOR",   "C", 01, 0 },;
                              { "TRANSCOUNT", "N", 10, 0 },;
                              { "PROCESSED", "L", 1, 0 },;
                              { "COMANDO",    "M", 10, 0 } ;
                           }

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
#ifdef __XHARBOUR__
   #include "hbsetup.ch"
#endif
#include "common.ch"
#include "tbrowse.ch"

/* E.F. 2006/04/22 - The #define DE_APPEND is for Append mode in dbEdit.
 * I have used tbrowse "cargo" to assign true/false for that.
 * (Append mode is undocumented Clipper's dbEdit feature)
 */

#ifndef DE_APPEND
#define DE_APPEND  3
#endif
#ifndef HB_COMPAT_C53
#define  HB_COMPAT_C53
#endif

static nAliasTmp := 0

FUNCTION OraEdit(nCursors, cTable, cWhere, aVarSust, nTop,;
                 nLeft,;
                 nBottom,;
                 nRight,;
                 axColumns,;
                 xUserFunc,;
                 acColumnSayPictures,;
                 acColumnHeaders,;
                 acHeadingSep,;
                 acColumnSep,;
                 acFootingSep,;
                 acColumnFootings,;
                 bPreBlock,;
                 bPostBlock )

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
Local cSql
Local cCount
Local n,cBind
Local cFile
Local nPosOrderBY
Local cCols :=""
Local nOldArea := Select()
Local cAlias
Local cQueryPage := ""
Local nHigerBound ,nLowerBound,nStep := 100
Local aRet := {}
Local cFiletoDelete :=""
Local lInEof :=.f.
Local aTemp,aPk,cdesc:='',nRecno
local lDescIndex := .f.
LOCAL aTempCols :={}
local nApeStart := 1
Local acolsadded := {}
Local cTmp
(nCursors)
//hHashData :=hash()
set server local
SR_SetRDDTemp("ADT")
 IF Empty(axColumns) .OR. ! HB_ISARRAY( axColumns )
    cCols := ' * '
  else
     FOR EACH i IN axColumns
        if ( "||" in I )

           cTmp := "TMP"+strzero(nApeStart++,3)
           cCols += i+" as "+cTmp+","
           AADD( atempcols,{ cTmp, hb_atokens(i,"||"), i } )

        ELSE
        cCols += i+","

        ENDIF
     NEXT


     //cCols := substr(cCols,1,len(cCols)-1)

     For EACH acolsadded in aTempCols
         For EACH n in acolsadded[2]
             cCols += n+","
     NEXT
     Next

     aPk := GETPRIMARYKEY( cTable )

     For EACH cTmp in aPk
        if At( Upper( cTmp ), Upper( cCols ) ) == 0
           cCols += cTmp+","
        EndIf
     Next

     cCols := substr(cCols,1,len(cCols)-1)
  endif

 cSql := "Select  " + cCols + "  from " + cTable
 cCount := "select count(*) from " + cTable

if !empty(cWhere) .and. valtype( aVarSust) == "A"

for i:=1 to len( aVarSust )
   cBind := ":"+alltrim(str( i))
   cWhere := strtran( cWhere,cBind,   sr_cdbvalue(aVarSust[i]) )
next
* nat := at
endif
if !empty( cWhere )
   IF ( "ORDER BY" in upper( cwhere) )
      if ( " DESC" in upper( cwhere) )
         lDescIndex := .t.
      endif
      nPosOrderBY := AT("ORDER BY" , upper( cwhere))

      if ! ( "WHERE " in upper(cWhere) ) .and. nPosOrderBY >1
         cSql += " where " + cWhere
         cCount += " where  " + cWhere

      else
         cSql += "  " + cWhere
         cCount += "  " + cWhere
      endif
   else
      aPk := GETPRIMARYKEY( cTable )
      if len(aPk) > 0
         for each aTemp in aPk
         cdesc += atemp + " ,"
      next
      cdesc := substr(cdesc,1,len(cdesc)-1)
   endif
   if !empty( cDesc )
      cSql += " where " + cWhere + " ORDER BY " +cDesc
      cCount += " where  " + cWhere
   else
      cSql += " where " + cWhere
      cCount += " where  " + cWhere
   endif
endif
else
  cSql += " ORDER BY 1"
endif

cSql :=  "select * from ( select a.*, rownum r from ( " + cSql + ") a where rownum <= :HigerBound  ) where r >= :LowerBound"
sr_getconnection():exec(ccount,,.t.,@aret)
if len(aRet) >0
   if aret[1,1] <100
      nHigerBound := aret[1,1]
      nLowerBound :=1
      nStep := aret[1,1]
   else
      nHigerBound := 100
      nLowerBound :=1
      nStep := 100
   endif
else
nHigerBound := 100
nLowerBound :=1
nStep := 100
endif
  fclose(HB_FTEMPCREATE('.','tmp',,@cFile))
  nRet := DE_CONT
  lAppend := .f.
hHashData[nAliasTmp]:=hash()

if nAliasTmp ==0
   cAlias := 'tmpedit'
   nAliasTmp++
   hHashData[nAliasTmp]:=hash()
   hHashData[nAliasTmp]["cFile"]:=strtran(cfile,'.tmp','')
else
   cAlias := 'tmpedit'+strzero(nAliasTmp,3)
   nAliasTmp++
   hHashData[nAliasTmp]:=hash()
   hHashData[nAliasTmp]["cFile"]:=strtran(cfile,'.tmp','')
endif
hHashData[nAliasTmp]["eof"] := .f.
refreshFullData(csql,cAlias,cfile,nHigerBound,nLowerBound,nStep)

createkeyfrompk(calias,ctable,lDescIndex) //cria o indice temporatio em cima da pk
cFiletoDelete :=(calias)->(dbinfo(10))
nLowerBound +=nHigerBound

  if ! Used()
#ifdef HB_C52_STRICT
     dbgobottom() /* Clipper compliance: call dbgobotom() to forces error message. */
#else
     /* Call Errorsys() with error 2001 if not database in use. */
     Throw( ErrorNew( "DBCMD", 0, 2001, procname(), "Workarea not in use" ) )
#endif
  elseif eof() .and. Lastrec() > 0
     /* DbEdit() moves cursor to the bottom record if eof() is reached at init. */
     dbGoBottom()
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


  IF Empty(axColumns) .OR. ! HB_ISARRAY( axColumns )

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

  // Save previous cursor shape and position.
  nCursor := SetCursor(SC_NONE)

  if HB_ISNIL(acFootingSep) .AND. !Empty(acColumnFootings)
     acFootingSep := Chr(196) + Chr(193) + Chr(196)
  endif

  /* 2007/JAN/30 - EF - To avoid dbedit blinking. */
  DispBegin()

  /* Create Tbrowse object */
  oTBR := TBrowseDB(nTop, nLeft, nBottom, nRight)

  /* E.F. 2006/04/22 - Set append mode off by default */
  oTBR:Cargo := .F.

  /* E.F. 2006/04/22 - Use a custom 'skipper' to handle append mode */
  oTBR:SkipBlock := { |x| dbe_Skipper( x, oTBR,calias ) }
*    oTBR:SkipBlock := { |x|__Skipped(x,lappend)}

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
       cTmp := ""
       if ( "||" in i )
          n := Ascan( atempcols, { |x| x[3] == i } )
          if n > 0
             cTmp := atempcols[n,1]
          endif
       endif

       If HB_ISARRAY(i)
          bFunc := IIf(HB_ISBLOCK(i[1]), i[1], &("{||" + i[1] + '}'))
       Else
          if !Empty( cTmp )
            bFunc := IIf(HB_ISBLOCK(cTmp), cTmp, &("{||" + cTmp + '}'))
          Else
          bFunc := IIf(HB_ISBLOCK(i), i, &("{||" + i + '}'))
          EndIf
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
  nRet := dbe_CallUDF(bFunc, DE_INIT, oTBR:colPos, ,oTBR,csql,cCount,cfile,calias,@nHigerBound,@nLowerBound,nStep,cTable)
#endif

 oTBR:ForceStable()
 oTBR:DeHilite()

 if hb_IsLogical(xUserFunc) .and. xUserFunc = .F.
    nRet := DE_ABORT
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
*        oTBR:RefreshAll()

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
     if (eof() .or. hHashData[nAliasTmp]["eof"])

         nRecno :=recno()

     sr_getconnection():exec(ccount,,.t.,@aret)
     if len(aRet) >0
         if (calias)->(lastrec()) < aret[1,1]
         nHigerBound += nStep
         refreshFullData(csql,cAlias,cfile,nHigerBound,nLowerBound,nStep)
         nLowerBound += nStep
         otbr:refreshall()
         dbgoto(nrecno)
         oTbr:forcestable()
         oTbr:down():forcestable()
         endif
     endif
*      oTbr:up():forcestable()
     endif
hHashData[nAliasTmp]["eof"]:=.f.
    if nRet = DE_CONT

       if ! lExcept

          if dbe_emptydb()
             nRet := dbe_CallUDF(bFunc, DE_EMPTY, oTBR:colPos,GetCurValue(calias), oTBR,csql,cCount,cfile,calias,@nHigerBound,@nLowerBound,nStep,cTable)

          elseif oTBR:HitTop
             oTBR:HitTop := .f.
             nRet := dbe_CallUDF(bFunc, DE_HITTOP, oTBR:colPos,GetCurValue(calias), oTBR,csql,cCount,cfile,calias,@nHigerBound,@nLowerBound,nStep,cTable)

          elseif oTBR:HitBottom
             oTBR:HitBottom := .f.
             nRet := dbe_CallUDF(bFunc, DE_HITBOTTOM, oTBR:colPos, GetCurValue(calias), oTBR,csql,cCount,cfile,calias,@nHigerBound,@nLowerBound,nStep,cTable)

          endif

       else

 //         nRet := dbe_CallUDF(bFunc, DE_EXCEPT, oTBR:colPos, GetCurValue(calias), oTBR,,,,,,,cTable)
          lExcept := .f.
          if lastkey() == K_ENTER
             oTBR:RefreshCurrent()
          endif
       endif

       // No keystrokes pending...
       if NextKey() = 0
          dbe_CallUDF(bFunc, DE_IDLE, oTBR:colPos, GetCurValue(calias), oTBR,csql,cCount,cfile,calias,@nHigerBound,@nLowerBound,nStep,cTable)
          // force dbedit DE_CONT state after IDLE mode.
          nRet := DE_CONT
       endif

    endif


    if nRet = DE_ABORT
       EXIT

    elseif nRet = DE_REFRESH
       LOOP

*     elseif nRet = DE_APPEND .and. ! oTBR:Cargo
*
*        oTBR:Cargo := .T.
*        lAppend := .T.
*
*        if ! eof() .or. ! dbe_emptydb()
*           oTBR:Down()
*        endif
*
*        oTBR:RefreshCurrent()
*        oTBR:ForceStable()
*        nRet := DE_CONT

    endif

    oTBR:Hilite()

    if Nextkey() != 0
       nKey := Inkey()
    else
       nKey := Inkey(0)
    endif

    if nKey != 0

       nRet := dbe_CallUDF(bFunc, DE_EXCEPT, oTBR:colPos, GetCurValue(calias), oTBR,csql,cCount,cfile,calias,@nHigerBound,@nLowerBound,nStep,ctable)

       if nRet == DE_ABORT
          EXIT
       endif

       if dbe_ProcessKey( nKey, oTBR) = DE_ABORT
          EXIT
       endif

       if ValType( SetKey(nKey) ) == 'B'
          Eval( SetKey(nKey), ProcName(1), ProcLine(1), "")
       endif

       lExcept := ! dbe_cursorkey( nKey )

    endif

 ENDDO

 SetCursor( nCursor )
 SetPos( row(),0 )
if select(cAlias )> 0
(cAlias)->(dbclosearea())
endif
if nOldArea > 0
   select( nOldArea )
endif
if file(hHashData[nAliasTmp]["cFile"]+"sqllog.dbf")
ferase(hHashData[nAliasTmp]["cFile"]+"sqllog.dbf")
ferase(hHashData[nAliasTmp]["cFile"]+"sqllog.dbt")
endif
if nAliasTmp >0
nAliasTmp--
endif
ferase(cFiletoDelete)
if file(strtran(cFiletoDelete,'.tmp','.adi'))
ferase(strtran(cFiletoDelete,'.tmp','.adi'))
endif
/* Clipper's NG says that DBEdit always returns NIL, but doesn't. */
RETURN 0 //.T.



*------------------------------------------------------*
STATIC FUNCTION dbe_CallUDF(bFunc, nMode, nColPos, avalue, oTBR, csql,;
 cCount, cfile,calias,nHigerBound,;
 nLowerBound,nStep,cTable)
*------------------------------------------------------*
LOCAL nRet, nRec, nKey, nLastRec, lDeleted, lChanged,aret :={}

Local aValues :={}
(avalue,csql,cfile,nHigerBound,nLowerBound,nStep)
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
        nRet := dbe_return( Eval(bFunc, DE_EXCEPT, nColPos, GetCurValue(calias)) )
        if nRet = DE_ABORT
           EXIT
        elseif nRet = DE_REFRESH
*            refreshFullData(csql,calias)
           oTBR:RefreshAll()
           oTBR:ForceStable()
        elseif nRet = DE_CONT
           oTBR:RefreshCurrent()
           oTBR:ForceStable()
        endif
        nKey := NextKey()
     enddo

     if nRet != DE_ABORT
        nRet := dbe_return( Eval(bFunc, DE_INIT, nColPos,   GetCurValue(calias),oTBR) )

     endif

     Return nRet

  elseif nMode = DE_EXCEPT

     oTBR:DeHilite()
     oTBR:ColorRect({oTBR:rowpos,oTBR:colpos,oTBR:rowpos,oTBR:colpos},{1,2})

  elseif nMode == DE_IDLE .or. nMode == DE_EMPTY

     keyboard chr(0)
     inkey()

  endif

  lDeleted := Deleted()
  nRec     := RecNo()
  nLastRec := ( cAlias )->( LastRec() )

  // Call UDF
  aValues := GetCurValue( calias )
  nkey := lastkey()
  if nKey == K_ENTER .or. nKey == K_DEL
     GETREFRESHCURVALUE( cAlias,ctable )
     aValues := GetCurValue( calias )
  endif
  SR_StartLog()
  nRet := dbe_return( Eval(bFunc, nMode, nColPos,  aValues,oTBR) )
  SR_StopLog()

  if nRet == DE_REFRESH

     if nKey == K_DEL
       if IsPrimaryKeyDeleted( cAlias ,cTable)
           (calias)->( rlock() )
           (calias)->( dbdelete() )
           (calias)->( dbunlock() )
      endif
        sr_getconnection():Exec(cCount,,.t.,@aret)
     elseif  nKey == K_INS

*         nHigerBound++
*         refreshFullData(csql,cAlias,cfile,nHigerBound,nLowerBound,nStep)
*         nLowerBound += 1
    //    nLastRec := ( cAlias )->( LastRec() )
        insertupdated(cAlias ,cTable)
        otbr:refreshall()
*     cSql := sr_getconnection():cLastcomm
*     if upper(ctable) in upper(cSql) .and. "INSERT" in upper(cSql )
*        cValues := substr(cSql,at("VALUES",upper(cSql)))
*        cSql := strtran(csql,cvalues,'')
*        cvalues := alltrim(values)
*        cSql := alltrim(cSql)
*        cSql := substr(csql,at('(',csql)+1)
*        csql :=strtran(csql,')','')
*        cvalues := alltrim(cvalues)
*        cvalues := substr(cvalues,at('(',cvalues)+1)
*        cvalues :=strtran(cvalues,')','')
*        aField := hb_atokens(csql,',')
*            aVal := hb_atokens(cvalues,',')
*        (calias)->(dbappend())
*        for i := 1 to len(afield)
*           try
*             (calias)->(fieldput( (calias)->(fieldpos(aField[i])),aval[i]))
*           catch
*           end
*        next
*        endif
*
*
         sr_getconnection():Exec(cCount,,.t.,@aret)
*
     else
       if nKey == K_ENTER
           GETREFRESHCURVALUE(cAlias,cTable)
         //  aValues := GetCurValue(calias)
        endif
     sr_getconnection():Exec(cCount,,.t.,@aret)
     endif



  endif
  // A change was occurred on UDF (append, delete or skip).
  lChanged := ( nLastRec != ( cAlias )->( lastrec() ) .or.;
                Deleted() != lDeleted .or.;
                nRec != Recno() )

   if len(aret ) > 0 .and. nRet == DE_REFRESH
      lChanged := lChanged .or. reccount()!=aret[1,1]
   endif
  if nRet = DE_ABORT .or. nRet = DE_APPEND
     Return nRet
  endif

  // The UDF has changed db/record, so dbedit need to be refreshed.
  if nRet == DE_REFRESH
   *      refreshFullData(csql,calias,cfile)
    otbr:refreshall()

  endif
  if lChanged

     if ( cAlias )->( LastRec() ) > nLastRec   // append.

        nKey := nextkey()
        *refreshFullData(csql,calias)


        if ( nKey != 0 .and. ! dbe_CursorKey(nKey) ) .or.;
           (calias)->(ordkeyno()) < oTBR:RowPos
           oTBR:Gotop()
        endif
*                  IF ( Set( _SET_DELETED ) .AND. Deleted() ) .OR. ;
*             ( !Empty( dbfilter() ) .AND. !&( dbFilter() ) )
*             dbSkip()
*          ENDIF
*          IF EOF()
*             dbGoBottom()
*          ENDIF
*
*
*          otbr:forceStable()
*

     elseif (calias)->(LastRec()) < nLastRec .or.  aret[1,1] < (calias)->(reccount() )  // pack

        oTBR:RowPos := 1

     elseif (calias)->(Deleted() ).and. ( cAlias )->( LastRec() ) != 0  .or.  aret[1,1]<=(calias)->(reccount() )// deleted

        if SET( _SET_DELETED )
           while ! eof() .and. deleted()
              dbSkip()
           enddo
        else
           dbe_syncpos(oTBR)
        endif

     elseif nRec != Recno() // moved.

        dbe_syncpos(oTBR)

     endif

     if (eof() .and. ( cAlias )->( LastRec() ) > 0 )

          dbskip(-1)

          oTBR:Up():forceStable()
*         hHashData[calias]["eof"]:=.f.
* *         dbgobottom()
//culik comentado para teste
*          nHigerBound += nStep
*          refreshFullData(csql,cAlias,cfile,nHigerBound,nLowerBound,nStep)
*          nLowerBound += nStep
     endif

     nRet := DE_REFRESH

  endif
  if eof()
*      dbskip(-1)

*             oTBR:Up():forceStable()
//culik comentado para teste
*         nHigerBound += nStep
*         refreshFullData(csql,cAlias,cfile,nHigerBound,nLowerBound,nStep)
*         nLowerBound += nStep
  elseif bof()
*      if nHigerBound > nStep
*         nHigerBound -= nStep
*         nLowerBound -= nStep
*         refreshFullData(csql,cAlias,cfile,nHigerBound,nLowerBound,nStep)
*         nLowerBound += nStep
*      else
*         nLowerBound :=100
*         nLowerBound := 1
*         refreshFullData(csql,cAlias,cfile,nHigerBound,nLowerBound,nStep)
*         nLowerBound += nStep
*      endif
*
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
   LOCAL lEof := .F.
   do case
   case ( nSkip = 0 .or. lastrec() = 0 )
      // Skip 0 (significant on a network)
      dbSkip( 0 )
   case ( nSkip > 0 .and. !eof() )
      while ( i < nSkip )           // Skip Foward
         dbskip( 1 )
         i++
         if eof() .and. ! lAppend
             hHashData[nAliastmp]["eof"] := .t.
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

*-------------------------------------*
STATIC FUNCTION dbe_emptydb()
*-------------------------------------*
* Verify if the current dbf is empty.
*-------------------------------------*
Local lEmpty

 if LastRec() = 0
    Return .T.
 endif

 if ! Empty( dbFilter() )
    lEmpty := ( Eof() .or. Recno() > Lastrec() )
 elseif IndexOrd() = 0
    lEmpty := ( ( Eof() .or. Recno() > LastRec() ) .and. Bof() )
 else
    //lEmpty := ( OrdKeyCount() = 0  ) // this code decrease dbedit's speed at large table.
    lEmpty := ( OrdKeyNo() = 0 )
 endif

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
      Case K_DOWN;oTb:down();Exit
      Case K_UP;oTb:up();Exit
      Case K_LEFT;oTb:left();Exit
      Case K_RIGHT;oTb:right();Exit
      Case K_PGDN;oTb:pageDown();Exit
      Case K_PGUP;oTb:pageUp();Exit
      Case K_CTRL_PGUP;oTb:goTop();Exit
      Case K_CTRL_PGDN;oTb:goBottom();Exit
      Case K_HOME;oTb:home();Exit
      Case K_END;oTb:end();Exit
      Case K_CTRL_HOME;oTb:panHome();Exit
      Case K_CTRL_END;oTb:panEnd();Exit
      Case K_CTRL_LEFT;oTb:panLeft();Exit
      Case K_CTRL_RIGHT;oTb:panRight();Exit
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

*------------------------------------*
STATIC FUNCTION dbe_cursorkey( nKey )
*------------------------------------*
Local aKeys := { K_LEFT,;
                 K_RIGHT,;
                 K_CTRL_LEFT,;
                 K_CTRL_RIGHT,;
                 K_UP,;
                 K_DOWN,;
                 K_HOME,;
                 K_END,;
                 K_CTRL_HOME,;
                 K_CTRL_END,;
                 K_PGUP,;
                 K_PGDN,;
                 K_CTRL_PGUP,;
                 K_CTRL_PGDN }

Return ( AScan( aKeys, nKey ) != 0 )

*--------------------------------*
STATIC FUNCTION dbe_syncpos(oTb)
*--------------------------------*
local nRec := Recno()
local nKeyNo := 0
local nDel := 0
local lDeleted := .f.

 if IndexOrd() != 0

    nKeyNo := OrdKeyNo()
    dbSkip(-1)

    if bof()
       oTb:RowPos := 1
    else
       lDeleted := Set( _SET_DELETED, .F. )
       if ! lDeleted
          dbGoto(nRec)
          oTb:RowPos := nKeyNo
       else
          dbGotop()
          while ! eof() .and. recno() != nRec
             if deleted()
                nDel++
             endif
             dbskip()
          enddo
          dbGoto(nRec)
          oTb:RowPos := nKeyNo - nDel
       endif
       Set( _SET_DELETED , lDeleted )
    endif

 else

    if nRec < oTb:RowCount
       oTb:RowPos := nRec
    endif

 endif

RETURN NIL


function GetCurValue(calias)
Local n
Local aTemp := {}
for n:=1 to (cAlias)->(fcount())
   aadd(aTemp,(cAlias)->(fieldget(n)))
next
   aadd(aTemp,(calias)->(recno()))
return aTemp

static function refreshFullData(csql,cAlias,cfile,nHigh,nLow)

Local nRecno :=0
Local nBeforeTotRec:=0, nAfterRec:=0
default cFile to (calias)->(dbinfo(10))
* if select(caLias) > 0
*    zap
* endif
*          cSql := strtran(csql,":HigerBound",str(nHigh))
*          cSql := strtran(csql,":LowerBound",str(nLow))
*
* sr_getconnection():exec( cSql,,.t.,,cfile, cAlias )
*       (calias)->(dbgotop())
   nBeforeTotRec := (calias)->(reccount())
if select(caLias) > 0
      if (calias)->(eof())
         nRecno := nBeforeTotRec
      else
   nRecno := (calias)->(recno())
endif
   endif
   cSql := strtran(csql,":HigerBound",str(nHigh))
   cSql := strtran(csql,":LowerBound",str(nLow))

sr_getconnection():exec( cSql,,.t.,,cfile, cAlias )
   nAfterRec:=(calias)->(reccount())

   if nAfterRec > nrecno .and. nBeforeTotRec<nAfterRec
   if nrecno == 0
      (calias)->(dbgotop())
   else
         (calias)->(dbgoto(nrecno+1))
      endif
   endif
return nil

function GETPRIMARYKEY(cTable)
Local aRet := {}
Local aFields := {}
Local aTemp
Local cSql
IF ("." IN CTABLE)
   CSQL :=  "SELECT cols.table_name, cols.column_name, cols.position, cons.status, cons.owner FROM all_constraints cons, all_cons_columns cols WHERE cols.table_name = " + sr_cdbvalue(upper(alltrim(SUBSTR(cTable,AT('.',CTABLE)+1))) ) + " AND cons.constraint_type = 'P' AND cons.constraint_name = cols.constraint_name AND cons.owner = cols.owner ORDER BY cols.table_name, cols.position"
ELSE
   CSQL :=  "SELECT cols.table_name, cols.column_name, cols.position, cons.status, cons.owner FROM all_constraints cons, all_cons_columns cols WHERE cols.table_name = " + sr_cdbvalue(upper(alltrim(cTable)) ) + " AND cons.constraint_type = 'P' AND cons.constraint_name = cols.constraint_name AND cons.owner = cols.owner ORDER BY cols.table_name, cols.position"
ENDIF
sr_getconnection():exec(cSql,,.t.,@aret)
if len(aRet) > 0
   for each aTemp in aRet
   aadd(aFields,alltrim(aTemp[2]))
   next
endif
return aFields

function GETREFRESHCURVALUE(calias,ctable)
//Local cTable := (calias)->(dbinfo(10))
Local aFields := GETPRIMARYKEY( cTable )
Local aFields2
Local aTemp
Local i :=1
Local cSql := "",aret:={}
Local adb:=(calias)->(dbstruct())
Local nPos
Local ckey
Local aTmp,nrecno
if len( aFields  )>0
   cSql := "select * from "+ ctable
   cSql += " where "

   for each aTemp in aFields
      cKey := (calias)->(fieldGet( (cAlias)->( fieldpos( aTemp ) ) ) )
      if empty(ckey)
      cSql += " " + aTemp  + " is null "
      else
      cSql += " " + aTemp  + " = " + sr_cdbvalue( ckey )
      endif
      cSql += " AND "
   NEXT
   cSql := substr(cSql,1,len(csql)-4)

   sr_getconnection():exec(cSql,,.t.,@aret)
   aFields2 := sr_getconnection():aFields
   if len(aret) > 0
      (calias)->(rlock())
      aTemp := aret[1]
      for each aTmp in aFields2

         nPos :=ascan(adb,{|x| x[1] ==  aFields2[i,1] })
         if nPos >0

         (cAlias)->(fieldput( (cAlias)->( fieldpos( adb[nPos,1] )),aTemp[i]))
         endif
         i++
      next
      (calias)->(dbunlock())
      nrecno := (calias)->(recno())
      (calias)->(dbgoto(nrecno))
   endif
endif
return nil


function GETREFRESHCURINSVALUE(calias,ctable,calias2)
//Local cTable := (calias)->(dbinfo(10))
Local aFields := GETPRIMARYKEY( cTable )
Local aFields2
Local aTemp
Local i :=1
Local cSql := "",aret:={}

local cfile:=(calias2)->(dbinfo(10))
Local cfield, aTmpField, nposf

if len( aFields  )>0
   cSql := "select * from "+ ctable
   cSql += " where "

   for each aTemp in aFields
      cSql += " " + aTemp  + " = " + sr_cdbvalue( (calias)->(fieldGet( (cAlias)->( fieldpos( aTemp ) ) ) ))
      cSql += " AND "
   NEXT

   cSql := substr(cSql,1,len(csql)-4)
   //corrigido neste ponto
*    sr_getconnection():exec( cSql,,.T.,,cfile, cAlias2,1 )
   //(calias2)->(dbgobottom())
//endif

   sr_getconnection():exec(cSql,,.t.,@aret)
   aFields2 := sr_getconnection():aFields

   if len(aret) > 0

*       for each aTemp in aRet
         (calias2)->(dbappend())
       //  tracelog(valtoprg(atemp),aFields2[i,1] )
         i:=1
         aTemp := aret[1]
         FOR each aTmpField in aFields2
            cField := aTmpField[1]
            nposf := (cAlias2)->(fieldpos( cField ))
            (cAlias2)->( fieldput(  nposf  , aTemp[ i ] ) )
            ++i
         NEXT
         (calias2)->(dbcommit())
         (calias2)->(dbunlock())
*       next

      //nrec:=(cAlias2)->(recno())
      //(calias2)->(dbgoto(nrec))

   endif

endif

return nil

function IsPrimaryKeyDeleted(calias,cTable)
local aret:={}

Local aFields := GETPRIMARYKEY( cTable )

Local aTemp
Local i :=1
Local cSql := ""
Local nFieldPos
local xVal
if len( aFields  )>0
   cSql := "select * from "+ ctable
   cSql += " where "
   for each aTemp in aFields
   nFieldPos := (cAlias)->( fieldpos( aTemp ) )
      xval := (calias)->(fieldGet( nfieldPos ) )
      if empty(xval)
         cSql += " "+aTemp  + " is null "
      else
         cSql += " "+aTemp  + " = " + sr_cdbvalue( xVal )
      endif
      cSql += " AND "
   NEXT
   cSql := substr(cSql,1,len(csql)-4)

   sr_getconnection():exec(cSql,,.t.,@aret)
   if len(aRet ) == 0
   return .t.
   endif
endif
return .f.


function insertupdated(calias,ctable)
Local aFields := GETPRIMARYKEY( cTable )
Local cFields:=""
Local cDesc  := ''
Local ctemp  := (calias)->(dbinfo(10))
Local cFileDrive := substr(cTemp,1,rat('\',cTemp)-2)
Local cFile := substr(cTemp,rat('\',cTemp)+1)
Local aVal
Local cSqlTmp := ""
Local aTemp
local atemp2:={},adb:=(calias)->(dbstruct()),nPos
Local i:=1

Local cInsert :=''
Local csql:=''
Local cvalues:=''
Local aField
cFile := substr(cfile,1,at('.',cfile)-1)
if len(aFields) > 0
   for each aTemp in afields

      cFields += aTemp+','
      cdesc += atemp + " DESC,"
      nPos := ascan(adb,{|x| upper(x[1]) == aTemp})
      if nPos >0
         aadd(aTemp2,adb[npos,2])
      endif

   next
   cdesc := substr(cdesc,1,len(cdesc)-1)
   cFields := substr(cFields,1,len(cFields)-1)
   cSql :=GetLastInsertCommand(cTable)

   if !empty(cSql)
         cValues := substr(cSql,at("VALUES",upper(cSql)))
         cSql := strtran(csql,cvalues,'')
         cvalues := alltrim(cvalues)
         cSql := alltrim(cSql)
         cSql := substr(csql,at('(',csql)+1)
         csql :=strtran(csql,')','')

         cSql := alltrim(cSql)
         cvalues := alltrim(cvalues)
         cvalues := substr(cvalues,at('(',cvalues)+1)
         cvalues :=strtran(cvalues,')','')
*        cvalues :=strtran(cvalues,"'",'')
         aField := hb_atokens(csql,',')
           aVal := hb_atokens(cvalues,',')
*        (calias)->(dbappend())
*        for i := 1 to len(afield)
*           try
*             (calias)->(fieldput( (calias)->(fieldpos(aField[i])),aval[i]))
*           catch
*           end
*        next

    cSql := "select " + cfields + " from " + cTable  + " where "
      for each aTemp in aFields
      nPos := ascan( afield,{|x| upper(x) == upper(aTemp)})
      if nPos >0
         if ("TO_DATE(" in upper(aval[npos]))
            aval[nPos]:=substr(aval[npos],at("TO_DATE(",upper(aval[nPos]))+8)
            aval[npos] := strtran(aval[npos],"'",'')
            aval[npos] :=stod(aval[npos])
         endif

         if valtype(aval[npos]) == 'C'
            cSql += " " + aTemp  + " = " +  aVal[nPos]
         elseif  valtype(aval[npos]) == 'N' .or.  valtype(aval[npos]) == 'D'
            cSql += " " + aTemp  + " = " +  sr_cdbvalue(aVal[nPos])
         endif
      cSql += " AND "
      endif
   NEXT
   cSql := substr(cSql,1,len(csql)-4)
   else
      csql := "select " + cfields + " from "+ ctable + " where rownum <4  order by " + cDesc
   endif
   use ( csql ) new alias "INSSQLTMP" via "SQLRDD"
   if INSSQLTMP->(reccount())>0

      inssqltmp->(dbgotop())
      while !inssqltmp->(eof())
         cSqlTmp :=""
         i:=1
         for each aTemp in aFields
            nPos := ascan(adb,{|x| upper(x[1]) == aTemp})
            if nPos >0
               if inssqltmp->(fieldtype(i)) == "C"
                  cSqlTmp += inssqltmp->(fieldget(i))
               elseif inssqltmp->(fieldtype(i)) == "N"
                  if  adb[npos,4] > 0
                     cSqlTmp +=  str(inssqltmp->(fieldget(i)),adb[npos,3],adb[npos,4])
                  else
                  cSqlTmp +=  str(inssqltmp->(fieldget(i)),adb[npos,3])
                  endif
               elseif inssqltmp->(fieldtype(i)) == "D"
                   cSqlTmp += dtos(inssqltmp->(fieldget(i)))
               endif
            endif
            i++
         next

         *use (cSqlTmp) new Alias "INSSQLTMP2" via "ADSADT"

         if !(calias)->(dbseek(csqltmp))
               GETREFRESHCURINSVALUE('INSSQLTMP',ctable,calias)

         endif

*          endif
         INSSQLTMP->(dbskip())
      enddo
   endif
   INSSQLTMP->(dbclosearea())
   endif

//(calias)->(dbgoto((calias)->(lastrec())))
select(calias)
return nil


*        (calias)->(dbappend())
*        for i := 1 to len(afield)
*           try
*             (calias)->(fieldput( (calias)->(fieldpos(aField[i])),aval[i]))
*           catch
*           end
*        next





function createkeyfrompk(calias,ctable,lDescIndex)
Local aFields := GETPRIMARYKEY( cTable )
Local aTemp
local ckey:=""
Local i:=1
local aDb := (calias)->(dbstruct())
Local lnumtostr:=.f.
Local lDatetoStr :=.f.
local nPos
Local aTemp2:={}
default lDescIndex to .f.

if len(aFields) > 0
   if len(afields) == 1
      cKey := afields[1]
   else
      for each aTemp in afields
         nPos := ascan(adb,{|x| upper(x[1]) == aTemp})
         if nPos >0
            aadd(aTemp2,adb[npos,2])
         endif
      next

      for each aTemp in afields
         nPos := ascan(adb,{|x| upper(x[1]) == aTemp})
         if nPos >0
            if adb[npos,2] =="C"
               ckey +=  atemp + "+"
            elseif adb[npos,2] =="N"
               if adb[npos,4]>0
                  ckey += "str("+atemp+","+str(adb[npos,3])+","+str(adb[npos,4])+")+"
               else
                  ckey += "str("+atemp+","+str(adb[npos,3])+")+"
               endif
            elseif adb[npos,2] =="D"
               ckey +=  "dtos("+atemp + ")+"
            endif

         endif
      next
      ckey:=alltrim(ckey)
      if substr(ckey,-1,1)=='+'
         cKey := substr( cKey, 1, len( ckey )-1 )
      endif
   endif
   if lDescIndex
      index on &(ckey) TAG T0001 DESC
   else
      index on &(ckey) TAG T0001
   endif
   set order to 1
   go top
endif
return nil


FUNCTION __Skipped( nRecs, lAppend )

   LOCAL nSkipped := 0

   IF LastRec() != 0
      IF nRecs == 0
         IF EOF() .AND. !lAppend
            dbSkip( -1 )
            nSkipped := -1
         ELSE
            dbSkip( 0 )
         ENDIF
      ELSEIF nRecs > 0 .AND. RecNo() != LastRec() + 1
         DO WHILE nSkipped < nRecs
            dbSkip()
            IF Eof()
               IF lAppend
                  nSkipped++
               ELSE
                  dbSkip( -1 )
               ENDIF
               EXIT
            ENDIF
            nSkipped++
         ENDDO
      ELSEIF nRecs < 0
         DO WHILE nSkipped > nRecs
            dbSkip( -1 )
            IF Bof()
               EXIT
            ENDIF
            nSkipped--
         ENDDO
      ENDIF
   ENDIF

   RETURN nSkipped

   Function SR_WriteDbLog(cComm, oCnn)

   Local nAlAtual := Select()

   Local cPre :=hHashData[nAliasTmp]["cFile"]
   (oCnn) // To remove warning

   DEFAULT cComm to ""

   Try

      If !sr_phFile( cpre+"sqllog.dbf" )
         dbCreate(  cpre+"sqllog.dbf", TRACE_STRUCT, "DBFNTX" )
      EndIf

      While .T.
         dbUseArea( .T., "DBFNTX",  cpre+"sqllog.dbf", "SQLLOG", .T., .F. )
         If !NetErr()
            exit
         EndIf
         ThreadSleep( 500 )
      EndDo
      if ("INSERT" in upper(cComm))
      SQLLOG->( dbAppend() )
      Replace SQLLOG->DATA         with Date()
      Replace SQLLOG->HORA         with Time()
      Replace SQLLOG->COMANDO      with cComm
      Replace SQLLOg->PROCESSED      with .F.
      endif
      SQLLOG->( dbCloseArea() )

   Catch

   End

   dbSelectArea( nAlAtual )

Return NIL

function GetLastInsertCommand(cTable)
lOCAL CrET := ''
Local nAlAtual := Select()
Local cPre := hHashData[nAliasTmp]["cFile"]
      If !sr_phFile( cpre+"sqllog.dbf" )
         dbCreate(  cpre+"sqllog.dbf", TRACE_STRUCT, "DBFNTX" )
      EndIf
      While .T.
         dbUseArea( .T., "DBFNTX",  cpre+"sqllog.dbf", "SQLLOG", .T., .F. )
         If !NetErr()
            exit
         EndIf
         ThreadSleep( 500 )
      EndDo

      SQLLOG->( dbgobottom() )
      while !SQLLOG->( BOF() )
         IF !SQLLOG->PROCESSED .AND. (UPPER(CTABLE) IN UPPER(SQLLOG->COMANDO))

            CRET :=  SQLLOG->COMANDO
            SQLLOG->( RLOCK() )

            Replace SQLLOg->PROCESSED      with .t.
            SQLLOG->(DBUNLOCK())
            EXIT

         ENDIF
      SQLLOG->(DBSKIP(-1))
      ENDDO
      SQLLOG->( dbCloseArea() )
   dbSelectArea( nAlAtual )
   RETURN CRET