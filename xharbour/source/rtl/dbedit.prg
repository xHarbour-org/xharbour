/*
 * $Id: dbedit.prg,v 1.14 2003/12/09 23:08:20 maurifull Exp $
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

#translate SETIFEMPTY(<x>,<v>) => Do Case; Case Empty(<x>); <x>:=<v>; End
#translate SETIFNIL(<x>,<v>) => Do Case; Case HB_ISNIL(<x>); <x>:=<v>; End

Function DBEdit(nTop, nLeft, nBottom, nRight, aCols, xFunc, xPict, xHdr, xHSep, xCSep, xFSep, xFoot)
Local oTBR, oTBC, i, nRet := DE_REFRESH, nKey, bFun, nCrs

  SetIfEmpty(nTop, 0)
  SetIfEmpty(nLeft, 0)
  SetIfEmpty(nBottom, MaxRow())
  SetIfEmpty(nRight, MaxCol())

  If Empty(aCols)
    // If no database in use, do nothing
    If !Used()
      Return .F.
    End
    aCols := Array(FCount())
    For Each i In aCols
      i := FieldName(HB_EnumIndex())
    Next
    xHdr := aCols
  End
  SetIfEmpty(xHdr, "")
  SetIfEmpty(xFoot, "")
  // NOTE: Heading/footing separator is SINGLE line instead of DOUBLE line
  //       this is because most codepages (unicode too) don't have DOUBLE line chars
  //       so the output is ugly with them
  SetIfNil(xHSep, Chr(196) + Chr(194) + Chr(196))
  SetIfNil(xCSep, ' ' + Chr(179) + ' ')
  
  IIf(HB_ISNIL(xFSep) .And. !Empty(xFoot), xFSep := Chr(196) + Chr(193) + Chr(196), .T.)

  oTBR := TBrowseDB(nTop, nLeft, nBottom, nRight)
  If HB_ISSTRING(xHSep)
    oTBR:headSep := xHSep
  End
  If HB_ISSTRING(xFSep)
    oTBR:footSep := xFSep
  End
  If HB_ISSTRING(xCSep)
    oTBR:colSep := xCSep
  End

  nCrs := SetCursor(SC_NONE)
#ifdef HB_COMPAT_C53
  // EXTENSION: Move columns inside dbedit :)
  oTBR:setKey(K_CTRL_UP, {|| _MoveCol(oTBR, K_CTRL_UP), 0})
  oTBR:setKey(K_CTRL_DOWN, {|| _MoveCol(oTBR, K_CTRL_DOWN), 0})
#endif

  For Each i In aCols
    If HB_ISARRAY(i)
      bFun := IIf(HB_ISBLOCK(i[1]), i[1], &("{||" + i[1] + '}'))
    Else
      bFun := IIf(HB_ISBLOCK(i), i, &("{||" + i + '}'))
    End
    If ValType(Eval(bFun)) == 'M'  // HB_ISMEMO() returns .T. for strings :(
      bFun := {|| "  <Memo>  "}
    End

    If HB_ISARRAY(xHdr) .And. HB_ISNIL(xHdr[HB_EnumIndex()])  // handle empty column headers
      IIf(HB_ISSTRING(i), xHdr[HB_EnumIndex()] := i, "<block>")
    End

    oTBC := TBColumnNew(IIf(HB_ISSTRING(xHdr), xHdr, xHdr[HB_EnumIndex()]), bFun)

    If HB_ISARRAY(i)
      oTBC:colorBlock := i[2]
    End
    If HB_ISARRAY(xCSep)
      oTBC:colSep := xCSep[HB_EnumIndex()]
    End
    If HB_ISARRAY(xHSep)
      oTBC:headSep := xHSep[HB_EnumIndex()]
    End
    If HB_ISARRAY(xFSep)
      oTBC:footSep := xFSep[HB_EnumIndex()]
    End
    If HB_ISARRAY(xFoot)
      oTBC:footing := xFoot[HB_EnumIndex()]
    ElseIf HB_ISSTRING(xFoot)
      oTBC:footing := xFoot
    End
    If HB_ISARRAY(xPict)
      oTBC:picture := xPict[HB_EnumIndex()]
    ElseIf HB_ISSTRING(xPict)
      oTBC:picture := xPict
    End

    oTBR:addColumn(oTBC)
  Next

  IIf(Empty(xFunc), bFun := {|| IIf(Chr(LastKey()) $ Chr(K_ESC) + Chr(K_ENTER), DE_ABORT, DE_CONT)}, bFun := IIf(HB_ISBLOCK(xFunc), xFunc, &("{|x, y, z|" + xFunc + "(x,y,z)}")))

  // EXTENSION: Initialization call
  _DoUserFunc(bFun, -1, oTBR:colPos, oTBR)

  i := RecNo()
  Go Top
  If (Eof() .Or. RecNo() == LastRec() + 1) .And. Bof()
    nRet := _DoUserFunc(bFun, DE_EMPTY, oTBR:colPos, oTBR)
  End
  Go (i)

  If nRet != DE_ABORT
    nRet := DE_REFRESH
  End

  While nRet != DE_ABORT
    Switch nRet
      Case DE_REFRESH
        oTBR:refreshAll()
        oTBR:invalidate()
	Exit
      Case DE_CONT
        oTBR:refreshCurrent()
	Exit
    End
    oTBR:forceStable()
    oTBR:refreshCurrent()
    oTBR:stabilize()
    If oTBR:hitTop
      nRet := _DoUserFunc(bFun, DE_HITTOP, oTBR:colPos, oTBR)
    ElseIf oTBR:hitBottom
      nRet := _DoUserFunc(bFun, DE_HITBOTTOM, oTBR:colPos, oTBR)
    End
    If nRet == DE_ABORT
      Exit
    End
    nRet := _DoUserFunc(bFun, DE_IDLE, oTBR:colPos, oTBR)
    If nRet == DE_ABORT
      Exit
    End
    oTBR:hilite()
    nKey := Inkey(0)

#ifdef HB_COMPAT_C53
    // xHarbour with 5.3 extensions code
    If ValType(oTBR:SetKey(nKey)) == 'B'
      IIf(oTBR:applyKey(nKey) == -1, nRet := 0, .T.)
      Loop
    End
#endif

    If ValType(SetKey(nKey)) == 'B'
      Eval(SetKey(nKey), ProcName(1), ProcLine(1), "")
      Loop
    End

#ifdef HB_COMPAT_C53
    // got a key exception
    oTBR:refreshCurrent()
    oTBR:stabilize()
    oTBR:hilite()
    nRet := _DoUserFunc(bFun, DE_EXCEPT, oTBR:colPos, oTBR)
#else
    // xHarbour without 5.3 extensions code
    Switch nKey
      Case K_DOWN
        oTBR:down()
        Exit
      Case K_UP
        oTBR:up()
        Exit
      Case K_LEFT
        oTBR:left()
        Exit
      Case K_RIGHT
        oTBR:right()
        Exit
      Case K_PGDN
        oTBR:pageDown()
        Exit
      Case K_PGUP
        oTBR:pageUp()
        Exit
      Case K_CTRL_PGUP
        oTBR:goTop()
        Exit
      Case K_CTRL_PGDN
        oTBR:goBottom()
        Exit
      Case K_HOME
        oTBR:home()
        Exit
      Case K_END
        oTBR:end()
        Exit
      Case K_CTRL_HOME
        oTBR:panHome()
        Exit
      Case K_CTRL_END
        oTBR:panEnd()
        Exit
      Case K_CTRL_LEFT
        oTBR:panLeft()
        Exit
      Case K_CTRL_RIGHT
        oTBR:panRight()
        Exit
      // EXTENSION: Move columns inside dbedit :)
      Case K_CTRL_UP
      Case K_CTRL_DOWN
        _MoveCol(oTBR, nKey)
        Exit
      Default
       // got a key exception
       oTBR:refreshCurrent()
       oTBR:stabilize()
       oTBR:hilite()
       nRet := _DoUserFunc(bFun, DE_EXCEPT, oTBR:colPos, oTBR)
    End
#endif

    // userfunc could delete recs...
    i := RecNo()
    Go Top
    If (Eof() .Or. RecNo() == LastRec() + 1) .And. Bof()
      nRet := _DoUserFunc(bFun, DE_EMPTY, oTBR:colPos, oTBR)
    End
    Go (i)
  End

  SetCursor(nCrs)
Return .T.

Static Function _MoveCol(oTBR, nKey)
Local oTB1, oTB2

  If nKey == K_CTRL_DOWN .And. oTBR:colPos < oTBR:colCount
    oTB1 := oTBR:getColumn(oTBR:colPos)
    oTB2 := oTBR:getColumn(oTBR:colPos + 1)
    oTBR:setColumn(oTBR:colPos, oTB2)
    oTBR:SetColumn(oTBR:colPos + 1, oTB1)
    oTBR:colPos++
    oTBR:invalidate()
  ElseIf nKey == K_CTRL_UP .And. oTBR:colPos > 1
    oTB1 := oTBR:getColumn(oTBR:colPos)
    oTB2 := oTBR:getColumn(oTBR:colPos - 1)
    oTBR:setColumn(oTBR:colPos, oTB2)
    oTBR:SetColumn(oTBR:colPos - 1, oTB1)
    oTBR:colPos--
    oTBR:invalidate()
  End
Return Nil

Static Function _DoUserFunc(bFun, nMode, nColPos, oTBR)
Local nRet, nRec := RecNo()

  nRet := Eval(bFun, nMode, nColPos, oTBR)
 
  If RecNo() != nRec .And. nRet != DE_ABORT
    nRet := DE_REFRESH
  End
  If !HB_ISNUMERIC(nRet) .Or. nRet < 0 .Or. nRet > 2
     nRet := DE_CONT
  End
Return nRet
