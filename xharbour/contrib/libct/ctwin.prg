********************************************************************
*   Library   CTWIN  ver 0.92                                      *
*   Emulation off windows functions from CA Tolls                  *
*   write by Adam Lubszczyk    alubszcz@rsw.pl                     *
********************************************************************
/*
 * $Id: ctwin.prg,v 1.4 2003/08/27 08:58:20 druzus Exp $
 */

/*
 * Harbour Project source code:
 *   CT3 Windows Like functions
 *     Copyright 2002-2003 Adam Lubszczyk <alubszcz@rsw.pl>
 *	 
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

#include "hbclass.ch"
#include "set.ch"

STATIC ctw_WINDOWS := {}
STATIC ctw_CURRENT := 0
STATIC ctw_STOS    := {}
STATIC ctw_0ROW    := 0
STATIC ctw_0COL    := 0
STATIC ctw_0COLOR
STATIC ctw_0CURSOR
STATIC ctw_BOARDT  := 0
STATIC ctw_BOARDL  := 0
STATIC ctw_BOARDB  := 0
STATIC ctw_BOARDR  := 0
STATIC ctw_MODET   := .F.
STATIC ctw_MODEL   := .F.
STATIC ctw_MODEB   := .F.
STATIC ctw_MODER   := .F.
STATIC ctw_CANMOVE := .T.
STATIC ctw_SHADOWATTR := -1
STATIC ctw_WBOX_Type := {;
            "…Õª∫ºÕ»∫ ",;   // 0      WB_DOUBLE_CLEAR
            "⁄ƒø≥Ÿƒ¿≥ ",;   // 1      WB_SINGLE_CLEAR
            "’Õ∏≥æÕ‘≥ ",;   // 2      WB_DOUBLE_SINGLE_CLEAR
            "÷ƒ∑∫Ωƒ”∫ ",;   // 3      WB_SINGLE_DOUBLE_CLEAR
            "…Õª∫ºÕ»∫" ,;   // 4      WB_DOUBLE
            "⁄ƒø≥Ÿƒ¿≥" ,;   // 5      WB_SINGLE
            "’Õ∏≥æÕ‘≥" ,;   // 6      WB_DOUBLE_SINGLE
            "÷ƒ∑∫Ωƒ”∫" ,;   // 7      WB_SINGLE_DOUBLE
            "€ﬂ€€€‹€€ ",;   // 8      WB_HALF_FULL_CLEAR
            "ﬁﬂ›››‹ﬁﬁ ",;   // 9      WB_HALF_CLEAR
            "ﬁ€›››€ﬁﬁ ",;   // 10      WB_FULL_HALF_CLEAR
            "€€€€€€€€ ",;   // 11      WB_FULL_CLEAR
            "€ﬂ€€€‹€€" ,;   // 12      WB_HALF_FULL
            "ﬁﬂ›››‹ﬁﬁ" ,;   // 13      WB_HALF
            "ﬁ€›››€ﬁﬁ" ,;   // 14      WB_FULL_HALF
            "€€€€€€€€" }   // 15      WB_FULL


********************************************************************
********************************************************************
********************************************************************
FUNCTION ctw_SCROLL(nT,nL,nB,nR,nV,nH)
 IF ctw_CURRENT == 0
   SCROLL(nT,nL,nB,nR,nV,nH)
 ELSE
   ctw_WINDOWS[ctw_CURRENT]:Scroll(nT,nL,nB,nR,nV,nH)
 ENDIF
RETURN NIL
********************************************

FUNCTION ctw_COL()
 LOCAL nRet
 IF ctw_CURRENT == 0
   nRet := COL()
 ELSE
   nRet := ctw_WINDOWS[ctw_CURRENT]:Col()
 ENDIF
RETURN nRet
********************************************
FUNCTION ctw_ROW()
 LOCAL nRet
 IF ctw_CURRENT == 0
   nRet := ROW()
 ELSE
   nRet := ctw_WINDOWS[ctw_CURRENT]:Row()
 ENDIF
RETURN nRet
********************************************
FUNCTION ctw_MAXCOL()
 LOCAL nRet
 IF ctw_CURRENT == 0
   nRet := MAXCOL()
 ELSE
   nRet := ctw_WINDOWS[ctw_CURRENT]:MaxCol()
 ENDIF
RETURN nRet
********************************************
FUNCTION ctw_MAXROW()
 LOCAL nRet
 IF ctw_CURRENT == 0
   nRet := MAXROW()
 ELSE
   nRet := ctw_WINDOWS[ctw_CURRENT]:MaxRow()
 ENDIF
RETURN nRet
********************************************
FUNCTION ctw_DISPOUT(xVal,xColor)
 IF ctw_CURRENT == 0
   DISPOUT(xVal,xColor)
 ELSE
   ctw_WINDOWS[ctw_CURRENT]:DispOut(xVal,xColor)
 ENDIF
RETURN NIL
********************************************
FUNCTION ctw_DISPOUTAT(nT,nL,xVal,xColor)
 IF ctw_CURRENT == 0
   DISPOUTAT(nT,nL,xVal,xColor)
 ELSE
   ctw_WINDOWS[ctw_CURRENT]:DispOutAT(nT,nL,xVal,xColor)
 ENDIF
RETURN NIL
********************************************
FUNCTION ctw_DEVOUT(xVal,xColor,nT,nL)
 IF ctw_CURRENT == 0
   DEVOUT(xVal,xColor,nT,nL)
 ELSE
   IF VALTYPE(nT)=="N" .AND. VALTYPE(nL)=="N"
     ctw_WINDOWS[ctw_CURRENT]:DevPos(nT,nL)
   ENDIF
   ctw_WINDOWS[ctw_CURRENT]:DevOut(xVal,xColor)
 ENDIF
RETURN NIL
********************************************
FUNCTION ctw_DEVOUTPICT(xVal,cPict,xColor,nT,nL)
 IF ctw_CURRENT == 0
   DEVOUTPICT(xVal,cPict,xColor,nT,nL)
 ELSE
   IF VALTYPE(nT)=="N" .AND. VALTYPE(nL)=="N"
     ctw_WINDOWS[ctw_CURRENT]:DevPos(nT,nL)
   ENDIF
   ctw_WINDOWS[ctw_CURRENT]:DevOutPict(xVal,cPict,xColor)
 ENDIF
RETURN NIL
********************************************
FUNCTION ctw_SETPOS(nT,nL)
 IF ctw_CURRENT == 0
   SETPOS(nT,nL)
 ELSE
   ctw_WINDOWS[ctw_CURRENT]:SetPos(nT,nL)
 ENDIF
RETURN NIL
********************************************
FUNCTION ctw_DEVPOS(nT,nL)
 IF ctw_CURRENT == 0
   DEVPOS(nT,nL)
 ELSE
   ctw_WINDOWS[ctw_CURRENT]:DevPos(nT,nL)
 ENDIF
RETURN NIL
********************************************
FUNCTION ctw_DISPBOX(nT,nL,nB,nR,cnStr,cColor)
 IF ctw_CURRENT == 0
   DISPBOX(nT,nL,nB,nR,cnStr,cColor)
 ELSE
   ctw_WINDOWS[ctw_CURRENT]:DispBox(nT,nL,nB,nR,cnStr,cColor)
 ENDIF
RETURN NIL
********************************************

FUNCTION ctw___AtPrompt(nT,nL,cItem,cMess)
 IF ctw_CURRENT == 0
   __AtPrompt(nT,nL,cItem,cMess)
 ELSE
   ctw_WINDOWS[ctw_CURRENT]:__AtPrompt(nT,nL,cItem,cMess)
 ENDIF
RETURN NIL
********************************************
FUNCTION ctw_ACHOICE(nT,nL,nB,nR,aMenu,aSel,cUser,nInit,nWRow)
 LOCAL nRet
 IF ctw_CURRENT == 0
   nRet := ACHOICE(nT,nL,nB,nR,aMenu,aSel,cUser,nInit,nWRow)
 ELSE
   nRet := ctw_WINDOWS[ctw_CURRENT]:AChoice(nT,nL,nB,nR,aMenu,aSel,cUser,nInit,nWRow)
 ENDIF
RETURN nRet
******************************************
FUNCTION ctw_QQOUT(...)
 LOCAL nPar := PCOUNT()
 LOCAL aPar, i
 LOCAL oW
 IF nPar == 0
    RETURN NIL
 ENDIF
// DISPBEGIN()
 aPar := HB_APARAMS()
 IF ctw_CURRENT == 0
    QQOUT(aPar[1])
    FOR i:= 2 TO LEN(aPar)
       QQOUT("",aPar[i])
    NEXT
 ELSE
    oW:=ctw_WINDOWS[ctw_CURRENT]
    oW:QQout(aPar[1])
    FOR i:= 2 TO LEN(aPar)
       oW:QQout(" ")
       oW:QQout(aPar[i])
    NEXT
 ENDIF
// DISPEND()
RETURN NIL
********************************************************************
FUNCTION ctw_QOUT(...)
 LOCAL nPar := PCOUNT()
 LOCAL aPar, i
 LOCAL oW
// DISPBEGIN()
 IF nPar == 0
    IF ctw_CURRENT == 0
       QOUT()
    ELSE
       ctw_WINDOWS[ctw_CURRENT]:ct__CR()
       ctw_WINDOWS[ctw_CURRENT]:ct__LF()
    ENDIF
//    DISPEND()
    RETURN NIL
 ENDIF
 aPar := HB_APARAMS()
 IF ctw_CURRENT == 0
    QOUT(aPar[1])
    FOR i:= 2 TO LEN(aPar)
       QQOUT("",aPar[i])
    NEXT
 ELSE
    oW:=ctw_WINDOWS[ctw_CURRENT]
    oW:Qout(aPar[1])
    FOR i:= 2 TO LEN(aPar)
       oW:QQout(" ")
       oW:QQout(aPar[i])
    NEXT
 ENDIF
// DISPEND()
RETURN NIL
*********************************
FUNCTION ctw_TBROWSENEW(nT,nL,nB,nR)
 IF ctw_CURRENT == 0
    RETURN TBROWSENEW(nT,nL,nB,nR)
 ENDIF
RETURN ctw_WINDOWS[ctw_CURRENT]:TBrowse(nT,nL,nB,nR,.F.)
*********************************
FUNCTION ctw_TBROWSEDB(nT,nL,nB,nR)
 IF ctw_CURRENT == 0
    RETURN TBROWSEDB(nT,nL,nB,nR)
 ENDIF
RETURN ctw_WINDOWS[ctw_CURRENT]:TBrowse(nT,nL,nB,nR,.T.)
*********************************
FUNCTION WMOVE(nT,nL)
  IF ctw_CURRENT != 0
    ctw_WINDOWS[ctw_CURRENT]:Move(nT,nL)
  ENDIF
RETURN ctw_CURRENT
*********************************
FUNCTION WSETMOVE(lMode)
 LOCAL lOldMode := ctw_CANMOVE
  IF VALTYPE(lMode) == "L"
    ctw_CANMOVE := lMode
  ENDIF
RETURN lOldMode
*********************************
FUNCTION WSETSHADOW(xAttr)
 LOCAL lOldAttr := ctw_SHADOWATTR
  IF !EMPTY(xAttr) .AND. ;
     ( VALTYPE(xAttr) == "C" .OR. ( VALTYPE(xAttr) == "N" .AND. xAttr == -1 ) )
    ctw_SHADOWATTR := xAttr
  ENDIF
RETURN lOldAttr
*********************************
FUNCTION WMODE(lT,lL,lB,lR)
  IF lT != NIL
     IF VALTYPE(lT) == "L"
        ctw_MODET := lT
     ENDIF
  ENDIF
  IF lL != NIL
     IF VALTYPE(lL) == "L"
        ctw_MODEL := lL
     ENDIF
  ENDIF
  IF lB != NIL
     IF VALTYPE(lB) == "L"
        ctw_MODEB := lB
     ENDIF
  ENDIF
  IF lR != NIL
     IF VALTYPE(lR) == "L"
        ctw_MODER := lR
     ENDIF
  ENDIF
RETURN 0
*********************************
FUNCTION WBOARD(nT,nL,nB,nR)
 LOCAL MaxC:=MAXCOL()
 LOCAL MaxR:=MAXROW()
  IF nT == NIL
     nT := 0
  ELSEIF nT < 0
     nT := 0
  ENDIF
  IF nL == NIL
     nL := 0
  ELSEIF nL < 0
     nL := 0
  ENDIF
  IF nB == NIL
     nB := MaxR
  ELSEIF nB > MaxR
     nB := MaxR
  ENDIF
  IF nR == NIL
     nR := MaxC
  ELSEIF nR > MaxC
     nR := MaxC
  ENDIF
  IF nT > nB .or. nL > nR
     RETURN -1        //error
  ENDIF
  ctw_BOARDT := nT
  ctw_BOARDL := nL
  ctw_BOARDB := nB
  ctw_BOARDR := nR
RETURN 0
*********************************
FUNCTION WOPEN(nTop,nLeft,nBottom,nRight,lCls)
  LOCAL oWin
  LOCAL nLen:=LEN(ctw_WINDOWS)
  LOCAL nNum
  IF lCls==NIL
    lCls := .F.
  ENDIF
  IF ctw_BOARDB == 0               //nie zainicjowano jeszcze
     ctw_BOARDB := MAXROW()
     ctw_BOARDR := MAXCOL()
  ENDIF
  FOR nNum := 1 TO nLen
     IF ctw_WINDOWS[nNum] == NIL       //puste miejsce
        EXIT
     ENDIF
  NEXT
  IF ctw_CURRENT > 0
     ctw_WINDOWS[ctw_CURRENT]:Save()
  ELSE
     ctw_0ROW := ROW()
     ctw_0COL := COL()
     ctw_0COLOR := SETCOLOR()
     ctw_0CURSOR := SETCURSOR()
  ENDIF
  oWin:=TctWIN():NEW(nTop,nLeft,nBottom,nRight)
  IF nNum > nLen
     AADD(ctw_WINDOWS,oWin)
  ELSE
     ctw_WINDOWS[nNum] := oWin
  ENDIF
  AADD(ctw_STOS,nNum)
  ctw_CURRENT := nNum
  IF lCls
    ctw_WINDOWS[nNum]:Scroll()
    ctw_WINDOWS[nNum]:SetPos(0,0)
  ENDIF
RETURN nNum

********************************************************************
FUNCTION WCENTER(lCen)
 LOCAL xT,xL
 IF lCen == NIL
    lCen := .F.
 ENDIF
 IF lCen .AND. ctw_CURRENT != 0
    xL := INT((ctw_BOARDR - ctw_BOARDL - ctw_WINDOWS[ctw_CURRENT]:PosR + ctw_WINDOWS[ctw_CURRENT]:PosL ) / 2)
    xT := INT((ctw_BOARDB - ctw_BOARDT - ctw_WINDOWS[ctw_CURRENT]:PosB + ctw_WINDOWS[ctw_CURRENT]:PosT ) / 2)
    ctw_WINDOWS[ctw_CURRENT]:move(xT,xL)
 ENDIF
 //jesli lCen==.F.   to chyba nic sie nie dzieje
RETURN ctw_CURRENT
********************************************************************
FUNCTION WCOL(lCen)
  IF lCen == NIL
    lCen := .F.
  ENDIF
  IF ctw_CURRENT == 0
     RETURN 0
  ENDIF
  IF !lCen
     RETURN ctw_WINDOWS[ctw_CURRENT]:PosL
  ENDIF
RETURN INT((ctw_BOARDR - ctw_BOARDL - ctw_WINDOWS[ctw_CURRENT]:PosR + ctw_WINDOWS[ctw_CURRENT]:PosL ) / 2)
*********************************
FUNCTION WLASTCOL(lCen)
  LOCAL xW
  IF lCen == NIL
    lCen := .F.
  ENDIF
  IF ctw_CURRENT == 0
     RETURN MAXCOL()
  ENDIF
  IF !lCen
     RETURN ctw_WINDOWS[ctw_CURRENT]:PosR
  ENDIF
  xW := ctw_WINDOWS[ctw_CURRENT]:PosR - ctw_WINDOWS[ctw_CURRENT]:PosL
RETURN INT((ctw_BOARDR - ctw_BOARDL - xW ) / 2) + xW
*********************************
FUNCTION WROW(lCen)
  IF lCen == NIL
    lCen := .F.
  ENDIF
  IF ctw_CURRENT == 0
     RETURN 0
  ENDIF
  IF !lCen
     RETURN ctw_WINDOWS[ctw_CURRENT]:PosT
  ENDIF
RETURN INT((ctw_BOARDB - ctw_BOARDT - ctw_WINDOWS[ctw_CURRENT]:PosB + ctw_WINDOWS[ctw_CURRENT]:PosT ) / 2)
*********************************
FUNCTION WLASTROW(lCen)
  LOCAL xW
  IF lCen == NIL
    lCen := .F.
  ENDIF
  IF ctw_CURRENT == 0
     RETURN MAXROW()
  ENDIF
  IF !lCen
     RETURN ctw_WINDOWS[ctw_CURRENT]:PosB
  ENDIF
  xW := ctw_WINDOWS[ctw_CURRENT]:PosB - ctw_WINDOWS[ctw_CURRENT]:PosT
RETURN INT((ctw_BOARDB - ctw_BOARDT - xW ) / 2) + xW
*********************************
FUNCTION WNUM

RETURN LEN(ctw_WINDOWS)
*********************************
FUNCTION WFCOL(lRel)
  IF lRel == NIL
    lRel := .F.
  ENDIF
  IF ctw_CURRENT == 0
//    IF !lRel
      RETURN ctw_BOARDL
//    ELSE
//      RETURN ctw_BOARDL - 0
//    ENDIF
  ENDIF
  IF !lRel
    RETURN ctw_WINDOWS[ctw_CURRENT]:UsedL
  ENDIF
RETURN  ctw_WINDOWS[ctw_CURRENT]:UsedL - ctw_WINDOWS[ctw_CURRENT]:PosL
*********************************
FUNCTION WFLASTCOL(lRel)
  IF lRel == NIL
    lRel := .F.
  ENDIF
  IF ctw_CURRENT == 0
    IF !lRel
      RETURN ctw_BOARDR
    ELSE
      RETURN MAXCOL() - ctw_BOARDR
    ENDIF
  ENDIF
  IF !lRel
    RETURN ctw_WINDOWS[ctw_CURRENT]:UsedR
  ENDIF
RETURN  ctw_WINDOWS[ctw_CURRENT]:PosR - ctw_WINDOWS[ctw_CURRENT]:UsedR
*********************************
FUNCTION WFROW(lRel)
  IF lRel == NIL
    lRel := .F.
  ENDIF
  IF ctw_CURRENT == 0
//    IF !lRel
      RETURN ctw_BOARDT
//    ELSE
//      RETURN ctw_BOARDT - 0
//    ENDIF
  ENDIF
  IF !lRel
    RETURN ctw_WINDOWS[ctw_CURRENT]:UsedT
  ENDIF
RETURN  ctw_WINDOWS[ctw_CURRENT]:UsedT - ctw_WINDOWS[ctw_CURRENT]:PosT
*********************************
FUNCTION WFLASTROW(lRel)
  IF lRel == NIL
    lRel := .F.
  ENDIF
  IF ctw_CURRENT == 0
    IF !lRel
      RETURN ctw_BOARDB
    ELSE
      RETURN MAXROW() - ctw_BOARDB
    ENDIF
  ENDIF
  IF !lRel
    RETURN ctw_WINDOWS[ctw_CURRENT]:UsedB
  ENDIF
RETURN  ctw_WINDOWS[ctw_CURRENT]:PosB - ctw_WINDOWS[ctw_CURRENT]:UsedB
*********************************
FUNCTION WBOX(ncType)
  IF ctw_CURRENT > 0
     DISPBEGIN()
     IF ctw_WINDOWS[ctw_CURRENT]:WBox(ncType)
        DISPEND()
        RETURN ctw_CURRENT
     ELSE
        DISPEND()
        RETURN -1
     ENDIF
  ENDIF
RETURN -1
********************************************************************
FUNCTION WSELECT(nSelect)
  LOCAL nRet := ctw_CURRENT
  IF nSelect == NIL
     RETURN nRet
  ENDIF
  IF VALTYPE(nSelect) != "N"
     RETURN -1
  ENDIF
  IF nSelect < 0 .or. nSelect > LEN(ctw_WINDOWS)
     RETURN -1
  ENDIF
  IF nSelect == nRet             //ten sam co aktywny
     RETURN nRet
  ENDIF
  IF nSelect == 0                 //SCREEN
     ctw_WINDOWS[nRet]:Save()
     ctw_CURRENT := 0
     SETPOS(ctw_0ROW,ctw_0COL)
     SETCOLOR(ctw_0COLOR)
     SETCURSOR(ctw_0CURSOR)
  ELSE  //nSelect == 0
    IF VALTYPE(ctw_WINDOWS[nSelect]) == "U"  //NIL
       RETURN -1
    ELSE
       DISPBEGIN()
       IF nRet == 0
           ctw_0ROW := ROW()
           ctw_0COL := COL()
           ctw_0COLOR := SETCOLOR()
           ctw_0CURSOR := SETCURSOR()
        ELSE
           ctw_WINDOWS[nRet]:Save()
        ENDIF  //nRet == 0
        ToFront(nSelect)
        ctw_CURRENT:=nSelect
        DISPEND()
    ENDIF //VALTYPE(ctw_WINDOWS[nSelect]) == "U"
  ENDIF //nSelect == 0
RETURN nRet
****************************************************
FUNCTION WFORMAT(nT,nL,nB,nR)
 IF ctw_CURRENT == 0
    RETURN 0    //lub -1 error ???
 ELSE
    ctw_WINDOWS[ctw_CURRENT]:WFormat(nT,nL,nB,nR)
 ENDIF
RETURN ctw_CURRENT
********************************************
FUNCTION WACLOSE()
 LOCAL i
  IF LEN(ctw_WINDOWS) == 0
     RETURN -1
  ENDIF
  DISPBEGIN()
  FOR i:= LEN(ctw_STOS) TO 1 STEP -1
    ctw_WINDOWS[ctw_STOS[i]]:RestoreBG()
    ctw_WINDOWS[ctw_STOS[i]]:Free()
    ctw_WINDOWS[ctw_STOS[i]]:=NIL
    ctw_STOS[i]:=NIL
  NEXT
  DISPEND()
  ASIZE(ctw_WINDOWS,0)
  ASIZE(ctw_STOS,0)
  ctw_CURRENT := 0
  SETPOS(ctw_0ROW,ctw_0COL)
  SETCOLOR(ctw_0COLOR)
  SETCURSOR(ctw_0CURSOR)
RETURN 0
********************************************
FUNCTION WCLOSE()
  LOCAL nLenW,nLenS
  LOCAL nRet
  IF ctw_CURRENT == 0
     RETURN -1
  ENDIF
  ctw_WINDOWS[ctw_CURRENT]:RestoreBG()
  nLenW:=LEN(ctw_WINDOWS)
  IF ctw_CURRENT == nLenW
     ctw_WINDOWS[nLenW]:Free()
     ASIZE(ctw_WINDOWS,--nLenW)
  ELSE
    ctw_WINDOWS[ctw_CURRENT]:Free()
    ctw_WINDOWS[ctw_CURRENT] := NIL
  ENDIF
  nLenS:=LEN(ctw_STOS)
  ASIZE(ctw_STOS,--nLenS)
  IF nLenS == 0
     ctw_CURRENT := 0
     SETPOS(ctw_0ROW,ctw_0COL)
     SETCOLOR(ctw_0COLOR)
     SETCURSOR(ctw_0CURSOR)
     RETURN 0
  ELSE
     nRet := ctw_STOS[nLenS]
     ctw_WINDOWS[nRet]:Restore()
     ctw_CURRENT := nRet
  ENDIF
RETURN nRet

****************************************************
****************************************************
STATIC FUNCTION ToFront(nW)
  LOCAL poz := ASCAN(ctw_STOS,nW)
  LOCAL nLen:=LEN(ctw_STOS)
  LOCAL i
  IF poz == nLen                           //on TOP
     ctw_WINDOWS[nW]:Restore()
     RETURN NIL
  ENDIF
  FOR i:= nLen TO poz STEP -1
      ctw_WINDOWS[ctw_STOS[i]]:RestoreBG()
  NEXT
  ADEL(ctw_STOS,poz)                     // move
  ctw_STOS[nLen]:=nW                     //    to TOP
  FOR i:=poz TO nLen - 1
      ctw_WINDOWS[ctw_STOS[i]]:SaveBG()
      ctw_WINDOWS[ctw_STOS[i]]:RestoreFG()
  NEXT
  ctw_WINDOWS[nW]:SaveBG()
  ctw_WINDOWS[nW]:Restore()
RETURN NIL
****************************************************
****************************************************
****************************************************
CLASS TctWIN
 DATA PosT
 DATA PosL
 DATA PosB
 DATA PosR
 DATA UsedT
 DATA UsedL
 DATA UsedB
 DATA UsedR
 DATA cBackground AS STRING
 DATA cSaveData AS STRING
 DATA nRow AS NUMERIC
 DATA nCol AS NUMERIC
 DATA nCursor AS NUMERIC
 DATA cColor AS STRING

 METHOD New(nT,nL,nB,nR) CONSTRUCTOR
 METHOD Save()
 METHOD Restore()
 METHOD Move(nT,nL)
 METHOD WBox(ncType)
 METHOD WFormat(nT,nL,nB,nR)
 METHOD SetPos(nT,nL)
 METHOD DevPos(nT,nL)
 METHOD Row()
 METHOD Col()
 METHOD MaxRow()
 METHOD MaxCol()
 METHOD Scroll(nT,nL,nB,nR,nV,nH)
 METHOD DispOut(xVal,xColor)
 METHOD DispOutAT(nT,nL,xVal,xColor)
 METHOD DispBox(nT,nL,nB,nR,cnStr,cColor)
 METHOD DevOut(xVal,xColor)
 METHOD DevOutPict(xVal,cPict,xColor)
 METHOD __AtPrompt(nT,nL,cItem,cMess)
 METHOD QQout(xVal)
 METHOD Qout(xVal)
 METHOD ct__CR()
 METHOD ct__LF()
 METHOD RestoreBG()
 METHOD SaveBG()
 METHOD RestoreFG()
 METHOD SaveFG()
 METHOD Free()
 METHOD AChoice(nT,nL,nB,nR,aMenu,aSel,cUser,nInit,nWRow)
 METHOD TBrowse(nT,nL,nB,nR,lDB)
ENDCLASS

*********************************************
METHOD TBrowse(nT,nL,nB,nR,lDB) CLASS TctWIN
   IF lDB == NIL
      lDB := .T.
   ENDIF
   IF nT == NIL
     nT := ::UsedT
   ELSEIF nT < 0
     nT := ::UsedT
   ELSE
     nT := nT + ::UsedT
     IF nT > ::UsedB
        nT := ::UsedT
     ENDIF
   ENDIF
   IF nL == NIL
     nL := ::UsedL
   ELSEIF nL < 0
     nL := ::UsedL
   ELSE
     nL := nL + ::UsedL
     IF nL > ::UsedR
        nL := ::UsedL
     ENDIF
   ENDIF
   IF nB == NIL
     nB := ::UsedB
   ELSEIF nB < 0
     nB := ::UsedB
   ELSE
     nB := nB + ::UsedT
     IF nB > ::UsedB
        nB := ::UsedB
     ENDIF
   ENDIF
   IF nR == NIL
     nR := ::UsedR
   ELSEIF nR < 0
     nR := ::UsedR
   ELSE
     nR := nR + ::UsedL
     IF nR > ::UsedR
        nR := ::UsedR
     ENDIF
   ENDIF
   IF lDB
      RETURN TBrowseDB(nT,nL,nB,nR)
   ENDIF
RETURN TBrowseNew(nT,nL,nB,nR)
*********************************************
METHOD AChoice(nT,nL,nB,nR,aMenu,aSel,cUser,nInit,nWRow) CLASS TctWIN
  IF nT == NIL
    nT := ::UsedT
  ELSE
    nT := nT + ::UsedT
    IF nT > ::UsedB
       nT := ::UsedB
    ENDIF
  ENDIF
  IF nL == NIL
    nL := ::UsedL
  ELSE
    nL := nL + ::UsedL
    IF nL > ::UsedR
       nL := ::UsedR
    ENDIF
  ENDIF
  IF nB == NIL
    nB := nT
  ELSE
    nB := nB + ::UsedT
    IF nB > ::UsedB
       nB := ::UsedB
    ENDIF
  ENDIF
  IF nR == NIL
    nR := nL
  ELSE
    nR := nR + ::UsedL
    IF nR > ::UsedR
       nR := ::UsedR
    ENDIF
  ENDIF
RETURN ACHOICE(nT,nL,nB,nR,aMenu,aSel,cUser,nInit,nWRow)
*********************************************
METHOD ct__CR() CLASS TctWIN
   SETPOS(ROW(),::UsedL)
RETURN NIL
**********
METHOD ct__LF() CLASS TctWIN
   LOCAL nRow := ROW()
   LOCAL nCol := COL()
   IF nRow == ::UsedB
      ::Scroll(,,,,1)
      SETPOS(::UsedB,nCol)
   ELSE
      SETPOS(++nRow,nCol)
   ENDIF
RETURN NIL
**********
METHOD QQout(xVal) CLASS TctWIN
 LOCAL cCR := CHR(13) , cLF:=CHR(10)
 LOCAL cVal := HB_VALTOSTR(xVal)
 LOCAL nLen := LEN(cVAL)
 LOCAL pCR := AT( cCR ,cVAL)
 LOCAL pLF := AT( cLF ,cVAL)
 LOCAL pp
 LOCAL nRest := ::UsedR - COL() + 1
 LOCAL cCode

 IF pCR == 0 .and. pLF == 0
    pp := 0
 ELSEIF pCR == 0 .or. pLF == 0
    pp := MAX(pCR,pLF)
 ELSE
    pp := MIN(pCR,pLF)
 ENDIF

 DO WHILE nLen > 0
   IF pp == 0
      IF nLen > nRest
         QQOUT(LEFT(cVal,nRest))
         ::ct__CR()
         ::ct__LF()
         cVal := SUBSTR(cVal,nRest +1)
         nLen := LEN(cVal)
         nRest := ::UsedR - ::UsedL + 1
      ELSE
        QQOUT(cVal)
        nLen := 0
      ENDIF  //nLen > nRest
   ELSE //pp == 0
      ::QQout(SUBSTR(cVal,1,pp-1))    //rekurencja :)
      cCode := SUBSTR(cVal,pp,1)
      cVal := SUBSTR(cVal,pp+1)
      IF cCode == cCR
         ::ct__CR()
      ELSE
         ::ct__LF()
      ENDIF //cCode == cCR
      nLen := LEN(cVal)
      pCR := AT( cCR ,cVal)
      pLF := AT( cLF ,cVal)
      IF pCR == 0 .and. pLF == 0
         pp := 0
      ELSEIF pCR == 0 .or. pLF == 0
         pp := MAX(pCR,pLF)
      ELSE
         pp := MIN(pCR,pLF)
      ENDIF
      nRest := ::UsedR - COL() + 1
   ENDIF  //pp == 0
 ENDDO    //nLen > 0

RETURN NIL
**************
METHOD Qout(xVal) CLASS TctWIN
  ::ct__CR()
  ::ct__LF()
  ::QQout(xVal)
RETURN NIL
****************
METHOD New(nT,nL,nB,nR) CLASS TctWIN
 LOCAL lMax:=.F.
 IF nT==NIL .or. nL==NIL .or. nB==NIL .or. nR==NIL
    lMax:=.T.
 ENDIF
 IF nT > nB .or. nL > nR
    lMax:=.T.
 ENDIF
 IF lMax
   nT:=ctw_BOARDT
   nL:=ctw_BOARDL
   nB:=ctw_BOARDB
   nR:=ctw_BOARDR
 ENDIF
 IF nT < ctw_BOARDT
    nT := ctw_BOARDT
 ENDIF
 IF nL < ctw_BOARDL
    nL := ctw_BOARDL
 ENDIF
 IF nB > ctw_BOARDB
    nB := ctw_BOARDB
 ENDIF
 IF nR > ctw_BOARDR
    nR := ctw_BOARDR
 ENDIF
 ::PosT := nT
 ::PosL := nL
 ::PosB := nB
 ::PosR := nR
 ::UsedT := nT
 ::UsedL := nL
 ::UsedB := nB
 ::UsedR := nR
 ::SaveBG()
 ::cBackground := SAVESCREEN(nT,nL,nB,nR)
 ::cSaveData := ::cBackground
 ::nRow := nT
 ::nCol := nL
 ::nCursor := SETCURSOR()
 ::cColor := SETCOLOR()
RETURN Self

***********************
METHOD Free() CLASS TctWIN
 ::PosT := NIL
 ::PosL := NIL
 ::PosB := NIL
 ::PosR := NIL
 ::UsedT := NIL
 ::UsedL := NIL
 ::UsedB := NIL
 ::UsedR := NIL
 ::cBackground := NIL
 ::cSaveData := NIL
 ::nRow := NIL
 ::nCol := NIL
 ::nCursor := NIL
 ::cColor := NIL

RETURN NIL

**********
METHOD __AtPrompt(nT,nL,cItem,cMess) CLASS TctWIN
 LOCAL nLen := LEN(cItem)
 ::SetPos(nT,nL)
 nT:=ROW()
 nL:=COL()
 IF ::UsedR - nL + 1 < nLen
    cItem := LEFT(cItem,::UsedR-nL+1)
 ENDIF
 __ATPROMPT(nT,nL,cItem,cMess)
RETURN NIL
***********
METHOD DispOut(xVal,xColor) CLASS TctWIN
 LOCAL cVal := HB_VALTOSTR(xVal)
 cVal := LEFT(cVal, ::UsedR - COL() + 1)     //ucinamy
 DISPOUT(cVal,xColor)
 IF COL() > ::UsedR
   IF ROW() == ::UsedB
      SETPOS(::UsedB,::UsedR)
   ELSE
      SETPOS(ROW()+1,::UsedL)
   ENDIF
 ENDIF
RETURN NIL
*************
METHOD DispOutAT(nT,nL,xVal,xColor) CLASS TctWIN
 ::SetPos(nT,nL)
 ::DispOut(xVal,xColor)
RETURN NIL
***********
METHOD DevOut(xVal,xColor) CLASS TctWIN
 LOCAL lDev
 IF SET(_SET_PRINTER)                      //jesli takze PRINTER
    lDev := SET(_SET_DEVICE , "PRINTER")   //tylko PRINTER
    DEVOUT(xVal,xColor)                     //standardowy out
    SET(_SET_DEVICE, lDev)                 //przywrocenie SET DEVICE
 ENDIF
 IF SET(_SET_CONSOLE)                    //jeûli takße CONSOLE
    ::DispOut(xVal,xColor)             //wlasny ::xxx()
 ENDIF
RETURN NIL
************
METHOD DevOutPict(xVal,cPict,xColor) CLASS TctWIN
  ::DevOut( TRANSFORM(xVAl,cPict) , xColor)
RETURN NIL
*************
METHOD Save() CLASS TctWIN
  ::cSaveData := SAVESCREEN(::PosT ,::PosL ,::PosB ,::PosR )
  ::nRow := ROW()
  ::nCol := COL()
  ::nCursor := SETCURSOR()
  ::cColor := SETCOLOR()
RETURN NIL
****
METHOD SaveBG() CLASS TctWIN
  ::cBackground:=SAVESCREEN(::PosT ,::PosL ,::PosB ,::PosR)
RETURN NIL
****
METHOD SaveFG() CLASS TctWIN
  ::cSaveData:=SAVESCREEN(::PosT ,::PosL ,::PosB ,::PosR)
RETURN NIL
****
METHOD RestoreBG() CLASS TctWIN
  DISPBEGIN()
  RESTSCREEN(::PosT ,::PosL ,::PosB ,::PosR ,::cBackground)
  DISPEND()
RETURN NIL
****
METHOD RestoreFG() CLASS TctWIN
  DISPBEGIN()
  RESTSCREEN(::PosT ,::PosL ,::PosB ,::PosR ,::cSaveData)
  DISPEND()
RETURN NIL
*************
METHOD Restore() CLASS TctWIN
  DISPBEGIN()
  RESTSCREEN(::PosT ,::PosL ,::PosB ,::PosR ,::cSaveData)
  SETPOS(::nRow , ::nCol)
  SETCURSOR(::nCursor)
  SETCOLOR(::cColor)
  DISPEND()
RETURN NIL
*********
METHOD Move(nT,nL) CLASS TctWIN
LOCAL dx, dy
LOCAL MaxR,MaxC
  IF nT == ::PosT .and. nL == ::PosL
     RETURN NIL
  ENDIF
  MaxR:=MAXROW()
  MaxC:=MAXCOL()
  DISPBEGIN()
  ::Save()
  ::RestoreBG()
  IF nT < 0
    nT := IIF(ctw_MODET,0,ctw_BOARDT)
  ENDIF
  IF nT < ctw_BOARDT .and. !ctw_MODET
     nT := ctw_BOARDT
  ENDIF
  IF nL < 0
    nL := IIF(ctw_MODEL,0,ctw_BOARDL)
  ENDIF
  IF nL < ctw_BOARDL .and. !ctw_MODEL
     nL := ctw_BOARDL
  ENDIF
  IF nT > MaxR - ::PosB + ::PosT
    nT := MaxR - ::PosB + ::PosT
  ENDIF
  IF nT > ctw_BOARDB - ::PosB + ::PosT   .and. !ctw_MODEB
    nT := ctw_BOARDB - ::PosB + ::PosT
  ENDIF
  IF nL > MaxC - ::PosR + ::PosL
    nL := MaxC - ::PosR + ::PosL
  ENDIF
  IF nL > ctw_BOARDR - ::PosR + ::PosL   .and. !ctw_MODER
    nL := ctw_BOARDR - ::PosR + ::PosL
  ENDIF
  dy := nT - ::PosT
  dx := nL - ::PosL
  ::nRow := ::nRow + dy
  ::nCol := ::nCol + dx
  ::PosT := nT
  ::PosL := nL
  ::PosB := ::PosB + dy
  ::PosR := ::PosR + dx
  ::UsedT := ::UsedT + dy
  ::UsedL := ::UsedL + dx
  ::UsedB := ::UsedB + dy
  ::UsedR := ::UsedR + dx
  ::SaveBG()
  ::Restore()
  DISPEND()
RETURN NIL
*********
METHOD WBox(ncType) CLASS TctWIN
  IF ::UsedB - ::UsedT < 2
     RETURN .F.
  ENDIF
  IF ::UsedR - ::UsedL < 2
     RETURN .F.
  ENDIF
  IF ncType == NIL
     ncType := ctw_WBOX_type[1]       //narazie DOUBLE CLEAR a nie wg SETCLEARB
  ELSEIF VALTYPE(ncType) == "N"
     IF ncType< 0 .or. ncType > 15
        ncType := ctw_WBOX_type[1]    //DOUBLE CLEAR
     ELSE
        ncType := ctw_WBOX_type[ncType+1]
     ENDIF
  ENDIF
  DISPBOX(::PosT ,::PosL ,::PosB ,::PosR ,ncType)
  ::WFormat(1,1,1,1)
RETURN .T.
**********
METHOD WFormat(nT,nL,nB,nR) CLASS TctWIN
 IF nT == NIL
    nT := ::PosT - ::UsedT
 ENDIF
 IF nL == NIL
    nL := ::PosL - ::UsedL
 ENDIF
 IF nB == NIL
    nB := ::UsedB - ::PosB
 ENDIF
 IF nR == NIL
    nR := ::UsedR - ::PosR
 ENDIF
 IF nT+nB > ::UsedB - ::UsedT + 1
    RETURN NIL
 ENDIF
 IF nL+nR > ::UsedR - ::UsedL + 1
    RETURN NIL
 ENDIF
 ::UsedT := ::UsedT + nT
 IF ::UsedT < ::PosT
    ::UsedT := ::PosT
 ENDIF
 ::UsedL := ::UsedL + nL
 IF ::UsedL < ::PosL
    ::UsedL := ::PosL
 ENDIF
 ::UsedB := ::UsedB - nB
 IF ::UsedB > ::PosB
    ::UsedB := ::PosB
 ENDIF
 ::UsedR := ::UsedR - nR
 IF ::UsedR > ::PosR
    ::UsedR := ::PosR
 ENDIF

RETURN NIL
***********
METHOD DevPos(nT,nL) CLASS TctWIN
 IF SET(_SET_PRINTER )                   //jesli PRINTER ON
    DEVPOS(nT,nL)                        //standardowy out
 ENDIF
 IF SET(_SET_CONSOLE)                    //jesli ma bys na console
    ::SetPos(nT,nL)                     //wlasny ::SetPos()
 ENDIF
RETURN NIL
************
METHOD SetPos(nT,nL) CLASS TctWIN
LOCAL nTop, nLeft
 nTop := ::UsedT + nT
 IF nTop < ::PosT
   nTop := ::PosT
 ELSE
   IF nTop > ::UsedB
      nTop := -1      //lub ???? tak aby nie bylow widac na ekranie
   ENDIF
 ENDIF
 IF nL < 0
    nLeft := ::UsedR + nL + 1
    DO WHILE nLeft < ::UsedL
       nLeft := nLeft + ::UsedR - ::UsedL
    ENDDO
 ELSE
    nLeft := ::UsedL + nL
    IF nLeft > ::UsedR
       nLeft := MAXCOL() + 1   //?????   poza ekran
    ENDIF
 ENDIF
 SETPOS(nTop,nLeft)
RETURN NIL
**************
METHOD Row() CLASS TctWIN

RETURN ROW() - ::UsedT
**************
METHOD Col() CLASS TctWIN

RETURN COL() - ::UsedL
**************
METHOD MaxRow() CLASS TctWIN

RETURN ::UsedB - ::UsedT
***************
METHOD MaxCol() CLASS TctWIN

RETURN ::UsedR - ::UsedL
***************
METHOD Scroll(nT,nL,nB,nR,nV,nH) CLASS TctWIN
 LOCAL mr:=MAXROW() + 1
 LOCAL mc:=MAXCOL() + 1
 IF nT ==  NIL
    nT := ::UsedT
 ELSEIF nT <= 0
   nT := ::UsedT
 ELSE
   nT := nT + ::UsedT
   IF nT > ::UsedB
      nT := mr      //poza ekran
   ENDIF
 ENDIF

 IF nL == NIL
   nL := ::UsedL
 ELSEIF nL <= 0
   nL := ::UsedL
 ELSE
   nL := nL + ::UsedL
   IF nL > ::UsedR
      nL := mc     //poza ekran
   ENDIF
 ENDIF

 IF nB == NIL
   nB := ::UsedB
 ELSE
   nB := nB + ::UsedT
 ENDIF
 IF nB > ::UsedB
   nB := mr        //poza ekran
 ENDIF

 IF nR == NIL
   nR := ::UsedR
 ELSE
 nR := nR + ::UsedL
 ENDIF
 IF nR > ::UsedR
   nR := mc        //pozz ekran
 ENDIF
 DISPBEGIN()
 SCROLL(nT,nL,nB,nR,nV,nH)
 DISPEND()
RETURN NIL
***************
METHOD DispBox(nT,nL,nB,nR,cnStr,cColor) CLASS TctWIN
 IF nT==NIL .or. nL==NIL .or. nB==NIL .or. nR==NIL
    RETURN NIL
 ENDIF
 IF nT <= 0
   nT := ::UsedT
 ELSE
   nT := nT + ::UsedT
   IF nT > ::UsedB
     RETURN NIL
   ENDIF
 ENDIF
 IF nL <= 0
   nL := ::UsedL
 ELSE
   nL := nL + ::UsedL
   IF nL > ::UsedR
     RETURN NIL
   ENDIF
 ENDIF
 IF nB <= 0
   RETURN NIL
 ELSE
   nB := nB + ::UsedT
   IF nB > ::UsedB
     nB := ::UsedB
   ENDIF
 ENDIF
 IF nR <= 0
   RETURN NIL
 ELSE
   nR := nR + ::UsedL
   IF nR > ::UsedR
     nR := ::UsedR
   ENDIF
 ENDIF
 DISPBOX(nT,nL,nB,nR,cnStr,cColor)

RETURN NIL

**********************
