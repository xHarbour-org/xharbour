/*
 * $Id: TCBrowser.prg,v 1.3 2002/10/11 03:53:16 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TBrowser CLASS wrapper to whBrowse in WhatPlus.lib
 *
 * Copyright 2002 Augusto Infante [augusto@2vias.com.ar]
 * www - http://www.xharbour.org
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
 */

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "wingdi.ch"
#include "debug.ch"

CLASS TCBrowser FROM whBrowse
   DATA Parent AS OBJECT
   DATA Left   INIT   0
   DATA Top    INIT   0
   DATA Width  INIT 100
   DATA Height INIT 100
   DATA Header AS ARRAY
   DATA Info   AS ARRAY
   METHOD New() CONSTRUCTOR
   METHOD Create()
   METHOD EditCell()
   METHOD GetItemText()
ENDCLASS

*-----------------------------------------------------------------------------*
   
METHOD New( oParent, nL, nT, nW, nH, aHeader, aInfo ) CLASS TCBrowser
   ::Parent     := oParent
   ::Left       := IFNIL( nL, ::Left,   nL )
   ::Top        := IFNIL( nT, ::Top,    nT )
   ::Width      := IFNIL( nW, ::Width,  nW )
   ::Height     := IFNIL( nH, ::Height, nH )

   ::Header     := aHeader
   ::Info       := aInfo
   
return( super:New( if( valtype( ::Info[1] )=="A", ::Info, ALIAS() ) ) )


METHOD Create() CLASS TCBrowser
   local oCol, aCols, c,i, a
   super:Create( ::Parent:handle,  ::Left, ::Top, ::Width, ::Height, WS_CHILD+WS_VISIBLE+WS_VSCROLL, WS_EX_CLIENTEDGE )

   for i:=1 to len( ::Header )
       oCol:=GetAColumn(::header,i)
       oCol:VertAlign :=TA_CENTER
       oCol:bSaveBlock:= ::Header[i][3]
       ::addColumn(oCol)
   next
   
   ::HeadFont      := GetMessageFont()
   ::Font          := GetMessageFont()
   ::bKillBlock    := {|| DeleteObject(::Font),DeleteObject(::HeadFont)}

   ::configure()

   return( self )


*-----------------------------------------------------------------------------*

FUNCTION GetAColumn(a,i)
 RETURN whColumn():INIT(a[i][1],{|oCol,oB,n| asString(oB:source[n,i]) } ,DT_LEFT, a[i][2] )

*-----------------------------------------------------------------------------*
METHOD EditCell(limit,bEndBlock,xStyle,lAsNumber,aColor,FirstKey) CLASS TCBrowser

   LOCAL hWin
   LOCAL nProc
   LOCAL aRect
   LOCAL cRect:=Space(8)
   LOCAL cText
   LOCAL hDC
   local nL,nR
   local o
   IF !(::wantHiliteAll .AND. ::ColCount > 0 )
      IF !::HitBottom .AND. !::HitTop
         SetFocus(::hWnd)
         IF (aRect:=::GetItemRect())==NIL
            RETURN(NIL)
         ENDIF

         cText:=::GetItemText()
         IF xStyle==NIL
            xStyle:=0
         ENDIF
         
         VIEW ValType(cText)=="A"

         IF cText==NIL
            RETURN(NIL)
         ELSEIF ValType(cText)=="A"
            cText:=a2str(cText,CHR(13)+CHR(10))


            o := ComboBrowser():New( self, 512, aRect[1]-1,aRect[2]-1,aRect[3]-aRect[1]+1,aRect[4]-aRect[2]+1 )


           ELSE
            hWin:=CreateWindow("edit",cText,;
                               WS_CHILD+WS_BORDER+WS_VISIBLE+ES_AUTOHSCROLL+ES_MULTILINE+4096+xStyle,;
                               aRect[1]-1,aRect[2]-1,aRect[3]-aRect[1]+1,aRect[4]-aRect[2]+1,;
                               ::hWnd,)
            nProc:=SetProcedure(hWin,{|hWin,nMsg,nwParam,nlParam| ;
                                      ::EditCellProc(nProc,hWin,nMsg,nwParam,nlParam,bEndBlock,,aColor)},;
                                {WM_KILLFOCUS,WM_KEYUP,WM_CTLCOLOREDIT,WM_KEYDOWN,WM_CHAR,WM_USER+322,WM_USER+321})

            IF ValType(limit)=='N'
               SendMessage(hWin,EM_LIMITTEXT,limit,0)
            ENDIF
            IF FirstKey # NIL
               PostMessage(hWin,WM_CHAR,FirstKey,0)
            ENDIF
            SendMessage(hWin,WM_SETFONT,::GetColFont(),MAKELPARAM(1,0))
            SendMessage(hWin,WM_USER+322,0,0)
            SendMessage(hWin,EM_SETSEL,0,-1)
            SetFocus(hWin)
         ENDIF
        ELSE
         MessageBeep(-1)
      ENDIF
   ENDIF

   RETURN(NIL)

METHOD GetItemText()
   LOCAL nPos
   IF ::RowPos> 0 .AND. ::RowPos<=::RowCountUsable .AND. ;
      ::ColPos >= ::LeftVisible .AND. ::ColPos <=::ColCount

      IF Len(::aData) >= ::RowPos .AND. Len(::aData[::RowPos,2]) >= ::ColPos
         IF !::wantHiliteAll
            RETURN Eval(::Columns[::ColPos]:block,::Columns[::ColPos],self,::aData[::RowPos,1],::RowPos)
            //RETURN(::aData[::RowPos,2,::ColPos])
         ENDIF
      ENDIF
   ENDIF

   RETURN(NIL)

//------------------------------------------------------------------------------

CLASS ComboBrowser FROM TComboBox
ENDCLASS

