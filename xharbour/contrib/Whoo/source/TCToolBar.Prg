/*
 * xHarbour Project source code:
 *
 * Whoo.lib TToolBar CLASS
 *
 * Copyright 2002 Augusto Infante [systems@quesoro.com]
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

#Include "windows.ch"
#include "hbclass.ch"
#Include "toolbar.ch"
#Include 'debug.ch'
#Include "tabctrl.ch"
#Include "wintypes.ch"
#Include "cstruct.ch"
#Include 'what32.ch'
#Include "tooltips.ch"
#Include "winlview.ch"

pragma pack(4)

IMPORT C STRUCTURE RECT
IMPORT C STRUCTURE NMHDR

typedef struct _TBBUTTON {;
    int iBitmap; 
    int idCommand; 
    BYTE fsState; 
    BYTE fsStyle; 
    DWORD dwData; 
    int iString; 
} TBBUTTON, NEAR* PTBBUTTON, FAR* LPTBBUTTON

typedef struct tagNMTOOLBAR {;
    NMHDR   hdr;
    int     iItem;
    TBBUTTON tbButton;
    int     cchText;
    LPSTR   pszText;
    RECT    rcButton;
} NMTOOLBAR, FAR* LPNMTOOLBAR



typedef struct tagTBADDBITMAP {;
        HINSTANCE       hInst;
        UINT            nID;
} TBADDBITMAP, *LPTBADDBITMAP

typedef struct {;
    NMHDR     hdr;
    LPTSTR    lpszText;
    char      szText[80];
    HINSTANCE hinst;
    UINT      uflags;
} TOOLTIPTEXT

CLASS TToolbar FROM TControl

   DATA abuttons PROTECTED
   DATA aBitmaps PROTECTED
   DATA hParent  PROTECTED 
   DATA Created  PROTECTED
   DATA hBmp     PROTECTED
   DATA ntProc   PROTECTED
   DATA nBtn     PROTECTED HIDDEN
   DATA aMenus   PROTECTED
   VAR nImg,hBMInst,nBMId,xBtn,yBtn,xBmp,yBmp PROTECTED

   METHOD New() CONSTRUCTOR

   METHOD AddButton()
   METHOD AddBitmap()
   METHOD Create()
   METHOD tbProc()

   METHOD Disable(nBtn)           INLINE ::SendMessage(TB_ENABLEBUTTON,nBtn,0)
   METHOD Enable(nBtn,lFlag)      INLINE lFlag := IFNIL(lFlag,.T.,lFlag),;
                                         ::SendMessage(TB_ENABLEBUTTON,nBtn,If(lFlag,1,0))
   METHOD Disableall()
   METHOD Enableall()
   METHOD CheckButton(nBtn,lFlag) INLINE lFlag := IFNIL(lFlag,.T.,lFlag),;
                                         ::SendMessage(TB_CHECKBUTTON,nBtn,If(lFlag,1,0))
   METHOD IsButtonChecked(nBtn)   INLINE IF(::SendMessage(TB_ISBUTTONCHECKED,nBtn,0)==0,.F.,.T.)
   METHOD AddMenu()
   
   METHOD SetListStyle()          INLINE ::Style := OR(::Style,TBSTYLE_LIST)
ENDCLASS


*-----------------------------------------------------------------------------*

METHOD New( oParent, nId, nImg, hBMInst, nBMId, xBtn, yBtn, xBmp, yBmp ) CLASS TToolBar

   InitCommonControlsEx(ICC_COOL_CLASSES)

   ::nImg    := nImg
   ::hBMInst := hBMInst
   ::nBMId   := nBMId
   ::xBtn    := xBtn
   ::yBtn    := yBtn
   ::xBmp    := xBmp
   ::yBmp    := yBmp
  
   ::aButtons:={}
   ::Style   :=TBSTYLE_FLAT + TBSTYLE_TOOLTIPS + WS_CLIPCHILDREN + ;
              WS_CLIPSIBLINGS + CCS_ADJUSTABLE + CCS_NODIVIDER + CCS_NORESIZE + WS_CHILD
   ::Id      := nId
   ::Created := .F.
   ::aBitmaps:={}
   ::aMenus  :={}

   ::lRegister := .F.
   ::lControl  := .T.

   ::Msgs      := IFNIL( ::Msgs, {WM_SIZE,WM_NOTIFY}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Left      := 0
   ::Top       := 0
   ::Width     := 0
   ::Height    := 0
   ::Name      := 'ToolBar32'
   ::ExStyle   := 0
   RETURN( super:New( oParent ) )

*-----------------------------------------------------------------------------*

METHOD AddBitmap(hInst, nhIdBmp, nButtons) CLASS TToolBar
 
   LOCAL tbab IS TBADDBITMAP
  
   DEFAULT nButtons TO 1
  
   tbab:hInst := hInst
   tbab:nId   := nhIdBmp
   AADD(::aBitmaps,{tbab,nButtons})
   IF ::created
     SendMessage(::handle,TB_ADDBITMAP,nButtons,tbab:value)
   ENDIF

   RETURN(1)

*-----------------------------------------------------------------------------*

METHOD AddButton( cName, oBtn ) CLASS TToolBar

   LOCAL tbb IS TBBUTTON
   DEFAULT oBtn TO ToolButton():New()
   
   oBtn:name     := cName
   oBtn:Parent   := self
   
   tbb:ibitmap   := oBtn:ImageIndex
   tbb:idCommand := oBtn:Id
   tbb:fsState   := oBtn:State
   tbb:fsStyle   := oBtn:Style
   tbb:dwData    := oBtn:cargo
   tbb:iString   := LEN(::aButtons)
   AADD(::aButtons,oBtn)

   SendMessage(::handle,TB_ADDBUTTONS,1,tbb:value)
   SendMessage(::handle,TB_ADDSTRING, 0,oBtn:Caption)

RETURN(self)

*-----------------------------------------------------------------------------*

METHOD AddMenu(nId,aMenuItems)

   AADD(::aMenus,{nId,aMenuItems})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD Create() CLASS TToolBar
   LOCAL cButtons:=""
   LOCAL cStrings:=""
   LOCAL tbb IS TBBUTTON
   LOCAL i
   IF ISNIL(::hBMInst) .AND. ISNIL(::nBMId)
      ::hBMInst:=HINST_COMMCTRL
      ::nBMId  :=IDB_STD_LARGE_COLOR
   ENDIF
   ::handle := CreateToolBarEx( ::Parent:handle, ::Style, ::Id, ::nImg,::hBMInst,::nBMId,cButtons,,;
                                ::xbtn,::yBtn,::xBmp,::yBmp, tbb:sizeof())
   ::ntProc:=SetProcedure(::Parent:handle,{|hWnd, nMsg,nwParam,nlParam| ::tbProc(hWnd,nMsg,nwParam,nlParam)},{WM_NOTIFY,WM_COMMAND})
   ::SendMessage(TB_SETEXTENDEDSTYLE,0,TBSTYLE_EX_DRAWDDARROWS )
   ::Created:=.T.
return( self )


*-----------------------------------------------------------------------------*

METHOD tbProc(hWnd,nMsg,nwParam,nlParam) CLASS TToolBar

   LOCAL Hdr
   LOCAL Ttt
   LOCAL nmt
   LOCAL nID
   LOCAL hMenu,rc,aRect
   LOCAL n,x
   LOCAL hic

   DO CASE
   CASE nMsg==WM_COMMAND
       IF (n:=ASCAN(::aButtons,{|btn| btn:id==nwParam})) > 0
          if ::aButtons[n]:Action != NIL
             if valtype(::aButtons[n]:Action) == "B"
                EVAL( ::aButtons[n]:Action,::aButtons[n] )
               else
                HB_Exec( ::aButtons[n]:Action,,::aButtons[n])
             endif
          endif
       endif
   CASE nMsg==WM_NOTIFY
     Hdr IS NMHDR
     Hdr:Buffer(peek(nlParam,Hdr:sizeof()))
     DO CASE
     CASE Hdr:code==TTN_NEEDTEXT
       IF (n:=ASCAN(::aButtons,{|btn| btn:id==Hdr:idFrom})) > 0
          Ttt IS TOOLTIPTEXT
          Ttt:Reset()
          Ttt:Buffer(peek(nlParam,Ttt:sizeof()))
          Ttt:lpszText:=::aButtons[n]:hint
          poke(nlParam,Ttt:value,Ttt:sizeof())
         RETURN(1)
       ENDIF

     CASE Hdr:code==TBN_QUERYINSERT
         RETURN(1)

     CASE Hdr:code==TBN_QUERYDELETE
         RETURN(1)

     CASE Hdr:code==TBN_GETBUTTONINFO
         Nmt IS NMTOOLBAR
         nmt:buffer(peek(nlParam,nmt:sizeof()))
         RETURN(1)

     ENDCASE
   ENDCASE

   RETURN(CallWindowProc(::ntProc,hWnd /*::Parent:handle*/,nMsg,nwParam,nlParam))

*-----------------------------------------------------------------------------*

METHOD disableall() CLASS TToolBar

   AEVAL(::aButtons,{|btn| ::disable(btn:id)})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD enableall() CLASS TToolBar

   AEVAL(::aButtons,{|btn| ::enable(btn:id)})

   RETURN(self)

*-----------------------------------------------------------------------------*


CLASS ToolButton
   DATA ImageIndex INIT -1
   DATA Name       INIT "ToolButton"
   DATA Style      INIT TBSTYLE_BUTTON
   DATA PopupMenu
   DATA Hint
   DATA Height
   DATA Width
   DATA Enabled
   DATA Caption
   DATA Action
   DATA Id
   DATA State      INIT TBSTATE_ENABLED
   DATA Cargo
   DATA TextIndex  INIT 0
   DATA Parent     HIDDEN
   METHOD New() CONSTRUCTOR
ENDCLASS

METHOD New(nImgIndex,cCaption,cHint,nId,bAction) CLASS ToolButton
   ::ImageIndex := IFNIL(nImgIndex,::ImageIndex,nImgIndex)
   ::Caption    := IFNIL(cCaption, ::Caption,   cCaption)
   ::Hint       := IFNIL(cHint,    ::Hint,      cHint)
   ::Action     := IFNIL(bAction,  ::Action,    bAction)
   ::Id         := nId
return(self)

