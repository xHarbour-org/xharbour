/*
 * $Id: TCToolBar.Prg,v 1.26 2002/11/07 20:05:56 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TToolBar CLASS
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
#include "classex.ch"

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


CLASS TToolBar FROM TCustomControl

   DATA Caption PROTECTED INIT ""
   DATA FLeft   PROTECTED  INIT    0
   DATA FTop    PROTECTED  INIT    0
   DATA FWidth  PROTECTED  INIT    50
   DATA FHeight PROTECTED  INIT    0

   DATA Style   INIT  TBSTYLE_FLAT + TBSTYLE_TOOLTIPS + WS_CLIPCHILDREN + ;
                      WS_CLIPSIBLINGS + CCS_ADJUSTABLE + CCS_NODIVIDER + CCS_NORESIZE + WS_CHILD
   DATA ExStyle INIT  0

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT { WM_SIZE, WM_MOVE }
   
   ACCESS RowCount          INLINE SendMessage( ::handle, TB_GETROWS, 0, 0 )
   ASSIGN RowCount( nRows ) INLINE SendMessage( ::handle, TB_SETROWS, nRows )

   DATA abuttons PROTECTED INIT {}
   DATA aBitmaps PROTECTED
   DATA Created  PROTECTED
   DATA ntProc   PROTECTED
   DATA aMenus   PROTECTED

   VAR nImg, hBMInst, nBMId, xBtn, yBtn, xBmp, yBmp PROTECTED

   DATA WinClass    PROTECTED INIT "toolbar32"
   DATA ControlName PROTECTED INIT "ToolBar"

   METHOD Add()
   METHOD AddBitmap()
   METHOD Create()
   METHOD CreateWnd()
   METHOD tbProc()

   METHOD DisableBtn( nBtn )      INLINE ::SendMessage( TB_ENABLEBUTTON, nBtn, 0)
   METHOD EnableBtn( nBtn )       INLINE ::SendMessage( TB_ENABLEBUTTON, nBtn, 1)
   
   METHOD Disableall()
   METHOD Enableall()
   
   METHOD CLickButton(nBtn,lFlag) INLINE lFlag := IFNIL( lFlag, .T., lFlag ),;
                                         ::SendMessage( TB_CHECKBUTTON, nBtn, If( lFlag, 1, 0 ) )
                                         
   METHOD IsButtonClicked(nBtn)   INLINE IF( ::SendMessage( TB_ISBUTTONCHECKED, nBtn, 0) == 0, .F., .T. )
   METHOD AddMenu()

   METHOD SetListStyle()          INLINE ::Style := OR(::Style,TBSTYLE_LIST)

   METHOD Disable()               INLINE ::DisableAll()//, EnableWindow( ::handle, .f. )
   METHOD Enable()                INLINE ::EnableAll()//, EnableWindow( ::handle, .t. )

ENDCLASS


*-----------------------------------------------------------------------------*

METHOD Create( oParent ) CLASS TToolBar

   InitCommonControlsEx(ICC_COOL_CLASSES)
   
   super:Create( oParent )
   ::Parent  := oParent
   ::Id      := 0

   ::nImg    := 15
   ::hBMInst := NIL
   ::nBMId   := NIL
   ::xBtn    := 26
   ::yBtn    := 26
   ::xBmp    := 20
   ::yBmp    := 20

   ::aButtons:={}
   ::Created := .F.
   ::aBitmaps:={}
   ::aMenus  :={}

   RETURN( Self )

*-----------------------------------------------------------------------------*

METHOD AddBitmap(hInst, nhIdBmp, nButtons) CLASS TToolBar

   LOCAL tbab IS TBADDBITMAP

   DEFAULT nButtons TO 1

   tbab:hInst := hInst
   tbab:nId   := nhIdBmp
   AADD(::aBitmaps,{tbab,nButtons})

   IF ::created
     SendMessage( ::handle, TB_ADDBITMAP, nButtons, tbab:value )
   ENDIF

   RETURN Self

*-----------------------------------------------------------------------------*

METHOD Add( oBtn ) CLASS TToolBar

   LOCAL tbb IS TBBUTTON

   tbb:ibitmap   := oBtn:ImageIndex
   tbb:idCommand := oBtn:Id
   tbb:fsState   := oBtn:State
   tbb:fsStyle   := oBtn:Style
   tbb:dwData    := oBtn:cargo
   tbb:iString   := LEN( ::aButtons )
   
   AADD(::aButtons,oBtn)

   SendMessage( ::handle, TB_ADDBUTTONS, 1, tbb:value)
   SendMessage( ::handle, TB_ADDSTRING, 0, oBtn:Caption)

RETURN(self)

*-----------------------------------------------------------------------------*

METHOD AddMenu(nId,aMenuItems)

   AADD(::aMenus,{nId,aMenuItems})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD CreateWnd() CLASS TToolBar
   LOCAL cButtons:=""
   LOCAL cStrings:=""
   LOCAL tbb IS TBBUTTON
   LOCAL i
   IF ISNIL(::hBMInst) .AND. ISNIL(::nBMId)
      ::hBMInst:=HINST_COMMCTRL
      ::nBMId  :=IDB_STD_LARGE_COLOR
   ENDIF
   ::Fhandle := CreateToolBarEx( ::Parent:handle, ::Style, ::Id, ::nImg,::hBMInst,::nBMId,cButtons,,;
                                ::xbtn,::yBtn,::xBmp,::yBmp, tbb:sizeof())
   ::ntProc:=SetProcedure(::Parent:handle,{|hWnd, nMsg,nwParam,nlParam| ::tbProc(hWnd,nMsg,nwParam,nlParam)},{WM_NOTIFY,WM_COMMAND})
   ::SendMessage(TB_SETEXTENDEDSTYLE,0,TBSTYLE_EX_DRAWDDARROWS )
   ::Created:=.T.
   ::Show()
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

   AEVAL(::aButtons,{|btn| ::disableBtn(btn:id)})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD enableall() CLASS TToolBar

   AEVAL(::aButtons,{|btn| ::enableBtn(btn:id)})

   RETURN(self)

*-----------------------------------------------------------------------------*


CLASS ToolButton
   DATA ImageIndex INIT -1
   DATA Name
   DATA ControlName INIT "ToolButton"
   DATA Style      PROTECTED INIT TBSTYLE_BUTTON
   DATA PopupMenu
   DATA Hint
   DATA Height
   DATA Width
   DATA Enabled
   DATA Action
   DATA Id         INIT 0
   DATA State      INIT TBSTATE_ENABLED
   DATA Cargo
   DATA TextIndex  INIT 0
   DATA TabIndex   INIT 1
   DATA Parent     PROTECTED
   PROPERTY Caption READ FCaption WRITE SetText

   METHOD Create()
   METHOD Disable()      INLINE ::Parent:DisableBtn( ::Id )
   METHOD Enable()       INLINE ::Parent:EnableBtn( ::Id )
   METHOD Click()        INLINE ::Parent:ClickButton( ::Id, .T. )
   METHOD SetText(cText) INLINE ::FCaption := cText

ENDCLASS


METHOD Create( oParent ) CLASS ToolButton //nImgIndex,cCaption,cHint,nId,bAction)
  
   LOCAL oCtrl, nInst := 1

   ::Parent     := oParent
   ::Id         := Len( ::Parent:aButtons ) + 1
   ::ImageIndex := Len( ::Parent:aButtons )

   IF ::Name == NIL
      FOR EACH oCtrl IN ::Parent:aButtons
          IF oCtrl:ControlName == ::ControlName
             nInst ++
          ENDIF
      NEXT
      ::Name := ( ::ControlName + AllTrim( Str( nInst ) ) )
   ENDIF

   __objAddData( ::Parent, ::Name, HB_OO_CLSTP_PROTECTED )
   __ObjSetValueList( ::Parent, { { ::Name, self } } )

   oParent:Add( Self )

RETURN Self
