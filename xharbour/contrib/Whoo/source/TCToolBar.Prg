/*
 * $Id: TCToolBar.Prg,v 1.39 2003/01/31 06:50:46 what32 Exp $
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

#include "winuser.ch"
#include "hbclass.ch"
#Include "commctrl.ch"
#Include 'debug.ch'
#Include "wintypes.ch"
#Include "cstruct.ch"
#Include 'what32.ch'
#include "classex.ch"

GLOBAL EXTERNAL Application

pragma pack(4)

IMPORT C STRUCTURE RECT
IMPORT C STRUCTURE NMHDR

typedef struct {;
    UINT cbSize;
    DWORD dwMask;
    int idCommand;
    int iImage;
    BYTE fsState;
    BYTE fsStyle;
    WORD cx;
    LONG lParam; //DWORD_PTR lParam;
    LPTSTR pszText;
    int cchText;
} TBBUTTONINFO

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

   DATA Caption EXPORTED INIT ""
   DATA FLeft   EXPORTED  INIT    0
   DATA FTop    EXPORTED  INIT    0
   DATA FWidth  EXPORTED  INIT    100
   DATA FHeight EXPORTED  INIT    24

   DATA FStyle   INIT  WS_CHILD + WS_VISIBLE + TBSTYLE_FLAT + TBSTYLE_TOOLTIPS + CCS_NODIVIDER + CCS_NORESIZE
   
   DATA ExStyle  INIT  0

   DATA lRegister EXPORTED INIT .F.
   DATA lControl  EXPORTED INIT .T.
   DATA Msgs      EXPORTED INIT { WM_SIZE, WM_MOVE }
   
   ACCESS RowCount          INLINE SendMessage( ::handle, TB_GETROWS, 0, 0 )
   ASSIGN RowCount( nRows ) INLINE SendMessage( ::handle, TB_SETROWS, nRows )

   DATA abuttons EXPORTED INIT {}
   DATA aBitmaps EXPORTED
   DATA Created  EXPORTED
   DATA ntProc   EXPORTED
   DATA aMenus   EXPORTED
   
   VAR nImg, hBMInst, nBMId, xBtn, yBtn, xBmp, yBmp EXPORTED

   DATA WinClass    EXPORTED INIT TOOLBARCLASSNAME
   DATA ControlName EXPORTED INIT "ToolBar"

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

   ::Super:Create( oParent )
   
   ::Id      := 0
   ::nImg    := 0
   ::hBMInst := NIL
   ::nBMId   := NIL
   ::xBtn    := 26
   ::yBtn    := 26
   ::xBmp    := 20
   ::yBmp    := 20
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

METHOD AddMenu(nId,aMenuItems)

   AADD(::aMenus,{nId,aMenuItems})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD CreateWnd() CLASS TToolBar

   LOCAL cButtons:=""
   LOCAL cStrings:=""
   LOCAL tbb IS TBBUTTON
   LOCAL i

   IF ISNIL( ::hBMInst ) .AND. ISNIL( ::nBMId )
      ::hBMInst:=HINST_COMMCTRL
      ::nBMId  :=IDB_STD_LARGE_COLOR
   ENDIF
   

// New REMEMBER TO MAKE IT WORK CreateToolBarEx is obsolete

//   ::FHandle := CreateWindowEx( 0, TOOLBARCLASSNAME,"", WS_CHILD,  ::FLeft, ::FTop, ::FWidth, ::FHeight, ::Parent:FHandle, ::Id, hInstance() )
//   SendMessage( ::FHandle, TB_BUTTONSTRUCTSIZE, tbb:sizeof(), 0 )
   

   ::FHandle := CreateToolBarEx( ::FParent:FHandle, ::FStyle, ::Id, ::nImg,::hBMInst,::nBMId,cButtons,,;
                                ::xbtn,::yBtn,::xBmp,::yBmp, tbb:sizeof())

   ::ntProc := SetProcedure(::FParent:FHandle,{|hWnd, nMsg,nwParam,nlParam| ::tbProc(hWnd,nMsg,nwParam,nlParam)},{WM_NOTIFY,WM_COMMAND})
   ::SendMessage(TB_SETEXTENDEDSTYLE,0,TBSTYLE_EX_DRAWDDARROWS )
   ::Created:=.T.
   ShowWindow( ::FHandle, SW_SHOW )

RETURN Self


*-----------------------------------------------------------------------------*

METHOD tbProc(hWnd,nMsg,nwParam,nlParam) CLASS TToolBar

   LOCAL Hdr
   LOCAL Ttt
   LOCAL nmt
   LOCAL nID
   LOCAL hMenu,rc,aRect, nRet, nCode
   LOCAL n,x
   LOCAL hic
   LOCAL xAction
   
   DO CASE

   CASE nMsg==WM_COMMAND
       IF nlParam == 0
          RETURN CallWindowProc( ::ntProc, hWnd, nMsg, nwParam, nlParam )
       ENDIF
       IF (n:=ASCAN(::aButtons,{|btn| btn:oStruct:idCommand==nwParam})) > 0
       
          xAction := IIF( ::aButtons[n]:Action == NIL, ::aButtons[n]:OnClick, ::aButtons[n]:Action )

          IF ValType( xAction ) == "B"
             EVAL( xAction, ::aButtons[n] )
          ELSEIF ValType( xAction ) == "N"
             HB_Exec( xAction, ::aButtons[n] )
          ENDIF
          RETURN 0
       ENDIF


   CASE nMsg==WM_NOTIFY
     Hdr IS NMHDR
     Hdr:Buffer(peek(nlParam,Hdr:sizeof()))
     DO CASE
     CASE Hdr:code==TTN_NEEDTEXT
       IF (n:=ASCAN(::aButtons,{|btn| btn:oStruct:idCommand==Hdr:idFrom})) > 0
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
   
   RETURN 0


*-----------------------------------------------------------------------------*

METHOD disableall() CLASS TToolBar

   AEVAL(::aButtons,{|btn| ::disableBtn(btn:oStruct:idCommand)})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD enableall() CLASS TToolBar

   AEVAL(::aButtons,{|btn| ::enableBtn(btn:oStruct:idCommand)})

   RETURN(self)

*-----------------------------------------------------------------------------*
METHOD Add( oBtn ) CLASS TToolBar

   oBtn:oStruct IS TBBUTTON

   oBtn:oStruct:ibitmap   := Len( ::aButtons )
   oBtn:oStruct:idCommand := Len( ::aButtons )
   oBtn:oStruct:fsState   := oBtn:State
   oBtn:oStruct:fsStyle   := oBtn:Style
   oBtn:oStruct:dwData    := oBtn:cargo
   oBtn:oStruct:iString   := LEN( ::aButtons )

   AADD(::aButtons,oBtn)

   SendMessage( ::handle, TB_ADDBUTTONS, 1, oBtn:oStruct:value)
   SendMessage( ::handle, TB_ADDSTRING, 0, oBtn:Caption)

RETURN Self

*-----------------------------------------------------------------------------*


CLASS ToolButton
   DATA ImageIndex INIT -1
   DATA Name
   DATA ControlName INIT "ToolButton"
   DATA Style      EXPORTED INIT TBSTYLE_BUTTON
   DATA PopupMenu
   DATA Height
   DATA Width
   DATA Enabled
   DATA Action
   DATA State      INIT TBSTATE_ENABLED
   DATA Cargo
   DATA TextIndex  INIT 0
   DATA TabIndex   INIT 1

   DATA Parent     EXPORTED
   DATA oStruct    EXPORTED

   PROPERTY Id READ oStruct:idCommand WRITE SetId DEFAULT 0
   
   PROPERTY Caption READ FCaption WRITE SetText
   PROPERTY Style   READ FStyle   WRITE SetStyle
   DATA Hint
  
   DATA OnClick

   METHOD Create()
   METHOD Disable()      INLINE ::Parent:DisableBtn( ::oStruct:idCommand )
   METHOD Enable()       INLINE ::Parent:EnableBtn( ::oStruct:idCommand )
   METHOD Click()        INLINE ::Parent:ClickButton( ::oStruct:idCommand, .T. )
   METHOD SetText(cText) INLINE ::FCaption := cText

   METHOD SetId( n )     INLINE ::FId := n, ::oStruct:idCommand := n, Self
   METHOD SetStyle()
ENDCLASS


METHOD Create( oParent ) CLASS ToolButton
  
   LOCAL oCtrl, nInst := 1
   
   IF ::Name == NIL
      FOR EACH oCtrl IN oParent:aButtons
          IF oCtrl:ControlName == ::ControlName
             nInst ++
          ENDIF
      NEXT
      ::Name := ( ::ControlName + AllTrim( Str( nInst ) ) )
   ENDIF

   __objAddData( oParent, ::Name, HB_OO_CLSTP_EXPORTED )
   __ObjSetValueList( oParent, { { ::Name, self } } )

   ::Parent := oParent

   oParent:Add( Self )

RETURN Self

METHOD SetStyle( n ) CLASS ToolButton

   LOCAL tbb IS TBBUTTONINFO

//   LOCAL TmpBuffer
   
   tbb:cbSize := tbb:SizeOf()
   tbb:dwMask := TBIF_STYLE
   
//   TmpBuffer := tbb:value
//   SendMessage( ::Parent:Handle, TB_GETBUTTONINFO, ::Id,  @TmpBuffer )
//   tbb:Buffer( TmpBuffer )

   tbb:fsStyle := n
   SendMessage( ::Parent:Handle, TB_SETBUTTONINFO, ::Id,  tbb:value )
RETURN Self
