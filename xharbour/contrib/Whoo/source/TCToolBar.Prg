// Augusto Infante - Andrew Wos
// Whoo.lib

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

typedef struct _RECT { ;
    LONG left; 
    LONG top; 
    LONG right; 
    LONG bottom; 
} RECT 

typedef struct tagNMHDR {;
    HWND hwndFrom; 
    UINT idFrom; 
    UINT code; 
} NMHDR

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
    NMHDR     hdr;        // required for all WM_NOTIFY messages
    LPTSTR    lpszText;   // see below
    WCHAR     szText[80]; // buffer for tool tip text
    HINSTANCE hinst;      // see below
    UINT      uflags;     // flag indicating how to interpret the idFrom member of the NMHDR structure that is included in the structure
} TOOLTIPTEXT, FAR *LPTOOLTIPTEXT

CLASS TToolbar FROM TControl

   DATA abuttons
   DATA aBitmaps
   DATA hParent
   DATA Created
   DATA hBmp
   DATA ntProc
   DATA nBtn HIDDEN
   DATA aMenus
   VAR nImg,hBMInst,nBMId,xBtn,yBtn,xBmp,yBmp

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

   ::Msgs      := IFNIL( ::Msgs, {WM_SIZE}, ::Msgs )
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

   //__objAddData( self, oBtn:name )
   //__ObjSetValueList( self, { { oBtn:name, oBtn } } )

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
   ::handle := CreateToolBarEx( ::Parent:handle, ::Style, ::Id, ::nImg,::hBMInst,::nBMId,cButtons,LEN(::aButtons),;
                                ::xbtn,::yBtn,::xBmp,::yBmp, tbb:sizeof())
   ::ntProc:=SetProcedure(::Parent:handle,{|hWnd, nMsg,nwParam,nlParam| ::tbProc(nMsg,nwParam,nlParam)},{WM_NOTIFY,WM_COMMAND})
   ::SendMessage(TB_SETEXTENDEDSTYLE,0,TBSTYLE_EX_DRAWDDARROWS )
   ::Created:=.T.
return( self )


*-----------------------------------------------------------------------------*

METHOD tbProc(nMsg,nwParam,nlParam) CLASS TToolBar

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
     Hdr:Buffer(peek(nlParam,Hdr:sizeof))
     DO CASE
     CASE Hdr:code==TTN_NEEDTEXT
       IF (n:=ASCAN(::aButtons,{|btn| btn:id==Hdr:idFrom})) > 0
         Ttt IS TOOLTIPTEXT
         Ttt:Buffer(peek(nlParam,Ttt:sizeof))
         Ttt:lpszText:=::aButtons[n]:hint
         poke(nlParam,Ttt:value,Ttt:sizeof)
       ENDIF

     CASE Hdr:code==TBN_QUERYINSERT
         RETURN(1)

     CASE Hdr:code==TBN_QUERYDELETE
         RETURN(1)

     CASE Hdr:code==TBN_GETBUTTONINFO
         Nmt IS NMTOOLBAR
         nmt:buffer(peek(nlParam,nmt:sizeof))
         RETURN(1)

     ENDCASE
   ENDCASE

   RETURN( CallWindowProc(::ntProc,::Parent:handle,nMsg,nwParam,nlParam))

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

