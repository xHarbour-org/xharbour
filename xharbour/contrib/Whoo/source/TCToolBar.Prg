// Augusto Infante - Andrew Wos
// Whoo.lib

#define TPM_LEFTBUTTON        0
#define TPM_CENTERALIGN       4
#define TPM_RIGHTALIGN        8

#define TPM_TOPALIGN              0
#define TPM_VCENTERALIGN         16
#define TPM_BOTTOMALIGN          32

#define TPM_HORIZONTAL            0     // Horz alignment matters more
#define TPM_VERTICAL             64     // Vert alignment matters more
#define TPM_NONOTIFY            128     // Don't send any notification msgs


// What32.Lib
// Tollbar class

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
   DATA aText
   DATA hBmp
   DATA ntProc
   DATA aTips
   DATA nBtn HIDDEN
   DATA aMenus
   VAR nImg,hBMInst,nBMId,xBtn,yBtn,xBmp,yBmp

   METHOD New() CONSTRUCTOR

   METHOD AddButton()
   METHOD AddBitmap()
   METHOD AddString()
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

ENDCLASS


*-----------------------------------------------------------------------------*

METHOD New( oParent, nId, nImg, hBMInst, nBMId, xBtn, yBtn, xBmp, yBmp ) CLASS TToolBar

   InitCommonControlsEx(ICC_BAR_CLASSES)
  
   ::nImg    := nImg
   ::hBMInst := hBMInst
   ::nBMId   := nBMId
   ::xBtn    := xBtn
   ::yBtn    := yBtn
   ::xBmp    := xBmp
   ::yBmp    := yBmp
  
   ::aButtons:={}
   ::aTips   :={}
   ::Style   :=TBSTYLE_FLAT + TBSTYLE_TOOLTIPS + WS_CLIPCHILDREN + ;
              WS_CLIPSIBLINGS + CCS_ADJUSTABLE + CCS_NODIVIDER + CCS_NORESIZE + WS_CHILD
   ::Id      := nId
   ::Created := .F.
   ::aText   :={}
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

METHOD AddButton(nIndex, nId, nState, nStyle, ncargo, nString, cText, cToolTip ) CLASS TToolBar

   LOCAL tbb IS TBBUTTON

   tbb:ibitmap   :=IFNIL(nIndex,-1,nIndex)
   tbb:idCommand :=nId // must be supplied
   tbb:fsState   :=IFNIL(nState,TBSTATE_ENABLED,nState)
   tbb:fsStyle   :=IFNIL(nStyle,TBSTYLE_BUTTON,nStyle)
   tbb:dwData    :=IFNIL(ncargo,0,nCargo)
   tbb:iString   :=IFNIL(nString,0,nString)

   AADD(::aButtons,tbb)
   AADD(::aTips,cToolTip)
   IF ::Created
     SendMessage(::handle,TB_ADDBUTTONS,1,tbb:value)
     
   ENDIF

RETURN(self)

*-----------------------------------------------------------------------------*

METHOD AddMenu(nId,aMenuItems)

   AADD(::aMenus,{nId,aMenuItems})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD addstring(cText) CLASS TToolBar

   IF ::created
      SendMessage(::handle,TB_ADDSTRING,0,cText)
   Else
      AADD(::aText,cText)
   Endif

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
   FOR i:=1 TO LEN(::aButtons)
       cButtons+=::aButtons[i]:Value
   NEXT
   ::handle := CreateToolBarEx( ::Parent:handle, ::Style, ::Id, ::nImg,::hBMInst,::nBMId,cButtons,LEN(::aButtons),;
                                ::xbtn,::yBtn,::xBmp,::yBmp, tbb:sizeof())
   ::ntProc:=SetProcedure(::Parent:handle,{|hWnd, nMsg,nwParam,nlParam| ::tbProc(nMsg,nwParam,nlParam)},{WM_NOTIFY})
   ::SendMessage(TB_SETEXTENDEDSTYLE,0,TBSTYLE_EX_DRAWDDARROWS )
   FOR i:=1 to LEN(::aText)
      ::SendMessage(TB_ADDSTRING,0,::aText[i])
   NEXT
   ::Created:=.T.

   RETURN( self )


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
   CASE nMsg==WM_NOTIFY
     Hdr IS NMHDR
     Hdr:Buffer(peek(nlParam,Hdr:sizeof))
     DO CASE
     CASE Hdr:code==TTN_NEEDTEXT
       IF (n:=ASCAN(::aButtons,{|btn| btn:idCommand==Hdr:idFrom})) > 0
         Ttt IS TOOLTIPTEXT
         Ttt:Buffer(peek(nlParam,Ttt:sizeof))
         Ttt:lpszText:=::aTips[n]
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

   AEVAL(::aButtons,{|btn| ::disable(btn:idCommand)})

   RETURN(self)

*-----------------------------------------------------------------------------*

METHOD enableall() CLASS TToolBar

   AEVAL(::aButtons,{|btn| ::enable(btn:idCommand)})

   RETURN(self)

*-----------------------------------------------------------------------------*