
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
    char      szText[80]; // buffer for tool tip text
    HINSTANCE hinst;      // see below
    UINT      uflags;     // flag indicating how to interpret the idFrom member of the NMHDR structure that is included in the structure
} TOOLTIPTEXT, FAR *LPTOOLTIPTEXT

/*
typedef struct tagNMTBHOTITEM {;
    NMHDR hdr;
    int idOld;
    int idNew;
    DWORD dwFlags;
} NMTBHOTITEM, FAR *LPNMTBHOTITEM
*/

// based on cToolBar  class
*-----------------------------------------------------------------------------*

CLASS TOOLBAR

 DATA abuttons
 DATA aBitmaps
 DATA hParent
 DATA hWnd
 DATA nId
 DATA nStyle
 DATA Created
 DATA aText
 DATA hBmp
 DATA nProc
 DATA aTips
 DATA nBtn HIDDEN
 DATA aMenus
METHOD Init() CONSTRUCTOR

METHOD AddButton
METHOD AddBitmap
METHOD AddString
method Create
METHOD createbuttons
METHOD tbProc

method setsizes(xBtn,yBtn,xImg,yImg )
method setheight(nHeight )
method loadbitmap
method setbitmap
method setbuttons
method commandtoindex
method GetItemId
method getitemrect
method getbuttonstyle
method getbuttoninfo
method setbuttoninfo
method getbuttontext
method setbuttontext
method gettollbarctrl
method disable
method enable
method disableall
method enableall
method CheckButton
method IsButtonChecked
method AddMenu
ENDCLASS


*-----------------------------------------------------------------------------*

METHOD Init()

  InitCommonControlsEx(ICC_BAR_CLASSES)
  ::aButtons:={}
  ::aTips   :={}
  ::nStyle  :=0
  ::nId     :=0
  ::Created := .F.
  ::aText   :={}
  ::aBitmaps:={}
  ::aMenus  :={}
RETURN(self)


*-----------------------------------------------------------------------------*

METHOD AddBitmap(hInst, nhIdBmp, nButtons)

  LOCAL tbab IS TBADDBITMAP

    DEFAULT nButtons TO 1

    tbab:hInst := hInst
    tbab:nId   := nhIdBmp

    AADD(::aBitmaps,{tbab,nButtons})
    IF ::created
      SendMessage(::hWnd,TB_ADDBITMAP,nButtons,tbab:value)
    ENDIF


    RETURN(1)

*-----------------------------------------------------------------------------*

METHOD AddButton(nIndex, nId, nState, nStyle, ncargo, nString, cText, cToolTip )

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
     SendMessage(::hWnd,TB_ADDBUTTONS,1,tbb:value)
  Endif


RETURN(self)

METHOD AddMenu(nId,aMenuItems)
AADD(::aMenus,{nId,aMenuItems})
return(self)

*-----------------------------------------------------------------------------*
METHOD addstring(cText)

  IF ::created
     SendMessage(::hWnd,TB_ADDSTRING,0,cText)
  Else
     AADD(::aText,cText)
  Endif

  RETURN(self)

*-----------------------------------------------------------------------------*
//  HWND CreateToolbarEx( HWND hwnd, DWORD ws, UINT wID,int nBitmaps,HINSTANCE hBMInst,
//                        UINT wBMID,LPCTBBUTTON lpButtons,int iNumButtons,int dxButton,
//                        int dyButton, int dxBitmap, int dyBitmap,UINT uStructSize );

METHOD Create(hParent,nStyle,nId,nImg,hBMInst,nBMId,xBtn,yBtn,xBmp,yBmp)

   LOCAL cButtons:=""
   LOCAL cStrings:=""
   LOCAL tbb IS TBBUTTON
   LOCAL i


   ::hParent:=hParent
   ::nStyle :=IFNIL(nStyle,TBSTYLE_FLAT+WS_CHILD+WS_VISIBLE,nStyle)
   ::nId    :=IFNIL(nId,0,nId)

   if ISNIL(hBMInst) .AND. ISNIL(nBMId)
     hBMInst:=HINST_COMMCTRL
     nBMId  :=IDB_STD_LARGE_COLOR
   endif

   FOR i:=1 TO LEN(::aButtons)
      cButtons+=::aButtons[i]:Value
   NEXT

// ::hWnd:=CreateWindowEx(0,TOOLBARCLASSNAME,"",::nStyle,0,0,300,30,::hParent,::nId)


 ::hWnd:=CreateToolBarEx(::hParent,::nStyle,::nId,nImg,hBMInst,nBMId,cButtons,LEN(::aButtons),;
                 xbtn,yBtn,xBmp,yBmp, tbb:sizeof())

 ::nProc:=SetProcedure(::hParent,{|hWnd, nMsg,nwParam,nlParam| ::tbProc(nMsg,nwParam,nlParam)},{WM_NOTIFY})


//   SendMessage(::hWnd,TB_BUTTONSTRUCTSIZE,::aButtons[1]:sizeof,0)

 //  FOR i:=1 TO LEN(::aBitmaps)
 //    SendMessage(::hWnd,TB_ADDBITMAP,::aBitmaps[i,2],::aBitmaps[i,1]:value)
 //  NEXT

   sendmessage(::hwnd,TB_SETEXTENDEDSTYLE,0,TBSTYLE_EX_DRAWDDARROWS )


   //SendMessage(::hWnd,TB_ADDBUTTONS,LEN(::aButtons),cButtons)




   FOR i:=1 to LEN(::aText)
      SendMessage(::hWnd,TB_ADDSTRING,0,::aText[i])
   NEXT

   

   ::Created:=.T.

RETURN(::hWnd)


*-----------------------------------------------------------------------------*

METHOD tbProc(nMsg,nwParam,nlParam)

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
     /*
     CASE Hdr:code==TBN_DROPDOWN
         Nmt IS NMTOOLBAR
         nmt:buffer(peek(nlParam,nmt:sizeof))
         IF (n:=ASCAN(::aMenus,{|a| a[1]==nmt:iItem})) > 0
            ::nBtn:=nmt:iItem
            SendMessage(::hWnd,TB_SETSTATE,nmt:iItem,MAKELONG(TBSTATE_ENABLED+TBSTATE_PRESSED,0) )
            hMenu = CreatePopupMenu( )
            FOR x:=1 TO LEN(::aMenus[n][2])
                AppendMenu( hMenu, MF_ENABLED + MF_STRING,::aMenus[n][2][1],::aMenus[n][2][2])
            NEXT
            aRect:=GetToolBarItemRect(::hWnd,n-1)
            TrackPopupMenu( hMenu,TPM_TOPALIGN + TPM_HORIZONTAL, aRect[1], aRect[4], 0, ::hWnd)
            DestroyMenu(hMenu)
         end
         */
     CASE Hdr:code==TTN_NEEDTEXT
       IF (n:=ASCAN(::aButtons,{|btn| btn:idCommand==Hdr:idFrom})) > 0
         Ttt IS TOOLTIPTEXT
         Ttt:Buffer(peek(nlParam,Ttt:sizeof))
         Ttt:lpszText:=::aTips[n] //"ID:"+STR(Hdr:IdFrom)
         poke(nlParam,Ttt:value,Ttt:sizeof)
       ENDIF

     CASE Hdr:code==TBN_QUERYINSERT
         RETURN(1)
     CASE Hdr:code==TBN_QUERYDELETE
         RETURN(1)
     CASE Hdr:code==TBN_GETBUTTONINFO
         Nmt IS NMTOOLBAR
         nmt:buffer(peek(nlParam,nmt:sizeof))
         /*
         int     iItem;      // cmd
         TBBUTTON tbButton;
         int     cchText;    // str len
         LPSTR   pszText;    // btn text
         RECT    rcButton;   // new (!)
         */
         RETURN(1)

     ENDCASE

   ENDCASE

RETURN( CallWindowProc(::nProc,::hParent,nMsg,nwParam,nlParam))

*-----------------------------------------------------------------------------*

METHOD CreateButtons()

   LOCAL aSize
   LOCAL i




   FOR i:=1 TO LEN(::aBitmaps)




   NEXT


RETURN(NIL)








*-----------------------------------------------------------------------------*


method setsizes(xBtn,yBtn,xImg,yImg )
RETURN(self)
*-----------------------------------------------------------------------------*

method setheight(nHeight )
RETURN(self)
*-----------------------------------------------------------------------------*

method loadbitmap
RETURN(self)
*-----------------------------------------------------------------------------*

method setbitmap
RETURN(self)
*-----------------------------------------------------------------------------*

method setbuttons
RETURN(self)
*-----------------------------------------------------------------------------*

method commandtoindex
RETURN(self)
*-----------------------------------------------------------------------------*

method GetItemId
RETURN(self)
*-----------------------------------------------------------------------------*

method getitemrect(nIndex)
RETURN(GetToolbarItemRect(::hWnd,nIndex))

*-----------------------------------------------------------------------------*

method getbuttonstyle
RETURN(self)
*-----------------------------------------------------------------------------*

method getbuttoninfo
RETURN(self)
*-----------------------------------------------------------------------------*

method setbuttoninfo
RETURN(self)
*-----------------------------------------------------------------------------*

method getbuttontext
RETURN(self)
*-----------------------------------------------------------------------------*

method setbuttontext
RETURN(self)
*-----------------------------------------------------------------------------*

method gettollbarctrl

RETURN(self)
*-----------------------------------------------------------------------------*

method disable(nBtn)
SendMessage(::hWnd,TB_ENABLEBUTTON,nBtn,0)
RETURN(SELF)

*-----------------------------------------------------------------------------*

method enable(nBtn,lFlag)
DEFAULT lFlag TO .T.
SendMessage(::hWnd,TB_ENABLEBUTTON,nBtn,If(lFlag,1,0))
RETURN(SELF)

*-----------------------------------------------------------------------------*

method disableall()
AEVAL(::aButtons,{|btn| ::disable(btn:idCommand)})
return(self)

*-----------------------------------------------------------------------------*

method enableall()
AEVAL(::aButtons,{|btn| ::enable(btn:idCommand)})
return(self)

method CheckButton(nBtn,lFlag)
DEFAULT lFlag TO !::IsButtonChecked(nBtn)
SendMessage(::hWnd,TB_CHECKBUTTON,nBtn,If(lFlag,1,0))
RETURN(SELF)

method IsButtonChecked(nBtn)
RETURN(IIF(SendMessage(::hWnd,TB_ISBUTTONCHECKED,nBtn,0)==0,.F.,.T.))
