// Augusto Infante
// Whoo.lib

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wingdi.ch"
#include "debug.ch"
#include "wintypes.ch"
#include "cstruct.ch"

pragma pack(4)
typedef struct tagRECT { ;
    LONG left; 
    LONG top; 
    LONG right; 
    LONG bottom; 
} RECT 

typedef struct tagDRAWITEMSTRUCT {;
    UINT  CtlType; 
    UINT  CtlID; 
    UINT  itemID; 
    UINT  itemAction; 
    UINT  itemState; 
    HWND  hwndItem; 
    HDC   hDC; 
    RECT  rcItem; 
    DWORD itemData; 
} DRAWITEMSTRUCT
  
*------------------------------------------------------------------------------*

CLASS TComboBox FROM TControl
   DATA odraw AS LOGICAL INIT .F.
   VAR nYes, nNo
   VAR npProc
   METHOD New() CONSTRUCTOR
   METHOD AddString( cText )        INLINE ::SendMessage( CB_ADDSTRING, 0, cText)
   METHOD InsertString(cText,nLine) INLINE ::SendMessage( CB_INSERTSTRING, nLine, cText )
   METHOD DeleteString(nLine)       INLINE ::SendMessage( CB_DELETESTRING, nLine, 0)
   METHOD SetCurSel(nLine)          INLINE ::SendMessage( CB_SETCURSEL, nLine, 0)
   METHOD FindString(nStart,cStr)   INLINE ::SendMessage( CB_FINDSTRING, IFNIL(nStart,-1,nStart), cStr)
   METHOD FindExact(nStart,cStr)    INLINE ::SendMessage( CB_FINDSTRINGEXACT, IFNIL(nStart,-1,nStart), cStr)
   METHOD GetCount()                INLINE ::SendMessage( CB_GETCOUNT, 0, 0)
   METHOD GetCurSel()               INLINE ::SendMessage( CB_GETCURSEL, 0, 0)
   METHOD Dir(nAttr, cFileSpec)     INLINE ::SendMessage( CB_DIR, nAttr, cFileSpec)
   METHOD OnCreate()
   METHOD CBProc()
ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight, lOdraw ) CLASS TComboBox
   DEFAULT lOdraw TO ::oDraw
   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY,WM_MEASUREITEM}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Left      := nLeft
   ::Top       := nTop
   ::Width     := IFNIL( nWidth , IFNIL( ::width , 80, ::width ), nWidth )
   ::Height    := IFNIL( nHeight, IFNIL( ::height, 20, ::height), nHeight)
   ::Name      := 'combobox'
   ::Style     := WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + CBS_DROPDOWNLIST + WS_VSCROLL + CBS_HASSTRINGS
   ::oDraw     := lOdraw
   if ::oDraw
      ::Style += CBS_OWNERDRAWFIXED
   endif
   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*

METHOD OnCreate() CLASS TComboBox
if ::oDraw
   ::npProc := SetProcedure( ::Parent:handle, HB_ObjMsgPtr( self, "CBProc" ), {WM_DRAWITEM}, self)
endif
return(super:OnCreate())

METHOD CBProc( hWnd, nMsg, nwParam, nlParam ) CLASS TComboBox
   LOCAL lselected
   LOCAL aclip, aRect
   LOCAL itemTxt, cText
   LOCAL nLen, n
   local dis
   do case
   case nMsg == WM_DRAWITEM
        dis IS DRAWITEMSTRUCT
        dis:Buffer( peek( nlParam, dis:sizeof() ) )
        if dis:CtlID==::id
           lselected := And( dis:itemState, ODS_SELECTED ) > 0
           aclip := { }
           aclip := { dis:rcItem:Left , dis:rcItem:Top  , ;
                      dis:rcItem:Right  , dis:rcItem:Bottom  }
           IF And( dis:itemAction, ODA_DRAWENTIRE ) > 0 .OR. And( dis:itemAction, ODA_SELECT ) > 0
              SetTextColor( dis:hDC  , GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
              SetBkColor( dis:hDC  , GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )
              nLen := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
              itemTxt := Space( nLen + 1 )
              SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )
              itemTxt:=left(itemTxt,nLen)
              cText := ""
              aRect := ACLONE(aClip)
              for n:=1 to nLen+1
                  if substr(itemTxt,n,1)==chr(9).or.n==nLen+1
                     exttextout( dis:hDC , dis:rcItem:Left + aRect[1]+2, dis:rcItem:Top , ;
                                 ETO_OPAQUE + ETO_CLIPPED, aRect, cText )
                     cText:=""
                     aRect[1]+=70
                     loop
                  endif
                  cText+=substr(itemTxt,n,1)
              next
           endif
           if And( dis:itemState, ODS_FOCUS ) > 0 .OR. And( dis:itemAction, ODA_FOCUS ) > 0
              drawfocusrect( dis:hDC  , aclip )
           endif
           return(1)
        endif
   endcase
return( CallWindowProc( ::npProc, ::Parent:handle, nMsg, nwParam, nlParam ) )
