// Augusto Infante
// Whoo.lib

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wingdi.ch"
#include "debug.ch"
#include "wintypes.ch"
#include "cstruct.ch"

*------------------------------------------------------------------------------*

CLASS TComboBox FROM TControl
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
   METHOD SetItemHeight(n,nHeight)  INLINE ::SendMessage( CB_SETITEMHEIGHT, n, nHeight )
ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight) CLASS TComboBox
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
   RETURN( super:new( oParent ) )
