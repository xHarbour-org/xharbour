#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

#define LB_OKAY             0
#define LB_ERR              (-1)
#define LB_ERRSPACE         (-2)

CLASS TListBox FROM TControl
   METHOD New() CONSTRUCTOR
   METHOD Add( cText )           INLINE SendMessage( ::handle, LB_ADDSTRING, 0, cText)
   METHOD Insert( cText, nLine ) INLINE SendMessage( ::handle, LB_INSERTSTRING, nLine, cText )
   METHOD Del(nLine)             INLINE SendMessage( ::handle, LB_DELETESTRING, nLine, 0)
   METHOD Get()
   METHOD Set(nLine)             INLINE SendMessage( ::handle, LB_SETCURSEL, nLine, 0)
   METHOD SetSel(nLine,lSel)     INLINE SendMessage( ::handle, LB_SETSEL, if(lSel,1,0), MAKELONG(nLine, 0))
ENDCLASS

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TListBox
   ::Name      := 'listbox'
   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Caption   := cCaption
   ::Left      := nLeft
   ::Top       := nTop
   ::width     := IFNIL( nWidth, IFNIL( ::width, 121, ::width), nWidth)
   ::Height    := IFNIL( nHeight, IFNIL( ::height, 97, ::height), nHeight)
   ::Style     := WS_CHILD + WS_VISIBLE + WS_TABSTOP + LBS_STANDARD
return( super:new( oParent ) )

METHOD Get(nLine) CLASS TListBox
   local nLen, cBuf
   cBuf := space(SendMessage(::handle, LB_GETTEXTLEN, nLine, 0) + 1)
   nLen := SendMessage(::handle, LB_GETTEXT, nLine, @cBuf)
return( if(nLen == LB_ERR, nil, left(cBuf, nLen) ) )

