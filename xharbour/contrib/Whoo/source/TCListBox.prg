#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

#define LB_OKAY             0
#define LB_ERR              (-1)
#define LB_ERRSPACE         (-2)

typedef struct _RECT { ;
    LONG left; 
    LONG top; 
    LONG right; 
    LONG bottom; 
} RECT 

CLASS TListBox FROM TControl
   ACCESS CurSel INLINE ::GetCurSel()

   METHOD New() CONSTRUCTOR
   METHOD GetString()
   METHOD GetItemRect()
   METHOD GetSelItems()
   METHOD AddString( cText )        INLINE ::SendMessage( LB_ADDSTRING, 0, cText)
   METHOD InsertString(cText,nLine) INLINE ::SendMessage( LB_INSERTSTRING, nLine, cText )
   METHOD DeleteString(nLine)       INLINE ::SendMessage( LB_DELETESTRING, nLine, 0)
   METHOD SetCurSel(nLine)          INLINE ::SendMessage( LB_SETCURSEL, nLine, 0)
   METHOD SetSel(nLine,lSel)        INLINE ::SendMessage( LB_SETSEL, if(lSel,1,0), MAKELPARAM(nLine, 0))
   METHOD FindString(nStart,cStr)   INLINE ::SendMessage( LB_FINDSTRING, IFNIL(nStart,-1,nStart), cStr)
   METHOD FindExact(nStart,cStr)    INLINE ::SendMessage( LB_FINDSTRINGEXACT, IFNIL(nStart,-1,nStart), cStr)
   METHOD GetCount()                INLINE ::SendMessage( LB_GETCOUNT, 0, 0)
   METHOD GetCurSel()               INLINE ::SendMessage( LB_GETCURSEL, 0, 0)
   METHOD Dir(nAttr, cFileSpec)     INLINE ::SendMessage( LB_DIR, nAttr, cFileSpec)
   METHOD GetSelCount()             INLINE ::SendMessage( LB_GETSELCOUNT, 0, 0)
ENDCLASS

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight ) CLASS TListBox
   ::Name      := 'listbox'
   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Left      := nLeft
   ::Top       := nTop
   ::width     := IFNIL( nWidth, IFNIL( ::width, 121, ::width), nWidth)
   ::Height    := IFNIL( nHeight, IFNIL( ::height, 97, ::height), nHeight)
   ::Style     := WS_CHILD + WS_VISIBLE + WS_TABSTOP + LBS_STANDARD
   ::ExStyle   := WS_EX_CLIENTEDGE
return( super:new( oParent ) )

METHOD GetString(nLine) CLASS TListBox
   local nLen, cBuf
   cBuf := space(SendMessage(::handle, LB_GETTEXTLEN, nLine, 0) + 1)
   nLen := SendMessage(::handle, LB_GETTEXT, nLine, @cBuf)
return( if(nLen == LB_ERR, nil, left(cBuf, nLen) ) )

METHOD GetItemRect( nLine) CLASS TListBox
   local rc IS RECT
   local cRect := space(16)
   SendMessage( ::handle, LB_GETITEMRECT, nLine, @cRect)
   rc:buffer( cRect )
return(rc:value)

METHOD GetSelItems() CLASS TListBox
   local n    := ::GetSelCount()
   local cBuf := space(n * 4)
   view n
   SendMessage( ::handle, LB_GETSELITEMS, n, @cBuf)
return( bin2array(cBuf, "int[" + str(n) + "]") )

/*






function LBGetSelLines(hLBox)
local i, a := LBGetSelItems(hLBox)
for i = 1 to len(a)
   a[i] = LBGetText(hLBox, a[i])
next i
return a



function LBGetText(hLBox, nLine)
local nLen, cBuf := space(SendMessage(hLBox, LB_GETTEXTLEN, nLine, 0) + 1)
nLen = SendMessage(hLBox, LB_GETTEXT, nLine, @cBuf)
return iif(nLen == LB_ERR, nil, left(cBuf, nLen))



function LBGetTextLen(hLBox, nLine)
return SendMessage(hLBox, LB_GETTEXTLEN, nLine, 0)



function LBInsertString(hLBox, cNewStr, nLine)
return SendMessage(hLBox, LB_INSERTSTRING, nLine, cNewStr)



procedure LBResetContent(hLBox)
SendMessage(hLBox, LB_RESETCONTENT, 0, 0)
return



function LBSelectString(hLBox, cStr, nStart)
if nStart == nil
   nStart = -1       // search from the start
endif
return SendMessage(hLBox, LB_SELECTSTRING, nStart, cStr)



function LBSetCurSel(hLBox, nLine)
return SendMessage(hLBox, LB_SETCURSEL, nLine, 0)



function LBSetHorzExtent(hLBox, nWidth)
return SendMessage(hLBox, LB_SETHORIZONTALEXTENT, nWidth, 0)



function LBSetSel(hLBox, nLine, lSelect)
return SendMessage(hLBox, LB_SETSEL, iif(lSelect,1,0), MAKELPARAM(nLine, 0))



function LBSetTabStops(hLBox, aTabs)
local nLen, cTabs := ""
if aTabs == nil
   cTabs := nLen := 0      // Windows default is 2 dialog units
else
   nLen = len(aTabs)
   aeval(aTabs, {|n| cTabs += i2bin(n)})
endif
return SendMessage(hLBox, LB_SETTABSTOPS, nLen, cTabs) != 0


*/