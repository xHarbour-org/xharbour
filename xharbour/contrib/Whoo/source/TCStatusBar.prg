
// WHOO.LIB

#Include "windows.ch"
#Include "statbar.ch"
#Include "hbclass.ch"
#include "wintypes.ch"
#include "what32.ch"
#Include "cstruct.ch"
#Include "debug.ch"

typedef struct _RECT { ;
    LONG left; 
    LONG top; 
    LONG right; 
    LONG bottom; 
} RECT 

*------------------------------------------------------------------------------*

CLASS TStatusBar FROM TControl

   DATA rect
   
   METHOD New() CONSTRUCTOR
   METHOD Create() INLINE ::handle := CreateStatusBar( ::Style, ::Caption, ::Parent:handle, ::Id  )
   METHOD SetPanels
   METHOD SetPanelText
   METHOD GetHeight
   METHOD GetPanelRect
   METHOD SetPanelIcon
   ACCESS height INLINE ::GetHeight()

ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, cCaption, nId ) CLASS TStatusBar

   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_SIZE}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Caption   := cCaption
   ::Left      := 0
   ::Top       := 0
   ::Width     := 0
   ::Height    := 0
   ::Name      := 'msctls_statusbar32'
   ::ExStyle   := 0
   ::Style     := WS_CHILD + WS_VISIBLE

   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*

METHOD SetPanels(aParts) CLASS TStatusBar

   LOCAL nLeft,n,aRect,bSizes := ""

   AEVAL(aParts,{|x| bSizes+=L2BIN(x)})

   RETURN( ::SendMessage( SB_SETPARTS, LEN( aParts ), bSizes ))

*------------------------------------------------------------------------------*

METHOD SetPanelText( nPart, cText ) CLASS TStatusBar

   RETURN( ::SendMessage( SB_SETTEXT, nPart, cText ))

*------------------------------------------------------------------------------*

METHOD GetHeight() CLASS TStatusBar

   ::rect := ::WindowRect()

   RETURN(::rect[4]-::rect[2])

*------------------------------------------------------------------------------*

METHOD GetPanelRect(nPanel) CLASS TStatusBar

   local aRect,aPt

   aRect:=StatusBarGetRect(::handle,nPanel)
   aRect[3]-=aRect[1]
   aRect[4]-=aRect[2]

   RETURN(aRect)

*------------------------------------------------------------------------------*

METHOD SetPanelIcon(nPanel,hIcon) CLASS TStatusBar

   SetStatusIcon( ::handle, 0, hIcon )

   RETURN(self)

*------------------------------------------------------------------------------*
