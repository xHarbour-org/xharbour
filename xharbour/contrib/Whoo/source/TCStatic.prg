
// WHOO.LIB

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TStatic FROM TControl

   METHOD New() CONSTRUCTOR

ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TStatic
   
   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Caption   := cCaption
   ::Left      := nLeft
   ::Top       := nTop
   ::Width     := IFNIL( nWidth, IFNIL( ::Width,  80, ::Width) , nWidth )
   ::Height    := IFNIL( nHeight,IFNIL( ::height, 20, ::height), nHeight) 
   ::Name      := 'static'
   ::Style     := WS_CHILD + WS_VISIBLE + SS_LEFT
   
   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*