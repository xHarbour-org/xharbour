// Augusto Infante
// Whoo.lib

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TGroupBox FROM TControl

   METHOD New() CONSTRUCTOR

ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TGroupBox

   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Caption   := cCaption
   ::Left      := nLeft
   ::Top       := nTop
   ::Width     := IFNIL( nWidth , IFNIL( ::width , 185, ::width ), nWidth )
   ::Height    := IFNIL( nHeight, IFNIL( ::height, 105, ::height), nHeight)
   ::Name      := 'button'
   ::Style     := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_GROUPBOX

   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*