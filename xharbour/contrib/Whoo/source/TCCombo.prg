
// WHOO.LIB

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TComboBox FROM TControl

   METHOD New() CONSTRUCTOR

ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight ) CLASS TComboBox
   
   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Left      := nLeft
   ::Top       := nTop
   ::Width     := IFNIL( nWidth , IFNIL( ::width , 80, ::width ), nWidth )
   ::Height    := IFNIL( nHeight, IFNIL( ::height, 20, ::height), nHeight)
   ::Name      := 'combobox'
   ::Style     := WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + CBS_DROPDOWNLIST + WS_VSCROLL
   
   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*