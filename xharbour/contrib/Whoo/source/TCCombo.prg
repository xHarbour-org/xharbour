#include "windows.ch"
#include "HbClass.ch"
#include "debug.ch"

CLASS TComboBox FROM TControl
   METHOD New() CONSTRUCTOR
ENDCLASS

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TComboBox
   ::id       := nId
   ::lRegister:= .F.
   ::lControl := .T.
   ::Msgs     := {WM_DESTROY}
   ::WndProc  := 'FormProc'
   ::Caption  := cCaption
   ::Left     := nLeft
   ::Top      := nTop
   ::Width    := nWidth
   ::Height   := nHeight 
   ::Name     := 'combobox'
   ::Style    := WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + CBS_DROPDOWNLIST + WS_VSCROLL
return( super:new( oParent ) )

