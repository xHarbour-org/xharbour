#include "windows.ch"
#include "HbClass.ch"
#include "debug.ch"


CLASS TCheck FROM TControl
   METHOD New() CONSTRUCTOR
ENDCLASS

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TControl
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
   ::Name     := 'button'
   ::Style    := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_AUTOCHECKBOX + BS_LEFTTEXT
return( super:new( oParent ) )

