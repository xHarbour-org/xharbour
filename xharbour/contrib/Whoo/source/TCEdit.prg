#include "windows.ch"
#include "HbClass.ch"
#include "debug.ch"

CLASS TEdit FROM TControl
   METHOD New() CONSTRUCTOR
ENDCLASS

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TEdit
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
   ::Name     := 'edit'
   ::ExStyle  := WS_EX_CLIENTEDGE
   ::Style    := WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + ES_AUTOHSCROLL + ;
                 ES_AUTOVSCROLL + ES_LEFT + ES_WANTRETURN + ES_MULTILINE
return( super:new( oParent ) )

