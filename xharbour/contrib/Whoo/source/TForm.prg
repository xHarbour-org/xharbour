#include "hbclass.ch"
#include "windows.ch"

CLASS TForm FROM TWindow
   
   METHOD New()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New() CLASS TForm
   ::bProc := {| hWnd, nMsg, nwParam, nlParam| ::FormProc( hWnd, nMsg, nwParam, nlParam ) }
   ::Msgs  := -1
return Self

