#include "hbclass.ch"
#include "windows.ch"

CLASS TForm FROM TWindow
   
   METHOD New()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent ) CLASS TForm

   super:New( oParent )
   ::WndProc := {| hWnd, nMsg, nwParam, nlParam| ::FormProc( hWnd, nMsg, nwParam, nlParam ) }
   ::Msgs    := -1

return( self )
