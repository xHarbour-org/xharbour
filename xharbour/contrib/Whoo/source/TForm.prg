#include "hbclass.ch"
#include "windows.ch"

CLASS TForm FROM TWindow
   
   METHOD New()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent ) CLASS TForm

   ::WndProc := {| hWnd, nMsg, nwParam, nlParam| ::FormProc( hWnd, nMsg, nwParam, nlParam ) }
   ::Msgs    := -1
   ::FrameWnd:= .F.

   super:New( oParent )

return( self )

//----------------------------------------------------------------------------//

CLASS TFrame FROM TWindow
   
   METHOD New()

ENDCLASS


METHOD New( oParent ) CLASS TFrame

   ::WndProc := {| hWnd, nMsg, nwParam, nlParam| ::FormProc( hWnd, nMsg, nwParam, nlParam ) }
   ::Msgs    := -1
   ::FrameWnd:= .T.

   super:New( oParent )

return( self )

