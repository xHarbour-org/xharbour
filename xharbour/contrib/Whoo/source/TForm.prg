#include "hbclass.ch"
#include "windows.ch"

CLASS TForm FROM TWindow
   
   METHOD New()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent ) CLASS TForm

   super:New( oParent )
   ::WndProc := 'FormProc'
   ::Msgs    := -1
   ::FrameWnd:= .F.
   ::Style   := WS_OVERLAPPEDWINDOW + WS_DLGFRAME
   ::ExStyle := WS_EX_DLGMODALFRAME
   ::FormStyle := 0

return( self )

//----------------------------------------------------------------------------//

CLASS TFrame FROM TWindow
   
   METHOD New()

ENDCLASS


METHOD New( oParent ) CLASS TFrame

   ::WndProc := 'FormProc'
   ::Msgs    := -1
   ::FrameWnd:= .T.
   ::Style   := WS_OVERLAPPEDWINDOW
//   ::FormStyle := 1
   super:New( oParent )

return( self )

