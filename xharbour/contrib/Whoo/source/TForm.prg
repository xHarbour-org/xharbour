#include "hbclass.ch"
#include "windows.ch"

CLASS TForm FROM TWindow
   
   METHOD New()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent ) CLASS TForm

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .F.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::FormStyle := 1
   super:New( oParent )

return( self )

//----------------------------------------------------------------------------//

CLASS TFrame FROM TWindow
   
   METHOD New()

ENDCLASS


METHOD New( oParent ) CLASS TFrame

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .T.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::ExStyle   := WS_EX_APPWINDOW + WS_EX_CLIENTEDGE
   ::FormStyle := 1
   super:New( oParent )

return( self )

