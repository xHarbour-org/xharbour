#include "hbclass.ch"
#include "windows.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

CLASS TForm FROM TWindow
   
   METHOD New()
   METHOD Add()
   
ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent ) CLASS TForm

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .F.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::FormType  := RCF_WINDOW
   ::lRegister := .T.
   ::ExStyle   := 0
   
return( super:New( oParent ) )


METHOD Add( cName, oObj, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TForm

   __objAddData( self, cName )

   oObj := oObj:New( self, cCaption, nId, nLeft, nTop, nWidth, nHeight )

   __ObjSetValueList( self, { { cName, oObj } } )

   oObj:Create()

return( oObj )




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
   ::FormType  := RCF_WINDOW
   ::lRegister := .T.

return( super:New( oParent ) )

