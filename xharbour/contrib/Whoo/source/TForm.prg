#include "hbclass.ch"
#include "windows.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

CLASS TForm FROM TWindow
   
   METHOD New()
   METHOD AddControl()
   METHOD AddButton()
   
ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent ) CLASS TForm

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .F.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::FormType  := RCF_WINDOW
   ::lRegister := .T.
   ::ExStyle   := WS_EX_CONTROLPARENT
   
return( super:New( oParent ) )


METHOD AddButton( cName, cCaption, nId, nLeft, nTop, nWidth, nHeight, oBtn ) CLASS TForm
   ::AddControl( cName, 'button', cCaption, nId, nLeft, nTop, nWidth, nHeight, oBtn )
return( oBtn )



METHOD AddControl( cName, cClass, cCaption, nId, nLeft, nTop, nWidth, nHeight, oObj ) CLASS TForm

   __objAddData( self, cName )

   oObj := if( oObj != NIL, oObj:New( cClass, self, cCaption, nId, nLeft, nTop, nWidth, nHeight ),;
                            TControl():New( cClass, self, cCaption, nId, nLeft, nTop, nWidth, nHeight) )

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

