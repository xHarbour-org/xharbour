
// WHOO.LIB

#include "hbclass.ch"
#include "windows.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TFrame FROM TWindow
   DATA Controls INIT {}
   METHOD New()
   METHOD Add()
   
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TFrame

   InitCommonControls()
   InitCommonControlsEx(ICC_COOL_CLASSES)

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .T.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::ExStyle   := WS_EX_APPWINDOW
   ::FormType  := RCF_WINDOW
   ::lRegister := .T.

   return( super:New( oParent ) )

*-----------------------------------------------------------------------------*

METHOD Add( cName, oObj ) CLASS TFrame
   
   __objAddData( self, cName )
   __ObjSetValueList( self, { { cName, oObj } } )
   oObj:Create()
   
   return( oObj )

*-----------------------------------------------------------------------------*