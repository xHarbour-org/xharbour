
// WHOO.LIB

#include "hbclass.ch"
#include "windows.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TPanel FROM TForm

   METHOD New()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TPanel
   
   super:New( oParent )
   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .F.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::FormType  := RCF_DIALOG
   ::lRegister := .f.
   ::lControl  := .F.
   ::Modal     := .T.
   
   RETURN( self )

*-----------------------------------------------------------------------------*