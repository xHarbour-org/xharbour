// Augusto Infante
// Whoo.lib

#include "hbclass.ch"
#include "windows.ch"
#include "what32.ch"

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
   
   ::WndProc   := IFNIL(::WndProc,'FormProc',::WndProc)
   ::Msgs      := IFNIL(::Msgs,-1,::Msgs)
   ::FrameWnd  := .F.
   ::Style     := IFNIL(::Style,WS_OVERLAPPEDWINDOW,::Style)
   ::FormType  := RCF_DIALOG
   ::lRegister := IFNIL(::lRegister,.T.,::lRegister)
   ::lControl  := .F.
   ::ExStyle   := IFNIL(::ExStyle,0,::ExStyle)
   ::Modal     := IFNIL(::Modal,.F.,::Modal)

   RETURN( super:New( oParent ) )

*-----------------------------------------------------------------------------*