
#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"


*-----------------------------------------------------------------------------*

CLASS oCtrlMask FROM TWindow

   DATA Ctrl

   METHOD New() CONSTRUCTOR

   //METHOD OnMouseMove()
   //METHOD OnLButtonDown()
   //METHOD OnLButtonDown()
   METHOD OnSize()
   METHOD OnMove()
   //METHOD OnNCHitTest()
   //METHOD OnSetFocus()
   //METHOD OnKillFocus()
   //METHOD OnPaint()
   //METHOD OnNCPaint()
   //METHOD OnEraseBkgnd()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oCtrl ) CLASS oCtrlMask

   LOCAL aPt:={ oCtrl:left, oCtrl:top }

   //ClientToScreen(oCtrl:Parent:handle,aPt)
   ::Ctrl      := oCtrl
   ::left      := aPt[1]
   ::top       := aPt[2]
   ::width     := oCtrl:width
   ::height    := oCtrl:height
   ::style     := WS_CHILD+WS_VISIBLE+WS_THICKFRAME
   ::name      := "CTRLMASK"
   ::color     := GetStockObject(NULL_BRUSH)
   ::ExStyle   := 0 //WS_EX_TRANSPARENT
   ::Cursor    := LoadCursor(,IDC_WAIT)
   ::Caption   :=""
   ::lRegister :=.T.
   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  :=.T.
   ::FormType := 1

   RETURN( super:new( oCtrl:Parent ) )


*-----------------------------------------------------------------------------*

METHOD OnSize(nFlag,width,height)

   LOCAL aRect:=GetWindowRect(::Handle)
   LOCAL aPt:={aRect[1],aRect[2]}

   ScreenToClient(::Parent:Handle,aPt)
   view MoveWindow(::Ctrl:Handle,aPt[1],aPt[2],aRect[3]-aRect[1],aRect[4]-aRect[2],.T.)
   //view RedrawWindow(::Ctrl:Handle,{aPt[1],aPt[2],width,height},,RDW_INTERNALPAINT)


   view "OnSize"
   RETURN(NIL)

*-----------------------------------------------------------------------------*

METHOD OnMove(wParam, xPos, yPos )
    /*
    LOCAL aPt:={xPos,yPos}
    LOCAL aRect:=GetWindowRect(::Handle)

    ScreenToClient(::Parent:Handle,aPt)
    MoveWindow(::Ctrl:Handle,aPt[1],aPt[2],aRect[3]-aRect[1],aRect[4]-aRect[2],.T.)
    */

   view "OnMove"
   RETURN(NIL)

