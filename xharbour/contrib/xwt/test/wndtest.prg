************************************************************
* wndtest - test for XWT windows 
*
* Giancarlo Niccolai et al.:
* (PLZ. add your copyright...)
*
* $Id: wndtest.prg,v 1.1 2003/10/09 23:18:34 jonnymind Exp $
*

#include "xwt.ch"
#include "xwtcmd.ch"


PROCEDURE Main()
   LOCAL oWindow

   XwtInit()

   /*** Window creation ****/
   DEFINE WINDOW oWindow TITLE "New FrameWindow from XWT"
   oWindow:AddEventListener( XWT_E_DESTROY_REQ, @ConfirmQuit() )
   
   oWindow:AddEventListener( XWT_E_DESTROY, @DoQuit() )

   /*** Showing window ***/
   oWindow:Resize( 200, 200 )
   oWindow:Show()

   /*** Main LOOP ***/
   XwtMainLoop()
RETURN

FUNCTION ConfirmQuit( oSender )
   IF XWT_MsgBox( "Really want to quit?", ;
         XWT_MSGBOX_YES + XWT_MSGBOX_NO, XWT_MSGBOX_QUESTION,;
         oSender, "Quit?" ) == XWT_MSGBOX_YES
      RETURN .F. 
   ENDIF
RETURN .T.

FUNCTION DoQuit()
   XwtQuit()
RETURN .F. // Allow prosecuting of EVENT LISTENING