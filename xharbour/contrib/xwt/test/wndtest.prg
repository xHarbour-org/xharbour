************************************************************
* wndtest - test for XWT windows 
*
* Giancarlo Niccolai et al.:
* (PLZ. add your copyright...)
*
* $Id$
*

#include "xwt.ch"
#include "xwtcmd.ch"


PROCEDURE Main()
   LOCAL oWindow

   XwtInit()

   /*** Window creation ****/
   DEFINE WINDOW oWindow TITLE "New FrameWindow from XWT"
   oWindow:AddEventListener( XWT_E_DESTROY_REQ, @DoQuit() )

   /*** Showing window ***/
   oWindow:Resize( 200, 200 )
   oWindow:Show()

   /*** Main LOOP ***/
   XwtMainLoop()
RETURN

Procedure DoQuit()
   XwtQuit()
RETURN .F. // Allow prosecuting of EVENT LISTENING