************************************************************
* wndtest - test for XWT windows 
*
* Giancarlo Niccolai et al.:
* (PLZ. add your copyright...)
*
* $Id: wndtest.prg,v 1.2 2003/10/13 11:54:08 jonnymind Exp $
*

#include "xwt.ch"
#include "xwtcmd.ch"


PROCEDURE Main()
   LOCAL oWindow

   XwtInit()

   /*** Window creation ****/
   DEFINE WINDOW oWindow TITLE "New FrameWindow from XWT" MENU BuildMenu()

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

FUNCTION BuildMenu()
   Local oMenu,oMenuItem,oMenuSec,oMenuHelp

   MENU oMenu PROMPT "File"
        MENUITEM oMenuItem PROMPT "Op_en" OF oMenu ACTION @FileEvent()
        MENUITEM oMenuItem PROMPT "Font s_el"  OF oMenu ACTION @FileEvent()
        MENUITEM PROMPT "Close" ACTION @FileEvent() OF oMenu ACTION @FileEvent()

        MENU oMenuSec PROMPT "Sub_Menu" OF oMenu 
             MENUITEM PROMPT "Opt_1" ID 10 OF oMenuSec ACTION @FileEvent()
             MENUITEM PROMPT "Opt_2" ID 11 OF oMenuSec ACTION @FileEvent()

        MENUITEM PROMPT "_QUIT"  ID 99 ACTION @FileEvent() OF oMenu SIZE 40

RETURN( { oMenu }  )

FUNCTION FileEvent( oEvent )
   ?  "Menu activated: ", oEvent:oSender:nId
   IF oEvent:oSender:nId == 99
      IF .not. ConfirmQuit( oEvent )
         XwtQuit()
      ENDIF
   ENDIF
RETURN .F.

