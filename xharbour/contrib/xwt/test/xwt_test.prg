#include "xwt.ch"

Global oOtherBox

PROCEDURE MAIN()
   LOCAL oWindow, oButton, oMenuItem, oMenu, oMenuHelp
   LOCAL oMenuSec, oPane, oTextbox, oLabel
   LOCAL oImg

   XwtInit()

   oWindow:= XwtFrameWindow():New("HelloWorld")
   oWindow:AddEventListener( XWT_E_DESTROY_REQ, @XwtQuit() )
  
   oPane := XwtPane():New()
   oWindow:Add( oPane )
   oButton := XwtButton():New( "Hello" )
   oButton:AddListener( @DumpEvent() )
   oPane:Add( oButton )
   oLabel := XwtLabel():New( "Text: ",10,10 )
   
   oPane:Add( oLabel )
   oButton:Move( 50, 10 )
   
   oTextbox := XwtTextbox():New("A text", 10, 40 )
   oTextbox:AddEventListener(XWT_E_UPDATED, @BoxModified())
   oPane:Add( oTextbox  )
   oOtherBox := XwtTextbox():New("Another box", 10, 70 )
   oPane:Add( oOtherBox )
   
   oWindow:Resize( 200, 200 )
   oWindow:Show()
   oWindow:AddListener( @WindowReceive() )
   
   oMenu := XwtMenu():New( "File" )
   
   oMenuItem := XwtMenuItem():New( "Op_en", 1 , @FileEvent())

   oMenu:Add( oMenuItem )
   oMenu:Add( XwtMenuItem():New( "Close", 2 , @FileEvent()))
   oMenu:Add( XwtMenuItem():New( "QUIT", 3 , @FileEvent()))
   
   oMenuSec := XwtMenu():New( "Submenu" )
   oMenuSec:Add( XwtMenuItem():New( "Opt1", 10 , @FileEvent()))
   oMenuSec:Add( XwtMenuItem():New( "Opt2", 11 , @FileEvent()))
   oMenu:Add( oMenuSec )

   oMenuHelp := XwtMenu():New( "Help" )
   oMenuHelp:Add( XwtMenuItem():New( "About", 5 , @FileEvent()) )
   oMenuHelp:Add( XwtMenuItem():New( "Help", 6 , @FileEvent()) )

   oWindow:SetMenuBar( { oMenu, oMenuHelp } )

   oImg := XwtImage():New( "icon.png" )
   oImg:Move( 10, 100 )
   oImg:SetSensible()
   oPane:add( oImg )
   XwtMainLoop()
   
   oWindow:Destroy()
RETURN

FUNCTION WindowReceive( oEvent )
   ? "Received event at top level ", oEvent:nType, " from ", oEvent:oSender:GetText()
RETURN .F.

FUNCTION DumpEvent( oEvent )
   ? "Event type: ", oEvent:nType
   IF oEvent:nType == XWT_E_CLICKED
      IF XWT_MsgBox( "Are you really, really, sure?", ;
         XWT_MSGBOX_YES + XWT_MSGBOX_NO, XWT_MSGBOX_QUESTION) == XWT_MSGBOX_YES
            oEvent:oSender:Hide()
      ENDIF
   ENDIF
RETURN .F.

FUNCTION FileEvent( oEvent )
   ? "Menu activated: ", oEvent:oSender:nId
RETURN .F.

FUNCTION BoxModified( oEvent )
   ? "Text entered in box: ", oEvent:oSender:getText()
   oEvent:oSender:SetText( "Reset" )
   oOtherBox:SetFocus()
RETURN .F.
