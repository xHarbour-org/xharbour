************************************************************
* XWT_TEST. 
*
* Giancarlo Niccolai et al.:
* (PLZ. add your copyright...)
*
* $Id: xwt_test.prg,v 1.27 2004/01/30 18:34:13 lculik Exp $
*


#include "xwt.ch"


#xcommand ? <data,...> => OutStd( <data>, HB_OSNewLine() )

PROCEDURE MAIN()
   LOCAL oWindow, oLabel, oLayout
   XwtInit()

   /*** Window creation ****/
   oWindow := XWTFrameWindow():New( "text: Welcome from xHarbour" )
   oWindow:SetStyle( "id: 1; height: 200; width: 500; x: 100; y: 100" )
   oWindow:addListener( @WindowReceive() )
   oWindow:AddEventListener( "destroy", @ReallyQuit() )
   oLayOut := XWTLayout():New( "id: 2; padding: 5; mode: vertical; expand: .T.; fill: .T.; frame: .T.; align: left" )
   oLabel := XWTLabel():New( "id: 3; text: Center; align:center; valign:top" )
   ? "LABEL1: ", oLabel:GetStyle()
   oLayOut:Add( oLabel )
   oLabel := XWTLabel():New( "id: 4; sensibility: .T.; text: Left; align:left; valign:center; width:200" ) 
      ? "LABEL2: ", oLabel:GetStyle()
   oLayOut:Add( oLabel )
   oLabel := XWTLabel():New( "id: 5; sensibility: .T.; text: Right; align:right; valign:center" )
   ? "LABEL3: ", oLabel:GetStyle()   
   oLayOut:Add( oLabel )
   oLayOut:Add( XWTTextBox("id:6; text: enter a text here" ) )
   oLayOut:Add( XWTButton("id:7; text: Click here!" ) )

   oWindow:connect( oLayOut )
   oWindow:show()
   ? "LABEL3 AFTER: ", oLabel:GetStyle()   
   
   /*** Main LOOP ***/
   XwtMainLoop()

   ? "END OF PROGRAM"
RETURN


FUNCTION ReallyQuit()
   IF XWT_MsgBox( "Really quit?", { "title" => "Quit query", "yes" =>.T., "no" => .T. } ) == XWT_MSGBOX_YES
      XwtQuit()
   ENDIF
RETURN .T.
   

FUNCTION WindowReceive( oEvent )
   LOCAL elem
   ?  "FROM", oEvent:oSender:GetText(),":", oEvent:cType, "{", Len(oEvent:aParams), "}"
   For Each elem in oEvent:aParams
      ? elem
   Next
RETURN .F.

