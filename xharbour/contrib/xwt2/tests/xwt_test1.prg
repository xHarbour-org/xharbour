************************************************************
* XWT_TEST.
*
* Giancarlo Niccolai et al.:
* (PLZ. add your copyright...)
*
* $Id: xwt_test.prg,v 1.3 2004/05/20 15:41:37 jonnymind Exp $
*


#include "xwt.ch"


#xcommand ? <data,...> => OutStd( <data>, HB_OSNewLine() )
GLOBAL oMenuChanged
GLOBAL oWindow

PROCEDURE MAIN()
   LOCAL oLabel, oLayout, oMenu,oLabel1,oLabel2
   LOCAL oGrid, oViewport

   XwtInit()

   /*** Window creation ****/
   oWindow := XWTFrameWindow():New( "text: Welcome from xHarbour; statusbar: All Ready!; bgcolor: #33ffdd" )
   oWindow:SetProperty( { "id"=> 1, "height"=> 200, "width"=> 500, "x" => 100, "y"=> 100} )
   oWindow:addListener( @WindowReceive() )
   oWindow:AddEventListener( "destroy", @ReallyQuit() )
   oLayOut := XWTLayout():New( "id: 2; padding: 5; mode: vertical; expand: .T.; fill: .T.; frame: .T.; align: left" )
   oLabel := XWTLabel():New( "id: 3; text: Center; align:center; valign:top; fgcolor: #326514" )
   ? "LABEL1: ", oLabel:GetStyle()
   oLayOut:Add( oLabel )
   oLabel1 := XWTLabel():New( "id: 4; sensibility: .T.; text: Left; align:left; valign:center; width:200; fgcolor: #FFAACC" )
      ? "LABEL2: ", oLabel1:GetStyle()
   oLayOut:Add( oLabel1 )
   oLabel2 := XWTLabel():New( "id: 5; sensibility: .T.; text: Right; align:right; valign:center; bgcolor: #CCAAFF")
   ? "LABEL3: ", oLabel2:GetStyle()
   oLayOut:Add( oLabel2 )
   oLayOut:Add( XWTTextBox("id:6; text: enter a text here" ) )
   oLayOut:Add( XWTButton("id:7; text: Click here!; bgcolor: #12A4B6; textcolor: #F3AD7C; fgcolor: #652D3A; basecolor: #DA41CB" ) )

   oMenu := XwtMenu():New( {"text"=> "Main _menu"})
   oMenu:Add( XwtMenuItem( "id: 1; text: _about" ) )
   oMenu:Add( XwtMenuItem( {"id"=> 3, "checkbox"=> .T., "text" => "_Enable other item" } ) )
   oMenuChanged := XwtMenuItem( "id: 4; enabled: .F.; text: _Disabled item" )
   oMenu:Add( oMenuChanged )
   oMenu:Add( XwtMenuItem( "separator: .T. " ) )
   oMenu:Add( XwtMenuItem( "id: 2; text: _quit" ) )
   oWindow:SetMenuBar( { oMenu } )

   oGrid := XWTGrid( "rows:1; columns:2; colpadding: 3; expand: .T.; fill:.T." )
   oGrid:Attach( XWTTextArea( {"text" => e"A new born\nTEXT AREA!!!" } ) , 1, 2, 1, 1 )
   oGrid:Attach( oLayOut, 1 ,1 ,1 ,1 )
   oViewport := XWTViewPort( "expand .F.; fill:.F." )
   oViewPort:connect( oGrid )
   oWindow:connect( oViewPort )
   oWindow:show( )
   ? "LABEL3 AFTER: ", oLabel:GetStyle()

   /*** Main LOOP ***/
   XwtMainLoop()

   ? "END OF PROGRAM"
RETURN


FUNCTION ReallyQuit()
   IF XWT_MsgBox( "Really quit?", { "title" => "Quit query", "yes" =>.T., "no" => .T., "type"=> "question" } ) == XWT_MSGBOX_YES
      XwtQuit()
   ENDIF
RETURN .T.


FUNCTION WindowReceive( oEvent )
   LOCAL elem
   ?  "FROM", oEvent:oSender:GetText(),":", oEvent:cType, "{", Len(oEvent:aParams), "}"
   oWindow:SetProperty( "statusbar", "Last event: " + oEvent:cType )
   For Each elem in oEvent:aParams
      ? elem
   Next

   IF oEvent:cType == "menu"
      Switch oEvent:aParams[1]
         CASE 1
            XWT_MsgBox( e"XWT2 Basic test.\nGiancarlo Niccolai", { "title" => "About", "ok" =>.T. } )
         EXIT

         CASE 2
            ? "QUIT FROM MENU"
            XwtQuit()
         EXIT

         CASE 3
            // this is the status BEFORE the change
            IF oEvent:oSender:GetProperty( "checked" )
               oMenuChanged:SetStyle( "text: Enabled _item; enabled: .T." )
            ELSE
               oMenuChanged:SetStyle( "text: _Disabled item; enabled: .F." )
            ENDIF
         EXIT

         CASE 4
            XWT_MsgBox( e"Re-enabled item selected", { "title" => "Information", "ok" =>.T. } )
         EXIT

      END
   ENDIF

RETURN .F.

