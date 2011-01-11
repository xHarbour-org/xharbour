STATIC s_lQuit := .F., oIE, oDocument, oForm, oWindow

PROCEDURE Main()

   LOCAL cHtml, hIEEventHandler := Hash()

   TEXT INTO cHtml
<html>
   <head>
      <meta http-equiv="Content-Language" content="en-us">
      <meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
      <title>My App</title>
   </head>  
   
   <body>
      <form method="POST" action="--WEBBOT-SELF--">
         <p>Name:&nbsp;&nbsp;&nbsp;
         <input type="text" id="FirstName" size="20" dir="ltr" tabindex="1"></p>
         <p>&nbsp;</p>
         <p><input type="submit" value="   Ok   " name="Ok">&nbsp;&nbsp;&nbsp;
         <input type="reset" value="Cancel" name="Cancel"></p>
      </form>
   </body>
</html>
   ENDTEXT

   oIE := CreateObject( "InternetExplorer.Application" )

   // Activate DHTML
   oIE:Navigate( "about:blank" );
   
   oIE:AddressBar := .F.
   
   hIEEventHandler[ "OnQuit" ] := ( @IE_OnQuit() )

   oDocument := oIE:Document   

   oDocument:Title := "My HTML Application"
   oDocument:Body:innerHTML := cHtml

   oForm := oDocument:Forms[0]
   oWindow := oDocument:parentWindow
   
   oIE:ConnectEvents( hIEEventHandler )
     
   oForm:Elements[ "FirstName" ]:OnActivate := GetRef( "When_FirstName" )
   oForm:Elements[ "FirstName" ]:OnDeactivate := GetRef( "Valid_FirstName" )
   
   oIE:Visible := .T.

   WHILE ! s_lQuit
      DoEvents()
   END
   
   // Make sure objects are released in proper order!
   oForm := NIL
   oWindow := NIL
   oDocument := NIL
   oIE := NIL

RETURN

PROCEDURE When_FirstName(...)
   
   // Generic way to retrieve the firing control.
   LOCAL Self := oForm:Elements[ oWindow:Event:srcElement:id ]
   
   // Explicit way 
   IF Empty( oForm:Elements[ "FirstName" ]:Value )
      Self:Value := "John"
   ENDIF
      
RETURN

PROCEDURE Valid_FirstName(...)
   
   IF Empty( oForm:Elements[ "FirstName" ]:Value )
      oWindow:Alert( "First name is a required field!" )
      oForm:Elements[ "FirstName" ]:SetActive()
   ENDIF   
   
RETURN

PROCEDURE IE_OnQuit
   s_lQuit := .T.
RETURN