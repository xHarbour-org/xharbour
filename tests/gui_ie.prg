PROCEDURE Main()

   LOCAL cHtml, oIE

   TEXT INTO cHtml
<html>
   <head>
       <p>Header</p>
   </head>

   <body>
      <p>Body</p>
   </body>
</html>
   ENDTEXT

   oIE := CreateObject( "InternetExplorer.Application" )

   // Activate DHTML
   oIE:Navigate( "about:blank" );

   oIE:Document:Body:innerHTML := cHtml
   oIE:AddressBar := .F.
   oIE:Document:Title := "My HTML Application"
   oIE:Visible := .T.

RETURN

