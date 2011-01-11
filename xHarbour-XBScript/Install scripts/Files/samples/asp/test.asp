<%@ Language=XBScript %>

<% #translate Response. => Response: %>

<p>Some Text Here</p>

<%
  Response:Write( "Current Path: " + CurDir() )

  TRY
     USE C:\InetPub\WWWRoot\CGI-Win\TEST VIA "DBFCDX"
     Response:Write( "<p>" + FIELD->First + "</p>" )
  CATCH oErr
     Response:Write( "<p>Make sure test.dbf is at specified location (must be located within this site's folder, and has all permissions</p>" )
     RETURN
  END
%>

<p>Some more text here with inline code: <%= FIELD->Last %> </p>
