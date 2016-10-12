<%@ Language=XBScript %>

<%
#translate Response. => Response:

PROCEDURE Main()

   LOCAL oErr, aVars, nVars, Counter

   Response:Write( "<p>Version: " + Version() + "</p>" )

   Response:Write( "<p>Server Variables: </p>" )

   Response:Flush()

   TRY
      aVars := Request:ServerVariables
      nVars := aVars:Count


      WITH OBJECT Response
         FOR Counter := 1 TO nVars
            :Write( "<p>" )
            :Write( aVars:Key( Counter ) )
            :Write( " = " )
            :Write( aVars[ Counter ] )
            :Write( "</p>" )
         NEXT
      END

   CATCH oErr
      Response:Write( "<p>Operation: " + oErr:Operation + "</p>" )
      Response:Write( "<p>Description: " + oErr:Description + "</p>" )
      RETURN
   END

RETURN
%>
