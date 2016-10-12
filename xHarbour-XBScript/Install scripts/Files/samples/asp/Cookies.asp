<%@ Language=XBScript %>

<%
#translate Response. => Response: 

PROCEDURE Main()

   LOCAL oCookie

   // Request:Cookies is READONLY!
   oCookie := Request:Cookies[ "xHarbour" ]

   IF oCookie:HasKeys
      LastDate := oCookie[ "LastDate" ]
      LastTime := oCookie[ "LastTime" ]
      
      Response:Write( "Last Visit: " + LastDate + " " + LastTime )
   ELSE
      Response:Write( "Welcome to xHarbour" )
   ENDIF

   // Response:Cookies is WRITEONLY!
   oCookie := Response:Cookies[ "xHarbour" ]

   oCookie:Expires := Date() + 30

   oCookie["LastDate"] := dtoc( Date() )
   oCookie["LastTime"] := Time()

   Response:Write( "<p>" + Version() + "</p>" )

   // Display all Cookies of our domain.
   FOR i := 1 TO Request:Cookies:Count
      Response:Write( "Cookie #" )
      Response:Write( i )

      Response:Write( " Name: " )
      Response:Write( Request:Cookies:Key(i) )

      IF Request:Cookies[i]:HasKeys
         Response:Write( "<br>" )

         FOR j := 1 TO Request:Cookies[i]:Count
            Response:Write( "Key: " + Request:Cookies[i]:Key(j) )
            Response:Write( " Value: " + Request:Cookies[i][j] )
            Response:Write( "<br>" )
         NEXT

         Response:Write( "<br>" )
      ELSE
         Response:Write( " Value: " )
         Response:Write( Request:Cookies[i] )
      ENDIF
      Response:Write( "<br>" )
   Next 
      
RETURN
%>
