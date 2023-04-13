 	// Sends a query to Google and displays the Links from the response HTML page

PROCEDURE Main
   LOCAL oHttp, cHtml, hQuery, aLink, oNode, oDoc,ohttp1
Local cRef
   oHttp1:= TIpClientHttp():new( "http://www.google.com/",.t. )
   
   // build the Google query
   hQUery := Hash()
   hSetCaseMatch( hQuery, .F. )

   hQuery["q"]    := "xHarbour"
   hQuery["hl"]   := "en"
   hQuery["btnG"] := "Google+Search"

   // add query data to the TUrl object
   oHttp1:oUrl:addGetForm( hQuery )

   // Connect to the HTTP server
   IF .NOT. oHttp1:open()
      ? "Connection error:", oHttp1:lastErrorMessage()
      QUIT
   ENDIF
altd()
   // download the Google response
   cHtml   := oHttp1:readAll()
   if "302 Moved" in cHtml
   oDoc := THtmlDocument():new( cHtml )
   oNode := oDoc:body:a
   cRef := strtran(onode:Attr["HREF"],"&amp;","&")+"&btnG=Google+Search"
   tracelog(valtoprg(ohttp1))
   oHttp1:close()
   cRef := strtran(cRef,"/www.google.com.br/","/www.google.com.br/search")
   oHttp:= TIpClientHttp():new( cref,.t. )
   oHttp:cConnetion:='keep-alive'
   oHttp:open()
   cHtml:=ohttp:readall()
   endif
   tracelog(valtoprg(ohttp))
   oHttp:close()
   ? Len(cHtml), "bytes received "

   oDoc := THtmlDocument():new( cHtml )

   oDoc:writeFile( "Google.html" )

   // ":a" retrieves the first <a href="url"> text </a> tag
   oNode := oDoc:body:a
   ? oNode:getText(""), oNode:href

   // ":divs(5)" returns the 5th <div> tag
   oNode := oDoc:body

   // "aS" is the plural of "a" and returns all <a href="url"> tags
   aLink := oNode:aS

   FOR EACH oNode IN aLink
      ? HtmlToOem( oNode:getText("") ), oNode:href
   NEXT
RETURN
