STATIC s_aUrls := {}
STATIC s_lQuit := .F.

PROCEDURE Main()

   LOCAL oIE, hEventHandler := {=>}, cUrl

   // Literal Function name sample.
   hEventHandler[ "BeforeNavigate2" ]  := "IE_BeforeNavigate2"
   
   // Function pointer sample.
   hEventHandler[ "NavigateComplete2" ] := ( @IE_NavigateComplete2() )
   
   // Codeblock sample (no need to declare the arguments).
   hEventHandler[ "OnQuit" ]           := {|| IE_OnQuit() }

   // You could pass the EventHandler as a 2nd Paramater, or assign at any later time, as per below!
   oIE := CreateObject( "InternetExplorer.Application" )

   // Event handling will now be activated, using our specified Handler.   
   #ifdef EXPLICIT
      // Connect to DWebBrowserEvents2 Events Set
      oIE:ConnectEvents( hEventHandler, { InterfaceByName( oIE:TypeLib, "DWebBrowserEvents2" ) } )
   #else
      // Connect the default Events Set 
   oIE:ConnectEvents( hEventHandler )
   #endif
   
   oIE:Navigate( "http://www.xharbour.com" )
   oIE:Visible := .T.

   WHILE ! s_lQuit
      DoEvents()
   END

   ? "You visited:"
   FOR EACH cUrl IN s_aUrls
      ? "   ", cUrl
   NEXT

RETURN

PROCEDURE IE_BeforeNavigate2( pDisp, URL, Flags, TargetFrameName, PostData, Headers, Cancel )
         
   IF Lower( Left( URL, 7 ) ) == "http://" .AND. Lower( SubStr( URL, 8, 4 ) ) != "www."
      Alert( "Only www domain are allowed!;;Invalid page: " + URL  )

      // Tell IE we want to cancel the intended navigation.
      Cancel := .T.

      // Needed to avoid a default cancellation page.
      pDisp:Stop()
      
      // Force new destination.
      pDisp:Navigate( "http://www.xHarbour.com" )
   ENDIF
      
RETURN

PROCEDURE IE_NavigateComplete2( pDisp, cUrl )
   TraceLog( cUrl )
   aAdd( s_aUrls, cUrl )
RETURN

PROCEDURE IE_OnQuit
   s_lQuit := .T.
RETURN