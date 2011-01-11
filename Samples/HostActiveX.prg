#include "setcurs.ch"

STATIC s_lQuit := .F.
STATIC s_hWnd

PROCEDURE Main( cOcx )

   LOCAL oOcx
   LOCAL hEventHandler := Hash()
      
   Wvw_SetCodePage(,255)  // #define OEM_CHARSET 255 - from wingdi.h
   WVW_SetTitle(NIL, "ActiveX Host Sample - Press Esc to terminate" )
   SetCursor( SC_NONE )  
   SetColor("N/W")
   CLS
   
   wvw_ShowWindow()      
   s_hWnd := wvw_GetWindowHandle()
   
   // Some deafult OCX
   IF Empty( cOcx )
      cOcx := "Shell.Explorer"
   ENDIF
   
   oOcx := CreateActiveX( s_hWnd, cOcx )   
   
   IF cOcx == "Shell.Explorer"
      oOcx:Navigate( "http://www.xHarbour.com" )
   ELSEIF cOcx == "DotNetControl.DotNetToolbar"   
      // Codeblock sample.
      hEventHandler[ "menuNewClick" ]  := {|| MessageBox( s_hWnd, "New...", "Event", 0 ) }
      
      // Pointer sample.
      hEventHandler[ "menuExitClick" ] := ( @menuExitClick() )
      
      // Attach the event handler.
      oOcx:ConnectEvents( hEventHandler )
   ENDIF
   
   Win_SetFocus( s_hWnd )
   
   // Main loop
   DO WHILE Inkey() != 27 .AND. ! s_lQuit
   ENDDO   

   oOcx := NIL
   
RETURN

PROCEDURE menuExitClick
   MessageBox( s_hWnd, ProcName(), "Event", 0 )
   s_lQuit := .T.
RETURN   