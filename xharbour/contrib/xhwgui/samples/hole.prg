#include "windows.ch"
#include "guilib.ch"

Function Main
Local oFont
Local oAgent, oEdit
Private oMainWindow, oChar

   PREPARE FONT oFont NAME "Times New Roman" WIDTH 0 HEIGHT -17 CHARSET 204

   INIT WINDOW oMainWindow MAIN TITLE "Example"  ;
     COLOR COLOR_3DLIGHT+1                       ;
     AT 200,0 SIZE 400,250                       ;
     FONT oFont

   oAgent:=TOleAuto():New("Agent.Control.2")
   IF oAgent == Nil .OR. oAgent:hObj == 0
      cText := "Ms Agent isn't installed !"
   ELSE
      oAgent:Connected := 1
      oAgent:Characters:Load("Default")
      oChar := oAgent:Characters("Default")
      IF oChar == Nil .OR. oChar:hObj == 0
         cText := "No default character !"
      ELSE
         @ 280,20 BUTTON "Speak!"  SIZE 100,30  ON CLICK {||SpeakIt(oEdit)}
      ENDIF
      @ 260,90 BUTTON "Set Default"  SIZE 120,30  ON CLICK {||oAgent:showDefaultCharacterProperties()}
   ENDIF

   @ 20,20 EDITBOX oEdit CAPTION "Hello, world !" SIZE 260,30 STYLE ES_AUTOHSCROLL

   @ 20,200 LINE LENGTH 180
   @ 260,170 BUTTON "Close"  SIZE 120,30  ON CLICK {||EndWindow()}

   ACTIVATE WINDOW oMainWindow

   IF oAgent != Nil .AND. oAgent:hObj != 0
      oAgent:Characters:UnLoad("Default")
      oAgent:End()
   ENDIF

Return Nil

Static Function SpeakIt( oEdit )
Local aTop := ClientToScreen( oMainWindow:handle,0,0 )
Local cText := GetEditText( oEdit:oParent:handle, oEdit:id )

   oChar:Show()
   oChar:MoveTo( aTop[1]+20, aTop[2]+70 )
   oChar:LanguageID := Iif( Asc(cText)>122,"&H0419","&H0409" )
   oChar:Speak( cText )
   oChar:Hide()
Return Nil
