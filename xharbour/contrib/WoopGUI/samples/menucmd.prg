/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
*
*/

// Sample of use menu with preprocessor commands

#include "woopgui.ch"

function main

   local oApp, oWnd1
   local oMenuBar

   DEFINE APPLICATION "Sample"
   //oApp := tApplication():New()
   APPLICATION CREATE
   //oApp:Create()
   
   DEFINE WINDOW oWnd1 TITLE "Sample Menu with Commands"
   //oWnd1 := TFrame():New("Sample Menu with Commands")
   
   SET WINDOW oWnd1 STATUSBAR "Sample Form window - Status bar"
   //oWnd1:SetStatusBar("Sample Form window - Status bar")

   DEFINE MENUBAR oMenuBar
       POPUP "&File"
           ITEM "&New"                      ACTION NothingToDo()
           ITEM "&Open"
           ITEM "&Save"
           ITEM "Save &As .."
           SEPARATOR
           POPUP "&Preferences"
               ITEM "&Memory"               ACTION NothingToDo()
               ITEM "&Printers"
               POPUP "&Others"
                   POPUP "&Another Submenu"
                       ITEM "&Hey Guys :-)" ACTION NothingToDo()
                   END POPUP
               END POPUP
           END POPUP
           SEPARATOR
           ITEM "&Exit" ACTION oWnd1:Destroy()
       END POPUP
       POPUP "&Modify"
           ITEM "&Cut"
           ITEM "&Copy"
           ITEM "&Paste"
       END POPUP
       POPUP "&?"
           ITEM "&About" ACTION MessageBox(0, "WoopGUI - A Win32 OOP GUI for Harbour"+CRLF+;
                                              "(C) Francesco Saverio Giudice - 2002")
       END POPUP
   END MENU
   
   SET WINDOW oWnd1 MENU oMenuBar
   
   WINDOW oWnd1 ACTIVATE
   //oWnd1:Show()
   APPLICATION ACTIVATE
   //oApp:Activate()
   APPLICATION QUIT
   //oApp:Quit()

return (0)

PROCEDURE NothingToDo()
   MessageBox(0, "This Item do nothing !" )
RETURN
