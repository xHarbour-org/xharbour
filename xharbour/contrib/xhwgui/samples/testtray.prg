#include "windows.ch"
#include "guilib.ch"

Function Main
Local oMainWindow, oTrayMenu, oIcon := HIcon():AddResource("ICON_1")

   INIT WINDOW oMainWindow MAIN TITLE "Example"

   CONTEXT MENU oTrayMenu
      MENUITEM "Message"  ACTION MsgInfo( "Tray Message !" )
      SEPARATOR
      MENUITEM "Exit"  ACTION EndWindow()
   ENDMENU

   oMainWindow:InitTray( oIcon,,oTrayMenu,"TestTray" )

   ACTIVATE WINDOW oMainWindow NOSHOW
   oTrayMenu:End()

Return Nil

