#include "windows.ch"
#include "guilib.ch"

Function Main
Local oMainWindow

   INIT WINDOW oMainWindow MAIN TITLE "Example" ;
     AT 200,0 SIZE 400,150

   MENU OF oMainWindow
      MENUITEM "&Exit" ACTION EndWindow()
      MENUITEM "&Dialog" ACTION DlgGet()
   ENDMENU

   ACTIVATE WINDOW oMainWindow
Return Nil

Function DlgGet
Local oForm := HFormTemplate():Open( "a2.frm","form1" )

   ReadExit( .T. )
   oForm:Execute()

Return Nil
