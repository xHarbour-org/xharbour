#include "windows.ch"
#include "guilib.ch"

Function Main
Local oMainWindow

   INIT WINDOW oMainWindow MAIN TITLE "Example" ;
     AT 200,0 SIZE 400,150

   MENU OF oMainWindow
      MENUITEM "&Exit" ACTION EndWindow()
      MENUITEM "&Tree" ACTION DlgGet()
   ENDMENU

   ACTIVATE WINDOW oMainWindow
Return Nil

Function DlgGet
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local oTree
Private oSay

   INIT DIALOG oDlg TITLE "TreeView control sample"  ;
   AT 210,10  SIZE 430,300                  ;
   FONT oFont                               ;
   ON INIT {||BuildTree(oDlg,oTree)}

   @ 10,20 TREE oTree OF oDlg SIZE 200,260 ;
        EDITABLE ;
        BITMAP { "..\image\cl_fl.bmp","..\image\op_fl.bmp" }

   @ 220,20 SAY oSay CAPTION "" SIZE 200,260 STYLE WS_BORDER

   ACTIVATE DIALOG oDlg
   oFont:Release()

Return Nil

Function BuildTree( oDlg,oTree )
Local oNode

   INSERT NODE "First" TO oTree ON CLICK {||NodeOut(1)}
   INSERT NODE "Second" TO oTree ON CLICK {||NodeOut(2)}
   INSERT NODE oNode CAPTION "Third" TO oTree ON CLICK {||NodeOut(0)}
      INSERT NODE "Third-1" TO oNode BITMAP {"..\image\book.bmp"} ON CLICK {||NodeOut(3)}
      INSERT NODE "Third-2" TO oNode BITMAP {"..\image\book.bmp"} ON CLICK {||NodeOut(4)}
   INSERT NODE "Forth" TO oTree ON CLICK {||NodeOut(5)}

   oTree:bExpand := {||.T.}

Return Nil

Static Function NodeOut( n )
Local aText := { ;
  "This is a sample application, which demonstrates using of TreeView control in HwGUI.", ;
  "'Second' item is selected", ;
  "'Third-1' item is selected", ;
  "'Third-2' item is selected", ;
  "'Forth' item is selected", ;
               }

   IF n == 0
      oSay:SetValue("")
   ELSE
      oSay:SetValue(aText[n])
   ENDIF

Return Nil
