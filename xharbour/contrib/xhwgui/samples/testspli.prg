#include "windows.ch"
#include "guilib.ch"

Function Main
Local oMainWindow, oFont, oSplitV, oSplitH, oEdit1, oEdit2

   PREPARE FONT oFont NAME "MS Sans Serif" WIDTH 0 HEIGHT -13

   INIT WINDOW oMainWindow MAIN TITLE "Example"  ;
     COLOR COLOR_3DLIGHT+1                       ;
     AT 200,0 SIZE 420,300                       ;
     FONT oFont

   @ 20,10 TREE oTree SIZE 140,100

   oTree:AddNode( "First" )
   oTree:AddNode( "Second" )
   oItem := oTree:AddNode( "Third" )
   oItem:AddNode( "Third-1" )
   oTree:AddNode( "Forth" )

   @ 163,10 EDITBOX oEdit1 CAPTION "Hello, World!"  SIZE 200,100

   @ 160,10 SPLITTER oSplitV SIZE 3,100 DIVIDE {oTree} FROM {oEdit1}
   oSplitV:hCursor := LoadCursor( "VSPLIT" )

   @ 20,113 EDITBOX oEdit2 CAPTION "Example"  SIZE 344,130

   @ 20,110 SPLITTER oSplitH SIZE 344,3 DIVIDE {oTree,oEdit1,oSplitV} FROM {oEdit2}
   oSplitH:hCursor := LoadCursor( "HSPLIT" )

   ACTIVATE WINDOW oMainWindow

Return nil
