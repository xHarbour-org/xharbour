/*
 * HWGUI using sample
 * Property sheet
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"

Function Main
Local oMainWindow

   INIT WINDOW oMainWindow MAIN TITLE "Example" ;
     AT 200,0 SIZE 400,150

   MENU OF oMainWindow
      MENUITEM "&Exit" ACTION EndWindow()
      MENUITEM "&Property Sheet" ACTION OpenConfig()
   ENDMENU

   ACTIVATE WINDOW oMainWindow
Return Nil

Function OpenConfig
Local aDlg1, aDlg2, aCombo := { "Aaaa","Bbbb" }
Local oBrw1, oBrw2
Local aSample1 := { {"Alex",17}, {"Victor",42}, {"John",31} }
Local aSample2 := { {"Line 1",10}, {"Line 2",22}, {"Line 3",40} }
Local e1 := "Xxxx"

   INIT DIALOG aDlg1 FROM RESOURCE TITLE "PAGE_1" ON EXIT {||MsgInfo("Exit"),.T.}
   REDEFINE GET e1 ID 103

   INIT DIALOG aDlg2 FROM RESOURCE TITLE "PAGE_2" ON EXIT {||.T.}
   REDEFINE COMBOBOX aCombo ID 101
   REDEFINE BROWSE oBrw1 ARRAY ID 104
   REDEFINE BROWSE oBrw2 ARRAY ID 105

   CreateArList( oBrw1,aSample1 )
   CreateArList( oBrw2,aSample2 )

   PropertySheet( GetActiveWindow(),{ aDlg1, aDlg2 }, "Sheet Example" )

Return Nil
