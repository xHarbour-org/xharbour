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
Local oModDlg, oBrw1, oBrw2
Local aSample1 := { {"Alex",17}, {"Victor",42}, {"John",31} }
Local aSample2 := { {.t.,"Line 1",10}, {.t.,"Line 2",22}, {.f.,"Line 3",40} }

   INIT DIALOG oModDlg TITLE "About"          ;
   AT 190,10  SIZE 400,240

   @ 20,30 BROWSE oBrw1 ARRAY SIZE 180,110 ;
        STYLE WS_BORDER + WS_VSCROLL + WS_HSCROLL

   @ 210,30 BROWSE oBrw2 ARRAY SIZE 180,110 ;
        STYLE WS_BORDER + WS_VSCROLL + WS_HSCROLL

   @ 80,180 OWNERBUTTON ON CLICK {|| EndDialog()} ;
       SIZE 180,35 FLAT                                  ;
       TEXT "Close" COLOR Vcolor("0000FF")

   CreateArList( oBrw1,aSample1 )

   CreateArList( oBrw2,aSample2 )
   oBmp := HBitmap():AddResource( OBM_CHECK )
   oBrw2:aColumns[1]:aBitmaps := { ;
      { {|l|l}, oBmp } ;
   }
   oBrw2:aColumns[2]:length := 6
   oBrw2:aColumns[3]:length := 4
   oBrw2:bKeyDown := {|o,key|BrwKey(o,key)}

   ACTIVATE DIALOG oModDlg
Return Nil

Static Function BrwKey( oBrw, key )
   IF key == 32
      oBrw:msrec[ oBrw:tekzp,1 ] := !oBrw:msrec[ oBrw:tekzp,1 ]
      oBrw:RefreshLine()
   ENDIF
Return .T.
