/*
 * HWGUI using sample
 * 
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"

// REQUEST HB_CODEPAGE_RU866
// REQUEST HB_CODEPAGE_RU1251

function Main
Private oMainWindow, oPanel
Private oFont := Nil, cImageDir := "\"+Curdir()+"\..\image\"
Private nColor, oBmp2

   // hb_SetCodepage( "RU1251" )

   INIT WINDOW oMainWindow MDI TITLE "Example" ;
         MENUPOS 3

   @ 0,0 PANEL oPanel SIZE 0,32
   @ 2,3 OWNERBUTTON OF oPanel ON CLICK {||CreateChildWindow()} ;
       SIZE 32,26 FLAT ;
       BITMAP cImageDir+"new.bmp" COORDINATES 0,4,0,0 TOOLTIP "New MDI child window"

   ADD STATUS TO oMainWindow PARTS 80,0

   MENU OF oMainWindow
      MENU TITLE "&File"
         MENUITEM "&New" ACTION CreateChildWindow()
         MENUITEM "&Open" ACTION FileOpen()
         SEPARATOR
         MENUITEM "&Font" ACTION oFont:=HFont():Select(oFont)
         MENUITEM "&Color" ACTION (nColor:=Hwg_ChooseColor(nColor,.F.), ;
                     MsgInfo(Iif(nColor!=Nil,str(nColor),"--"),"Color value"))
         SEPARATOR
         MENUITEM "&Exit" ACTION EndWindow()
      ENDMENU
      MENU TITLE "&Samples"
         MENUITEM "&Checked" ID 1001 ;
               ACTION CheckMenuItem( ,1001, !IsCheckedMenuItem( ,1001 ) )
         SEPARATOR
         MENUITEM "&MsgGet" ;
               ACTION CopyStringToClipboard(MsgGet("Dialog Sample","Input table name"))
         MENUITEM "&Dialog from prg" ACTION DialogFromPrg()
         MENUITEM "&DOS print" ACTION PrintDos()
         MENUITEM "&Windows print" ;
               ACTION Iif( OpenReport("a.rpt","Simple"),PrintReport(),.F.)
         MENUITEM "&Print Preview" ACTION PrnTest()
      ENDMENU
      MENU TITLE "&Help"
         MENUITEM "&About" ACTION OpenAbout()
         MENUITEM "&Window2Bitmap" ACTION About2()
      ENDMENU
      MENU TITLE "&Windows"
         MENUITEM "&Tile"  ;
            ACTION  SendMessage(HWindow():GetMain():handle,WM_MDITILE,MDITILE_HORIZONTAL,0)
      ENDMENU
   ENDMENU

   oMainWindow:Activate()

return nil

Function CreateChildWindow
Local oChildWnd, oPanel, oFontBtn

   PREPARE FONT oFontBtn NAME "MS Sans Serif" WIDTH 0 HEIGHT -12

   INIT WINDOW oChildWnd MDICHILD TITLE "Child" // STYLE WS_CHILD + WS_VISIBLE + WS_THICKFRAME + WS_CAPTION

   @ 0,0 PANEL oPanel OF oChildWnd SIZE 0,44

   @ 2,3 OWNERBUTTON OF oPanel ID 108 ON CLICK {||MsgInfo("Button 1")} ;
       SIZE 44,38 FLAT ;
       TEXT "New" FONT oFontBtn COORDINATES 0,20,0,0  ;
       BITMAP cImageDir+"new.bmp" COORDINATES 0,4,0,0 TOOLTIP "New"
   @ 46,3 OWNERBUTTON OF oPanel ID 109 ON CLICK {||MsgInfo("Button 2")} ;
       SIZE 44,38 FLAT ;
       TEXT "Open" FONT oFontBtn COORDINATES 0,20,0,0 ;
       BITMAP cImageDir+"open.bmp" COORDINATES 0,4,0,0 TOOLTIP "Open"
       // BITMAP OBM_CHECK COORDINATES 0,4,0,0

   oChildWnd:Activate()

Return Nil

function OpenAbout
Local oModDlg, oFontBtn, oFontDlg, oBrw
Local aSample := { {.t.,"Line 1",10}, {.t.,"Line 2",22}, {.f.,"Line 3",40} }
Local oBmp, oIcon := HIcon():AddFile("..\image\PIM.ICO")

   PREPARE FONT oFontDlg NAME "MS Sans Serif" WIDTH 0 HEIGHT -13
   PREPARE FONT oFontBtn NAME "MS Sans Serif" WIDTH 0 HEIGHT -13 ITALIC UNDERLINE

   INIT DIALOG oModDlg TITLE "About"     ;
   AT 190,10  SIZE 360,240               ;
   ICON oIcon                            ;
   ON EXIT {||oBmp2 := HBitmap():AddWindow(oBrw),.T.} ;
   FONT oFontDlg

   oModDlg:bActivate := {||MsgInfo("!!")}
      

   // @ 20,30 BITMAP "..\image\OPEN.BMP"
   // @ 20,20 ICON "..\image\PIM.ICO"
   @ 10,10 IMAGE "..\image\ASTRO.JPG" SIZE 50,50

   @ 20,60 SAY "Sample Dialog"        ;
        SIZE 130, 22 STYLE SS_CENTER  ;
        COLOR Vcolor("0000FF")
   @ 20,80 SAY "Written as a sample"  ;
        SIZE 130, 22 STYLE SS_CENTER
   @ 20,100 SAY "of Harbour GUI" ;
        SIZE 130, 22 STYLE SS_CENTER
   @ 20,120 SAY "application"    ;
        SIZE 130, 22 STYLE SS_CENTER

   @ 160,30 BROWSE oBrw ARRAY SIZE 180,110 ;
        STYLE WS_BORDER + WS_VSCROLL + WS_HSCROLL

   @ 80,180 OWNERBUTTON ON CLICK {|| MsgInfo(oModDlg:ClassName()),EndDialog()} ;
       SIZE 180,35 FLAT                                  ;
       TEXT "Close" COLOR Vcolor("0000FF") FONT oFontBtn ;
       BITMAP cImageDir+"door.bmp" COORDINATES 40,10,0,0
       // 

   CreateArList( oBrw,aSample )
   oBrw:bColorSel    := 12507070  // 15149157449

   oBmp := HBitmap():AddResource( OBM_LFARROWI )
   oBrw:aColumns[1]:aBitmaps := { ;
      { {|l|l}, oBmp } ;
   }
   oBrw:aColumns[2]:length := 6
   oBrw:aColumns[3]:length := 4
   oBrw:bKeyDown := {|o,key|BrwKey(o,key)}

   ACTIVATE DIALOG oModDlg
   oIcon:Release()

Return Nil

Static Function About2()

   IF oBmp2 == Nil
      Return
   ENDIF

   INIT DIALOG oModDlg TITLE "About2"   ;
   AT 190,10  SIZE 360,240

   @ 10, 10 BITMAP oBmp2

   ACTIVATE DIALOG oModDlg

Return Nil

Static Function BrwKey( oBrw, key )
   IF key == 32
      oBrw:msrec[ oBrw:tekzp,1 ] := !oBrw:msrec[ oBrw:tekzp,1 ]
      oBrw:RefreshLine()
   ENDIF
Return .T.

Function FileOpen
Local aModDlg, oBrw
Local mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
Local fname := SelectFile( "xBase files( *.dbf )", "*.dbf", mypath )
Local nId

   IF !Empty( fname )
      mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
      // use &fname new codepage RU866
      use &fname new
      nId := 111

      INIT DIALOG aModDlg TITLE "1"                    ;
            AT 210,10  SIZE 300,300                    ;
            ON INIT {|o|SetWindowText(o:handle,fname)} ;
            ON EXIT {|o|Fileclose(o)}

      @ 0,0 BROWSE oBrw DATABASE OF aModDlg ID nId ;
            SIZE 300,300                           ;
            STYLE WS_VSCROLL + WS_HSCROLL          ;
            ON SIZE {|o,x,y|MoveWindow(o:handle,0,0,x,y)} ;
            ON GETFOCUS {|o|dbSelectArea(o:alias)}
      CreateList( oBrw,.T. )
      IF oFont != Nil
         oBrw:ofont := oFont
      ENDIF

      ACTIVATE DIALOG aModDlg NOMODAL
   ENDIF
Return Nil

Function FileClose( oDlg )
   Local oBrw := oDlg:FindControl( 111 )
   dbSelectArea( oBrw:alias )
   dbCloseArea()
Return .T.

function printdos
Local han := fcreate( "LPT1",0 )
  if han != -1
     fwrite( han, Chr(10)+Chr(13)+"Example of dos printing ..."+Chr(10)+Chr(13) )
     fwrite( han, "Line 2 ..."+Chr(10)+Chr(13) )
     fwrite( han, "---------------------------"+Chr(10)+Chr(13)+Chr(12) )
     fclose( han )
  else
     MsgStop("Can't open printer port!")
  endif
return nil

Function PrnTest
Local oFont := HFont():Add( "Times New Roman",0,-13 )
/*
Local hDC := PrintSetup()
Local hDCmeta := CreateEnhMetaFile( HWindow():GetMain():handle,"hrb.emf" ), hEmf
   SelectObject( hDCmeta,oFont:handle )
   DrawText( hDCmeta, "Windows printing first sample !", 100,10,300,26,DT_CENTER  )
   DrawLine( hDCmeta, 90,30,310,30 )
   DrawLine( hDCmeta, 90,5,90,30 )
   DrawLine( hDCmeta, 310,5,310,30 )
   hEmf := CloseEnhMetaFile( hDCmeta )
   PrintPreview( hDC,hEmf )
   DeleteEnhMetaFile( hEmf )
   oFont:Release()
   DeleteDC( hDC )
*/
Local oPrinter := HPrinter():New()
   oPrinter:StartDoc( .T. )
   oPrinter:StartPage()
   SelectObject( oPrinter:hDC,oFont:handle )
   oPrinter:Say( "Windows printing first sample !", 100,10,300,26,DT_CENTER,oFont  )
   oPrinter:Line( 90,30,310,30 )
   oPrinter:Line( 90,5,90,30 )
   oPrinter:Line( 310,5,310,30 )
   oPrinter:Say( "----------", 600,500,680,526,DT_CENTER  )
   oPrinter:Say( "End Of Report", 660,1427,160,26,DT_CENTER  )
   oPrinter:EndPage()
   oPrinter:EndDoc()
   oPrinter:Preview()
   oPrinter:End()
Return Nil

Function DialogFromPrg( o )
Local cTitle := "Dialog from prg", cText := "Input something"
Local oModDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local cRes, aCombo := { "First","Second" }, oEdit, vard := "Monday"
// Local aTabs := { "Monday","Tuesday","Wednesday","Thursday","Friday" }

   // o:bGetFocus := Nil
   INIT DIALOG oModDlg TITLE cTitle           ;
   AT 210,10  SIZE 300,300                    ;
   FONT oFont                                 ;
   ON EXIT {||MsgYesNo("Really exit ?")}

   @ 20,10 SAY cText SIZE 260, 22
   @ 20,35 EDITBOX oEdit CAPTION ""    ;
        STYLE WS_DLGFRAME              ;
        SIZE 260, 26 COLOR Vcolor("FF0000")

   @ 20,70 CHECKBOX "Check 1" SIZE 90, 20
   @ 20,95 CHECKBOX "Check 2"  ;
        SIZE 90, 20 COLOR Iif( nColor==Nil,Vcolor("0000FF"),nColor )

   @ 160,70 GROUPBOX "RadioGroup"  SIZE 130, 75

   RADIOGROUP
   @ 180,90 RADIOBUTTON "Radio 1"  ;
        SIZE 90, 20 ON CLICK {||oEdit:SetColor(Vcolor("0000FF"),,.T.)}
   @ 180,115 RADIOBUTTON "Radio 2" ;
        SIZE 90, 20 ON CLICK {||oEdit:SetColor(Vcolor("FF0000"),,.T.)}
   END RADIOGROUP SELECTED 2

   @ 20,120 COMBOBOX aCombo STYLE WS_TABSTOP ;
        SIZE 100, 150

   @ 20,160 UPDOWN 10 RANGE -10,50 SIZE 50,32 STYLE WS_BORDER

   @ 160,160 TAB oTab ITEMS {} SIZE 130,56
   BEGIN PAGE "Monday" OF oTab
      @ 20,28 GET vard SIZE 80,22 STYLE WS_BORDER
   END PAGE OF oTab
   BEGIN PAGE "Tuesday" OF oTab
      @ 20,28 EDITBOX "" SIZE 80,22 STYLE WS_BORDER
   END PAGE OF oTab

   @ 100,220 LINE LENGTH 100

   @ 20,240 BUTTON "Ok" OF oModDlg ID IDOK  ;
        SIZE 100, 32 COLOR Vcolor("FF0000")
   @ 140,240 BUTTON "11" OF oModDlg  ;
        SIZE 20, 32 ON CLICK {|o|CreateC(o)}
   @ 180,240 BUTTON "Cancel" OF oModDlg ID IDCANCEL  ;
        SIZE 100, 32

   ACTIVATE DIALOG oModDlg
   oFont:Release()

Return Nil

Static Function CreateC( oDlg )
Static lFirst := .F.
   IF !lFirst
      @ 100,200 DATEPICKER SIZE 80, 24
      lFirst := .T.
   ENDIF
Return Nil

#define EM_GETSEL               176  // 0x00B0
#define EM_SETSEL               177  // 0x00B1

