#include "windows.ch"
#include "guilib.ch"

Function Main
Local oMainWindow

   INIT WINDOW oMainWindow MAIN TITLE "Example" ;
     AT 200,0 SIZE 400,150

   MENU OF oMainWindow
      MENUITEM "&Exit" ACTION EndWindow()
      MENUITEM "&Get a value" ACTION DlgGet()
   ENDMENU

   ACTIVATE WINDOW oMainWindow
Return Nil

Function DlgGet
Local oModDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local cRes, aCombo := { "First","Second" }
Local oGet
Local e1 := "Dialog from prg", c1 := .F., c2 := .T., r1 := 2, cm := 1
Local upd := 12, d1 := Date()+1

   INIT DIALOG oModDlg TITLE "Get a value"  ;
   AT 210,10  SIZE 300,300                  ;
   FONT oFont

   @ 20,10 SAY "Input something:" SIZE 260, 22
   @ 20,35 GET oGet VAR e1  ;
        STYLE WS_DLGFRAME   ;
        SIZE 260, 26 COLOR Vcolor("FF0000")

   @ 20,70 GET CHECKBOX c1 CAPTION "Check 1" SIZE 90, 20
   @ 20,95 GET CHECKBOX c2 CAPTION "Check 2" SIZE 90, 20 COLOR Vcolor("0000FF")

   @ 160,70 GROUPBOX "RadioGroup" SIZE 130, 75

   GET RADIOGROUP r1
   @ 180,90 RADIOBUTTON "Radio 1"  ;
        SIZE 90, 20 ON CLICK {||oGet:SetColor(Vcolor("0000FF"),,.T.)}
   @ 180,115 RADIOBUTTON "Radio 2" ;
        SIZE 90, 20 ON CLICK {||oGet:SetColor(Vcolor("FF0000"),,.T.)}
   END RADIOGROUP

   @ 20,120 GET COMBOBOX cm ITEMS aCombo SIZE 100, 150

   @ 20,170 GET UPDOWN upd RANGE 0,80 SIZE 50,30
   @ 160,170 GET DATEPICKER d1 SIZE 80, 20

   @ 20,240 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 180,240 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oModDlg
   oFont:Release()

   IF oModDlg:lResult
      MsgInfo( e1 + chr(10) + chr(13) +                               ;
               "Check1 - " + Iif(c1,"On","Off") + chr(10) + chr(13) + ;
               "Check2 - " + Iif(c2,"On","Off") + chr(10) + chr(13) + ;
               "Radio: " + Str(r1,1) + chr(10) + chr(13) +            ;
               "Combo: " + aCombo[cm] + chr(10) + chr(13) +           ;
               "UpDown: "+Str(upd) + chr(10) + chr(13) +              ;
               "DatePicker: "+Dtoc(d1)                                ;
               ,"Results:" )
   ENDIF
Return Nil