/*
 *$Id: testget2.prg,v 1.4 2004/02/06 11:38:22 lculik Exp $
 *
 * HwGUI Samples
 * testget2.prg - GET system and Timer in dialog box.
 */

#include "windows.ch"
#include "guilib.ch"

Function Main
Local oMainWindow
Private var1 := 10320.54

   INIT WINDOW oMainWindow MAIN TITLE "Example" ;
     AT 200,0 SIZE 400,150

   MENU OF oMainWindow
      MENUITEM "&Exit" ACTION EndWindow()
      MENUITEM "&Get a value" ACTION DlgGet()
      MENUITEM "&Text Ballon" ACTION TestBallon()
      MENUITEM "&Hd Serial  " ACTION MsgInfo( HdSerial("C:\"),"HD Serial number" )
   ENDMENU

   ACTIVATE WINDOW oMainWindow
Return Nil

Function DlgGet()
Local oModDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 ), oTimer
Local e1 := "Dialog from prg"
Local e2 := Date()
Local e3 := 10320.54
Local e4:="11222333444455"
Local e5 := 10320.54
Private oSayT

   INIT DIALOG oModDlg CLIPPER NOEXIT TITLE "Get a value"  ;
   AT 210,10  SIZE 300,320                  ;
   FONT oFont ;
   ON INIT {|| SetTimer(oModDlg,@oTimer)}

   SET KEY FSHIFT,VK_F3 TO MsgInfo("Shift-F3") 
   SET KEY FCONTROL,VK_F3 TO MsgInfo("Ctrl-F3") 
   SET KEY 0,VK_F3 TO MsgInfo("F3") 

   @ 20,10 SAY "Input something:" SIZE 260, 22

   @ 20,35 GET e1                       ;
        PICTURE "XXXXXXXXXXXXXXX"       ;
        SIZE 260, 26

   @ 20,65 GET e2  SIZE 260, 26

   @ 20,95 GET e3  SIZE 260, 26

   @ 20,125 GET e4                      ;
        PICTURE "@R 99.999.999/9999-99" ;
        SIZE 260, 26

   @ 20,155 GET e5                      ;
        PICTURE "@e 999,999,999.99"     ;
        SIZE 260, 26

   @ 20,240  BUTTON "Ok" SIZE 100, 32 ON CLICK {||oModDlg:lResult:=.T.,EndDialog()}
   @ 180,240 BUTTON "Cancel" ID IDCANCEL SIZE 100, 32

   @ 100,285 SAY oSayT CAPTION "" SIZE 100,22 STYLE WS_BORDER + SS_CENTER ;
      COLOR 10485760 BACKCOLOR 12507070

   ReadExit( .T. )
   ACTIVATE DIALOG oModDlg

   oTimer:End()

   IF oModDlg:lResult
      MsgInfo( e1 + chr(10) + chr(13) +       ;
               Dtoc(e2) + chr(10) + chr(13) + ;
               Str(e3) + chr(10) + chr(13) +  ;
               e4 + chr(10) + chr(13) +       ;
               Str(e5) + chr(10) + chr(13)    ;
               ,"Results:" )
   ENDIF

Return Nil

Static Function SetTimer( oDlg,oTimer )

   SET TIMER oTimer OF oDlg VALUE 1000 ACTION {||TimerFunc()}
Return Nil

Static Function TimerFunc()

   oSayT:SetValue( Time() )
Return Nil


Function TestBallon

   Local oWnd

   SetToolTipBalloon(.t.)


   INIT DIALOG oWnd CLIPPER TITLE "Dialog text Balon" ;
      AT 100,100 SIZE 140,100

   @ 20,20 BUTTON "Button 1" ON CLICK {||MsgInfo("Button 1")} SIZE 100,40 ;
       TOOLTIP "ToolTip do Button 1"

   ACTIVATE DIALOG oWnd

   Return Nil

