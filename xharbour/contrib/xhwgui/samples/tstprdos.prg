/*
 * HWGUI using sample
 * 
 * Demo for PrintDos CLASS
 * Copyright 2003 Sandro R. R. Freire <sandrorrfreire@yahoo.com.br>
 * www - http://www.lumainformatica.com.br
*/
*************************************************************************************
* Example to conversion
*
* In Clipper
* SET PRINTER TO LPT1
* SET DEVICE TO PRINTER
* @ 10, 10 SAY "PRINTER THIS DEMO"
* @ PROW(), PCOL()+1 SAY " PROW AND PCOL+1 "
* @ 12, 10 SAY 988942.99 PICTURE "@E 999,999,999.99"
* EJECT
* SETPRC(0,0)
* SET DEVICE TO SCREEN
*
*
* In HwGUI    
* SET PRINTER TO "LPT1" OF oPrint 
* //SET DEVICE TO PRINTER
* @ 10, 10 PSAY "PRINTER THIS DEMO" OF oPrint
* @ wPROW(oPrint), wPCOL(oPrint)+1 PSAY " PROW AND PCOL+1 " OF oPrint
* @ 12, 10 PSAY 988942.99 PICTURE "@E 999,999,999.99"  OF oPrint
* EJECT OF oPrint
* wSETPRC(0,0,oPrint)
* //SET DEVICE TO SCREEN
* END PRINTER oPrint
*
*************************************************************************************


#include "windows.ch"
#include "guilib.ch"
#include "fileio.ch"
 
#define PF_BUFFERS   2048

function Main
Local oMain 
  
   INIT WINDOW oMain MAIN TITLE "Example for PrintDos Class" 
  
 
   MENU OF oMain
      MENU TITLE "&Test to File"
         MENUITEM "&Print DOS CLASS"     ACTION TestDosClass("Test.prn")
         MENUITEM "&CLASS Style Clipper" ACTION TestDosClipper("Test.prn")
         SEPARATOR
         MENUITEM "&Printer File using Method" ACTION TestPrinterFile("Test.prn")
         MENUITEM "&Printer in Graphic Mode" ACTION TestGraphic()
         MENUITEM "&Test in Preview        " ACTION TestPreview()
         SEPARATOR
         MENUITEM "&Exit" ACTION EndWindow()
      ENDMENU
 
      MENU TITLE "T&est to Print"
         MENUITEM "&Print DOS CLASS"     ACTION TestDosClass()
         MENUITEM "&CLASS Style Clipper" ACTION TestDosClipper()
  
      ENDMENU
      MENU TITLE "&Help"
         MENUITEM "&About" ACTION ShellAbout("Test PrintDos","By Sandro R. R. Freire")
      ENDMENU
       
   ENDMENU

   oMain:Activate()

return nil

Function TestDosClass(oTest)

If MsgYesNo("Printing PrintDos Class to "+Iif(oTest==Nil,"LPT1",oTest),"PrintDos Class Demo")

   oPrint:=Printdos():New(oTest)   //oTest=Nil LPT1  
   
   oPrint:Say(0,  1,  "LINE 0 COL 1")
   oPrint:Say(10, 11, "LINE 10 COL 11")
   oPrint:Say(10, 31, "LINE 10 COL 31")
   oPrint:Say(14, 21, "LINE 14 COL 21")
   oPrint:Say(30, 34, "LINE 30 COL 34")
   oPrint:Say(oPrint:nProw, oPrint:nPCol, "LINE "+STR(oPrint:nProw)+ " COL "+STR(oPrint:nPcol))
   oPrint:Say(40, 24, "11222333000144","@r 99.999.999/9999-99") 
   oPrint:Say(oPrint:nProw+1, oPrint:nPcol,"Valor" )
   oPrint:Say(oPrint:nProw, oPrint:nPcol, 996659.8, "@E 999,999,999.99" )
   oPrint:Say(oPrint:nProw, oPrint:nPcol+2,22.11)
   oPrint:Say(oPrint:nProw, oPrint:nPcol+1, DATE())
 
   oPrint:Eject()
   
   oPrint:Say(01, 10,"End of printer text, the PrintDos Class")

   oPrint:SetPrc(0,0)
   
   oPrint:End()
 
   if !Empty(oTest)
      OpenRel(oTest)
   EndIF  
   
Endif

Return Nil

Function TestDosClipper(oTest)
Local oPrinter

If MsgYesNo("Printing style clipper to "+Iif(oTest==Nil,"LPT1",oTest),"PrintDos Class Demo")

   SET PRINTER TO oTest OF oPrinter

   @  0,  1 PSAY  "LINE 0 COL 1"  OF oPrinter
   @ 10, 11 PSAY "LINE 10 COL 11" OF oPrinter
   @ 10, 31 PSAY "LINE 10 COL 31" OF oPrinter
   @ 14, 21 PSAY "LINE 14 COL 21" OF oPrinter
   @ 30, 34 PSAY "LINE 30 COL 34" OF oPrinter
   @ wProw(oPrinter), wPCol(oPrinter) PSAY "LINE "+STR(wProw(oPrinter))+ " COL "+STR(wPcol(oPrinter)) OF oPrinter
   @ 40, 24 PSAY "11222333000144" PICTURE "@r 99.999.999/9999-99"  OF oPrinter
   @ wprow(oPrinter)+1, wPcol(oPrinter) PSAY "Valor" OF oPrinter
   @ wprow(oPrinter), wPcol(oPrinter)   PSAY 996659.85 PICTURE "@E 999,999,999.99" OF oPrinter
   @ wprow(oPrinter), wPcol(oPrinter)+1 PSAY  22.11  OF oPrinter
   @ wprow(oPrinter), wPcol(oPrinter)+1 PSAY DATE() OF oPrinter
   
   EJECT OF oPrinter
   
   @ 01, 10 PSAY "End of printer text, the PrintDos Class - Style Clipper" OF oPrinter

   wSetPrc(0,0,oPrinter)
   
   END PRINTER oPrinter

   If !Empty(oTest)
     OpenRel(oTest)
   EndIF  

Endif

Return Nil

FUNCTION OpenRel(oText)
LOCAL oDlg
Local oFont
Local lText   := MemoRead( oText ) 

   PREPARE FONT oFont NAME "MS Sans Serif" WIDTH 0 HEIGHT -12

   INIT DIALOG oDlg TITLE "Open File:"+oText ;
        AT 204,25 SIZE 777, 440 FONT oFont
        
   @ 1,3 EDITBOX lText SIZE 772,384 STYLE WS_VSCROLL + WS_HSCROLL + ES_MULTILINE 

   @ 332,402 BUTTON "Close" ON CLICK {||EndDialog()} SIZE 80,32 

   ACTIVATE DIALOG oDlg

RETURN

Function TestPrinterFile(oTest)

If MsgYesNo("Printing File "+oTest)

   oPrint:=Printdos():New()   //oTest=Nil LPT1  
   oPrint:PrinterFile(oTest)
   oPrint:End()

EndIf

Return Nil

Function TestGraphic()
Local oPrint, oPrint1

oPrint:=Printdos():New("Graphic.txt")

oPrint:Say(0,0,"*************************************************************************************")
oPrint:Say(1,0,"* Example to conversion")
oPrint:Say(2,0,"*")
oPrint:Say(3,0,"* In Clipper")
oPrint:Say(4,0,"* SET PRINTER TO LPT1")
oPrint:Say(5,0,"* SET DEVICE TO PRINTER")
oPrint:Say(6,0,"* @ 10, 10 SAY 'PRINTER THIS DEMO'")
oPrint:Say(7,0,"* @ PROW(), PCOL()+1 SAY 'PROW AND PCOL+1 '")
oPrint:Say(8,0,"* EJECT")
oPrint:Say(9,0,"* SETPRC(0,0)")
oPrint:Say(10,0,"* SET DEVICE TO SCREEN")
oPrint:Say(11,0,"*")
oPrint:Say(12,0,"*")
oPrint:Say(13,0,"* In HwGUI    ")
oPrint:Say(14,0,"* SET PRINTER TO 'LPT1' OF oPrint ")
oPrint:Say(15,0,"* //SET DEVICE TO PRINTER")
oPrint:Say(16,0,"* @ 10, 10 PSAY 'PRINTER THIS DEMO' OF oPrint")
oPrint:Say(17,0,"* @ wPROW(oPrint), wPCOL(oPrint)+1 PSAY 1 PROW AND PCOL+1 1 OF oPrint')")
oPrint:Say(18,0,"* EJECT OF oPrint")
oPrint:Say(19,0,"* wSETPRC(0,0,oPrint)")
oPrint:Say(20,0,"* //SET DEVICE TO SCREEN                                                               ")
oPrint:Say(21,0,"* END PRINTER oPrint                                                                   ")
oPrint:Say(22,0,"*                                                                                    ")
oPrint:Say(23,0,"*************************************************************************************")
oPrint:Eject()
oPrint:Say(0,0,"New Page 02")
oPrint:end()

oPrint1:=Printdos():New("GRAPHIC")
oPrint1:txttoGraphic("Graphic.txt",-13,.T.) //Parameters Name graphic, Size, Preview

oPrint1:End()
Return Nil

Function TestPreview()
Local oPrint, oPrint1

oPrint:=Printdos():New("Preview.txt")

oPrint:Say(0,0,"*************************************************************************************")
oPrint:Say(1,0,"* Example to conversion")
oPrint:Say(2,0,"*")
oPrint:Say(3,0,"* In Clipper")
oPrint:Say(4,0,"* SET PRINTER TO LPT1")
oPrint:Say(5,0,"* SET DEVICE TO PRINTER")
oPrint:Say(6,0,"* @ 10, 10 SAY 'PRINTER THIS DEMO'")
oPrint:Say(7,0,"* @ PROW(), PCOL()+1 SAY 'PROW AND PCOL+1 '")
oPrint:Say(8,0,"* EJECT")
oPrint:Say(9,0,"* SETPRC(0,0)")
oPrint:Say(10,0,"* SET DEVICE TO SCREEN")
oPrint:Say(11,0,"*")
oPrint:Say(12,0,"*")
oPrint:Say(13,0,"* In HwGUI    ")
oPrint:Say(14,0,"* SET PRINTER TO 'LPT1' OF oPrint ")
oPrint:Say(15,0,"* //SET DEVICE TO PRINTER")
oPrint:Say(16,0,"* @ 10, 10 PSAY 'PRINTER THIS DEMO' OF oPrint")
oPrint:Say(17,0,"* @ wPROW(oPrint), wPCOL(oPrint)+1 PSAY 1 PROW AND PCOL+1 1 OF oPrint')")
oPrint:Say(18,0,"* EJECT OF oPrint")
oPrint:Say(19,0,"* wSETPRC(0,0,oPrint)")
oPrint:Say(20,0,"* //SET DEVICE TO SCREEN                                                               ")
oPrint:Say(21,0,"* END PRINTER oPrint                                                                   ")
oPrint:Say(22,0,"*                                                                                    ")
oPrint:Say(23,0,"*************************************************************************************")
oPrint:Eject()
oPrint:Say(0,0,"New Page 02")
oPrint:Eject()
oPrint:Say(0,0,"New Page 03")
oPrint:end()

oPrint1:=Printdos():New("PREVIEW")
oPrint1:Preview("Preview.txt")
oPrint1:End()

Return Nil