
#define FORM_A4 9

FUNCTION Main()
  LOCAL nPrn:=1
  LOCAL aPrn:= GetPrinters()
  DO WHILE !EMPTY(nPrn)
    CLS
    @ 0,0 SAY 'TPRINT() Class test program. Choose a printer to test'
    @ 1,0 TO maxRow(),maxCol()
    nPrn:= ACHOICE(2,1,maxRow()-1,maxCol()-1,aPrn,.T.,,nPrn)
    IF !EMPTY(nPrn)
      PrnTest(aPrn[nPrn])
    ENDIF
  ENDDO
  RETURN(NIL)

STATIC FUNCTION PrnTest(cPrinter)
  LOCAL oPrinter:= TPrint():New(cPrinter), aFonts, x, nColFixed, nColTTF, nColCharSet
  oPrinter:Landscape:= .F.
  oPrinter:FormType := FORM_A4
  oPrinter:Copies   := 1
  IF !oPrinter:Create()
    Alert("Cannot Create Printer")
  ELSE
    IF !oPrinter:startDoc('TPRINT(Doc name in Printer Properties)')
      Alert("StartDoc() failed")
    ELSE
      oPrinter:Bold(800)
      oPrinter:TextOut(oPrinter:PrinterName+': MaxRow() = '+STR(oPrinter:MaxRow(),4)+'   MaxCol() = '+STR(oPrinter:MaxCol(),4))
      oPrinter:Bold(0)     // Normal
      oPrinter:NewLine()
      oPrinter:TextOut('   Partial list of available fonts that are available for OEM_')
      oPrinter:NewLine()
      oPrinter:UnderLine(.T.)
      oPrinter:Italic(.T.)
//      oPrinter:SetFont('Courier New',7,{3,-50})  // Compressed print
      nColFixed:= 40 * oPrinter:CharWidth
      nColTTF  := 48 * oPrinter:CharWidth
      nColCharSet  := 60 * oPrinter:CharWidth
      oPrinter:TextOut('FontName')
      oPrinter:SetPos(nColFixed)
      oPrinter:TextOut('Fixed?')
      oPrinter:SetPos(nColTTF)
      oPrinter:TextOut('TrueType?')
      oPrinter:SetPos(nColCharset)
      oPrinter:TextOut('CharSet#',.T.)
      oPrinter:NewLine()
      oPrinter:Italic(.F.)
      oPrinter:UnderLine(.F.)
      aFonts:= oPrinter:GetFonts()
      oPrinter:NewLine()
      FOR x:= 1 TO LEN(aFonts) STEP 2
        oPrinter:CharSet(aFonts[x,4])
        IF oPrinter:SetFont(aFonts[x,1])       // Could use "IF oPrinter:SetFontOk" after call to oPrinter:SetFont()
          IF oPrinter:FontName == aFonts[x,1]  // Make sure Windows didn't pick a different font
            oPrinter:TextOut(aFonts[x,1])
            oPrinter:SetPos(nColFixed)
            oPrinter:TextOut(IIF(aFonts[x,2],'Yes','No'))
            oPrinter:SetPos(nColTTF)
            oPrinter:TextOut(IIF(aFonts[x,3],'Yes','No'))
            oPrinter:SetPos(nColCharSet)
            oPrinter:TextOut(STR(aFonts[x,4],5))
            oPrinter:SetPos(oPrinter:LeftMargin, oPrinter:PosY + (oPrinter:CharHeight*2))
            IF oPrinter:PRow() > oPrinter:MaxRow() - 10  // Could use "oPrinter:NewPage()" to start a new page
              EXIT
          ENDIF
          ENDIF
        ENDIF
      NEXT x
      oPrinter:SetFont('Lucida Console',8,{3,-50})  // Alternative Compressed print
      oPrinter:CharSet(0)  // Reset default charset
      oPrinter:Bold(800)
      oPrinter:NewLine()
      oPrinter:TextOut('This is on line'+STR(oPrinter:Prow(),4)+', Printed bold, ' )
      oPrinter:TextOut(' finishing at Column: ')
      oPrinter:TextOut(STR(oPrinter:Pcol(),4))
      oPrinter:SetPrc(oPrinter:Prow()+3, 0)
      oPrinter:Bold(0)
      oPrinter:TextOut("Notice: UNDERLINE only prints correctly if there is a blank line after",.T.)
      oPrinter:TextOut("        it. This is because of ::LineHeight and the next line",.T.)
      oPrinter:TextOut("        printing over top of the underline. To avoid this happening",.T.)
      oPrinter:TextOut("        you can to alter ::LineHeight")
      oPrinter:NewLine()
      oPrinter:NewLine()
      oPrinter:SetFont('Lucida Console',18, 0)  // Large print
      oPrinter:TextOut("Finally some larger print")
      oPrinter:EndDoc()
    ENDIF
    oPrinter:Destroy()
  ENDIF
  RETURN(NIL)


