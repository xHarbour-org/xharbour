#include "vxh.ch"
#include "PrinterClassTest_Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//


//----------------------------------------------------------------------------------------------------//

METHOD Button1_OnClick( Sender ) CLASS PrinterClassTest_Form1
   WinPrintOutput(Self, .T.)
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Button2_OnClick( Sender ) CLASS PrinterClassTest_Form1
   WinPrintTestOutput(Self, .F.)   
RETURN Self


FUNCTION WinPrintTestOutput(Self, lPreview)

   LOCAL oPrn,nLineWidth,cText,cPath

   DEFAULT lPreview TO .T.
   
   oPrn:=WinPrint():New(lPreview,.F.)  

   IF oPrn=NIL
      RETURN NIL
   ENDIF
   
   WITH OBJECT oPrn
      :nTopMargin:=10
      :nLeftMargin:=10
      :nRightMargin:=10
      :nBottomMargin:=10
   END   

   oPrn:cPrinter:=GetDefaultPrinter()

   oPrn:cFormType:="9" //A4
   
   oPrn:nCopies:=2

   oPrn:lGreyScale:=.F.

   oPrn:lBestQuality:=.T.
   oPrn:lDuplex:=.F.

   IF !oPrn:Create()
      RETURN NIL
   ENDIF

   nLineWidth:=oPrn:nPageWidth-oPrn:nLeftMargin-oPrn:nRightMargin
   
   oPrn:SetPenWidth(0.1)
   
   oPrn:SetFont("Times New Roman",10,.F.,.F.,.F.,.F.,0)
   
   cText:="Hello world with WinPrn class for VXH!"   
   oPrn:Say(oPrn:nLeftMargin,20,cText,1,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))
   cText:="This is page 1 test."   
   oPrn:Say(oPrn:nLeftMargin,120,cText,1,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))
   
   cPath:=Left( GetModuleFileName(), Rat("\" ,GetModuleFileName() )-1 )
      
   oPrn:Image(cPath+"\Pic1.jpg", 150, 100, 25, 50 )
   oPrn:PageBreak()

   cText:="Page 2"   
   oPrn:Say(oPrn:nLeftMargin,20,cText,1,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))
   oPrn:Image(cPath+"\Pic2.jpg", 200, 75, 25, 50 )
   oPrn:PageBreak()

   cText:="Page 3"   
   oPrn:Say(oPrn:nLeftMargin,20,cText,1,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))
   //oPrn:PageBreak()

   IF lPreview
      oPrn:Preview(Self)
   ENDIF
   
   oPrn:Close()
   
RETURN NIL
   
   
