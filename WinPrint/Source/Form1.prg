#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//


//----------------------------------------------------------------------------------------------------//

METHOD Button1_OnClick( Sender ) CLASS Form1

   LOCAL oPrn,nLineWidth,cText,cPath
   
   oPrn:=WinPrint():New(.T.,.F.)  // Preview 

   IF oPrn=NIL
      RETURN Self
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
      RETURN Self
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

   oPrn:Preview(Self)
   
   oPrn:Close()
   
RETURN Self
