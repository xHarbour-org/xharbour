# WinPrint
Generic printer class for xHarbour

It uses Win32Prn class with some improvements to print easily. It has following properties and methods.

   DATA oPrinter,nPage,lPaperLength,lPreload,lPreview,memDC,lBestQuality
   DATA cPrinter,nCharHeight,nCopies,cFormType,lAskProperties
   DATA nPageWidth,nPageHeight,lLandScape
   DATA nTopMargin,nLeftMargin,nRightMargin,nBottomMargin
   DATA cHexShadowColor,nMaxY,aPrn,cFile,cAlias,nPenWidth,lError,lDuplex
   DATA cPrevFontName,nPrevFontSize,lPrevBold,lPrevItalic,lPrevUnderLine,lPrevStrikeOut,nForeColor,lGreyScale

   METHOD New() CONSTRUCTOR

   METHOD Create()
   METHOD WinPrnSetFont()
   METHOD SetFont()
   METHOD SetFontArray()
   METHOD Say()
   METHOD GetJustifiedString()
   METHOD Line()
   METHOD Box()
   METHOD Shadow()
   METHOD Image()
   METHOD PageBreak()
   METHOD GetTextWidthMM()
   METHOD GetTextHeightMM()
   METHOD mm2x()
   METHOD mm2y()
   METHOD x2mm()
   METHOD y2mm()
   METHOD SetPenWidth()
   METHOD Preview() // For this you need to have PrintPreview.EXE file stored in the same folder of main EXE file.

   METHOD Close()
   
Small code snippet to use this class :

FUNCTION BasicReport(lPreview, lLandscape, nCopies, cPrinter, lCustomPage)

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

   oPrn:cPrinter:=cPrinter
 
   IF lCustomPage
      oPrn:cFormType:="0"
      oPrn:nPageWidth:=150 // in mm
      oPrn:nPageHeight:=250 // in mm
   ELSE
      oPrn:cFormType:="9" //A4
   ENDIF
     
   oPrn:nCopies:=nCopies

   oPrn:lGreyScale:=.F.

   oPrn:lBestQuality:=.T.
   oPrn:lDuplex:=.F.
   
   oPrn:lLandscape:=lLandscape

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
      oPrn:Preview(0)
   ENDIF
   
   oPrn:Close()
   
RETURN NIL


