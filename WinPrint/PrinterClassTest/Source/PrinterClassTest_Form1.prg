#include "vxh.ch"
#include "PrinterClassTest_Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------
METHOD PrinterClassTest_Form1_OnLoad( Sender ) CLASS PrinterClassTest_Form1
   LOCAL i, aPrn
   
   aPrn:=GetPrinters()
   FOR i=1 TO Len(aPrn)
      ::ComboBox1:AddItem(aPrn[i])
   NEXT
   
   ::ComboBox1:SetCurSel(::ComboBox1:FindString(1,GetDefaultPrinter()))
RETURN Self


//----------------------------------------------------------------------------------------------------//

METHOD Button1_OnClick( Sender ) CLASS PrinterClassTest_Form1
   DO CASE
      CASE ::RadioButton1:Checked()
         BasicReport(Self, .T.)
      CASE ::RadioButton2:Checked()
         SaleReport(Self, .T.)
   ENDCASE
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Button2_OnClick( Sender ) CLASS PrinterClassTest_Form1
   DO CASE
      CASE ::RadioButton1:Checked()
         BasicReport(Self, .F.)
      CASE ::RadioButton2:Checked()
         SaleReport(Self, .F.)
   ENDCASE
RETURN Self


//----------------------------------------------------------------------------------------------------

FUNCTION BasicReport(Self, lPreview)

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

   oPrn:cPrinter:=::ComboBox1:GetString()

   oPrn:cFormType:="9" //A4
   
   oPrn:nCopies:=If(Val(::EditBox1:Text)<1,1,Val(::EditBox1:Text))

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
   
   

//----------------------------------------------------------------------------------------------------

FUNCTION SaleReport(Self, lPreview)

   LOCAL oPrn,nLineWidth,cText,nRow,nCol,nPctr
   LOCAL nRowCtr,nColCtr,aCol:={{"Date",20,1,"DToC(FIELD->BILLDT)"}, {"Number",20,1,"FIELD->BILLNO"}, {"Customer",80,1,"FIELD->CUSTNM"}, {"Amount",25,3,"TRANSFORM(FIELD->AMT,'999,999,999.99')"}},nGap
   LOCAL cDbf:=Left( GetModuleFileName(), Rat("\" ,GetModuleFileName() ) )+"sale.dbf",cAlias:="A1",nTotAmt

   DEFAULT lPreview TO .T.
   
   DbCreate(cDbf,{{"billno","C",10,0},{"billdt","D",8,0},{"custnm","C",50,0},{"amt","N",10,2}})
   
   USE (cDbf) ALIAS (cAlias)
   
   FOR nRowCtr=1 TO 100
      (cAlias)->(DbAppend())
      (cAlias)->BILLNO:=Str(nRowCtr,10,0)
      (cAlias)->BILLDT:=Date()
      (cAlias)->CUSTNM:="Customer "+NToC(nRowCtr)
      (cAlias)->AMT:=HB_RandomInt(10,10000)
      (cAlias)->(DbUnlock())
   NEXT
   
   oPrn:=WinPrint():New(lPreview,.F.)  

   IF oPrn=NIL
      (cAlias)->(DbCloseArea())
      RETURN NIL
   ENDIF
   
   WITH OBJECT oPrn
      :nTopMargin:=10
      :nLeftMargin:=10
      :nRightMargin:=10
      :nBottomMargin:=10
   END   

   oPrn:cPrinter:=::ComboBox1:GetString()

   oPrn:cFormType:="9" //A4
   
   oPrn:nCopies:=If(Val(::EditBox1:Text)<1,1,Val(::EditBox1:Text))

   oPrn:lGreyScale:=.F.

   oPrn:lBestQuality:=.T.
   oPrn:lDuplex:=.F.

   IF !oPrn:Create()
      RETURN NIL
   ENDIF

   nLineWidth:=oPrn:nPageWidth-oPrn:nLeftMargin-oPrn:nRightMargin
   
   oPrn:SetPenWidth(0.1)
   
   nGap:=0
   IF Len(aCol)>1
      FOR nColCtr=1 TO Len(aCol)
         nGap+=aCol[nColCtr][2]
      NEXT
      
      nGap:=(nLineWidth-nGap)/(Len(aCol)-1)
   ENDIF
   
   oPrn:SetFont("Arial",12,.F.,.F.,.F.,.F.,0)   
   
   nRow:=0
   nTotAmt:=0.00
   (cAlias)->(DbGoTop())
   nPctr:=0
   WHILE !(cAlias)->(Eof())
      IF nRow=0 .OR. nRow>=oPrn:nPageHeight-oPrn:nTopMargin-oPrn:nBottomMargin-oPrn:nCharHeight*2

         IF nRow>=oPrn:nPageHeight-oPrn:nTopMargin-oPrn:nBottomMargin-oPrn:nCharHeight*2
            oPrn:PageBreak()
         ENDIF

         oPrn:SetFont("Arial",16,.T.,.T.,.T.,.F.,0)   
         cText:="Sales Report" 
         nRow:=20
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,nRow,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))

         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",14,.T.,.F.,.F.,.F.,0)   
         cText:="ABC Company Ltd." 
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,nRow,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))

         nRow+=oPrn:nCharHeight
         PrintLine(oPrn,nRow,nLineWidth)
         oPrn:SetFont("Arial",12,.T.,.F.,.F.,.F.,0)   
         nRow+=oPrn:nCharHeight/2
         
         nCol:=0
         FOR nColCtr=1 TO Len(aCol)
            cText:=aCol[nColCtr][1]
            oPrn:Say(oPrn:nLeftMargin+nCol+IF(aCol[nColCtr][3]=1,0,If(aCol[nColCtr][3]=2,aCol[nColCtr][2]/2,aCol[nColCtr][2])),;
                     nRow,;
                     cText,;
                     aCol[nColCtr][3],;
                     aCol[nColCtr][2],;
                     oPrn:GetTextHeightMM(cText,aCol[nColCtr][2]))
            nCol+=nGap+IF(aCol[nColCtr][3]=1,aCol[nColCtr][2],If(aCol[nColCtr][3]=2,aCol[nColCtr][2]/2,0))
         NEXT
         nRow+=oPrn:nCharHeight
         PrintLine(oPrn,nRow,nLineWidth)
         nRow+=oPrn:nCharHeight/2

         oPrn:SetFont("Arial",10,.T.,.F.,.F.,.F.,0)   
         nPctr++
         cText:="Page : "+NToC(nPctr)
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,oPrn:nPageHeight-oPrn:nTopMargin-oPrn:nBottomMargin,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))         
   
      ENDIF
     
      oPrn:SetFont("Arial",10,.F.,.F.,.F.,.F.,0)   
      
      nCol:=0
      FOR nColCtr=1 TO Len(aCol)
         cText:=&(StrTran(aCol[nColCtr][4],"FIELD->",cAlias+"->"))
         oPrn:Say(oPrn:nLeftMargin+nCol+IF(aCol[nColCtr][3]=1,0,If(aCol[nColCtr][3]=2,aCol[nColCtr][2]/2,aCol[nColCtr][2])),;
                  nRow,;
                  cText,;
                  aCol[nColCtr][3],;
                  aCol[nColCtr][2],;
                  oPrn:GetTextHeightMM(cText,aCol[nColCtr][2]))
         nCol+=nGap+IF(aCol[nColCtr][3]=1,aCol[nColCtr][2],If(aCol[nColCtr][3]=2,aCol[nColCtr][2]/2,0))
      NEXT

      nTotAmt+=(cAlias)->AMT
      nRow+=oPrn:nCharHeight 
      
      (cAlias)->(DbSkip())
   ENDDO

   oPrn:SetFont("Arial",12,.T.,.F.,.F.,.F.,0)   
   PrintLine(oPrn,nRow,nLineWidth)
   nRow+=oPrn:nCharHeight/2+1

   cText:="Total amount :" 
   oPrn:Say(oPrn:nLeftMargin,nRow,cText,1,30,oPrn:GetTextHeightMM(cText,30))
   
   cText:=Transform(nTotAmt,"999,999,999.99")
   oPrn:Say(oPrn:nLeftMargin+nLineWidth,nRow,cText,3,30,oPrn:GetTextHeightMM(cText,30))
   
   nRow+=oPrn:nCharHeight
   PrintLine(oPrn,nRow,nLineWidth)
   nRow+=oPrn:nCharHeight/2


   IF lPreview
      oPrn:Preview(Self)
   ENDIF
   
   (cAlias)->(DbCloseArea())
   oPrn:Close()


RETURN NIL



//----------------------------------------------------------------------------------------------------


FUNCTION PrintLine(oPrn,t,w)

   t:=t-oPrn:nCharHeight*0.5

   oPrn:Line(t,oPrn:nLeftMargin,t,oPrn:nLeftMargin+w)

RETURN Nil
