#include "vxh.ch"
#include "BillPrint_Form1.xfm"

#define BOTTOM_POS   50
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------
METHOD BillPrint_Form1_OnLoad( Sender ) CLASS BillPrint_Form1
   LOCAL i, aPrn
   
   aPrn:=GetPrinters()
   FOR i=1 TO Len(aPrn)
      ::ComboBox1:AddItem(aPrn[i])
   NEXT
   
   ::ComboBox1:SetCurSel(::ComboBox1:FindString(1,GetDefaultPrinter()))
RETURN Self


//----------------------------------------------------------------------------------------------------//

METHOD Button1_OnClick( Sender ) CLASS BillPrint_Form1
   Bill(Self, .T.)
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Button2_OnClick( Sender ) CLASS BillPrint_Form1
   Bill(Self, .F.)
RETURN Self


//----------------------------------------------------------------------------------------------------

FUNCTION Bill(Self, lPreview)

   LOCAL oPrn,nLineWidth,cText,nRow,nCol,nPctr
   LOCAL nRowCtr,nColCtr
   LOCAL aCol:={{"Item",40,1,"FIELD->ITEMDESC"},;
              {"Rate",20,3,"TRANSFORM(FIELD->RATE,'99,999.99')"}, ;
              {"Qty",20,3,"TRANSFORM(FIELD->QTY,'99,999')"}, ;
              {"VAT %",20,3,"TRANSFORM(FIELD->VAT,'999.99 %')"}, ;              
              {"Amount",25,3,"TRANSFORM(FIELD->RATE*FIELD->QTY*(1+FIELD->VAT/100),'999,999,999.99')"}}
   LOCAL cDbf:=Left( GetModuleFileName(), Rat("\" ,GetModuleFileName() ) )+"SALEDTL.dbf",cAlias:="A1",nTotAmt,nGap

   DEFAULT lPreview TO .T.
   
   DbCreate(cDbf,{{"ITEMDESC","C",40,0},{"RATE","N",9,2},{"VAT","N",6,2},{"QTY","N",6,0}})
   
   USE (cDbf) ALIAS (cAlias)
   
   FOR nRowCtr=1 TO 10
      (cAlias)->(DbAppend())
      (cAlias)->ITEMDESC:="Item "+NToC(nRowCtr)
      (cAlias)->RATE:=HB_RandomInt(100,1000)      
      (cAlias)->QTY:=HB_RandomInt(1,100)
      (cAlias)->VAT:=HB_RandomInt(0,25)
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
   
   oPrn:lLandscape:=.F.
   
   IF !oPrn:Create()
      (cAlias)->(DbCloseArea())
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
      IF nRow=0 .OR. nRow>=oPrn:nPageHeight-oPrn:nTopMargin-oPrn:nBottomMargin-BOTTOM_POS-oPrn:nCharHeight

         IF nRow>=oPrn:nPageHeight-oPrn:nTopMargin-oPrn:nBottomMargin-oPrn:nCharHeight*2
            oPrn:PageBreak()
         ENDIF

         oPrn:SetFont("Arial",16,.T.,.T.,.T.,.F.,0)   
         cText:="Sales Bill" 
         nRow:=20
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,nRow,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))

         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",14,.T.,.F.,.F.,.F.,0)   
         cText:="ABC Company Ltd." 
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,nRow,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))

         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",10,.F.,.F.,.F.,.F.,0)   
         cText:="Address line 1" 
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,nRow,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))

         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",10,.F.,.F.,.F.,.F.,0)   
         cText:="Address line 2" 
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,nRow,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))

         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",10,.F.,.F.,.F.,.F.,0)   
         cText:="Address line 3" 
         oPrn:Say(oPrn:nLeftMargin+nLineWidth/2,nRow,cText,2,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))


         nRow+=oPrn:nCharHeight*2
         oPrn:SetFont("Arial",12,.T.,.F.,.F.,.F.,0)   
         cText:="XYZ Company Ltd" 
         oPrn:Say(oPrn:nLeftMargin,nRow,cText,1,,oPrn:GetTextHeightMM(cText,nLineWidth))
         

         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",10,.F.,.F.,.F.,.F.,0)   
         cText:="First line of address" 
         oPrn:Say(oPrn:nLeftMargin,nRow,cText,1,,oPrn:GetTextHeightMM(cText,nLineWidth))

         
         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",10,.F.,.F.,.F.,.F.,0)   
         cText:="Second line of address" 
         oPrn:Say(oPrn:nLeftMargin,nRow,cText,1,,oPrn:GetTextHeightMM(cText,nLineWidth))
         
         nRow+=oPrn:nCharHeight
         oPrn:SetFont("Arial",10,.F.,.F.,.F.,.F.,0)   
         cText:="Third line of address" 
         oPrn:Say(oPrn:nLeftMargin,nRow,cText,1,nLineWidth,oPrn:GetTextHeightMM(cText,nLineWidth))
         

         nRow+=oPrn:nCharHeight*2
         oPrn:SetFont("Arial",12,.T.,.F.,.F.,.F.,0)   
         cText:="Bill no.: 00000001" 
         oPrn:Say(oPrn:nLeftMargin,nRow,cText,1,,oPrn:GetTextHeightMM(cText,nLineWidth))

         cText:="Date: "+DToC(Date()) 
         oPrn:Say(oPrn:nLeftMargin+nLineWidth,nRow,cText,3,,oPrn:GetTextHeightMM(cText,nLineWidth))


         nRow+=oPrn:nCharHeight
         PrintLine(oPrn,nRow,nLineWidth)
         oPrn:SetFont("Arial",12,.T.,.F.,.F.,.F.,0)   
         nRow+=oPrn:nCharHeight/2
         
         nCol:=oPrn:nLeftMargin
         FOR nColCtr=1 TO Len(aCol)
            cText:=aCol[nColCtr][1]
            nCol+=IF(aCol[nColCtr][3]=1,0,If(aCol[nColCtr][3]=2,aCol[nColCtr][2]/2,aCol[nColCtr][2]))
            oPrn:Say(nCol,;
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
      
      nCol:=oPrn:nLeftMargin
      FOR nColCtr=1 TO Len(aCol)
         cText:=&(StrTran(aCol[nColCtr][4],"FIELD->",cAlias+"->"))
         nCol+=IF(aCol[nColCtr][3]=1,0,If(aCol[nColCtr][3]=2,aCol[nColCtr][2]/2,aCol[nColCtr][2]))
         oPrn:Say(nCol,;
                  nRow,;
                  cText,;
                  aCol[nColCtr][3],;
                  aCol[nColCtr][2],;
                  oPrn:GetTextHeightMM(cText,aCol[nColCtr][2]))
         nCol+=nGap+IF(aCol[nColCtr][3]=1,aCol[nColCtr][2],If(aCol[nColCtr][3]=2,aCol[nColCtr][2]/2,0))
      NEXT

      nTotAmt+=(cAlias)->RATE*(cAlias)->QTY*(1+(cAlias)->VAT/100)
      nRow+=oPrn:nCharHeight 
      
      (cAlias)->(DbSkip())
   ENDDO

   nRow:=oPrn:nPageHeight-oPrn:nTopMargin-oPrn:nBottomMargin-BOTTOM_POS
   
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
   
   nRow+=oPrn:nCharHeight*2
   cText:="Authorised signatory"
   oPrn:SetFont("Arial",12,.T.,.T.,.F.,.F.,0)   
   oPrn:Say(oPrn:nLeftMargin+nLineWidth,nRow,cText,3,50,oPrn:GetTextHeightMM(cText,50))
   


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

