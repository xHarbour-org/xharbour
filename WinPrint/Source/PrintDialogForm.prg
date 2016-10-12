#include "vxh.ch"

#include "PrinterDialog_Form.xfm"
//---------------------------------------- End of system code ----------------------------------------//


FUNCTION GetPrinterDialogWin(oWin,cPrinter,nCopies,lGreyScale,cOkBtnText,lOkToPrint)

   LOCAL lOk

   lOk:=PrinterDialog_Form(oWin, {@cPrinter,@nCopies,@lGreyScale,cOkBtnText,@lOkToPrint}):lReturn

   RETURN lOk

END FUNCTION



//----------------------------------------------------------------------------------------------------//


FUNCTION GetPrinterDialog(cPrinter,nCopies,lGreyScale,cOkBtnText,lOkToPrint)

   LOCAL lOk

   lOk:=PrinterDialog_Form( oApp:Mainform:hWnd, {@cPrinter,@nCopies,@lGreyScale,cOkBtnText,@lOkToPrint}):lReturn

   RETURN lOk

END FUNCTION


//----------------------------------------------------------------------------------------------------//


METHOD PrinterDialog_Form_OnLoad() CLASS PrinterDialog_Form

   LOCAL aPrn,i
   
   
   aPrn:=GetPrinters()
   
   ::lReturn:=.F.
   
   FOR i=1 TO Len(aPrn)
      ::PrinterComboBox:Additem(aPrn[i])
   NEXT
      
   IF Empty(::Params) .OR. Empty(::Params[1])
      i:=Ascan(aPrn,GetDefaultPrinter())
   ELSE
      i:=Ascan(aPrn,{|x| Upper(x)==Upper(::Params[1])})
   ENDIF
   
   IF i>0
      ::PrinterComboBox:Setcursel(i)
   ELSE
      ::PrinterComboBox:Setcursel(1)
   ENDIF

   IF !Empty(::Params) .AND. Len(::Params)>= 2 .AND. !Empty(::Params[2])
      ::CopyMaskEdit:Text:=::Params[2]
   ELSE
      ::CopyMaskEdit:Text:=1
   ENDIF
   
   IF !Empty(::Params) .AND. Len(::Params)>=4 .AND. !Empty(::Params[3])
      IF ::Params[3]
         ::Checkbox3:Check()
      ENDIF
   ENDIF
      
   IF !Empty(::Params) .AND. Len(::Params)>= 4 .AND. !Empty(::Params[4])
      ::Button1:Text:=::Params[4]
   ENDIF

   IF !Empty(::Params) .AND. Len(::Params)>= 5 .AND. ::Params[5]!=NIL
      ::Checkbox4:Visible:=.T.
      ::Checkbox4:State:=::Params[5] 
   ELSE
      ::Checkbox4:Visible:=.F.
   ENDIF


   ::PrinterComboBox:Setfocus()

   RETURN Self
   
END METHOD


//----------------------------------------------------------------------------------------------------//


METHOD CheckBox1_OnClick() CLASS PrinterDialog_Form

   IF ::CheckBox1:Checked()
      ::CheckBox2:Uncheck()
      ::CopyMaskEdit:Text:=1
   ENDIF

   RETURN Self

END METHOD


//----------------------------------------------------------------------------------------------------//


METHOD CheckBox2_OnClick() CLASS PrinterDialog_Form

   IF ::CheckBox2:Checked()
      ::CheckBox1:Uncheck()
      ::CopyMaskEdit:Text:=2
   ENDIF

   RETURN Self

END METHOD


//----------------------------------------------------------------------------------------------------//


METHOD CopyMaskEdit_OnEn_Change( Sender ) CLASS PrinterDialog_Form

   IF Sender:Text=1
      ::CheckBox1:Check()
      ::CheckBox2:Uncheck()
   ELSEIF Sender:Text=2
      ::CheckBox1:Uncheck()
      ::CheckBox2:Check()
   ELSE
      ::CheckBox1:Uncheck()
      ::CheckBox2:Uncheck()
   ENDIF

   RETURN Self

END METHOD


//----------------------------------------------------------------------------------------------------//


METHOD CopyMaskEdit_OnVertScroll( Sender ) CLASS PrinterDialog_Form
   
   IF LoWord(Sender:wParam)==1 .AND. Sender:Caption > 1
      Sender:Caption:=Sender:Caption-1
   ELSEIF LoWord(Sender:wParam) < 1 
      Sender:Caption:=Sender:Caption+1
   ENDIF

   RETURN Self

END METHOD


//----------------------------------------------------------------------------------------------------//


METHOD Button1_OnClick() CLASS PrinterDialog_Form
   
   IF !Empty(::Params) .AND. Len(::Params)>=1
      ::Params[1]:=::PrinterComboBox:Getstring(::PrinterCombobox:Getcursel())
   ENDIF
   
   IF !Empty(::Params) .AND. Len(::Params)>=2
      ::Params[2]:=::CopyMaskEdit:Text
   ENDIF

   IF !Empty(::Params) .AND. Len(::Params)>=3
      ::Params[3]:=::CheckBox3:Checked()
   ENDIF

   IF !Empty(::Params) .AND. Len(::Params)>=5
      ::Params[5]:=::CheckBox4:Checked()
   ENDIF
   
   ::lReturn:=.T.
   
   ::Close()
   
   RETURN Self

END METHOD


//----------------------------------------------------------------------------------------------------//


METHOD PrnPrefButton_OnClick() CLASS PrinterDialog_Form
   ::Hide()
   IF !(::Parent=NIL)
      ::Parent:Hide()
   ENDIF
   WaitExecute("rundll32",'printui.dll PrintUIEntry /p /n"'+::PrinterComboBox:Getstring(::PrinterCombobox:Getcursel())+'"')
//   ShellExecute(::hWnd,"OPEN","rundll32",'printui.dll PrintUIEntry /p /n"'+::PrinterComboBox:Getstring(::PrinterCombobox:Getcursel())+'"',"",1)
   IF !(::Parent=NIL)
      ::Parent:Show()
   ENDIF
   ::Show()
   //rundll32 printui.dll PrintUIEntry /p /nFinePrint
   
    
   RETURN Self

END METHOD


//----------------------------------------------------------------------------------------------------//
