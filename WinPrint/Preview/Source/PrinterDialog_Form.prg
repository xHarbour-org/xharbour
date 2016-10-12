#include "vxh.ch"

#include "PrinterDialog_Form.xfm"
//---------------------------------------- End of system code ----------------------------------------//


FUNCTION GetPrinterDialog(oWin,cPrinter,nCopies,aPages,lOkToPrint)

   LOCAL lOk

   lOk:=PrinterDialog_Form(oWin, {@cPrinter,@nCopies,@aPages,@lOkToPrint}):lReturn

RETURN lOk


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
      IF ::Params[2]=2
         ::CheckBox2:Check()
      ENDIF
      ::CopyEditBox:Text:=NToC(::Params[2])
   ELSE
      ::CheckBox1:Check()
      ::CopyEditBox:Text:="1"
   ENDIF
   
   IF !Empty(::Params) .AND. Len(::Params)>=3 .AND. !Empty(::Params[3])   
      IF ::Params[3]==NIL .OR. Empty(::Params[3]) .OR. ::Params[3][1]
         ::AllPageCheckBox:Check()
      ELSEIF ::Params[3][2]
         ::CurPageCheckBox:Check()
      ELSEIF Empty(::Params[3][3])
         ::AllPageCheckBox:Check()
      ELSE
         ::PageEditBox:Text:=::Params[3][3]
      ENDIF
   ELSE
      ::AllPageCheckBox:Check()
   ENDIF

   IF !Empty(::Params) .AND. Len(::Params)>= 4 .AND. ::Params[4]!=NIL
      ::Checkbox4:Visible:=.T.
      ::Checkbox4:State:=::Params[4] 
   ELSE
      ::Checkbox4:Visible:=.F.
   ENDIF

   ::PrinterComboBox:Setfocus()

RETURN Self
   

//----------------------------------------------------------------------------------------------------//


METHOD CheckBox1_OnClick() CLASS PrinterDialog_Form

   IF ::CheckBox1:Checked()
      ::CheckBox2:Uncheck()
      ::CopyEditBox:Text:="1"
   ENDIF

RETURN Self



//----------------------------------------------------------------------------------------------------//


METHOD CheckBox2_OnClick() CLASS PrinterDialog_Form

   IF ::CheckBox2:Checked()
      ::CheckBox1:Uncheck()
      ::CopyEditBox:Text:="2"
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD CopyEditBox_OnEn_Change( Sender ) CLASS PrinterDialog_Form

   IF Val(Sender:Text)=1
      ::CheckBox1:Check()
      ::CheckBox2:Uncheck()
   ELSEIF Val(Sender:Text)=2
      ::CheckBox1:Uncheck()
      ::CheckBox2:Check()
   ELSE
      ::CheckBox1:Uncheck()
      ::CheckBox2:Uncheck()
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD CopyEditBox_OnVertScroll( Sender ) CLASS PrinterDialog_Form
   
   IF LoWord(Sender:wParam)==1 .AND. Val(Sender:Text) > 1
      Sender:Text:=NToC(Val(Sender:Text)-1)
   ELSEIF LoWord(Sender:wParam) < 1 
      Sender:Text:=NToC(Val(Sender:Text)+1)
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Button1_OnClick() CLASS PrinterDialog_Form
   
   IF !Empty(::Params) .AND. Len(::Params)>=1
      ::Params[1]:=::PrinterComboBox:Getstring(::PrinterCombobox:Getcursel())
   ENDIF
   
   IF !Empty(::Params) .AND. Len(::Params)>=2
      ::Params[2]:=Val(::CopyEditBox:Text)
   ENDIF

   IF !Empty(::Params) .AND. Len(::Params)>=3   
      ::Params[3]:={::AllPageCheckBox:Checked(),::CurPageCheckBox:Checked(),::PageEditBox:Text}
   ENDIF
   

   IF !Empty(::Params) .AND. Len(::Params)>=4
      ::Params[4]:=::CheckBox4:Checked()
   ENDIF
   
   ::lReturn:=.T.
   
   ::Close()
   
RETURN Self


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


//----------------------------------------------------------------------------------------------------//

//----------------------------------------------------------------------------------------------------
METHOD AllPageCheckBox_OnClick( Sender ) CLASS PrinterDialog_Form
   IF Sender:Checked() 
      IF ::CurPageCheckBox:Checked()
         ::CurPageCheckBox:Uncheck()
      ENDIF
      ::PageEditBox:Text:={}
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD CurPageCheckBox_OnClick( Sender ) CLASS PrinterDialog_Form
   IF Sender:Checked() 
      IF ::AllPageCheckBox:Checked()
         ::AllPageCheckBox:Uncheck()
      ENDIF
      ::PageEditBox:Text:={}
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD PageEditBox_OnChar( Sender ) CLASS PrinterDialog_Form
   LOCAL nKey := Sender:wParam
   ::AllPageCheckBox:Uncheck()
   ::CurPageCheckBox:Uncheck()
RETURN Self
