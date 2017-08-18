#include "vxh.ch"


#include "PrintPreview_Form2.xfm"
//---------------------------------------- End of system code ----------------------------------------//


//----------------------------------------------------------------------------------------------------//


FUNCTION PrintPreview_GetPageNo(oForm, nPageNo, nMaxPageNo)

   LOCAL lOk

   lOk:=PrintPreview_Form2(oForm, {@nPageNo, nMaxPageNo}):lReturn    

RETURN lOk




//----------------------------------------------------------------------------------------------------//


METHOD PrintPreview_Form2_OnLoad() CLASS PrintPreview_Form2

   ::Caption := "Goto page"
   ::GroupBox1:Caption := "Goto page"
   ::Label1:Caption := "Page no"+" :"

   ::lReturn := .F.
   ::edtPageNo:Caption := ::Params[1]
   ::edtPageNo:SetFocus()

RETURN Self



//----------------------------------------------------------------------------------------------------//


METHOD edtPageNo_OnVertScroll( Sender ) CLASS PrintPreview_Form2
   IF LoWord( Sender:wParam ) == 1 .AND. Sender:Caption > 1
      Sender:Caption := Sender:Caption-1
   ELSEIF LoWord( Sender:wParam ) < 1 .AND. Sender:Caption < ::Params[2]
      Sender:Caption := Sender:Caption+1
   ENDIF
RETURN Self



//----------------------------------------------------------------------------------------------------//


METHOD Button1_OnClick( Sender ) CLASS PrintPreview_Form2
   (Sender)
   ::Params[1] := ::edtPageNo:Caption
   ::lReturn   := .T.
   ::Close()
RETURN Self

