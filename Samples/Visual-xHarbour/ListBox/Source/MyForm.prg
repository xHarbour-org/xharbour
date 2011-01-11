GLOBAL AppCaption

#include "vxh.ch"
#include "MyForm.xfm"

#translate xCRLF => CHR(13) + CHR(10)
//---------------------------------------- End of system code ----------------------------------------//

//-- CONFIRM QUIT ------------------------------------------------------------------------------------//
METHOD MyForm_OnClose( Sender ) CLASS MyForm
   IF ::Messagebox( "Exit this Visual xHarbour sample?", AppCaption + " | Exit", MB_YESNO + MB_ICONQUESTION) <> 6
      RETURN .F.
   END
RETURN Self

//-- GENERAL LINK ------------------------------------------------------------------------------------//
METHOD LinkWebsite_OnClick( Sender ) CLASS MyForm
   ShellExecute(::hWnd, "OPEN", Sender:Url, , , SW_SHOW)
RETURN Self

//-- QUICK MENU --------------------------------------------------------------------------------------//
METHOD QMenu_About_OnClick( Sender ) CLASS MyForm
   ::MessageBox( "This Visual xHarbour sample project was created by xHarbour.com Inc." + xCRLF + xCRLF + "The user is free to change the source code of this sample project to his/her own desire." , AppCaption + " | About", MB_OK + MB_ICONASTERISK)
RETURN Self

METHOD QMenu_MainSite_OnClick( Sender ) CLASS MyForm
   ShellExecute(::hWnd, "OPEN", "http://www.xHarbour.com/", , , SW_SHOW)
RETURN Self

METHOD QMenu_ShopSite_OnClick( Sender ) CLASS MyForm
   ShellExecute(::hWnd, "OPEN", "http://www.xHarbour.com/Order/", , , SW_SHOW)
RETURN Self

METHOD QMenu_Exit_OnClick( Sender ) CLASS MyForm
   ::Close()
RETURN Self   
//----------------------------------------------------------------------------------------------------//
METHOD ButtonReset_OnClick( Sender ) CLASS MyForm
   ::BoxYear:SetCurSel( ::BoxYear:GetCount() )
   ::BoxMonth:SetCurSel( Month( Date() ) )
   ::BoxDay:SetCurSel( Day( Date() ) )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDisplay_OnClick( Sender ) CLASS MyForm
   LOCAL dMyDate := CToD(AllTrim(Str(::BoxMonth:GetCurSel())) + "-" + ::BoxDay:GetString() + "-" + ::BoxYear:GetString())
   LOCAL aMonths := {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }

   IF ::RadLong:GetState() == 1
      ::MessageBox( "Long date notation:" + xCRLF + xCRLF + CDoW(dMyDate) + ", " + ::BoxMonth:GetString() + " " + ::BoxDay:GetString() + ", " + ::BoxYear:GetString() , AppCaption)
   ELSEIF ::RadShort:GetState() == 1
      ::MessageBox( "Short date notation:" + xCRLF + xCRLF + aMonths[::BoxMonth:GetCurSel()] + " " + ::BoxDay:GetString() + ", " + ::BoxYear:GetString() , AppCaption)
   END
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm   
   LOCAL i
   
   // SAMPLE DETAILS
   AppCaption := ::Application:Name + " " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption

   WITH OBJECT ::BoxYear
      FOR i := 1880 TO Year(Date())
         :AddString( AllTrim(Str(i)))
      NEXT
      :SetCurSel(:GetCount())
   END

   WITH OBJECT ::BoxMonth
      FOR i := 1 TO 12
         :AddString(NToCMonth(i))
      NEXT
      :SetCurSel(Month(Date()))
   END

   WITH OBJECT ::BoxDay
      FOR i := 1 TO 31
         :AddString(AllTrim(Str(i)))
      NEXT
      :SetCurSel(Day(Date()))
   END
RETURN Self