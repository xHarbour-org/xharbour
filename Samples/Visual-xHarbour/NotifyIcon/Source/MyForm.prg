GLOBAL AppCaption

#include "vxh.ch"
#include "MyForm.xfm"

#translate xCRLF => CHR(13) + CHR(10)
//---------------------------------------- End of system code ----------------------------------------//


//-- CONFIRM QUIT ------------------------------------------------------------------------------------//
METHOD MyForm_OnClose( Sender ) CLASS MyForm
   IF ::Messagebox( "Exit this Visual xHarbour sample?", AppCaption + " | Exit", MB_YESNO + MB_ICONQUESTION) <> IDYES
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
METHOD ButtonShow_OnClick( Sender ) CLASS MyForm
   WITH OBJECT ::MyNotifyIcon
      :BalloonTipTitle := alltrim( ::EditTitle:Caption )
      :BalloonTipText := alltrim( ::EditText:Caption )
      :BalloonTipIcon := ::BoxTypes:GetCurSel()
      :Text := alltrim( ::EditIcon:Caption )
      :Visible:=.t.
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ButtonHide_OnClick( Sender ) CLASS MyForm
   ::MyNotifyIcon:Visible:=.F.
RETURN Self

//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm  
   // SAMPLE DETAILS
   AppCaption := ::Application:Name + " " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption

   WITH OBJECT ::BoxTypes
      :AddString("None")
      :AddString("Info")
      :AddString("Warning")
      :AddString("Error")
      :SetCurSel(1)
   END
   
   ::MyNotifyIcon:Text:="NotifyIcon sample"+xCRLF+"tray icon"
   
RETURN Self