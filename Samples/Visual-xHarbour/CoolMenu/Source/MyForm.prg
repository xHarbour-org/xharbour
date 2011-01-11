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
METHOD FileOpen_OnClick( Sender ) CLASS MyForm
   ::MessageBox( "Open ...", AppCaption, MB_OK)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FileClose_OnClick( Sender ) CLASS MyForm
   ::MessageBox( "Close ...", AppCaption, MB_OK)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FileExit_OnClick( Sender ) CLASS MyForm
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD SettingsOne_OnClick( Sender ) CLASS MyForm
   IF ::MenuFile:Enabled == .T.
      ::MenuFile:Enabled := .F.
      Sender:Caption := "Enable File Menu"
   ELSE
      ::MenuFile:Enabled := .T.
      Sender:Caption := "Disable File Menu"
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD SettingsDummy1_OnClick( Sender ) CLASS MyForm
   IF Sender:IsChecked()
      Sender:UnCheck()
   ELSE
      Sender:Check()
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD SettingsDummy2_OnClick( Sender ) CLASS MyForm
   IF Sender:IsChecked()
      Sender:UnCheck()
   ELSE
      Sender:Check()
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD SettingsDummy3_OnClick( Sender ) CLASS MyForm
   IF Sender:IsChecked()
      Sender:UnCheck()
   ELSE
      Sender:Check()
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD SettingsDummy4_OnClick( Sender ) CLASS MyForm
   IF Sender:IsChecked()
      Sender:UnCheck()
   ELSE
      Sender:Check()
   END
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   AppCaption := ::AppLication:Name + " " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption   
RETURN Self