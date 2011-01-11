GLOBAL AppCaption, lReady:=.F.
GLOBAL aHistory, nHistoryPointer

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
METHOD File_Exit_OnClick( Sender ) CLASS MyForm
   ::Close()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD MyWebBrowser_ProgressChange( Sender, Progress, ProgressMax ) CLASS MyForm
   ::MyProgressBar:StepIt()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD MyWebBrowser_DocumentComplete( Sender, pDisp, URL ) CLASS MyForm
   ::MyProgressBar:Visible := .F.
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button_Go_OnClick( Sender ) CLASS MyForm
   ::MyWebBrowser:Url := ::Edit_Address:Caption
   AADD(aHistory, ::Edit_Address:Caption)
   nHistoryPointer := LEN(aHistory)
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Edit_Address_OnKeyUp( Sender ) CLASS MyForm
   IF Sender:wParam == 13
      ::MyWebBrowser:Url := Sender:Caption
      AADD(aHistory, ::Edit_Address:Caption)
      nHistoryPointer := LEN(aHistory)
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Main_xHarbour_OnClick( Sender ) CLASS MyForm
   ::MyWebBrowser:Url := "http://www.xHarbour.com"
   ::Edit_Address:Caption := "http://www.xHarbour.com"

   AADD(aHistory, ::Edit_Address:Caption)
   nHistoryPointer := LEN(aHistory)
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Main_Back_OnClick( Sender ) CLASS MyForm
   nHistoryPointer := IF(nHistoryPointer > 1, nHistoryPointer - 1, 1)

   ::MyWebBrowser:Url := aHistory[nHistoryPointer]
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Main_Forward_OnClick( Sender ) CLASS MyForm
   nHistoryPointer := IF(nHistoryPointer < LEN(aHistory), nHistoryPointer + 1, nHistoryPointer)

   ::MyWebBrowser:Url := aHistory[nHistoryPointer]
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Main_Home_OnClick( Sender ) CLASS MyForm
   ::MyWebBrowser:Url := "http://www.xHarbour.com"
   ::Edit_Address:Caption := "http://www.xHarbour.com"

   AADD(aHistory, ::Edit_Address:Caption)
   nHistoryPointer := LEN(aHistory)
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Edit_Address_OnLButtonUp( Sender ) CLASS MyForm
   ::Edit_Address:SetSel(0, LEN(::Edit_Address:Caption)+1)
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Navigate_Back_OnClick( Sender ) CLASS MyForm
   nHistoryPointer := IF(nHistoryPointer > 1, nHistoryPointer - 1, 1)

   ::MyWebBrowser:Url := aHistory[nHistoryPointer]
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Navigate_Forward_OnClick( Sender ) CLASS MyForm
   nHistoryPointer := IF(nHistoryPointer < LEN(aHistory), nHistoryPointer + 1, nHistoryPointer)

   ::MyWebBrowser:Url := aHistory[nHistoryPointer]
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Navigate_Home_OnClick( Sender ) CLASS MyForm
   ::MyWebBrowser:Url := "http://www.xHarbour.com"
   ::Edit_Address:Caption := "http://www.xHarbour.com"

   AADD(aHistory, ::Edit_Address:Caption)
   nHistoryPointer := LEN(aHistory)
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   lReady:=.t.
   AppCaption := ::Application:Name + " " + ::Application:Version

   ::Caption := "xHarbour.com Training Center | " + AppCaption

   aHistory := {"http://www.xHarbour.com"}
   nHistoryPointer := 1   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyWebBrowser_BeforeNavigate2( Sender, pDisp, URL, Flags, TargetFrameName, PostData, Headers, Cancel ) CLASS MyForm
   if !lReady
      return Self
   endif
   ::MyProgressBar:Visible:=.t.
RETURN Self