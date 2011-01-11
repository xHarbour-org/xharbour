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
   IF ::Messagebox( "Exit this Visual xHarbour sample?", AppCaption + " | Exit", MB_YESNO + MB_ICONQUESTION) == 6
      ::Close()
   END
RETURN Self   

//----------------------------------------------------------------------------------------------------//
METHOD ButtonStart_OnClick( Sender ) CLASS MyForm
   ::BoxTextTyped:Caption := ""
   ::MyProgressBar:Position := 0
   ::MyProgressBar:ForeColor := 32768
   ::MyTimer:Start()
   ::MyProgressBar:Visible := .T.
   Sender:Enabled := .F.
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD MyTimer_OnTimeOut( Sender ) CLASS MyForm
   WITH OBJECT ::MyProgressBar
      :Position += 1

      IF :Position > 85
         :ForeColor := 255
      END

      IF :Position >= :MaxRange
         ::MyTimer:Stop()
         ::ButtonStart:Enabled := .T.
         ::MessageBox( "Your test results:" + xCRLF + xCRLF + "Characters per minute: " + AllTrim(Str(Len(::BoxTextTyped:Caption))) + xCRLF + xCRLF + "Words per minute: " + AllTrim(Str(NumToken(AllTrim(::BoxTextTyped:Caption), " "))), AppCaption, MB_OK)
      END
   END
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   AppCaption := "Typing Test " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption   
RETURN Self