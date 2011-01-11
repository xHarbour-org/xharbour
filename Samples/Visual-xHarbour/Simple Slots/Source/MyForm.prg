GLOBAL AppCaption

GLOBAL lSlot1, lSlot2, lSlot3

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
METHOD ButtonSpin_OnClick( Sender ) CLASS MyForm
   lSlot1 := .T.
   lSlot2 := .T.
   lSlot3 := .T.

   ::MyTimer:Start()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonLucky_OnClick( Sender ) CLASS MyForm
   lSlot1 := .F.
   lSlot2 := .F.
   lSlot3 := .F.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSlot1_OnClick( Sender ) CLASS MyForm
   lSlot1 := .F.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSlot2_OnClick( Sender ) CLASS MyForm
   lSlot2 := .F.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSlot3_OnClick( Sender ) CLASS MyForm
   lSlot3 := .F.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyTimer_OnTimeOut( Sender ) CLASS MyForm
   LOCAL nNumbers := "0123456789"

   IF lSlot1
      ::LabelSlot1:Caption := nNumbers[HB_RANDOM(1, LEN(nNumbers))]
   END
   IF lSlot2
      ::LabelSlot2:Caption := nNumbers[HB_RANDOM(1, LEN(nNumbers))]
   END
   IF lSlot3
      ::LabelSlot3:Caption := nNumbers[HB_RANDOM(1, LEN(nNumbers))]
   END

   IF lSlot1 == .F. .AND. lSlot2 == .F. .AND. lslot3 == .F.
      ::MyTimer:Stop()

      IF ::LabelSlot1:Caption == ::LabelSlot2:Caption .AND. ::LabelSlot1:Caption == ::LabelSlot3:Caption
         IF ::MessageBox( "Congratulations!" + xCRLF + xCRLF + "Would you like to play again?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
            lSlot1 := .T.
            lSlot2 := .T.
            lSlot3 := .T.
            ::MyTimer:Start()
         END
      ELSEIF ::LabelSlot1:Caption == ::LabelSlot2:Caption .OR. ::LabelSlot1:Caption == ::LabelSlot3:Caption .OR. ::LabelSlot2:Caption == ::LabelSlot3:Caption
         IF ::MessageBox( "Almost there!" + xCRLF + xCRLF + "Would you like to play again?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
            lSlot1 := .T.
            lSlot2 := .T.
            lSlot3 := .T.
            ::MyTimer:Start()
         END
      ELSE
         IF ::MessageBox( "Too bad!" + xCRLF + xCRLF + "Would you like to play again?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
            lSlot1 := .T.
            lSlot2 := .T.
            lSlot3 := .T.
            ::MyTimer:Start()
         END
      END
   END
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   AppCaption := "Simple Slots " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption   
RETURN Self