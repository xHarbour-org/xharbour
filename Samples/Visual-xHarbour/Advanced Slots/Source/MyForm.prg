GLOBAL AppCaption, cImagePath
GLOBAL GameLevel, GameSlotImages
GLOBAL Slot1, Slot2, Slot3

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
METHOD MyTimer_OnTimeOut( Sender ) CLASS MyForm
   local lOk:=.T., cFile
   IF Slot1 == .T.
      cFile := cImagePath + GameSlotImages[HB_RANDOMINT( 1, LEN(GameSlotImages) )]
      try
         ::Slot1:ImageName := cFile
      catch
         lOk:=.f.
      end
   END
   IF lOk .and. Slot2 == .T.
      cFile := cImagePath + GameSlotImages[HB_RANDOMINT( 1, LEN(GameSlotImages) )]
      try
         ::Slot2:ImageName := cFile
      catch
         lOk:=.f.
      end         
   END
   IF lOk .and. Slot3 == .T.
      cFile := cImagePath + GameSlotImages[HB_RANDOMINT( 1, LEN(GameSlotImages) )]
      try
         ::Slot3:ImageName := cFile
      catch
         lOk:=.f.
      end
   END
   if !lOk
      ::MyTimer:Stop()
      ::MessageBox( "Missing file " + cFile + xCRLF + "The application needs to stop!", AppCaption, MB_OK|MB_ICONHAND )
      ::Application:Exit()
   endif

   IF Slot1 == .F. .AND. Slot2 == .F. .AND. Slot3 == .F.
      ::MyTimer:Stop()

      IF ::Slot1:ImageName == ::Slot2:ImageName .AND. ::Slot1:ImageName == ::Slot3:ImageName
         IF ::MessageBox( "Congratulations!" + xCRLF + xCRLF + "Would you like to play again?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
            StartNewGame()
            ::MyTimer:Start()
         END
      ELSEIF ::Slot1:ImageName == ::Slot2:ImageName .OR. ::Slot1:ImageName == ::Slot3:ImageName .OR. ::Slot2:ImageName == ::Slot3:ImageName
         IF ::MessageBox( "Almost there!" + xCRLF + xCRLF + "Would you like to play again?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
            StartNewGame()
            ::MyTimer:Start()
         END
      ELSE
         IF ::MessageBox( "Too bad!" + xCRLF + xCRLF + "Would you like to play again?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
            StartNewGame()
            ::MyTimer:Start()
         END
      END
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ButtonSpin_OnClick( Sender ) CLASS MyForm
   Slot1 := .T.
   Slot2 := .T.
   Slot3 := .T.

   ::MyTimer:Start()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ButtonSlot1_OnClick( Sender ) CLASS MyForm
   Slot1 := .F.
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ButtonSlot2_OnClick( Sender ) CLASS MyForm
   Slot2 := .F.
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ButtonSlot3_OnClick( Sender ) CLASS MyForm
   Slot3 := .F.
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ButtonLucky_OnClick( Sender ) CLASS MyForm
   Slot1 := .F.
   Slot2 := .F.
   Slot3 := .F.
RETURN Self

//----------------------------------------------------------------------------------------------------//
FUNCTION StartNewGame()
   GameSlotImages := {"1.jpg", "2.jpg", "3.jpg", "4.jpg", "5.jpg", "6.jpg"}

   Slot1 := .T.
   Slot2 := .T.
   Slot3 := .T.
RETURN NIL
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   DirChange( ::Application:Path )
   DirChange("..\")
   cImagePath:=CurDrive() + ":\" + CurDir() + "\Images\"
   AppCaption := "Advanced Slots " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption

   StartNewGame()   
RETURN Self