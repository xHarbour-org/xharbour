GLOBAL AppCaption

GLOBAL MDI1:=NIL, MDI2:=NIL

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
METHOD FileMDI1_OnClick( Sender ) CLASS MyForm
   LOCAL lMaximize := .F.

   IF MDI2 != NIL
      IF MDI2:ShowMode == 3
         lMaximize := .T.
      END
   END

   IF MDI1 == NIL
      MDI1 := MyMDI1(::This)
   ELSE
      MDI1:SetFocus()
   END

   IF lMaximize == .T.
      MDI1:MDIMaximize()
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FileMDI2_OnClick( Sender ) CLASS MyForm
   LOCAL lMaximize := .F.

   IF MDI1 != NIL
      IF MDI1:ShowMode == 3
         lMaximize := .T.
      END
   END

   IF MDI2 == NIL
      MDI2 := MyMDI2(::This)
   ELSE
      MDI2:SetFocus()
   END

   IF lMaximize == .T.
      MDI2:MDIMaximize()
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FileExit_OnClick( Sender ) CLASS MyForm
   ::Close()
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   AppCaption := ::Application:Name + " " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption   
RETURN Self