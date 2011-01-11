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
METHOD ButtonCheck_OnClick( Sender ) CLASS MyForm
   LOCAL nScore := 0

   IF ::BoxQuestion1:GetString() == "Pius XII"
      nScore = nScore + 1
   END
   IF ::BoxQuestion2:GetString() == "Shangai, China"
      nScore = nScore + 1
   END
   IF ::BoxQuestion3:GetString() == "Sixty-three"
      nScore = nScore + 1
   END
   IF ::BoxQuestion4:GetString() == "2005"
      nScore = nScore + 1
   END
   IF ::BoxQuestion5:GetString() == "Royal Flush"
      nScore = nScore + 1
   END

   IF ::MessageBox( "You answered " + AllTrim(Str(nScore)) + " questions out of 5 correct." + xCRLF + xCRLF + "Would you like to see the correct answers?", "Score", MB_YESNO) == 6
      ::LabelAnswer1:Visible := .T.
      ::LabelAnswer2:Visible := .T.
      ::LabelAnswer3:Visible := .T.
      ::LabelAnswer4:Visible := .T.
      ::LabelAnswer5:Visible := .T.
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonExit_OnClick( Sender ) CLASS MyForm
   ::Close()
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   AppCaption := ::Application:Name + " " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption

   WITH OBJECT ::BoxQuestion1
      :AddString("Pius XII")
      :AddString("John Paul I")
      :AddString("Benedict XV")
      :AddString("Clement XIV")
      :AddString("Blessed Innocent XI")
   END

   WITH OBJECT ::BoxQuestion2
      :AddString("Moscow, Russia")
      :AddString("Mexico City, Mexico")
      :AddString("New York, USA")
      :AddString("Shangai, China")
      :AddString("Mumbai, India")
   END

   WITH OBJECT ::BoxQuestion3
      :AddString("None")
      :AddString("One")
      :AddString("Three")
      :AddString("Twenty-one")
      :AddString("Sixty-three")
   END

   WITH OBJECT ::BoxQuestion4
      :AddString("1992")
      :AddString("1997")
      :AddString("1999")
      :AddString("2001")
      :AddString("2005")
   END

   WITH OBJECT ::BoxQuestion5
      :AddString("Four of a kind")
      :AddString("Royal Flush")
      :AddString("Full House")
      :AddString("Straight")
      :AddString("Two Pair")
   END   
RETURN Self