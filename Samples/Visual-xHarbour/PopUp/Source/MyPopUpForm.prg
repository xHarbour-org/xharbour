GLOBAL EXTERNAL AppCaption
GLOBAL EXTERNAL MyPopUp

#include "vxh.ch"
#include "MyPopUpForm.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//-- GENERAL LINK ------------------------------------------------------------------------------------//
METHOD LinkWebsite_OnClick( Sender ) CLASS MyPopUpForm
   ShellExecute(::hWnd, "OPEN", Sender:Url, , , SW_SHOW)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyPopUpForm_OnClose( Sender ) CLASS MyPopUpForm
   MyPopUp := NIL
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyPopUpForm_OnLoad( Sender ) CLASS MyPopUpForm
   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption   
RETURN Self