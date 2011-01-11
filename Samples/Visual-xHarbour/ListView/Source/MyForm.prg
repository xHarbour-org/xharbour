GLOBAL AppCaption

#include "vxh.ch"
#include "MyForm.xfm"

#translate xCRLF => CHR(13) + CHR(10)
//---------------------------------------- End of system code ----------------------------------------//

//-- CONFIRM QUIT ------------------------------------------------------------------------------------//
METHOD MyForm_OnClose( Sender ) CLASS MyForm
   IF ::Messagebox( "Exit this Visual xHarbour sample?", AppCaption + " | Exit", MB_YESNO + MB_ICONQUESTION) <> IDYES
      RETURN 0
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
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   local nPos, cPath, cFile:="test.dbf", lOk:=.t.
   // SAMPLE DETAILS
   nPos:=Len(::Application:Name)+5
   cPath:=stuff( ::Application:Path, len(::Application:Path)-nPos+1, nPos, "" )
   AppCaption := ::Application:Name + " " + ::Application:Version

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption
   try
      WITH OBJECT ::MyDataTable
         :Path:=cPath
         :FileName:=cFile
         :Alias:="MyDataTable"
         :Open()
      END
   catch
      lOk:=.f.
   end
   if !lOk
      ::MessageBox( "File not available - " + cPath + "\" + cFile, AppCaption, MB_OK|MB_ICONHAND )
      ::Application:Exit()
   endif
      
   ::MyListView:DataSource := ::MyDataTable

RETURN Self