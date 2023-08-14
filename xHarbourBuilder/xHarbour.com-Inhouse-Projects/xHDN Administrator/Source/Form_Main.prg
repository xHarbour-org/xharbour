GLOBAL EXTERNAL System
GLOBAL EXTERNAL Application

GLOBAL AppName, AppVersion, AppCaption, AppFolder
GLOBAL oIni, AppFileLocation

GLOBAL aTemporaryArray
GLOBAL aFileLines

#translate xCRLF => CHR(13) + CHR(10)

#include "vxh.ch"
#include "Form_Main.xfm"

#include "Custom_Functions.prg"
#include "Get_Functions.prg"

#include "mysql.ch"
#include "sqlrdd.ch"

REQUEST SQLRDD
REQUEST SR_MYSQL


// APPLICATION INIT
//
METHOD Form_Main_OnLoad( Sender ) CLASS Form_Main
   AppName := "xHDN Manager"
   AppVersion := "1.0 (beta)"
   AppCaption := AppName + " " + AppVersion
   AppFolder := LEFT(HB_ARGV(0), RAT( "\", HB_ARGV(0)))

   Sender:Caption := AppCaption

   oIni := IniFile(AppFolder + "xHDNManager.ini", .F., , .F.)

   IF File(AppFolder + "xHDNManager.ini") == .F.
      IF MessageBox(Self:Handle, "No configuration file was found. Would you like to create and modify settings now?" + xCRLF + xCRLF + "If you choose no, a default set of settings will be used.", AppCaption, MB_YESNO + MB_ICONASTERISK) == 6
         oIni:WriteString("APPLICATION", "NAME", AppName)
         oIni:WriteString("APPLICATION", "VERSION", AppVersion)
         oIni:WriteString("APPLICATION", "INIVERSION", "1.0")

         oIni:WriteString("GENERAL", "USERNAME", "Unknown user")
         oIni:WriteNumber("GENERAL", "TIMEZONE", 30)

         oIni:WriteString("FILES", "LOCATION", AppFolder)
         oIni:WriteLogical("FILES", "SAVEDEFAULT", .F.)

         oIni:WriteLogical("DATABASE", "USECONNSTRING", .T.)
         oIni:WriteString("DATABASE", "CONNSTRING", "HST=mssql.xharbour.com;DATABASE=xharbour_xhdn;USER=xHarbour;PASSWORD=ncj45djc;")
         oIni:WriteLogical("DATABASE", "USEDATABASECRED", .F.)
         oIni:WriteString("DATABASE", "SERVER", "mssql.xharbour.com")
         oIni:WriteString("DATABASE", "DATABASE", "xharbour_xhdn")
         oIni:WriteString("DATABASE", "USER", "xHarbour")
         oIni:WriteString("DATABASE", "PASSWORD", "ncj45djc")

         oIni:WriteLogical("PREFERENCES", "OPENRECURSIVE", .T.)
         oIni:WriteLogical("PREFERENCES", "FILTERFILES", .T.)
         oIni:WriteLogical("PREFERENCES", "EXPANDTREE", .T.)
         oIni:WriteLogical("PREFERENCES", "CONFIRMEXIT", .F.)
         oIni:WriteLogical("PREFERENCES", "SAVEPOSITIONS", .T.)
         oIni:WriteNumber("PREFERENCES", "WINDOW_TOP", ::Top)
         oIni:WriteNumber("PREFERENCES", "WINDOW_LEFT", ::Left)
         oIni:WriteNumber("PREFERENCES", "WINDOW_HEIGHT", ::Height)
         oIni:WriteNumber("PREFERENCES", "WINDOW_WIDTH", ::Width)

         Form_Settings(NIL)
      ELSE
         oIni:WriteString("APPLICATION", "NAME", AppName)
         oIni:WriteString("APPLICATION", "VERSION", AppVersion)
         oIni:WriteString("APPLICATION", "INIVERSION", "1.0")

         oIni:WriteString("GENERAL", "USERNAME", "Unknown user")
         oIni:WriteNumber("GENERAL", "TIMEZONE", 30)

         oIni:WriteString("FILES", "LOCATION", AppFolder)
         oIni:WriteLogical("FILES", "SAVEDEFAULT", .F.)

         oIni:WriteLogical("DATABASE", "USECONNSTRING", .T.)
         oIni:WriteString("DATABASE", "CONNSTRING", "HST=mssql.xharbour.com;DATABASE=xharbour_xhdn;USER=xHarbour;PASSWORD=ncj45djc;")
         oIni:WriteLogical("DATABASE", "USEDATABASECRED", .F.)
         oIni:WriteString("DATABASE", "SERVER", "mssql.xharbour.com")
         oIni:WriteString("DATABASE", "DATABASE", "xharbour_xhdn")
         oIni:WriteString("DATABASE", "USER", "xHarbour")
         oIni:WriteString("DATABASE", "PASSWORD", "ncj45djc")

         oIni:WriteLogical("PREFERENCES", "OPENRECURSIVE", .T.)
         oIni:WriteLogical("PREFERENCES", "FILTERFILES", .T.)
         oIni:WriteLogical("PREFERENCES", "EXPANDTREE", .T.)
         oIni:WriteLogical("PREFERENCES", "CONFIRMEXIT", .F.)
         oIni:WriteLogical("PREFERENCES", "SAVEPOSITIONS", .T.)
         oIni:WriteNumber("PREFERENCES", "WINDOW_TOP", ::Top)
         oIni:WriteNumber("PREFERENCES", "WINDOW_LEFT", ::Left)
         oIni:WriteNumber("PREFERENCES", "WINDOW_HEIGHT", ::Height)
         oIni:WriteNumber("PREFERENCES", "WINDOW_WIDTH", ::Width)
      END
   END

   IF oIni:ReadLogical("PREFERENCES", "SAVEPOSITIONS") == .T.
      ::Center := .F.
      ::Top := oIni:ReadNumber("PREFERENCES", "WINDOW_TOP")
      ::Left := oIni:ReadNumber("PREFERENCES", "WINDOW_LEFT")
      ::Height := oIni:ReadNumber("PREFERENCES", "WINDOW_HEIGHT")
      ::Width := oIni:ReadNumber("PREFERENCES", "WINDOW_WIDTH")
   END

   AppFileLocation := oIni:ReadString("FILES", "LOCATION")

   FilterTree(::Tree_Files, AppFileLocation, 1, "", .F.)
   IF oIni:ReadLogical("PREFERENCES", "EXPANDTREE") == .T.
      ::Tree_Files:ExpandAll()
   END

   IF oIni:ReadLogical("PREFERENCES", "OPENRECURSIVE") == .T.
      ::Options_OpenRecursive:Check()
   END
   IF oIni:ReadLogical("PREFERENCES", "FILTERFILES") == .T.
      ::Options_FilterFiles:Check()
   END

   aTemporaryArray := {}
   aFileLines := {}

   ::Combo_Filter:AddString("All Files")
   ::Combo_Filter:AddString("New Files")
   ::Combo_Filter:AddString("Updated Files")
   ::Combo_Filter:AddString("Approved Files")
   ::Combo_Filter:AddString("Disapproved Files")
   ::Combo_Filter:SetCurSel(1)
RETURN Self
//
// APPLICATION INIT END


// APPLICATION TERMINATE
//
METHOD Form_Main_OnClose( Sender ) CLASS Form_Main
   IF oIni:ReadLogical("PREFERENCES", "CONFIRMEXIT") == .T.
      IF MessageBox(Self:Handle, "Are you sure you wish to exit the manager?", AppCaption, MB_YESNO + MB_ICONQUESTION) <> 6
         RETURN .F.
      END
   END

   IF oIni:ReadLogical("PREFERENCES", "SAVEPOSITIONS") == .T.
      oIni:WriteNumber("PREFERENCES", "WINDOW_TOP", ::Top)
      oIni:WriteNumber("PREFERENCES", "WINDOW_LEFT", ::Left)
      oIni:WriteNumber("PREFERENCES", "WINDOW_HEIGHT", ::Height)
      oIni:WriteNumber("PREFERENCES", "WINDOW_WIDTH", ::Width)
   END
RETURN Self
//
// APPLICATION TERMINATE END


// MENUBAR BUTTONS
//
METHOD File_OpenLocation_OnClick( Sender ) CLASS Form_Main
   ::Folder_Main:RootFolder := AppFileLocation
   ::Folder_Main:Show(AppFolder)

   IF LEN(::Folder_Main:SelectedPath) > 0
      AppFileLocation := ::Folder_Main:SelectedPath + "\"
      ::Tree_Files:ResetContent()
      FilterTree(::Tree_Files, AppFileLocation, 1, "", .F.)

      IF oIni:ReadLogical("FILES", "SAVEDEFAULT") == .T.
         oIni:WriteString("FILES", "LOCATION", AppFileLocation)
      END

      IF oIni:ReadLogical("PREFERENCES", "EXPANDTREE") == .T.
         ::Tree_Files:ExpandAll()
      END

      ::Panel_Main:Caption := ""
      ::Panel_Status:Caption := ""
   END
RETURN Self

METHOD File_SaveChanges_OnClick( Sender ) CLASS Form_Main
   IF LEN(::Edit_FileSource:Caption) <> 0 .AND. HB_ISNIL(::Tree_Files:GetSelected()) == .F.
      IF MEMOWRIT(GetFullPathFromTree(::Tree_Files), ::Edit_FileSource:Caption)
         // TODO: update HTML view

         ::Edit_FileSource:Caption := MEMOREAD(GetFullPathFromTree(::Tree_Files))

         ::Tree_Files:SetFocus()
      ELSE
         MessageBox(Self:Handle, "An error occured trying to save the changes.", AppCaption, MB_OK + MB_ICONASTERISK)
      END
   END
RETURN Self

METHOD File_Exit_OnClick( Sender ) CLASS Form_Main
   ::Form_Main_OnClose(Self)
RETURN Self

METHOD Approve_Active_OnClick( Sender ) CLASS Form_Main
   IF HB_ISNIL(::Tree_Files:GetSelected()) == .F.
      IF GetLatestStatus(GetFullPathFromTree(::Tree_Files)) == "Approved" .AND. GetLatestCRC(GetFullPathFromTree(::Tree_Files)) == CalculateCRC(::Edit_FileSource:Caption)
         MessageBox(Self:Handle, "The file was already marked 'Approved' and has not changed since.", AppCaption, MB_OK + MB_ICONASTERISK)
         RETURN Self
      END

      ApproveFile(GetFullPathFromTree(::Tree_Files))

      ::Tree_Files_AfterSelect(::Tree_Files)
   ELSE
      MessageBox(Self:Handle, "To mark a file 'Approved', please select the file first.", AppCaption)
   END
RETURN Self

METHOD Approve_All_OnClick( Sender ) CLASS Form_Main
   GetPathsFromTree(::Tree_Files)

   IF LEN(aTemporaryArray) > 0
      IF MessageBox(Self:Handle, "Are you sure you want to mark all files 'Approved'?" + xCRLF + xCRLF + ALLTRIM(STR(LEN(aTemporaryArray))) + " files will be updated.", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
         ApproveFile(aTemporaryArray)

         IF HB_ISNIL(::Tree_Files:GetSelected()) == .F.
            ::Tree_Files_AfterSelect(::Tree_Files)
         END
      END
   ELSE
      // ...
   END

   aTemporaryArray := {}
RETURN Self

METHOD Disapprove_Active_OnClick( Sender ) CLASS Form_Main
   IF HB_ISNIL(::Tree_Files:GetSelected()) == .F.
      IF GetLatestStatus(GetFullPathFromTree(::Tree_Files)) == "Disapproved" .AND. GetLatestCRC(GetFullPathFromTree(::Tree_Files)) == CalculateCRC(::Edit_FileSource:Caption)
         MessageBox(Self:Handle, "The file was already marked 'Disapproved' and has not changed since.", AppCaption, MB_OK + MB_ICONASTERISK)
         RETURN Self
      END

      DisapproveFile(GetFullPathFromTree(::Tree_Files))

      ::Tree_Files_AfterSelect(::Tree_Files)
   ELSE
      MessageBox(Self:Handle, "To mark a file 'Disapproved', please select the file first.", AppCaption)
   END
RETURN Self

METHOD Disapprove_All_OnClick( Sender ) CLASS Form_Main
   GetPathsFromTree(::Tree_Files)

   IF LEN(aTemporaryArray) > 0
      IF MessageBox(Self:Handle, "Are you sure you want to mark all files 'Disapproved'?" + xCRLF + xCRLF + ALLTRIM(STR(LEN(aTemporaryArray))) + " files will be updated.", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
         DisapproveFile(aTemporaryArray)

         IF HB_ISNIL(::Tree_Files:GetSelected()) == .F.
            ::Tree_Files_AfterSelect(::Tree_Files)
         END
      END
   ELSE
      // ...
   END

   aTemporaryArray := {}
RETURN Self

METHOD Upload_Active_OnClick( Sender ) CLASS Form_Main
   
   IF HB_ISNIL(::Tree_Files:GetSelected()) == .F.
      
      IF GetLatestStatus(GetFullPathFromTree(::Tree_Files)) <> "Approved"
         IF MessageBox(Self:Handle, "The file is not marked 'Approved'. Do you wish to continue with the upload?", AppCaption, MB_YESNO + MB_ICONASTERISK) <> 6
            RETURN Self
         END
      END

      UploadFile(GetFullPathFromTree(::Tree_Files))

      ::Tree_Files_AfterSelect(::Tree_Files)
   ELSE
      MessageBox(Self:Handle, "To mark a file 'Disapproved', please select the file first.", AppCaption)
   END   
RETURN Self

METHOD Upload_All_OnClick( Sender ) CLASS Form_Main
   
RETURN Self

METHOD Actions_UpdateCVS_OnClick( Sender ) CLASS Form_Main
   
RETURN Self

METHOD Options_OpenRecursive_OnClick( Sender ) CLASS Form_Main
   /*
   IF Sender:Check == .T.
      messagebox(, "if")
   ELSE
      messagebox(, "else")
   END
   */
   /*
   IF Sender:Checked() := .T.
      Sender:UnCheck()
   ELSE
      Sender:Check()
   END
   */
RETURN Self

METHOD Options_FilterFiles_OnClick( Sender ) CLASS Form_Main
   
RETURN Self

METHOD Options_ExpandTree_OnClick( Sender ) CLASS Form_Main
   
RETURN Self

METHOD Options_ConfirmExit_OnClick( Sender ) CLASS Form_Main
   
RETURN Self

METHOD Options_SavePositions_OnClick( Sender ) CLASS Form_Main
   
RETURN Self

METHOD Options_Settings_OnClick( Sender ) CLASS Form_Main
   Form_Settings(NIL)
RETURN Self

METHOD Help_Topics_OnClick( Sender ) CLASS Form_Main
   
RETURN Self

METHOD Help_About_OnClick( Sender ) CLASS Form_Main
   
RETURN Self
//
// MENUBAR BUTTONS END


// TOOLBAR BUTTONS
//
METHOD Main_OpenLocation_OnClick( Sender ) CLASS Form_Main
   ::File_OpenLocation_OnClick(Self)
RETURN Self

METHOD Main_SaveChanges_OnClick( Sender ) CLASS Form_Main
   ::File_SaveChanges_OnClick(Self)
RETURN Self

METHOD Main_ApproveFile_OnClick( Sender ) CLASS Form_Main
   ::ApproveFile_Active_OnClick(Self)
RETURN Self

METHOD Main_DisapproveFile_OnClick( Sender ) CLASS Form_Main
   ::DisapproveFile_Active_OnClick(Self)
RETURN Self

METHOD Main_UploadFile_OnClick( Sender ) CLASS Form_Main
   ::Upload_Active_OnClick(Self)
RETURN Self

METHOD Main_UpdateCVS_OnClick( Sender ) CLASS Form_Main
   ::Actions_UpdateCVS_OnClick(Self)
RETURN Self
//
// TOOLBAR BUTTONS END


// CONTEXTMENUS
//
METHOD ApproveFile_Active_OnClick( Sender ) CLASS Form_Main
   ::Approve_Active_OnClick(Self)
RETURN Self

METHOD ApproveFile_All_OnClick( Sender ) CLASS Form_Main
   ::Approve_All_OnClick(Self)
RETURN Self

METHOD DisapproveFile_Active_OnClick( Sender ) CLASS Form_Main
   ::Disapprove_Active_OnClick(Self)
RETURN Self

METHOD DisapproveFile_All_OnClick( Sender ) CLASS Form_Main
   ::Disapprove_All_OnClick(Self)
RETURN Self

METHOD UploadFile_Active_OnClick( Sender ) CLASS Form_Main
   ::Upload_Active_OnClick(Self)
RETURN Self

METHOD UploadFile_All_OnClick( Sender ) CLASS Form_Main
   ::Upload_All_OnClick(Self)
RETURN Self
//
// CONTEXTMENUS END


// LEFT PANE BUTTONS
//
METHOD Button_Search_OnClick( Sender ) CLASS Form_Main
   ::Tree_Files:ResetContent()

   IF ::Check_SearchInFiles:Checked
      FilterTree(::Tree_Files, AppFileLocation, 1, ::Edit_Search:Caption, .T.)
   ELSE
      FilterTree(::Tree_Files, AppFileLocation, 1, ::Edit_Search:Caption, .F.)
   END
   ::Tree_Files:ExpandAll()
RETURN Self

METHOD Combo_Filter_OnCBNSelEndOk( Sender ) CLASS Form_Main
   ::Tree_Files:ResetContent()
   FilterTree(::Tree_Files, AppFileLocation, Sender:GetCurSel(), ::Edit_Search:Caption, .F.)
RETURN Self
//
// LEFT PANEL BUTTONS END


METHOD Tree_Files_AfterSelect( Sender ) CLASS Form_Main
   LOCAL cPath := AppFileLocation
   LOCAL oTreeItem := Sender:GetSelected()

   IF LEN(Sender:GetSelected():Items) == 0
      ::Edit_FileSource:Caption := MemoRead(GetFullPathFromTree(Sender))

      ::Panel_Main:Caption := GetFullPathFromTree(Sender)
      ::Panel_Status:Caption := "Status: " + GetLatestStatus(GetFullPathFromTree(Sender))
   ELSE
      ::Edit_FileSource:Caption := ""

      ::Panel_Main:Caption := GetFullPathFromTree(Sender)
      ::Panel_Status:Caption := ""
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//