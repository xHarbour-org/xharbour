GLOBAL EXTERNAL System
GLOBAL EXTERNAL Application

GLOBAL AppName, AppVersion, AppCaption, AppFolder
GLOBAL aDBKeys, aDBKeysFilter

REQUEST DBFCDX

#include "vxh.ch"
#include "Form1.xfm"

#include "TagParser.prg"

#translate xCRLF => Chr(13) + Chr(10)
//---------------------------------------- End of system code ----------------------------------------//


//-- PROGRAM START -----------------------------------------------------------------------------------//
METHOD Form1_OnShowWindow( Sender ) CLASS Form1
   AppName := "xHDN Help & Support"
   AppVersion := "1.0 (beta)"
   AppCaption := AppName + " " + AppVersion
   AppFolder := LEFT(HB_ARGV(0), RAT("\", HB_ARGV(0)))

   ::Caption := AppName + " " + AppVersion

   aDBKeys := {}
   aDBKeysFilter := {}

   IF CheckDataFiles(AppFolder + "Datafiles\") <> "11111"
      MessageBox(, "Error opening the datafiles." + xCRLF + xCRLF + "Make sure the datafiles are present and not damaged.", AppCaption, MB_OK + MB_ICONERROR)
   ELSE
      FillListBox(::ListBox_SearchResults)
   END
RETURN Self


//-- PROGRAM END -------------------------------------------------------------------------------------//
METHOD Form1_OnClose( Sender ) CLASS Form1
   IF MessageBox(, "Are you sure you want to quit?", AppCaption, MB_YESNO + MB_ICONQUESTION) <> 6
      RETURN .F.
   END
RETURN Self


//-- LISTBOX SELECTION (MOUSE) -----------------------------------------------------------------------//
METHOD ListBox_SearchResults_OnLButtonUp( Sender ) CLASS Form1
   IF Sender:GetCurSel() >= 0 .AND. Sender:GetCurSel() < Sender:GetCount()
      ShowInfo(aDBKeysFilter[Sender:GetCurSel() + 1], ::Panel_DocContent)
   ELSE
      MessageBox(, "No documentation item is selected.", AppCaption, MB_OK + MB_ICONERROR)
   END
RETURN Self


//-- LISTBOX SELECTION (KEYBOARD) --------------------------------------------------------------------//
METHOD ListBox_SearchResults_OnKeyUp( Sender ) CLASS Form1
   IF Sender:wParam == 38 .OR. Sender:wParam == 40 .OR. Sender:wParam == 33 .OR. Sender:wParam == 34 .OR. Sender:wParam == 35 .OR. Sender:wParam == 36
      // KEY UP / KEY DOWN / PAGE UP / PAGE DOWN / END / HOME
      IF Sender:GetCurSel() >= 0 .AND. Sender:GetCurSel() < Sender:GetCount()
         ShowInfo(aDBKeysFilter[Sender:GetCurSel() + 1], ::Panel_DocContent)
      ELSE
         MessageBox(, "No documentation item is selected.", AppCaption, MB_OK + MB_ICONERROR)
      END
   END
RETURN Self


//-- SEARCH (KEYBOARD) -------------------------------------------------------------------------------//
METHOD Edit_Search_OnKeyUp( Sender ) CLASS Form1
   LOCAL i
   LOCAL cSearchValue := UPPER(ALLTRIM(::Edit_Search:Caption))
   LOCAL cSearchIn

   IF ::Box_WholeWords:Checked()
      cSearchValue := " " + cSearchValue + " "
   END

   ::ListBox_SearchResults:ResetContent()
   aDBKeysFilter := {}

   IF LEN(::Edit_Search:Caption) > 0
      FOR i := 1 TO LEN(aDBKeys)
         IF ::Box_WholeWords:Checked()
            cSearchIn := " " + UPPER(ALLTRIM(aDBKeys[i][4])) + " "
            cSearchIn := STRTRAN(cSearchIn, "()", "")
         ELSE
            cSearchIn := UPPER(ALLTRIM(aDBKeys[i][4]))
         END

         IF AT(cSearchValue, cSearchIn) > 0
            IF ::Box_Extensions:Checked()

            ELSE
               ::ListBox_SearchResults:AddString(aDBKeys[i][4])
               AADD(aDBKeysFilter, aDBKeys[i])
            END
         END
      NEXT
   ELSE
      FOR i := 1 TO LEN(aDBKeys)
         ::ListBox_SearchResults:AddString(aDBKeys[i][4])
      NEXT

      aDBKeysFilter := ACLONE(aDBKeys)
   END
RETURN Self


//-- FILL LISTBOX ------------------------------------------------------------------------------------//
FUNCTION FillListBox(oListBox)
   LOCAL cDBName
   LOCAL aDataFiles := {"language_classes", "language_misc", "language_class_methods", "language_class_properties"}
   LOCAL i

   FOR i := 1 TO LEN(aDataFiles)
      cDBName := AppFolder + "Datafiles\" + aDataFiles[i]

      USE &(cDBName) NEW VIA "DBFCDX" ALIAS xDB

      WHILE xDB->(EOF()) == .F.
         AAdd(aDBKeys, {aDataFiles[i], xDB->(RECNO()), 0, xDB->NAME})

         xDB->(DBSKIP())
      END

      CLOSE ALL
   NEXT

   ASort(aDBKeys, , , {|x, y| x[4] < y[4] })

   FOR i := 1 TO LEN(aDBKeys)
      aDBKeys[i][3] := i
      oListBox:AddString(aDBKeys[i][4])
   NEXT

   aDBKeysFilter := ACLONE(aDBKeys)
RETURN


//-- DATAFILES PRESENT? ------------------------------------------------------------------------------//
FUNCTION CheckDataFiles(cPath)
   LOCAL cDataFiles := "00000"
   LOCAL aDataFiles := {"language_articles", "language_classes", "language_misc", "language_class_methods", "language_class_properties"}
   LOCAL i

   FOR i := 1 TO LEN(aDataFiles)
      IF FILE(cPath + aDataFiles[i] + ".dbf") .AND. FILE(cPath + aDataFiles[i] + ".dbt")
         cDataFiles[i] := "1"
      ELSE
         cDataFiles[i] := "0"
      END
   NEXT
RETURN cDataFiles


//-- SHOW DOCUMENTATION ------------------------------------------------------------------------------//
FUNCTION ShowInfo(aInfoDetails, oInfoPanel)
   WITH OBJECT oInfoPanel
      :ActiveX := CreateActiveX(:hWnd, "Shell.Explorer", , -5)
      :ActiveX:Navigate(CreateHTML(aInfoDetails))
   END
RETURN NIL


//-- CREATE HTML CACHE FILE --------------------------------------------------------------------------//
FUNCTION CreateHTML(aInfoDetails)
   LOCAL oHTMLFile := FCreate(AppFolder + "Cache\xHDNCacheFile.htm", 0)
   LOCAL cHTMLContent := ""
   LOCAL cDBName, i := 0

   LOCAL DB_NAME, DB_TYPE, DB_ONELINER, DB_CATEGORY

   IF oHTMLFile <> -1
      IF aInfoDetails[1] == "language_misc"
         cDBName := AppFolder + "Datafiles\language_misc"

         USE &(cDBName) NEW VIA "DBFCDX" ALIAS xDB
         GOTO aInfoDetails[2]

         DB_NAME := xDB->NAME
         DB_TYPE := AllTrim(xDB->TYPE)
         DB_CATEGORY := xDB->CATEGORY
         DB_ONELINER := xDB->ONELINER
         DB_SYNTAX := xDB->SYNTAX
         DB_ARGUMENTS := xDB->ARGUMENTS
         DB_RETURNS := xDB->RETURNS
         DB_DESCRIPTION := xDB->DESCRIPTIO
         DB_EXAMPLES := xDB->EXAMPLES
         DB_REQUIRED_FILE := xDB->REQUIRED_F
         DB_REQUIRED_LIB := xDB->REQUIRED_L
         DB_REQUIRED_DLL := xDB->REQUIRED_D
         DB_SOURCE_FILE := xDB->SOURCE_FIL
         DB_LIBRARY_FILE := xDB->LIBRARY_FI
         DB_EXTENSION := xDB->EXTENSION
         DB_SEE_ALSO := xDB->SEE_ALSO

         CLOSE ALL
      END

      IF aInfoDetails[1] == "language_classes"
         cDBName := AppFolder + "Datafiles\language_misc"

         USE &(cDBName) NEW VIA "DBFCDX" ALIAS xDB
         GOTO aInfoDetails[2]

         DB_NAME := xDB->Name
         DB_TYPE := "Classes"
         DB_PARENT_CLASS := xDB->PARENT_CLA
         DB_SYNTAX := xDB->SYNTAX
         DB_ARGUMENTS := xDB->ARTUMENTS
         DB_RETURNS := xDB->RETURNS
         DB_DESCRIPTION := xDB->DESCRIPTIO
         DB_EXAMPLES := xDB->EXAMPLES
         DB_REQUIRED_FILE := xDB->REQUIRED_F
         DB_REQUIRED_LIB := xDB->REQUIRED_L
         DB_REQUIRED_DLL := xDB->REQUIRED_D
         DB_SOURCE_FILE := xDB->SOURCE_FIL
         DB_LIBRARY_FILE := xDB->LIBRARY_FI
         DB_EXTENSION := xDB->EXTENSION
         DB_SEE_ALSO := xDB->SEE_ALSO

         CLOSE ALL
      END

      IF aInfoDetails[1] == "language_class_methods"
         DB_NAME := ""
         DB_TYPE := "Methods"
      END

      IF aInfoDetails[1] == "language_class_properties"
         DB_NAME := ""
         DB_TYPE := "Properties"
      END

      cHTMLContent := cHTMLContent + "<html>" + xCRLF
      cHTMLContent := cHTMLContent + "<head>" + xCRLF
      cHTMLContent := cHTMLContent + "<link href='stylesheet.css' rel='stylesheet' type='text/css'>" + xCRLF
      cHTMLContent := cHTMLContent + "<script language='JavaScript' src='javascript.js'></script>" + xCRLF
      cHTMLContent := cHTMLContent + "</head>" + xCRLF
      cHTMLContent := cHTMLContent + "<body>" + xCRLF

         cHTMLContent := cHTMLContent + "<table align='center' border='0' cellpadding='0' cellspacing='0' width='850' style='background-color:#fafafa;'>" + xCRLF
         cHTMLContent := cHTMLContent + "<tr><td class='corner_sub_01' width='20' style='padding-top:6px;'>&nbsp;</td><td class='border_sub_top'></td><td class='corner_sub_11' width='20'></td></tr>" + xCRLF
         cHTMLContent := cHTMLContent + "<tr><td class='border_sub_left' width='20'>&nbsp;</td><td class='xhdn_text'>" + xCRLF

         //' PAGE TITLE
         IF DB_TYPE == "Articles"
            cHTMLContent := cHTMLContent + "<strong style='font-size:15px;'>" + DocTag(DB_TITLE) + "</strong>" + xCRLF
         ELSE
            cHTMLContent := cHTMLContent + "<strong style='font-size:15px;'>" + DocTag(DB_NAME) + "</strong>" + xCRLF
            cHTMLContent := cHTMLContent + "<div style='font-size:11px;'>" + DocTag(DB_ONELINER) + "</div>" + xCRLF
         END

         //' PAGE CATEGORY
         cHTMLContent := cHTMLContent + "<div style='margin-top:6px;padding-top:4px;padding-bottom:4px;padding-left:3px;padding-right:3px;border-top:1px dotted #666666;border-bottom:1px dotted #666666;'>" + xCRLF
         IF DB_TYPE == "Articles"
            cHTMLContent := cHTMLContent + "<strong>Category:</strong> xHDN Articles" + xCRLF
         ELSEIF DB_TYPE == "Methods" .OR. DB_TYPE == "Properties"
            cHTMLContent := cHTMLContent + "<strong>Class:</strong> " + DocTag(DB_CLASS) + xCRLF
         ELSE
            cHTMLContent := cHTMLContent + "<strong>Category:</strong> " + DocTag(DB_CATEGORY) + xCRLF
         END
         cHTMLContent := cHTMLContent + "</div>" + xCRLF

         cHTMLContent := cHTMLContent + "<div style='margin-top:12px;margin-bottom:12px;margin-left:5px;'>" + xCRLF

         IF DB_TYPE == "Commands" .OR. DB_TYPE == "Directives" .OR. DB_TYPE == "Functions" .OR. DB_TYPE == "Operators" .OR. DB_TYPE == "Statements"
            // SYNTAX
            IF LEN(DocTagCode(DB_SYNTAX)) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('syntax');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Syntax</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='syntax' style='font-family:courier new;font-size:12px;border:1px solid #b0b0b0;background-color:#e0e0e0;padding:3px;'>" + xCRLF
                     cHTMLContent := cHTMLContent + DocTagCode(DB_SYNTAX) + xCRLF
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // ARGUMENTS
            cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('arguments');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Arguments</strong>" + xCRLF
            cHTMLContent := cHTMLContent + "</span>" + xCRLF
            cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<div id='arguments' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
               IF LEN(DocTagArguments(DB_ARGUMENTS)) > 0
                  cHTMLContent := cHTMLContent + DocTagArguments(DB_ARGUMENTS) + xCRLF
               ELSE
                  cHTMLContent := cHTMLContent + "No arguments required." + xCRLF
               END
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            cHTMLContent := cHTMLContent + "</div>" + xCRLF

            // RETURN VALUE
            cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('return');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Return value</strong>" + xCRLF
            cHTMLContent := cHTMLContent + "</span>" + xCRLF
            cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<div id='return' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
               IF LEN(DocTag(DB_RETURNS)) > 0
                  cHTMLContent := cHTMLContent + DocTag(DB_RETURNS) + xCRLF
               ELSE
                  cHTMLContent := cHTMLContent + "No value is returned." + xCRLF
               END
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            cHTMLContent := cHTMLContent + "</div>" + xCRLF

            // DESCRIPTION
            IF LEN(DocTagDescription(DB_DESCRIPTION)) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('description');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Description</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='description' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
                     cHTMLContent := cHTMLContent + DocTagDescription(DB_DESCRIPTION) + xCRLF
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // EXAMPLES
            IF LEN(DB_EXAMPLES) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('examples');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Examples</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='examples' style='font-family:courier new;font-size:12px;border:1px solid #b0b0b0;background-color:#e0e0e0;padding:3px;'>" + xCRLF
                  IF LEN(DocTagCode(DB_EXAMPLES)) > 0
                     cHTMLContent := cHTMLContent + DocTagCode(DB_EXAMPLES) + xCRLF
                  ELSE
                     cHTMLContent := cHTMLContent + "No examples available." + xCRLF
                  END
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // FILE INFORMATION
            IF LEN(DocTag(DB_REQUIRED_LIB)) > 0 .OR. LEN(DocTag(DB_REQUIRED_DLL)) > 0 .OR. LEN(DocTag(DB_SOURCE_FILE)) > 0 .OR. (LEN(DocTag(DB_REQUIRED_FILE)) > 0 .AND. LEFT(DocTag(DB_REQUIRED_FILE), 4) <> "<!--")
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('files');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>File information</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='files' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF

                  IF LEN(DocTag(DB_REQUIRED_LIB)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>Library:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_REQUIRED_LIB) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  IF LEN(DocTag(DB_REQUIRED_DLL)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>DLL:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_REQUIRED_DLL) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  IF LEN(DocTag(DB_SOURCE_FILE)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>Source:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_SOURCE_FILE) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  IF LEN(DocTag(DB_REQUIRED_FILE)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>Other files required:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_REQUIRED_FILE) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // SEE ALSO
            IF LEN(STRTRAN(DocTag(DB_SEE_ALSO), ",", ", ")) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('seealso');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Related topics</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='seealso' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
                     cHTMLContent := cHTMLContent + STRTRAN(DocTag(DB_SEE_ALSO), ",", ", ") + xCRLF
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

         END

         IF DB_TYPE == "Classes"
            // PARENT CLASS
            IF LEN(DocTagCode(DB_PARENT_CLASS)) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('parentclass');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Parent Class</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='parentclass' style='font-family:courier new;font-size:12px;border-top:1px solid #b0b0b0;border-bottom:1px solid #b0b0b0;background-color:#e0e0e0;padding:2px;'>" + xCRLF
                  IF LEN(DocTag(DB_PARENT_CLASS)) > 0
                     cHTMLContent := cHTMLContent + DocTag(DB_PARENT_CLASS) + xCRLF
                  ELSE
                     cHTMLContent := "This class has no parent class." + xCRLF
                  END
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // CONSTRUCTOR
            IF LEN(DocTagCode(DB_CONSTRUCTOR)) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('constructor');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Syntax</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='constructor' style='font-family:courier new;font-size:12px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
                     cHTMLContent := cHTMLContent + DocTag(DB_CONSTRUCTOR) + xCRLF
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // SYNTAX
            IF LEN(DocTagCode(DB_SYNTAX)) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('syntax');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Syntax</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='syntax' style='font-family:courier new;font-size:12px;border:1px solid #b0b0b0;background-color:#e0e0e0;padding:3px;'>" + xCRLF
                     cHTMLContent := cHTMLContent + DocTagCode(DB_SYNTAX) + xCRLF
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // ARGUMENTS
            cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('arguments');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Arguments</strong>" + xCRLF
            cHTMLContent := cHTMLContent + "</span>" + xCRLF
            cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<div id='arguments' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
               IF LEN(DocTagArguments(DB_ARGUMENTS)) > 0
                  cHTMLContent := cHTMLContent + DocTagArguments(DB_ARGUMENTS) + xCRLF
               ELSE
                  cHTMLContent := cHTMLContent + "No arguments required." + xCRLF
               END
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            cHTMLContent := cHTMLContent + "</div>" + xCRLF

            // RETURN VALUE
            cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('return');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Return value</strong>" + xCRLF
            cHTMLContent := cHTMLContent + "</span>" + xCRLF
            cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<div id='return' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
               IF LEN(Doctag(DB_RETURNS)) > 0
                  cHTMLContent := cHTMLContent + DocTag(DB_RETURNS) + xCRLF
               ELSE
                  cHTMLContent := cHTMLContent + "No value is returned." + xCRLF
               END
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            cHTMLContent := cHTMLContent + "</div>" + xCRLF

            // DESCRIPTION
            IF LEN(DB_DESCRIPTION) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('description');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Description</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='description' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
                     cHTMLContent := cHTMLContent + DocTagDescription(DB_DESCRIPTION) + xCRLF
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // CLASS PROPERTIES
            /*
            cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('properties');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Class Properties</strong>" + xCRLF
            cHTMLContent := cHTMLContent + "</span>" + xCRLF
            cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<div id='properties' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
               IF LEN(DB_CLASS_PROPERTIES) > 0
                  ASORT(DB_CLASS_PROPERTIES, , , { |x, y| LOWER(x[1]) < LOWER(y[1]) })
                  FOR i := 1 TO LEN(DB_CLASS_PROPERTIES)
                     cHTMLContent := cHTMLContent + "&bull;&nbsp; " + DocTag(DB_CLASS_PROPERTIES[i][1]) + "<br />" + xCRLF
                  NEXT
               ELSE
                  cHTMLContent := cHTMLContent + "&bull;&nbsp; This class has no properties." + xCRLF
               END
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            cHTMLContent := cHTMLContent + "</div>" + xCRLF
            */

            // CLASS METHODS
            /*
            cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('methods');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Class Methods</strong>" + xCRLF
            cHTMLContent := cHTMLContent + "</span>" + xCRLF
            cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
               cHTMLContent := cHTMLContent + "<div id='methods' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
               IF LEN(DB_CLASS_METHODS) > 0
                  ASORT(DB_CLASS_METHODS, , , { |x, y| LOWER(x[1]) < LOWER(y[1]) })
                  FOR i := 1 TO LEN(DB_CLASS_METHODS)
                     cHTMLContent := cHTMLContent + "&bull;&nbsp; " + DocTag(DB_CLASS_METHODS[i][1]) + "<br />" + xCRLF
                  NEXT
               ELSE
                  cHTMLContent := cHTMLContent + "&bull;&nbsp; This class has no methods." + xCRLF
               END
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            cHTMLContent := cHTMLContent + "</div>" + xCRLF
            */

            // EXAMPLES
            IF LEN(DocTagcode(DB_EXAMPLES)) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('examples');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Examples</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='examples' style='font-family:courier new;font-size:12px;border:1px solid #b0b0b0;background-color:#e0e0e0;padding:3px;'>" + xCRLF
                  IF LEN(DB_EXAMPLES) > 0
                     cHTMLContent := cHTMLContent + DocTagCode(DB_EXAMPLES) + xCRLF
                  ELSE
                     cHTMLContent := cHTMLContent + "No examples available." + xCRLF
                  END
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // FILE INFORMATION
            IF LEN(DocTag(DB_REQ_LIB)) > 0 .OR. LEN(DocTag(DB_REQ_DLL)) > 0 .OR. LEN(DocTag(DB_SOURCE_FILE)) > 0 .OR. (LEN(DocTag(DB_REQ_FILE)) > 0 .AND. LEFT(DocTag(DB_REQ_FILE), 4) <> "<!--")
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('files');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>File information</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='files' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF

                  IF LEN(DocTag(DB_REQ_LIB)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>Library:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_REQ_LIB) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  IF LEN(DocTag(DB_REQ_DLL)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>DLL:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_REQ_DLL) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  IF LEN(DocTag(DB_SOURCE_FILE)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>Source:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_SOURCE_FILE) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  IF LEN(DocTag(DB_REQ_FILE)) > 0
                     cHTMLContent := cHTMLContent + "<div style='margin-bottom:2px;margin-top:2px;'>&bull; <strong>Other files required:</strong> " + xCRLF
                        cHTMLContent := cHTMLContent + DocTag(DB_REQ_FILE) + xCRLF
                     cHTMLContent := cHTMLContent + "</div>" + xCRLF
                  END

                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

            // SEE ALSO
            IF LEN(STRTRAN(DocTag(DB_SEE_ALSO), ",", ", ")) > 0
               cHTMLContent := cHTMLContent + "<span onclick=" + chr(34) + "ToggleDiv('seealso');" + chr(34) + " style='cursor:pointer;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<strong style='font-family:arial;font-size:18px;'>&rsaquo;</strong>&nbsp; <strong style='font-family:tahoma;font-size:12px;'>Related topics</strong>" + xCRLF
               cHTMLContent := cHTMLContent + "</span>" + xCRLF
               cHTMLContent := cHTMLContent + "<div style='margin-bottom:12px;margin-top:2px;margin-left:9px;'>" + xCRLF
                  cHTMLContent := cHTMLContent + "<div id='seealso' style='font-family:verdana;font-size:11px;border:1px solid #eaeaea;background-color:#f8f8f8;padding:3px;'>" + xCRLF
                     cHTMLContent := cHTMLContent + STRTRAN(DocTag(DB_SEE_ALSO), ",", ", ") + xCRLF
                  cHTMLContent := cHTMLContent + "</div>" + xCRLF
               cHTMLContent := cHTMLContent + "</div>" + xCRLF
            END

         END

         IF DB_TYPE == "Methods"

         END

         IF DB_TYPE == "Properties"

         END

         cHTMLContent := cHTMLContent + "</div>" + xCRLF

         cHTMLContent := cHTMLContent + "</td><td class='border_sub_right' width='20'>&nbsp;</td></tr>" + xCRLF
         cHTMLContent := cHTMLContent + "<tr><td class='corner_sub_00' width='20' style='padding-bottom:6px;'>&nbsp;</td><td class='border_sub_bottom'>&nbsp;</td><td class='corner_sub_10' width='20'>&nbsp;</td></tr>" + xCRLF
         cHTMLContent := cHTMLContent + "</table>" + xCRLF

      cHTMLContent := cHTMLContent + "</body>" + xCRLF
      cHTMLContent := cHTMLContent + "</html>"

      FWRITE(oHTMLFile, cHTMLContent)
      FCLOSE(oHTMLFile)
   END
RETURN AppFolder + "Cache\xHDNCacheFile.htm"


//-- COUNT OCCURANCE OF SUBSTRING --------------------------------------------------------------------//
FUNCTION COUNTCHARS(cLine, cChar)
   IF LEN(cLine) > 0
      RETURN (LEN(cLine) - LEN(STRTRAN(cLINE, cChar, ""))) / LEN(cChar)
   END
RETURN 0


//-- EXTENDED AT FUNCTION ----------------------------------------------------------------------------//
FUNCTION AT_EXT(cSign, cLine, nStart)
   LOCAL cNewLine := SUBSTR(cLine, nStart)

   IF AT(cSign, cNewLine) = 0
      RETURN 0
   ELSE
      RETURN nStart + AT(cSign, cNewLine) - 1
   END
RETURN 0


//----------------------------------------------------------------------------------------------------//
METHOD File_Print_OnClick( Sender ) CLASS Form1

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD File_PrintOptions_OnClick( Sender ) CLASS Form1

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD File_Exit_OnClick( Sender ) CLASS Form1
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Options_Preferences_OnClick( Sender ) CLASS Form1
   Form2(Self)
RETURN Self
