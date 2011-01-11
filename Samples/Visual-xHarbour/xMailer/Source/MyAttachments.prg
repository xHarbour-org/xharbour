GLOBAL EXTERNAL AppCaption, AppSettings, MAttachments

GLOBAL MAttachmentsMirror

#include "vxh.ch"
#include "MyAttachments.xfm"

#translate xCRLF => CHR(13) + CHR(10)
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Button_OK_OnClick( Sender ) CLASS MyAttachments
   LOCAL i := 1

   with object ::Application:MainForm
      
      :List_Attachments:ResetContent()
      FOR i := 1 TO LEN(MAttachments)
         :List_Attachments:AddString(SUBSTR(MAttachments[i], RAT("\", MAttachments[i]) + 1))
      NEXT

      IF LEN(MAttachments) > 0
         :PanelAttachments:Visible := .T.
         :View_Attachment:Caption := "Hide Attachment List"
      ELSE
         :PanelAttachments:Visible := .F.
         :View_Attachment:Caption := "Show Attachment List"
      END
   
   end // with Application:MainForm

   ::Close()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button_Cancel_OnClick( Sender ) CLASS MyAttachments
   MAttachments := MAttachmentsMirror
   ::Close()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button_Add_OnClick( Sender ) CLASS MyAttachments
   local cFile
   
   with object ::Application:MainForm:MyOpen
      :Title:="Add File..."
      :FileName:=""
      :Filter:="All Files (*.*)|*.*"
      :Show()
      cFile:=:FileName 
   end

   IF empty( cFile )
      RETURN Self
   ELSE
      WITH OBJECT ::ListAttachments
         :AddString( cFile )
         :SetCurSel(:GetCount() - 1)
         AADD(MAttachments, cFile)
      END
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button_Delete_OnClick( Sender ) CLASS MyAttachments
   LOCAL aSwitch := {}
   LOCAL i := 1

   WITH OBJECT ::ListAttachments
      IF :GetCurSel() >= 0
         IF ::MessageBox( "Are you sure you wish to delete this attachment?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
            ADEL(MAttachments, :GetCurSel() + 1)
            :DeleteString(:GetCurSel() + 1)

            IF :GetCount() > 0
               :SetCurSel(:GetCount() - 1)
            END

            FOR i := 1 TO LEN(MAttachments)
               IF HB_ISNIL(MAttachments[i]) == .F.
                  AADD(aSwitch, MAttachments[i])
               END
            NEXT

            MAttachments := ACLONE(aSwitch)
         END
      ELSE
         ::MessageBox( "No attachment selected. The requested operation can not be executed.", AppCaption, MB_OK + MB_ICONEXCLAMATION)
      END
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyAttachments_OnLoad( Sender ) CLASS MyAttachments
   LOCAL i := 1

   MAttachmentsMirror := {}

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := AppCaption + " | Attachment Manager"

   FOR i := 1 TO LEN(MAttachments)
      ::ListAttachments:AddString(MAttachments[i])
   NEXT
   MAttachmentsMirror := MAttachments   
RETURN Self