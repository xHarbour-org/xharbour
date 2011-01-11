GLOBAL EXTERNAL AppCaption, AppSettings

GLOBAL nSignature, MAttachments

#include "vxh.ch"
#include "MySettings.xfm"

#translate xCRLF => CHR(13) + CHR(10)
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Button_OK_OnClick( Sender ) CLASS MySettings
   local nAuth:=if( ::Check_SMTPAuthentication:Checked(), BST_CHECKED, BST_UNCHECKED )
   
   with object AppSettings
      :WriteString("MAIL-OUT", "SERVER", AllTrim(::Box_SMTP:Caption))
      :WriteString("MAIL-OUT", "PORT", AllTrim(::Box_SMTPPort:Caption))
      :WriteNumber("MAIL-OUT", "AUTH", nAuth )
      :WriteString("MAIL-OUT", "NAME", AllTrim(::Box_Name:Caption))
      :WriteString("MAIL-OUT", "EMAIL", AllTrim(::Box_Reply:Caption))
      :WriteString("MAIL-IN", "SERVER", AllTrim(::Box_POP:Caption))
      :WriteString("MAIL-IN", "USER", AllTrim(::Box_User:Caption))
      :WriteString("MAIL-IN", "PASS", AllTrim(::Box_Password:Caption))
      :WriteNumber("SETTINGS", "PRIORITY", ::Combo_Priority:GetCurSel())
      :WriteNumber("SETTINGS", "RECEIPT", ::Combo_Receipt:GetCurSel())
      :WriteNumber("SETTINGS", "ADD_SIGNATURE", ::Check_Signature:State())
      :WriteString("SETTINGS", "SIGNATURE", STRTRAN(::Box_Signature:Caption, xCRLF, "xCRLF"))
   end

   with object ::Application:MainForm
      
      IF LEN(AllTrim(::Box_Name:Caption)) > 0 .AND. LEN(AllTrim(::Box_Reply:Caption)) > 0
         :MailFrom:Caption := AllTrim(::Box_Name:Caption) + " <" + AllTrim(::Box_Reply:Caption) + ">"
      END
      IF nSignature == 0 .AND. ::Check_Signature:State() == 1
         :MailBody:Caption := :MailBody:Caption + xCRLF + xCRLF + "--" + xCRLF + ::Box_Signature:Caption
      END

      :Importance_High:UnCheck()
      :Importance_Normal:UnCheck()
      :Importance_Low:UnCheck()
   
      :Tool_Priority_High:Release()
      :Tool_Priority_Normal:Release()
      :Tool_Priority_Low:Release()

      IF ::Combo_Priority:GetCurSel() == 0
         :Importance_High:Check()
         :Tool_Priority_High:Press()
      ELSEIF ::Combo_Priority:GetCurSel() == 1
         :Importance_Normal:Check()
         :Tool_Priority_Normal:Press()
      ELSEIF ::Combo_Priority:GetCurSel() == 2
         :Importance_Low:Check()
         :Tool_Priority_Low:Press()
      END

      IF ::Combo_Receipt:GetCurSel() == 0
         :Options_Receipt:Check()
         :Tool_Receipt:Press()
      ELSE
         :Options_Receipt:UnCheck()
         :Tool_Receipt:Release()
      END
   
   end // with Application:Mainform

   ::Close()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button_Cancel_OnClick( Sender ) CLASS MySettings
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MySettings_OnLoad( Sender ) CLASS MySettings
   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := AppCaption + " | Settings & Preferences"

   ::Combo_Receipt:AddString("Yes")
   ::Combo_Receipt:AddString("No")
   ::Combo_Receipt:AddString("Ask me before sending")

   ::Combo_Priority:AddString("High")
   ::Combo_Priority:AddString("Normal")
   ::Combo_Priority:AddString("Low")

   ::Box_SMTP:Caption := AppSettings:ReadString("MAIL-OUT", "SERVER")
   ::Box_SMTPPort:Caption := AppSettings:ReadString("MAIL-OUT", "PORT")
   ::Check_SMTPAuthentication:SetState(AppSettings:ReadNumber("MAIL-OUT", "AUTH"))
   ::Box_POP:Caption := AppSettings:ReadString("MAIL-IN", "SERVER")
   ::Box_User:Caption := AppSettings:ReadString("MAIL-IN", "USER")
   ::Box_Password:Caption := AppSettings:ReadString("MAIL-IN", "PASS")

   ::Box_Name:Caption := AppSettings:ReadString("MAIL-OUT", "NAME")
   ::Box_Reply:Caption := AppSettings:ReadString("MAIL-OUT", "EMAIL")

   ::Combo_Receipt:SetCurSel(AppSettings:ReadNumber("SETTINGS", "RECEIPT"))
   ::Combo_Priority:SetCurSel(AppSettings:ReadNumber("SETTINGS", "PRIORITY"))
   ::Check_Signature:SetState(AppSettings:ReadNumber("SETTINGS", "ADD_SIGNATURE"))
   ::Box_Signature:Caption := STRTRAN(AppSettings:ReadString("SETTINGS", "SIGNATURE"), "xCRLF", xCRLF)

   nSignature := AppSettings:ReadNumber("SETTINGS", "ADD_SIGNATURE")   
RETURN Self