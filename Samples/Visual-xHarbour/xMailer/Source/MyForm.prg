GLOBAL AppCaption, AppSettings

GLOBAL MAttachments
GLOBAL MailProgress

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

//-- MAIN MENU ---------------------------------------------------------------------------------------//
METHOD File_New_OnClick( Sender ) CLASS MyForm
   IF ::MessageBox( "Are you sure you want to create a new message?" + xCRLF + xCRLF + "Creating a new message will discard current changes to the message.", AppCaption + " | Discard changes?", MB_YESNO + MB_ICONQUESTION) == 6
      MAttachments := {}

      ::MailTo:Caption := ""
      ::MailSubject:Caption := ""
      IF AppSettings:ReadNumber("SETTINGS", "ADD_SIGNATURE") == 1
         ::MailBody:Caption := xCRLF + xCRLF + "--" + xCRLF + AppSettings:ReadString("SETTINGS", "SIGNATURE")
      ELSE
         ::MailBody:Caption := ""
      END
      IF LEN(AppSettings:ReadString("MAIL-OUT", "NAME")) > 0 .AND. LEN(AppSettings:ReadString("MAIL-OUT", "EMAIL")) > 0
         ::MailFrom:Caption := AllTrim(AppSettings:ReadString("MAIL-OUT", "NAME")) + " <" + AllTrim(AppSettings:ReadString("MAIL-OUT", "EMAIL")) + ">"
      END
   END
RETURN Self

METHOD File_Attachments_OnClick( Sender ) CLASS MyForm
   MyAttachments( ::this )
RETURN Self

METHOD File_Send_OnClick( Sender ) CLASS MyForm
   LOCAL aMailTo := {}, cMailFrom, cMailSubject, cMailBody
   LOCAL cSMTPServer, nPort, lSMTPAuth, cPOPServer, cUser, cPassword, nPriority, nReceipt
   LOCAL cTemp := ""

   // SMTP
   cSMTPServer := AppSettings:ReadString("MAIL-OUT", "SERVER")
   nPort := AppSettings:ReadNumber("MAIL-OUT", "PORT")

   // POP3
   cPOPServer := AppSettings:ReadString("MAIL-IN", "SERVER")
   cUser := AppSettings:ReadString("MAIL-IN", "USER")
   cPassword := AppSettings:ReadString("MAIL-IN", "PASS")

   // MISC
   IF LEN(ALLTRIM(::MailTo:Caption)) > 0
      cTemp := AllTrim(::MailTo:Caption) + " "
      cTemp := StrTran(cTemp, ", ", "_SEP_")
      cTemp := StrTran(cTemp, ",", "_SEP_")
      cTemp := StrTran(cTemp, "; ", "_SEP_")
      cTemp := StrTran(cTemp, ";", "_SEP_")
      cTemp := StrTran(cTemp, " ", "_SEP_")

      DO WHILE At("_SEP_", cTemp) <> 0
         AAdd(aMailTo, Left(cTemp, At("_SEP_", cTemp) - 1))
         cTemp := Right(cTemp, Len(cTemp) - At("_SEP_", cTemp) - 4)
      END
   END

   cMailFrom := ::MailFrom:Caption
   cMailSubject := ::MailSubject:Caption
   cMailBody := ::MailBody:Caption
   nPriority := AppSettings:ReadNumber("SETTINGS", "PRIORITY")
   nReceipt := AppSettings:ReadNumber("SETTINGS", "RECEIPT")

   IF LEN(cSMTPServer) <= 0
      ::MessageBox( "No SMTP server is specified!" + xCRLF + xCRLF + "Please configure your SMTP server using the 'Settings' menu option.", AppCaption + " | Error", MB_ICONERROR)
      RETURN Self
   END
   IF nPort == 0
      ::MessageBox( "No port number for the SMTP server is specified!" + xCRLF + xCRLF + "Please configure your SMTP server using the 'Settings' menu option.", AppCaption + " | Error", MB_ICONERROR)
      RETURN Self
   END

   IF LEN(cPOPServer) <= 0
      ::MessageBox( "No POP3 server is specified!" + xCRLF + xCRLF + "Please configure your POP3 server using the 'Settings' menu option.", AppCaption + " | Error", MB_ICONERROR)
      RETURN Self
   END
   IF LEN(cUser) <= 0
      ::MessageBox( "The username for the POP3 server is not specified!" + xCRLF + xCRLF + "Please configure your POP3 server login data using the 'Settings' menu option.", AppCaption + " | Error", MB_ICONERROR)
      RETURN Self
   END
   IF LEN(cPassword) <= 0
      ::MessageBox( "The password for the POP3 server is specified!" + xCRLF + xCRLF + "Please configure your POP3 server using the 'Settings' menu option.", AppCaption + " | Error", MB_ICONERROR)
      RETURN Self
   END

   IF LEN(aMailTo) == 0
      ::MessageBox( "The message is missing a recipient!" + xCRLF + xCRLF + "Please enter a valid e-mail address in the 'To:' box.", AppCaption + " | Error", MB_ICONERROR)
      ::MailTo:SetFocus()
      RETURN Self
   END
   IF LEN(cMailFrom) <= 0
      ::MessageBox( "The message is missing a sender!" + xCRLF + xCRLF + "Please enter a valid e-mail address in the 'From:' box, or by using the 'Settings' menu option.", AppCaption + " | Error", MB_ICONERROR)
      ::MailFrom:SetFocus()
      RETURN Self
   END
   IF LEN(cMailSubject) <= 0
      IF ::MessageBox( "The message is missing a subject." + xCRLF + xCRLF + "Are you sure you want to send this message without a subject?", AppCaption + " | Warning", MB_YESNO + MB_ICONQUESTION) <> 6
         ::MailSubject:SetFocus()
         RETURN Self
      END
   END
   IF LEN(cMailBody) <= 0
      IF ::MessageBox( "The message is missing a body." + xCRLF + xCRLF + "Are you sure you want to send this message without a body?", AppCaption + " | Warning", MB_YESNO + MB_ICONQUESTION) <> 6
         ::MailBody:SetFocus()
         RETURN Self
      END
   END

   IF nPriority == 0
      nPriority := 1
   ELSEIF nPriority == 2
      nPriority := 5
   ELSE
      nPriority := 3
   END

   IF nReceipt == 2
      IF ::MessageBox( "Do you want the receiver to send a receipt?", AppCaption + " | Request receipt?", MB_YESNO + MB_ICONQUESTION) == 6
         nReceipt := .T.
      ELSE
         nReceipt := .f.
      END
   ELSEIF nReceipt == 1
      nReceipt := .F.
   ELSEIF nReceipt == 0
      nReceipt := .T.
   else
      nReceipt:=.f.
   END

   IF AppSettings:ReadNumber("MAIL-OUT", "AUTH") == 1
      lSMTPAuth := .T.
   ELSE
      lSMTPAuth := .F.
   END

   SendMail(cSMTPServer, nPort, cMailFrom, aMailTo, cMailBody, cMailSubject, MAttachments, cUser, cPassword, cPOPServer, nPriority, nReceipt, .F., lSMTPAuth)
RETURN Self

METHOD File_Exit_OnClick( Sender ) CLASS MyForm
   ::Close()
RETURN Self

METHOD Options_Importance_OnClick( Sender ) CLASS MyForm
   
RETURN Self

METHOD Importance_High_OnClick( Sender ) CLASS MyForm
   Sender:Check()
   ::Importance_Normal:UnCheck()
   ::Importance_Low:UnCheck()

   AppSettings:WriteNumber("SETTINGS", "PRIORITY", 0)

   ::Tool_Priority_High:Press()
   ::Tool_Priority_Normal:Release()
   ::Tool_Priority_Low:Release()
RETURN Self

METHOD Importance_Normal_OnClick( Sender ) CLASS MyForm
   Sender:Check()
   ::Importance_High:UnCheck()
   ::Importance_Low:UnCheck()

   AppSettings:WriteNumber("SETTINGS", "PRIORITY", 1)

   ::Tool_Priority_High:Release()
   ::Tool_Priority_Normal:Press()
   ::Tool_Priority_Low:Release()
RETURN Self

METHOD Importance_Low_OnClick( Sender ) CLASS MyForm
   Sender:Check()
   ::Importance_High:UnCheck()
   ::Importance_Normal:UnCheck()

   AppSettings:WriteNumber("SETTINGS", "PRIORITY", 2)

   ::Tool_Priority_High:Release()
   ::Tool_Priority_Normal:Release()
   ::Tool_Priority_Low:Press()
RETURN Self

METHOD Options_Receipt_OnClick( Sender ) CLASS MyForm
   IF Sender:IsChecked()
      Sender:UnCheck()
      ::Tool_Receipt:Release()
      AppSettings:WriteNumber("SETTINGS", "RECEIPT", 1)
   ELSE
      Sender:Check()
      ::Tool_Receipt:Press()
      AppSettings:WriteNumber("SETTINGS", "RECEIPT", 0)
   END
RETURN Self

METHOD Options_Settings_OnClick( Sender ) CLASS MyForm
   MySettings( ::this )
RETURN Self


FUNCTION SendMail( cServerIP, nPort, cFrom, aTo, cMsg, cSubject, aFiles, cUser, cPass, cPopServer, nPriority, lRead, lTrace ,lPopAuth)

LOCAL oInMail
LOCAL lSair      := .F.
LOCAL nStart
LOCAL nRetry     := 1
LOCAL oUrl
LOCAL oUrl1
LOCAL oMail
LOCAL cTo        := ""
LOCAL aThisFile
LOCAL cFile
LOCAL cData2
LOCAL cFname
LOCAL cFext
LOCAL cData
LOCAL cConnect   := ""
LOCAL CC         := ""
LOCAL lRet       := .T.
LOCAL oPop
LOCAL lSecure    := .F.
LOCAL lAuthLogin := .F.
LOCAL lAuthPlain := .F.
LOCAL lConnect   := .T.
LOCAL cMimeText  := ""
LOCAL lConnectPlain := .f.
Local cMsgTemp
LOCAL cLastError

LOCAL oAttach

   DEFAULT cUser TO ""
   DEFAULT cPass TO ""
   DEFAULT nPort TO 25
   DEFAULT aFiles TO {}
   DEFAULT nPriority TO 3
   DEFAULT lRead TO .f.
   DEFAULT lTrace to .F.
   DEFAULT lPopAuth to .T.
   cLastError := ""
   cUser      := Strtran( cUser, "@", "&at;" )

   IF Valtype( aTo ) == "A"

      IF Len( aTo ) > 1

         FOR EACH cTo IN aTo

            IF HB_EnumIndex() != 1

               cC += cTo + ","

            ENDIF

         NEXT

         cC := Substr( cC, 1, Len( cC ) - 1 )

      ENDIF

      cTo := aTo[ 1 ]

      IF Len( cC ) > 0

         cTo += "," + cC

      ENDIF

   ELSE

      cTo := Alltrim( aTo )

   ENDIF
//TraceLog(cServerIP, nPort, cFrom, aTo, cMsg, cSubject, aFiles, cUser, cPass, cPopServer, nPriority, lRead, lTrace ,lPopAuth)
//tracelog(cUser)
   // This is required. Many smtp server, requires that first user connect to popserver, to validade user, and the allow smtp access
   IF cPopServer != NIL .AND. lPopAuth

      oUrl1 := tUrl():New( "pop://" + cUser + ":" + cPass + "@" + cPopServer + "/" )
      oUrl1:cUserid := Strtran( cUser, "&at;", "@" )
      if "gmail" in lower(cPopserver)
      oUrl1:nPort := 995
      endif
      oPop  := tIPClientPop():new( oUrl1, lTrace )

      IF oPop:Open()

         oPop:Close()

      ENDIF

   ENDIF

   cConnect := "smtp://" + cUser + "@" + cServerIp + '/' + cTo

   oUrl         := tUrl():New( cConnect )
   cUser        := Strtran( cUser, "&at;", "@" )
   oUrl:nPort   := nPort
   oUrl:cUserid := cUser
   
   oMail   := TipMail( ):new()
   
   IF (".htm" IN Lower( cMsg ) .OR. ".html" IN Lower( cMsg ) ) .and. File(cMsg)
      
      oAttach := Tipmail():new()
      oAttach:setEncoder( "7-bit" )
      
      cMimeText := "text/html ; charset=ISO-8859-1"
      oAttach:hHeaders[ "Content-Type" ] := cMimeText
      cMsgTemp := cMsg
      cMsg := MemoRead( cMsgTemp )
      
      oAttach:setbody( cmsg )
      oMail:attach( oAttach )
      
   ELSE
      oMail:hHeaders[ "Content-Type" ] := "text/plain; charset=iso8851"
      
      oMail:SetBody( cMsg )
      
   ENDIF   
   
   oUrl:cFile                       := cTo
   
   oMail:hHeaders[ "Date" ]         := Tip_Timestamp()

   oMail:hHeaders[ "From" ] := cFrom

   oInMail := tIPClientSmtp():new( oUrl, lTrace )

   IF oInMail:Opensecure()

      WHILE .T.

         oInMail:GetOk()

         IF oInMail:cReply == NIL

            EXIT

         ELSEIF "LOGIN" IN oInMail:cReply

            lAuthLogin := .T.

         ELSEIF "PLAIN" IN oInMail:cReply

            lAuthPlain := .T.

         ENDIF

      ENDDO
      
      IF lAuthLogin

         IF !oInMail:Auth( cUser, cPass )

            lConnect := .F.
            //oInMail:Quit()

         ELSE
            lConnectPlain  := .t.

         ENDIF

      ENDIF

      IF lAuthPlain .AND. !lConnect

         IF !oInMail:AuthPlain( cUser, cPass )

            lConnect := .F.
            //oInMail:Quit()           

         ENDIF

      ELSE
         IF !lConnectPlain
            oInmail:Getok()
            lConnect := .F.
         ENDIF

      ENDIF

   ELSE

      lConnect := .F.

   ENDIF

   IF !lConnect

      oInMail:close()

      IF !oInMail:Open()

         lConnect := .F.
         oInmail:close()
         RETURN .F.

      ENDIF

      WHILE .T.

         oInMail:GetOk()

         IF oInMail:cReply == NIL

            EXIT

         ENDIF

      ENDDO

   ENDIF
   
   oMail:hHeaders[ "To" ]      := cTo
   oMail:hHeaders[ "Subject" ] := cSubject

   FOR EACH aThisFile IN AFiles

      IF Valtype( aThisFile ) == "C"

         cFile := aThisFile
         cData := Memoread( cFile )

      ELSEIF Valtype( aThisFile ) == "A" .AND. Len( aThisFile ) >= 2

         cFile := aThisFile[ 1 ]
         cData := aThisFile[ 2 ]

      ELSE

         lRet := .F.
         EXIT

      ENDIF

      oAttach := TipMail():New()

      //TODO: mime type magic auto-finder

      HB_FNameSplit( cFile,, @cFname, @cFext )

      IF Lower( cFile ) LIKE ".+\.(zip|jp|jpeg|png|jpg|pdf|bin|dms|lha|lzh|exe|class|so|dll|dmg)" .or. Empty(cFExt)   

         oAttach:SetEncoder( "base64" )

      ELSE

         oAttach:SetEncoder( "7-bit" )

      ENDIF
         
      cMimeText := SetMimeType( cFile, cFname, cFext )
      // Some EMAIL readers use Content-Type to check for filename

      IF ".html" in lower( cFext) .or. ".htm" in lower( cFext)

         cMimeText += "; charset=ISO-8859-1"

      ENDIF

      oAttach:hHeaders[ "Content-Type" ] := cMimeText
      // But usually, original filename is set here
      oAttach:hHeaders[ "Content-Disposition" ] := ;
          "attachment; filename=" + cFname + cFext
      oAttach:SetBody( cData )
      oMail:Attach( oAttach )

   NEXT
   
   IF lRead

      oMail:hHeaders[ "Disposition-Notification-To" ] := cUser

   ENDIF

   IF nPriority != 3

      oMail:hHeaders[ "X-Priority" ] := Str( nPriority, 1 )

   ENDIF
   lRet := .T.

   IF lRet

      cData2 := oMail:ToString()
      oInmail:Write( cData2 )
      oInMail:commit()

   ENDIF

//   oInMail:quit()
   oInMail:close()

   IF lRet

      cLastError := ""

   ENDIF

RETURN lRet

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function SetMimeType()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+

FUNCTION SetMimeType( cFile, cFname, cFext )

   cFile := Lower( cFile )
   
   IF cFile LIKE ".+\.zip"
      RETURN "application/x-zip-compressed;filename=" + cFname + cFext
   ELSEIF cFile LIKE ".+\.(jpeg|jpg|jp)"
      RETURN "image/jpeg;filename=" + cFname + cFext
   ELSEIF cFile LIKE ".+\.(png|bmp)"
      RETURN "image/png;filename=" + cFname + cFext
   ELSEIF cFile LIKE ".+\.(bmp)"
      RETURN "image/bitmap;filename=" + cFname + cFext
   ELSEIF cFile LIKE ".+\.html?" 
      RETURN "text/html;filename=" + cFname + cFext
   ELSEIF cFile LIKE ".+\.pdf"
      RETURN "application/pdf;filename=" + cFname + cFext
   ELSEIF cFile LIKE ".+\.txt"
      RETURN "test/plain;filename=" + cFname + cFext
   ELSEIF cFile LIKE ".+\.(bin|dms|lha|lzh|exe|class|so|dll|dmg)" .or. Empty(cFExt)
      RETURN "application/octet-stream;filename=" + cFname + cFext
   ENDIF
   
RETURN  "text/plain;filename=" + cFname + cFext
*+ EOF: MAIL1.PRG

FUNCTION recvMail( cServerIP, nPort, cFrom, aTo, cMsg, cSubject, aFiles, cUser, cPass, cPopServer, nPriority, lRead, lTrace ,lPopAuth)

LOCAL oInMail
LOCAL lSair      := .F.
LOCAL nStart
LOCAL nRetry     := 1
LOCAL oUrl
LOCAL oUrl1
LOCAL oMail
LOCAL cTo        := ""
LOCAL aThisFile
LOCAL cFile
LOCAL cData2
LOCAL cFname
LOCAL cFext
LOCAL cData
LOCAL cConnect   := ""
LOCAL CC         := ""
LOCAL lRet       := .T.
LOCAL oPop
LOCAL lSecure    := .F.
LOCAL lAuthLogin := .F.
LOCAL lAuthPlain := .F.
LOCAL lConnect   := .T.
LOCAL cMimeText  := ""
LOCAL lConnectPlain := .f.
Local cMsgTemp
LOCAL cLastError
   DEFAULT cUser TO ""
   DEFAULT cPass TO ""
   DEFAULT nPort TO 25
   DEFAULT aFiles TO {}
   DEFAULT nPriority TO 3
   DEFAULT lRead TO .f.
   DEFAULT lTrace to .F.
   DEFAULT lPopAuth to .T.
   cLastError := ""
   cUser      := Strtran( cUser, "@", "&at;" )

   IF Valtype( aTo ) == "A"

      IF Len( aTo ) > 1

         FOR EACH cTo IN aTo

            IF HB_EnumIndex() != 1

               cC += cTo + ","

            ENDIF

         NEXT

         cC := Substr( cC, 1, Len( cC ) - 1 )

      ENDIF

      cTo := aTo[ 1 ]

      IF Len( cC ) > 0

         cTo += "," + cC

      ENDIF

   ELSE

      cTo := Alltrim( aTo )

   ENDIF
   // This is required. Many smtp server, requires that first user connect to popserver, to validade user, and the allow smtp access
   IF cPopServer != NIL .AND. lPopAuth

      oUrl1 := tUrl():New( "pop://" + cUser + ":" + cPass + "@" + cPopServer + "/" )
      oUrl1:cUserid := Strtran( cUser, "&at;", "@" )
      oPop  := tIPClientPop():new( oUrl1, lTrace )

      IF oPop:Open()
//         ? opop:list()

         oPop:Close()

      ENDIF

   ENDIF

return nil


//----------------------------------------------------------------------------------------------------//
METHOD View_Toolbar_OnClick( Sender ) CLASS MyForm
   WITH OBJECT ::MyToolBar
      :Visible := ! :Visible
      IF :Visible
         Sender:Caption := "Hide Toolbar"
      ELSE
         Sender:Caption := "Show Toolbar"
      END
   END
RETURN Self

METHOD View_Attachment_OnClick( Sender ) CLASS MyForm
   ::PanelAttachments:Visible := ! ::PanelAttachments:Visible

   IF ::PanelAttachments:Visible
      Sender:Caption := "Hide Attachment List"
   ELSE
      Sender:Caption := "Show Attachment List"
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Tool_Send_OnClick( Sender ) CLASS MyForm
   ::File_Send_OnClick(::File_Send)
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Tool_New_OnClick( Sender ) CLASS MyForm
    ::File_New_OnClick(::File_New)
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Tool_Priority_High_OnClick( Sender ) CLASS MyForm
   ::Importance_High_OnClick(::Importance_High)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Tool_Priority_Normal_OnClick( Sender ) CLASS MyForm
   ::Importance_Normal_OnClick(::Importance_Normal)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Tool_Priority_Low_OnClick( Sender ) CLASS MyForm
   ::Importance_Low_OnClick(::Importance_Low)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Tool_Receipt_OnClick( Sender ) CLASS MyForm
   ::Options_Receipt_OnClick(::Options_Receipt)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContAtt_Open_OnClick( Sender ) CLASS MyForm
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContAtt_Delete_OnClick( Sender ) CLASS MyForm
   LOCAL nSelAttachment := ::List_Attachments:GetCurSel()
   LOCAL aSwitch := {}
   LOCAL i

   IF nSelAttachment == -1
      ::MessageBox( "No attachment selected. The requested operation can not be executed.", AppCaption, MB_OK + MB_ICONEXCLAMATION)
   ELSE
      IF ::MessageBox( "Are you sure you wish to delete this attachment?", AppCaption, MB_YESNO + MB_ICONQUESTION) == 6
         ADEL(MAttachments, nSelAttachment + 1)
         WITH OBJECT ::List_Attachments
            :DeleteString(nSelAttachment + 1)

            //IF :GetCount() > 0
               //:SetCurSel(:GetCount() - 1)
            //END
         END
      END
   END

   FOR i := 1 TO LEN(MAttachments)
      IF HB_ISNIL(MAttachments[i]) == .F.
         AADD(aSwitch, MAttachments[i])
      END
   NEXT

   MAttachments := ACLONE(aSwitch)

   IF LEN(MAttachments) == 0
      ::PanelAttachments:Visible := .F.
      ::View_Attachment:Caption := "Show Attachment List"
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContAtt_Manage_OnClick( Sender ) CLASS MyForm
   MyAttachments( ::this )
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD File_Outlook_OnClick( Sender ) CLASS MyForm
   LOCAL oOutlook, oMail, i
   LOCAL oError

   TRY
      oOutlook = CREATEOBJECT("Outlook.Application")
      oMail = oOutlook:CreateItem(0)
 
      oMail:Recipients:Add(::MailTo:Caption)
      oMail:Subject = ::MailSubject:Caption
      oMail:Body = ::MailBody:Caption
      //oMail:HTMLBody = MEMOREAD("c:\xxx.htm")

      FOR i := 1 TO LEN(MAttachments)
         oMail:Attachments:Add(MAttachments[i])
      NEXT

      oMail:Display = .T.
   CATCH oError
      MessageBox(, "An error occured while trying to open the message in Outlook.", AppCaption, MB_OK + MB_ICONASTERISK)
   END
RETURN Self
//-- INITIALISE SAMPLE -------------------------------------------------------------------------------//
METHOD MyForm_OnLoad( Sender ) CLASS MyForm
   // SAMPLE DETAILS
   local cIniFile:=::Application:Path + "\xMailer.ini"
   AppCaption := "xMailer " + ::Application:Version

   MAttachments := {}
   IF !FILE( cIniFile )
      AppSettings := IniFile( cIniFile )

      AppSettings:WriteString("MAIL-OUT", "SERVER", "outgoing.server.com")
      AppSettings:WriteString("MAIL-OUT", "PORT", "25")
      AppSettings:WriteNumber("MAIL-OUT", "AUTH", 0)
      AppSettings:WriteString("MAIL-OUT", "NAME", "Your Name")
      AppSettings:WriteString("MAIL-OUT", "EMAIL", "yourname@domain.com")
      AppSettings:WriteString("MAIL-IN", "SERVER", "incoming.server.com")
      AppSettings:WriteString("MAIL-IN", "USER", "username")
      AppSettings:WriteString("MAIL-IN", "PASS", "password")
      AppSettings:WriteNumber("SETTINGS", "PRIORITY", 1)
      AppSettings:WriteNumber("SETTINGS", "RECEIPT", 1)
      AppSettings:WriteNumber("SETTINGS", "ADD_SIGNATURE", 1)
      AppSettings:WriteString("SETTINGS", "SIGNATURE", "Using xHarbour.com Inc. " + AppCaption + ".")

      MySettings( ::this )
   ELSE
      AppSettings := IniFile( cIniFile )
   END
   IF AppSettings:ReadNumber("SETTINGS", "PRIORITY") == 0
      ::Importance_High:Check()
      ::Tool_Priority_High:Press()
      ::Tool_Priority_Normal:Release()
      ::Tool_Priority_Low:Release()
   END
   IF AppSettings:ReadNumber("SETTINGS", "PRIORITY") == 1
      ::Importance_Normal:Check()
      ::Tool_Priority_High:Release()
      ::Tool_Priority_Normal:Press()
      ::Tool_Priority_Low:Release()
   END
   IF AppSettings:ReadNumber("SETTINGS", "PRIORITY") == 2
      ::Importance_Low:Check()
      ::Tool_Priority_High:Release()
      ::Tool_Priority_Normal:Release()
      ::Tool_Priority_Low:Press()
   END
   IF AppSettings:ReadNumber("SETTINGS", "RECEIPT") == 0
      ::Options_Receipt:Check()
      ::Tool_Receipt:Press()
   END
   IF LEN(AppSettings:ReadString("MAIL-OUT", "NAME")) > 0 .AND. LEN(AppSettings:ReadString("MAIL-OUT", "EMAIL")) > 0
      ::MailFrom:Caption := AllTrim(AppSettings:ReadString("MAIL-OUT", "NAME")) + " <" + AllTrim(AppSettings:ReadString("MAIL-OUT", "EMAIL")) + ">"
   END
   IF AppSettings:ReadNumber("SETTINGS", "ADD_SIGNATURE") == 1
      ::MailBody:Caption := xCRLF + xCRLF + "--" + xCRLF + AppSettings:ReadString("SETTINGS", "SIGNATURE")
   END

   ::MailTo:SetFocus()

   ::LinkWebsite:Caption := "http://www.xHarbour.com/TrainingCenter/"
   ::LinkWebsite:Url := "http://www.xharbour.com/trainingcenter/"

   ::Caption := "xHarbour.com Training Center | " + AppCaption

   MailProgress := ::MyProgress
   ::MyToolBar:Visible := .F.
   ::PanelAttachments:Visible := .F.   
RETURN Self