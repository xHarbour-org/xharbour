/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Mapi.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "commdlg.ch"
#include "debug.ch"
#include "vxh.ch"
#include "fileio.ch"

CLASS eMail INHERIT Component
   DATA To                PUBLISHED INIT ""
   DATA From              PUBLISHED INIT ""
   DATA CC                PUBLISHED INIT ""
   DATA BCC               PUBLISHED INIT ""
   DATA ReplyTo           PUBLISHED INIT ""
    
   DATA Subject           EXPORTED  INIT ""
   DATA HTMLBody          EXPORTED  INIT ""
   DATA TextBody          EXPORTED  INIT ""
   DATA EnumSendUsing     EXPORTED  INIT {{"Pickup","Port"},{1,2}}

   DATA SendUsing         PUBLISHED INIT 2
   DATA SMTPServer        PUBLISHED INIT ""
   DATA MimeFormatted     PUBLISHED INIT .T.
   DATA SMTPServerPort    PUBLISHED INIT 0
   DATA SMTPAuthenticate  PUBLISHED INIT .T.
   DATA SMTPUseSSL        PUBLISHED INIT .F.
   
   DATA SendUserName      PUBLISHED INIT ""
   DATA SendPassword      PUBLISHED INIT ""
   DATA Attachments       PUBLISHED

   METHOD Init() CONSTRUCTOR
   METHOD Send()
ENDCLASS

METHOD Init( oOwner ) CLASS eMail
   ::__xCtrlName   := "eMail"
   ::ClsName       := "eMail"
   ::ComponentType := "eMail"
   ::Super:Init( oOwner )
RETURN Self

METHOD Send() CLASS eMail
/*
   HB_SendMail( ::SMTPServer,;
                ::SMTPServerPort,;
                ::From,;
                hb_aTokens( ::To, ";" ),;
                hb_aTokens( ::CC, ";" ),;
                hb_aTokens( ::BCC, ";" ),;
                ::TextBody,;
                ::Subject,;
                hb_aTokens( ::Attachments, ";" ),;
                ::SendUserName,;
                ::SendPassword,;
                "", , , , .T., .T., , ::ReplyTo, ::SMTPUseSSL, ,  )

*/
   LOCAL oMsg, e, oConf, oFlds, cSchema, aFiles, cFile, lReturn := .T.
   LOCAL hEventHandler := {=>}
   TRY
      oMsg := GetActiveObject( "CDO.Message" )
    CATCH
      TRY
         oMsg := CreateObject( "CDO.Message" )
      CATCH
         ::Application:MainForm:MessageBox( "Cannot initialize mail interface (Message)", "Visual xHarbour", MB_OK | MB_ICONEXCLAMATION )
         RETURN .F.
      END
   END
   TRY
      oConf := GetActiveObject( "CDO.Configuration" )
    CATCH
      TRY
         oConf := CreateObject("CDO.Configuration")
       CATCH
         ::Application:MainForm:MessageBox( "Cannot initialize mail interface (Configuration)", "Visual xHarbour", MB_OK | MB_ICONEXCLAMATION )
         RETURN .F.
      END
   END
   oFlds := oConf:Fields

   cSchema := "http://schemas.microsoft.com/cdo/configuration/"

   oFlds:Item( cSchema + "sendusing" ):Value        := ::SendUsing
   oFlds:Item( cSchema + "smtpserver" ):Value       := ::SMTPServer
   oFlds:Item( cSchema + "smtpserverport" ):Value   := ::SMTPServerPort
   oFlds:Item( cSchema + "smtpauthenticate" ):Value := IIF( ::SMTPAuthenticate, 1, 0 )
   oFlds:Item( cSchema + "sendusername" ):Value     := ::SendUserName
   oFlds:Item( cSchema + "sendpassword" ):Value     := ::SendPassword
   oFlds:Item( cSchema + "smtpusessl" ):Value       := ::SMTPUseSSL

   oFlds:Update()

   WITH OBJECT oMsg
      :Configuration := oConf

      :To            := ::To
      :From          := ::From
      :Subject       := ::Subject
      :HTMLBody      := ::HTMLBody
      :TextBody      := ::TextBody
      :CC            := ::CC
      :BCC           := ::BCC
      :ReplyTo       := ::ReplyTo
      :MimeFormatted := ::MimeFormatted

      IF ! EMPTY( ::Attachments )
         IF VALTYPE( ::Attachments ) == "C"
            aFiles := hb_aTokens( ::Attachments, ";" )
          ELSE
            aFiles := ::Attachments
         ENDIF
         FOR EACH cFile IN aFiles
             IF ! FILE( cFile )
                cFile := ::Application:Path + "\" + cFile
             ENDIF
             IF FILE( cFile )
                :AddAttachment( cFile )
             ENDIF
         NEXT
      ENDIF
      TRY
         :Send()
      CATCH e
         lReturn := .F.
      END
   END
RETURN lReturn

//------------------------------------------------------------------------------------------------

#define MAPI_ORIG  0
#define MAPI_TO    1
#define MAPI_CC    2
#define MAPI_BCC   3

#define MAPI_DIALOG       0x00000008
#define MAPI_PASSWORD_UI  0x00020000
#define MAPI_LOGON_UI     0x00000001
#define MAPI_NEW_SESSION  0x00000002

CLASS MSendMail
   PROPERTY ShowDialog INDEX MAPI_DIALOG      READ xShowDialog WRITE SetFlags DEFAULT .T. PROTECTED
   PROPERTY LogonUI    INDEX MAPI_LOGON_UI    READ xLogonUI    WRITE SetFlags DEFAULT .F. PROTECTED
   PROPERTY NewSession INDEX MAPI_NEW_SESSION READ NewSession  WRITE SetFlags DEFAULT .F. PROTECTED

   DATA Subject           EXPORTED INIT ""
   DATA MessageBody       EXPORTED INIT ""
   DATA SenderName        EXPORTED INIT ""
   DATA SenderAddress     EXPORTED INIT ""
   DATA RecipientName     EXPORTED INIT ""
   DATA RecipientAddress  EXPORTED INIT ""
   DATA Attachments       EXPORTED

   DATA __Flags       PROTECTED INIT MAPI_DIALOG

   METHOD SetFlags()
   METHOD Send()
ENDCLASS

METHOD SetFlags( nFlag, lSet ) CLASS MSendMail
   DEFAULT lSet TO .T.
   IF lSet
      ::__Flags := ::__Flags | nFlag
    ELSE
      ::__Flags := ::__Flags & NOT( nFlag )
   ENDIF
RETURN Self

METHOD Send() CLASS MSendMail
   LOCAL n, aAttach, pSender, pRecip, pFile, pMsg, nRecipients := 0

   pSender := (struct MapiRecipDesc)
   pRecip  := (struct MapiRecipDesc)
   pMsg    := (struct MapiMessage)

   pSender:ulReserved      := NIL
   pSender:ulRecipClass    := MAPI_ORIG
   pSender:lpszName        := IIF( EMPTY( ::SenderName ), ::SenderAddress, ::SenderName )
   pSender:lpszAddress     := "SMTP:" + ::SenderAddress
   pSender:ulEIDSize       := NIL
   pSender:lpEntryID       := NIL

   IF !EMPTY( ::RecipientAddress )
      pRecip:ulReserved       := NIL
      pRecip:ulRecipClass     := MAPI_TO
      pRecip:lpszName         := IIF( EMPTY( ::RecipientName ), ::RecipientAddress, ::RecipientName )
      pRecip:lpszAddress      := "SMTP:" + ::RecipientAddress
      pRecip:ulEIDSize        := NIL
      pRecip:lpEntryID        := NIL
      pMsg:lpRecips           := pRecip
      nRecipients ++
   ENDIF

   IF VALTYPE( ::Attachments ) == "C"
      ::Attachments := {::Attachments}
   ENDIF
   
   aAttach := {}
   FOR n := 1 TO LEN( ::Attachments )
       pFile := (struct MapiFileDesc)
       pFile:ulReserved        := NIL
       pFile:flFlags           := NIL
       pFile:nPosition         := -1
       pFile:lpszPathName      := ::Attachments[n]
       pFile:lpszFileName      := NIL
       pFile:lpFileType        := NIL
       AADD( aAttach, pFile )
   NEXT
   
   DEFAULT ::Subject     TO ""
   DEFAULT ::MessageBody TO ""

   pMsg:ulReserved         := NIL
   pMsg:lpszSubject        := ::Subject
   pMsg:lpszNoteText       := ::MessageBody
   pMsg:lpszMessageType    := NIL
   pMsg:lpszDateReceived   := NIL
   pMsg:lpszConversationID := NIL
   pMsg:flFlags            := NIL
   pMsg:nFileCount         := LEN( aAttach )
   pMsg:nRecipCount        := nRecipients

   pMsg:lpOriginator       := pSender
   pMsg:lpFiles            := IIF( LEN( aAttach ) == 1, aAttach[1], NIL )

RETURN MAPISendMail( 0, 0, pMsg, ::__Flags, 0, IIF( LEN( aAttach ) > 1, aAttach, NIL ) )
/*
#define MAPI_USER_ABORT  1
#define MAPI_E_USER_ABORT  MAPI_USER_ABORT
#define MAPI_E_FAILURE  2
#define MAPI_E_LOGON_FAILURE  3
#define MAPI_E_LOGIN_FAILURE  MAPI_E_LOGON_FAILURE
#define MAPI_E_DISK_FULL  4
#define MAPI_E_INSUFFICIENT_MEMORY  5
#define MAPI_E_ACCESS_DENIED  6
#define MAPI_E_TOO_MANY_SESSIONS  8
#define MAPI_E_TOO_MANY_FILES  9
#define MAPI_E_TOO_MANY_RECIPIENTS  10
#define MAPI_E_ATTACHMENT_NOT_FOUND  11
#define MAPI_E_ATTACHMENT_OPEN_FAILURE  12
#define MAPI_E_ATTACHMENT_WRITE_FAILURE 13
#define MAPI_E_UNKNOWN_RECIPIENT  14
#define MAPI_E_BAD_RECIPTYPE  15
#define MAPI_E_NO_MESSAGES  16
#define MAPI_E_INVALID_MESSAGE  17
#define MAPI_E_TEXT_TOO_LARGE  18
#define MAPI_E_INVALID_SESSION  19
#define MAPI_E_TYPE_NOT_SUPPORTED  20
#define MAPI_E_AMBIGUOUS_RECIPIENT  21
#define MAPI_E_AMBIG_RECIP  MAPI_E_AMBIGUOUS_RECIPIENT
#define MAPI_E_MESSAGE_IN_USE  22
#define MAPI_E_NETWORK_FAILURE  23
#define MAPI_E_INVALID_EDITFIELDS  24
#define MAPI_E_INVALID_RECIPS  25
#define MAPI_E_NOT_SUPPORTED  26
*/

/*
0×80040201  Exception %1 was generated at address %2.
0×80040202  No data source has been opened for the object.
0×80040203  The object does not support this type of data source.
0×80040204  The object does not support the requested property name or namespace.
0×80040205  The object does not support the requested property.
0×80040206  The object is not active. It may have been deleted or it may not have been opened.
0×80040207  The object does not support storing persistent state information for objects.
0×80040208  The requested property or feature, while supported, is not available at this time or in this context.
0×80040209  No default drop directory has been configured for this server.
0x8004020A  The SMTP server name is required, and was not found in the configuration source.
0x8004020B  The NNTP server name is required, and was not found in the configuration source.
0x8004020C  At least one recipient is required, but none were found.
0x8004020D  At least one of the From or Sender fields is required, and neither was found.
0x8004020E  The server rejected the sender address. The server response was: %1
0x8004020F  The server rejected one or more recipient addresses. The server response was: %1
0×80040210  The message could not be posted to the NNTP server. The transport error code was %2. The server response was %1
0×80040211  The message could not be sent to the SMTP server. The transport error code was %2. The server response was %1
0×80040212  The transport lost its connection to the server.
0×80040213  The transport failed to connect to the server.
0×80040214  The Subject, From, and Newsgroup fields are all required, and one or more was not found.
0×80040215  The server rejected the logon attempt due to authentication failure. The server response was: %1
0×80040216  The content type was not valid in this context. For example, the root of an MHTML message must be an HTML document.
0×80040217  The transport was unable to log on to the server.
0×80040218  The requested resource could not be found. The server response was: %1.
0×80040219  Access to the requested resource is denied. The server response was: %1.
0x8004021A  The HTTP request failed.  The server response was: %1.
0x8004021B  This is a multipart body part. It has no content other than the body parts contained within it.
0x8004021C  Multipart body parts must be encoded as 7bit, 8bit, or binary.
0x8004021E  The requested property was not found.
0×80040220  The "SendUsing" configuration value is invalid.
0×80040221  The "PostUsing" configuration value is invalid.
0×80040222  The pickup directory path is required and was not specified.
0×80040223  One or more messages could not be deleted.
0×80040227  The property is read-only.
0×80040228  The property cannot be deleted.
0×80040229  Data  written to the object are inconsistent or invalid.
0x8004022A  The requested property is not in the mail header namespace.
0x8004022B  The requested character set is not installed on the computer.
0x8004022C  The ADO stream has not been opened.
0x8004022D  The content properties are missing.
0x8004022E  Content properties XML must be encoded using UTF-8.
0x8004022F  Failed to parse content properties XML.
0×80040230  Failed to convert a property from XML to a requested type.
0×80040231  No directories were specified for resolution.
0×80040232  Failed to resolve against one or more of the specified directories.
0×80040233  Could not find the Sender’s mailbox.
0×80040234  Binding to self is not allowed.
0×80044000  The first argument is invalid.
0×80044001  The second argument is invalid.
0×80044002  The third argument is invalid.
0×80044003  The fourth argument is invalid.
0×80044004  The fifth argument is invalid.
0x800CCE05  The requested body part was not found in this message.
0x800CCE1D  The content encoding type is invalid.
*/
