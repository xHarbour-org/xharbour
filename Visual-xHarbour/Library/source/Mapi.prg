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
   DATA To                PUBLISHED
   DATA From              PUBLISHED
   DATA CC                PUBLISHED
   DATA BCC               PUBLISHED
   DATA ReplyTo           PUBLISHED
    
   DATA Subject           EXPORTED
   DATA HTMLBody          EXPORTED
   DATA TextBody          EXPORTED
   DATA EnumSendUsing     EXPORTED  INIT {{"Pickup","Port"},{1,2}}
   DATA SendUsing         PUBLISHED INIT 2
   DATA SMTPServer        PUBLISHED
   DATA MimeFormatted     PUBLISHED INIT .F.
   DATA SMTPServerPort    PUBLISHED INIT 0
   DATA SMTPAuthenticate  PUBLISHED INIT .T.
   DATA SMTPUseSSL        PUBLISHED INIT .F.
   
   DATA SendUserName      PUBLISHED
   DATA SendPassword      PUBLISHED
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
   LOCAL oMsg, oConf, oFlds, cSchema, aFiles, cFile
   TRY
      oMsg := GetActiveObject( "CDO.Message" )
    CATCH
      oMsg := CreateObject( "CDO.Message" )
   END
   TRY
      oConf := GetActiveObject( "CDO.Configuration" )
    CATCH
      oConf := CreateObject("CDO.Configuration")
   END
   oFlds := oConf:Fields

   cSchema := "http://schemas.microsoft.com/cdo/configuration/"
   oFlds:Item( cSchema + "sendusing",        ::SendUsing )
   oFlds:Item( cSchema + "smtpserver",       ::SMTPServer )
   oFlds:Item( cSchema + "smtpserverport",   IIF( VALTYPE( ::SMTPServerPort ) == "C", VAL( ::SMTPServerPort ), ::SMTPServerPort ) )
   oFlds:Item( cSchema + "smtpauthenticate", ::SMTPAuthenticate )
   oFlds:Item( cSchema + "sendusername",     ::SendUserName )
   oFlds:Item( cSchema + "sendpassword",     ::SendPassword )
   oFlds:Item( cSchema + "smtpusessl",       ::SMTPUseSSL )

   oFlds:Update()

   WITH OBJECT oMsg
      :To            := ::To
      :From          := ::From
      :Subject       := ::Subject
      :HTMLBody      := ::HTMLBody
      :TextBody      := ::TextBody
      :Configuration := oConf

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
      
      :Send()
   END
RETURN NIL

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

