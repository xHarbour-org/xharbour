/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * HB_SendMail()
 * ( This version of HB_SendMail() started from Luiz's original work on SendMail() )
 *
 * Copyright 2007 Luiz Rafael Culik Guimaraes & Patrick Mast
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "common.ch"


FUNCTION HB_SendMail( cServer, nPort, cFrom, aTo, aCC, aBCC, cBody, cSubject, aFiles, cUser, cPass, cPopServer, nPriority, lRead, lTrace, lPopAuth, lNoAuth, nTimeOut, cReplyTo, lSSL, CaFile, CAPath )
   /*
   cServer    -> Required. IP or domain name of the mail server
   nPort      -> Optional. Port used my email server
   cFrom      -> Required. Email address of the sender
   aTo        -> Required. Character string or array of email addresses to send the email to
   aCC        -> Optional. Character string or array of email adresses for CC (Carbon Copy)
   aBCC       -> Optional. Character string or array of email adresses for BCC (Blind Carbon Copy)
   cBody      -> Optional. The body message of the email as text, or the filename of the HTML message to send.
   cSubject   -> Optional. Subject of the sending email
   aFiles     -> Optional. Array of attachments to the email to send
   cUser      -> Required. User name for the POP3 server
   cPass      -> Required. Password for cUser
   cPopServer -> Required. Pop3 server name or address
   nPriority  -> Optional. Email priority: 1=High, 3=Normal (Standard), 5=Low
   lRead      -> Optional. If set to .T., a confirmation request is send. Standard setting is .F.
   lTrace     -> Optional. If set to .T., a log file is created (sendmail<nNr>.log). Standard setting is .F.
   lNoAuth    -> Optional. Disable Autentication methods
   nTimeOut   -> Optional. Number os ms to wait default 20000 (20s)
   cReplyTo   -> Optional.
   */

   LOCAL oInMail, cBodyTemp, oUrl, oMail, oAttach, aThisFile, cFile, cData, oUrl1

   LOCAL cTmp          :=""
   LOCAL cMimeText
   LOCAL cTo           := ""
   LOCAL cCC           := ""
   LOCAL cBCC          := ""

   LOCAL lConnectPlain := .F.
   LOCAL lReturn       := .T.
   LOCAL lAuthLogin    := .F.
   LOCAL lAuthPlain    := .F.
   LOCAL lConnect      := .T.
   LOCAL oPop
   LOCAL aData := {}, nCount
   LOCAL cTemp

   DEFAULT cUser       TO ""
   DEFAULT cPass       TO ""
   DEFAULT nPort       TO 25
   DEFAULT aFiles      TO {}
   DEFAULT nPriority   TO 3
   DEFAULT lRead       TO .F.
   DEFAULT lTrace      TO .F.
   DEFAULT lPopAuth    TO .T.
   DEFAULT lNoAuth     TO .F.
   DEFAULT nTimeOut    TO 3000
   DEFAULT cReplyTo    TO ""
   DEFAULT lSSL        TO .F.
   IF Valtype( aFiles ) == "C"
      cTemp := aFiles
      aFiles := { cTemp }
   ENDIF

   cUser := StrTran( cUser, "@", "&at;" )

   IF !( (".htm" IN Lower( cBody ) .OR. ".html" IN Lower( cBody ) ) .AND. File(cBody) )

      IF !( Right( cBody, 2 ) == HB_OSNewLine() )
         cBody += HB_OsNewLine()
      ENDIF

   ENDIF

   // cTo
   IF Valtype( aTo ) == "A"
      IF Len( aTo ) > 1
         FOR EACH cTo IN aTo
            IF HB_EnumIndex() != 1
               cTmp += cTo + ","
            ENDIF
         NEXT
         cTmp := Substr( cTmp, 1, Len( cTmp ) - 1 )
      ENDIF
      cTo := aTo[ 1 ]
      IF Len( cTmp ) > 0
         cTo += "," + cTmp
      ENDIF
   ELSE
      cTo := Alltrim( aTo )
   ENDIF


   // CC (Carbon Copy)
   IF Valtype(aCC) =="A"
      IF Len(aCC) >0
         FOR EACH cTmp IN aCC
            cCC += cTmp + ","
         NEXT
         cCC := Substr( cCC, 1, Len( cCC ) - 1 )
      ENDIF
   ELSEIF Valtype(aCC) =="C"
      cCC := Alltrim( aCC )
   ENDIF


   // BCC (Blind Carbon Copy)
   IF Valtype(aBCC) =="A"
      IF Len(aBCC)>0
         FOR EACH cTmp IN aBCC
            cBCC += cTmp + ","
         NEXT
         cBCC := Substr( cBCC, 1, Len( cBCC ) - 1 )
      ENDIF
   ELSEIF Valtype(aBCC) =="C"
      cBCC := Alltrim( aBCC )
   ENDIF

   IF cPopServer != NIL .AND. lPopAuth
      TRY
         oUrl1 := tUrl():New( "pop://" + cUser + ":" + cPass + "@" + cPopServer + "/" )
         oUrl1:cUserid := Strtran( cUser, "&at;", "@" )
         opop:= tIPClientPOP():New( oUrl1, lTrace, , CaFile, CAPath  )
         if !oPop:lSSL  .and. lSSL
            oPop:lSSL := .T.
         endif
         IF oPop:Open()
            oPop:Close()
         ENDIF
      CATCH
         lReturn := .F.
      END

   ENDIF

   IF !lReturn
      RETURN .F.
   ENDIF

   TRY
      oUrl := tUrl():New( "smtp://" + cUser + "@" + cServer + '/' + cTo )
   CATCH
      lReturn := .F.
   END

   IF !lReturn
      RETURN .F.
   ENDIF

   oUrl:nPort   := nPort
   oUrl:cUserid := Strtran( cUser, "&at;", "@" )

   oMail   := tipMail():new()
   oAttach := tipMail():new()
   oAttach:SetEncoder( "7bit" )

   IF (".htm" IN Lower( cBody ) .OR. ".html" IN Lower( cBody ) ) .AND. File(cBody)
      cMimeText := "text/html ; charset=ISO-8859-1"
      oAttach:hHeaders[ "Content-Type" ] := cMimeText
      cBodyTemp := cBody
      cBody     := MemoRead( cBodyTemp ) + chr( 13 ) + chr( 10 )

   ELSE
      oMail:hHeaders[ "Content-Type" ] := "text/plain; charset=iso-8859-1"
   ENDIF

   oAttach:SetBody( cBody )
   oMail:Attach( oAttach )
   oUrl:cFile := cTo + If( Empty(cCC), "", "," + cCC ) + If( Empty(cBCC), "", "," + cBCC)

   oMail:hHeaders[ "Date" ] := tip_Timestamp()
   oMail:hHeaders[ "From" ] := cFrom

   IF !Empty(cCC)
      oMail:hHeaders[ "Cc" ] := cCC
   ENDIF
   IF !Empty(cBCC)
      oMail:hHeaders[ "Bcc" ] := cBCC
   ENDIF
   IF !Empty(cReplyTo)
      oMail:hHeaders[ "Reply-To" ] := cReplyTo
   ENDIF

   TRY
      oInmail := tIPClientSMTP():New( oUrl, lTrace, , CaFile, CAPath  )
   CATCH
      lReturn := .F.
   END

   IF !lReturn
      RETURN .F.
   ENDIF
   if !oInmail:lSSL  .and. lSSL
      oInmail:lSSL := .T.
   endif

   oInmail:nConnTimeout:= nTimeOut

   IF !lNoAuth

      IF oInMail:Opensecure()

         WHILE .T.
            oInMail:GetOk()
            IF oInMail:cReply == NIL
               EXIT
            ELSEIF "LOGIN" IN oInMail:cReply
               lAuthLogin := .T.
            ELSEIF "PLAIN" IN oInMail:cReply
               lAuthPlain := .T.
            ELSEIF "TLS" in oInMail:cReply
               lSSL := .t.
            ENDIF
         ENDDO

         IF lAuthLogin
            IF !oInMail:Auth( StrTran( cUser, "&at;", "@" ), cPass )
               lConnect := .F.
            ELSE
               lConnectPlain := .T.
            ENDIF
         ENDIF

         IF lAuthPlain .AND. !lConnect
            IF !oInMail:AuthPlain( StrTran( cUser, "&at;", "@" ), cPass )
               lConnect := .F.
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
   ELSE
      lConnect := .F.
   ENDIF

   IF !lConnect

      if !lNoAuth
         oInMail:Close()
      endif

      TRY
         oInmail := tIPClientsmtp():New( oUrl, lTrace, , CaFile, CAPath  )
      CATCH
         lReturn := .F.
      END
      
      if !lReturn .AND. !oInmail:lSSL  .and. lSSL
         oInmail:lSSL := .T.
      endif

      oInmail:nConnTimeout:=nTimeOut


      IF !oInMail:Open()
         oInmail:Close()
         RETURN .F.
      ENDIF

      WHILE .T.
         oInMail:GetOk()
         IF oInMail:cReply == NIL
            EXIT
         ENDIF
      ENDDO

   ENDIF

   oInMail:oUrl:cUserid := cFrom
   oMail:hHeaders[ "To" ]      := cTo
   oMail:hHeaders[ "Subject" ] := cSubject

   FOR EACH aThisFile IN AFiles

      IF Valtype( aThisFile ) == "C"
         cFile := aThisFile
         Memoread( cFile )
      ELSEIF Valtype( aThisFile ) == "A" .AND. Len( aThisFile ) >= 2
         cFile := aThisFile[ 1 ]
      ELSE
         //lReturn := .F.
         //EXIT
         LOOP
      ENDIF

      omail:AttachFile( cFile )

   NEXT

   IF lRead
      oMail:hHeaders[ "Disposition-Notification-To" ] := StrTran( cUser, "&at;", "@" )
   ENDIF

   IF nPriority != 3
      oMail:hHeaders[ "X-Priority" ] := Str( nPriority, 1 )
   ENDIF

//   oInmail:Write( oMail:ToString() )
   cData := oMail:ToString()
   for nCount := 1 to len(cData) step 1024
       aadd(aData, substr( cData, nCount, 1024))
   next
   for nCount :=1 to len(aData)
      oInMail:Write( aData[nCount], len(aData[nCount]))
   next

   lReturn := oInMail:Commit()
   oInMail:Close()

RETURN lReturn


//-------------------------------------------------------------//


FUNCTION HB_SetMimeType( cFile, cFname, cFext )

   cFile := Lower( cFile )

IF cFile LIKE ".+\.vbd" 
   RETURN "application/activexdocument=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(asn|asz|asd)" 
   RETURN "application/astound=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pqi" 
   RETURN "application/cprplayer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tsp" 
   RETURN "application/dsptype=" + cFname + cFext
ELSEIF cFile LIKE ".+\.exe" 
   RETURN "application/exe=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(sml|ofml)" 
   RETURN "application/fml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pfr" 
   RETURN "application/font-tdpfr=" + cFname + cFext
ELSEIF cFile LIKE ".+\.frl" 
   RETURN "application/freeloader=" + cFname + cFext
ELSEIF cFile LIKE ".+\.spl" 
   RETURN "application/futuresplash =" + cFname + cFext
ELSEIF cFile LIKE ".+\.gz" 
   RETURN "application/gzip =" + cFname + cFext
ELSEIF cFile LIKE ".+\.stk" 
   RETURN "application/hstu =" + cFname + cFext
ELSEIF cFile LIKE ".+\.ips" 
   RETURN "application/ips=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ptlk" 
   RETURN "application/listenup =" + cFname + cFext
ELSEIF cFile LIKE ".+\.hqx" 
   RETURN "application/mac-binhex40 =" + cFname + cFext
ELSEIF cFile LIKE ".+\.mbd" 
   RETURN "application/mbedlet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mfp" 
   RETURN "application/mirage=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(pot|pps|ppt|ppz)" 
   RETURN "application/mspowerpoint =" + cFname + cFext
ELSEIF cFile LIKE ".+\.n2p" 
   RETURN "application/n2p=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(bin|class|lha|lzh|lzx|dbf)" 
   RETURN "application/octet-stream =" + cFname + cFext
ELSEIF cFile LIKE ".+\.oda" 
   RETURN "application/oda=" + cFname + cFext
ELSEIF cFile LIKE ".+\.axs" 
   RETURN "application/olescript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.zpa" 
   RETURN "application/pcphoto=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pdf" 
   RETURN "application/pdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(ai|eps|ps)" 
   RETURN "application/postscript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.shw" 
   RETURN "application/presentations=" + cFname + cFext
ELSEIF cFile LIKE ".+\.qrt" 
   RETURN "application/quest=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rtc" 
   RETURN "application/rtc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rtf" 
   RETURN "application/rtf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.smp" 
   RETURN "application/studiom=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dst" 
   RETURN "application/tajima=" + cFname + cFext
ELSEIF cFile LIKE ".+\.talk" 
   RETURN "application/talker=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tbk" 
   RETURN "application/toolbook =" + cFname + cFext
ELSEIF cFile LIKE ".+\.vmd" 
   RETURN "application/vocaltec-media-desc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vmf" 
   RETURN "application/vocaltec-media-file=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wri" 
   RETURN "application/write=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wid" 
   RETURN "application/x-DemoShield =" + cFname + cFext
ELSEIF cFile LIKE ".+\.rrf" 
   RETURN "application/x-InstallFromTheWeb=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wis" 
   RETURN "application/x-InstallShield=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ins" 
   RETURN "application/x-NET-Install=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tmv" 
   RETURN "application/x-Parable-Thing=" + cFname + cFext
ELSEIF cFile LIKE ".+\.arj" 
   RETURN "application/x-arj=" + cFname + cFext
ELSEIF cFile LIKE ".+\.asp" 
   RETURN "application/x-asap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aab" 
   RETURN "application/x-authorware-bin =" + cFname + cFext
ELSEIF cFile LIKE ".+\.(aam|aas)" 
   RETURN "application/x-authorware-map =" + cFname + cFext
ELSEIF cFile LIKE ".+\.bcpio" 
   RETURN "application/x-bcpio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vcd" 
   RETURN "application/x-cdlink =" + cFname + cFext
ELSEIF cFile LIKE ".+\.chat" 
   RETURN "application/x-chat=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cnc" 
   RETURN "application/x-cnc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(coda|page)" 
   RETURN "application/x-coda=" + cFname + cFext
ELSEIF cFile LIKE ".+\.z" 
   RETURN "application/x-compress=" + cFname + cFext
ELSEIF cFile LIKE ".+\.con" 
   RETURN "application/x-connector=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cpio" 
   RETURN "application/x-cpio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pqf" 
   RETURN "application/x-cprplayer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.csh" 
   RETURN "application/x-csh=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(cu|csm)" 
   RETURN "application/x-cu-seeme=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(dcr|dir|dxr|swa)" 
   RETURN "application/x-director=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dvi" 
   RETURN "application/x-dvi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.evy" 
   RETURN "application/x-envoy=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ebk" 
   RETURN "application/x-expandedbook=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gtar" 
   RETURN "application/x-gtar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hdf" 
   RETURN "application/x-hdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.map" 
   RETURN "application/x-httpd-imap =" + cFname + cFext
ELSEIF cFile LIKE ".+\.phtml" 
   RETURN "application/x-httpd-php=" + cFname + cFext
ELSEIF cFile LIKE ".+\.php3" 
   RETURN "application/x-httpd-php3 =" + cFname + cFext
ELSEIF cFile LIKE ".+\.ica" 
   RETURN "application/x-ica=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ipx" 
   RETURN "application/x-ipix=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ips" 
   RETURN "application/x-ipscript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.js" 
   RETURN "application/x-javascript =" + cFname + cFext
ELSEIF cFile LIKE ".+\.latex" 
   RETURN "application/x-latex=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bin" 
   RETURN "application/x-macbinary=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mif" 
   RETURN "application/x-mif=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(mpl|mpire)" 
   RETURN "application/x-mpire=" + cFname + cFext
ELSEIF cFile LIKE ".+\.adr" 
   RETURN "application/x-msaddr =" + cFname + cFext
ELSEIF cFile LIKE ".+\.wlt" 
   RETURN "application/x-mswallet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(nc|cdf)" 
   RETURN "application/x-netcdf =" + cFname + cFext
ELSEIF cFile LIKE ".+\.npx" 
   RETURN "application/x-netfpx =" + cFname + cFext
ELSEIF cFile LIKE ".+\.nsc" 
   RETURN "application/x-nschat =" + cFname + cFext
ELSEIF cFile LIKE ".+\.pgp" 
   RETURN "application/x-pgp-plugin =" + cFname + cFext
ELSEIF cFile LIKE ".+\.css" 
   RETURN "application/x-pointplus=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sh" 
   RETURN "application/x-sh =" + cFname + cFext
ELSEIF cFile LIKE ".+\.shar" 
   RETURN "application/x-shar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.swf" 
   RETURN "application/x-shockwave-flash=" + cFname + cFext
ELSEIF cFile LIKE ".+\.spr" 
   RETURN "application/x-sprite =" + cFname + cFext
ELSEIF cFile LIKE ".+\.sprite" 
   RETURN "application/x-sprite =" + cFname + cFext
ELSEIF cFile LIKE ".+\.sit" 
   RETURN "application/x-stuffit=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sca" 
   RETURN "application/x-supercard=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sv4cpio" 
   RETURN "application/x-sv4cpio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sv4crc" 
   RETURN "application/x-sv4crc =" + cFname + cFext
ELSEIF cFile LIKE ".+\.tar" 
   RETURN "application/x-tar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tcl" 
   RETURN "application/x-tcl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tex" 
   RETURN "application/x-tex=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(texinfo|texi)" 
   RETURN "application/x-texinfo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tlk" 
   RETURN "application/x-tlk=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(t|tr|roff)" 
   RETURN "application/x-troff=" + cFname + cFext
ELSEIF cFile LIKE ".+\.man" 
   RETURN "application/x-troff-man=" + cFname + cFext
ELSEIF cFile LIKE ".+\.me" 
   RETURN "application/x-troff-me=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ms" 
   RETURN "application/x-troff-ms=" + cFname + cFext
ELSEIF cFile LIKE ".+\.alt" 
   RETURN "application/x-up-alert=" + cFname + cFext
ELSEIF cFile LIKE ".+\.che" 
   RETURN "application/x-up-cacheop =" + cFname + cFext
ELSEIF cFile LIKE ".+\.ustar" 
   RETURN "application/x-ustar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.src" 
   RETURN "application/x-wais-source=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xls" 
   RETURN "application/xls=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xlt" 
   RETURN "application/xlt=" + cFname + cFext
ELSEIF cFile LIKE ".+\.zip" 
   RETURN "application/zip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(au|snd)" 
   RETURN "audio/basic=" + cFname + cFext
ELSEIF cFile LIKE ".+\.es" 
   RETURN "audio/echospeech =" + cFname + cFext
ELSEIF cFile LIKE ".+\.(gsm|gsd)" 
   RETURN "audio/gsm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rmf" 
   RETURN "audio/rmf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tsi" 
   RETURN "audio/tsplayer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vox" 
   RETURN "audio/voxware=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wtx" 
   RETURN "audio/wtx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(aif|aiff|aifc)" 
   RETURN "audio/x-aiff =" + cFname + cFext
ELSEIF cFile LIKE ".+\.(cht|dus)" 
   RETURN "audio/x-dspeech=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(mid|midi)" 
   RETURN "audio/x-midi =" + cFname + cFext
ELSEIF cFile LIKE ".+\.mp3" 
   RETURN "audio/x-mpeg =" + cFname + cFext
ELSEIF cFile LIKE ".+\.mp2" 
   RETURN "audio/x-mpeg =" + cFname + cFext
ELSEIF cFile LIKE ".+\.m3u" 
   RETURN "audio/x-mpegurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(ram|ra)" 
   RETURN "audio/x-pn-realaudio =" + cFname + cFext
ELSEIF cFile LIKE ".+\.rpm" 
   RETURN "audio/x-pn-realaudio-plugin=" + cFname + cFext
ELSEIF cFile LIKE ".+\.stream" 
   RETURN "audio/x-qt-stream=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rmf" 
   RETURN "audio/x-rmf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(vqf|vql)" 
   RETURN "audio/x-twinvq=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vqe" 
   RETURN "audio/x-twinvq-plugin=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wav" 
   RETURN "audio/x-wav=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wtx" 
   RETURN "audio/x-wtx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mol" 
   RETURN "chemical/x-mdl-molfile=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pdb" 
   RETURN "chemical/x-pdb=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dwf" 
   RETURN "drawing/x-dwf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ivr" 
   RETURN "i-world/i-vrml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cod" 
   RETURN "image/cis-cod=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cpi" 
   RETURN "image/cpi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fif" 
   RETURN "image/fif=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gif" 
   RETURN "image/gif=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ief" 
   RETURN "image/ief=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(jpeg|jpg|jpe)" 
   RETURN "image/jpeg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rip" 
   RETURN "image/rip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.svh" 
   RETURN "image/svh=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(tiff|tif)" 
   RETURN "image/tiff=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mcf" 
   RETURN "image/vasa=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(svf|dwg|dxf)" 
   RETURN "image/vnd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wi" 
   RETURN "image/wavelet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ras" 
   RETURN "image/x-cmu-raster=" + cFname + cFext
ELSEIF cFile LIKE ".+\.etf" 
   RETURN "image/x-etf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fpx" 
   RETURN "image/x-fpx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(fh5|fh4|fhc)" 
   RETURN "image/x-freehand =" + cFname + cFext
ELSEIF cFile LIKE ".+\.dsf" 
   RETURN "image/x-mgx-dsf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pnm" 
   RETURN "image/x-portable-anymap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pbm" 
   RETURN "image/x-portable-bitmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pgm" 
   RETURN "image/x-portable-graymap =" + cFname + cFext
ELSEIF cFile LIKE ".+\.ppm" 
   RETURN "image/x-portable-pixmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rgb" 
   RETURN "image/x-rgb=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xbm" 
   RETURN "image/x-xbitmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xpm" 
   RETURN "image/x-xpixmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xwd" 
   RETURN "image/x-xwindowdump=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dig" 
   RETURN "multipart/mixed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.push" 
   RETURN "multipart/x-mixed-replace=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(wan|waf)" 
   RETURN "plugin/wanimate=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ccs" 
   RETURN "text/ccs =" + cFname + cFext
ELSEIF cFile LIKE ".+\.(htm|html)" 
   RETURN "text/html=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pgr" 
   RETURN "text/parsnegar-document=" + cFname + cFext
ELSEIF cFile LIKE ".+\.txt" 
   RETURN "text/plain=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rtx" 
   RETURN "text/richtext=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tsv" 
   RETURN "text/tab-separated-values=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hdml" 
   RETURN "text/x-hdml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.etx" 
   RETURN "text/x-setext=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(talk|spc)" 
   RETURN "text/x-speech=" + cFname + cFext
ELSEIF cFile LIKE ".+\.afl" 
   RETURN "video/animaflex=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(mpeg|mpg|mpe)" 
   RETURN "video/mpeg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(qt|mov)" 
   RETURN "video/quicktime=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(viv|vivo)" 
   RETURN "video/vnd.vivo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(asf|asx)" 
   RETURN "video/x-ms-asf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.avi" 
   RETURN "video/x-msvideo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.movie" 
   RETURN "video/x-sgi-movie=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(vgm|vgx|xdr)" 
   RETURN "video/x-videogram=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vgp" 
   RETURN "video/x-videogram-plugin =" + cFname + cFext
ELSEIF cFile LIKE ".+\.vts" 
   RETURN "workbook/formulaone=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vtts" 
   RETURN "workbook/formulaone=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(3dmf|3dm|qd3d|qd3)" 
   RETURN "x-world/x-3dmf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.svr" 
   RETURN "x-world/x-svr=" + cFname + cFext
ELSEIF cFile LIKE ".+\.(wrl|wrz)" 
   RETURN "x-world/x-vrml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vrt" 
   RETURN "x-world/x-vrt=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rar" 
   RETURN "application/x-rar-compressed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.torrent" 
   RETURN "application/x-bittorrent=" + cFname + cFext
ELSEIF cFile LIKE ".+\.x3d" 
   RETURN "application/vnd.hzn-3d-crossword=" + cFname + cFext
ELSEIF cFile LIKE ".+\.3gp" 
   RETURN "video/3gpp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.3g2" 
   RETURN "video/3gpp2=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mseq" 
   RETURN "application/vnd.mseq=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pwn" 
   RETURN "application/vnd.3m.post-it-notes=" + cFname + cFext
ELSEIF cFile LIKE ".+\.plb" 
   RETURN "application/vnd.3gpp.pic-bw-large=" + cFname + cFext
ELSEIF cFile LIKE ".+\.psb" 
   RETURN "application/vnd.3gpp.pic-bw-small=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pvb" 
   RETURN "application/vnd.3gpp.pic-bw-var=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tcap" 
   RETURN "application/vnd.3gpp2.tcap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.7z" 
   RETURN "application/x-7z-compressed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.abw" 
   RETURN "application/x-abiword=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ace" 
   RETURN "application/x-ace-compressed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.acc" 
   RETURN "application/vnd.americandynamics.acc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.acu" 
   RETURN "application/vnd.acucobol=" + cFname + cFext
ELSEIF cFile LIKE ".+\.atc" 
   RETURN "application/vnd.acucorp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.adp" 
   RETURN "audio/adpcm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aab" 
   RETURN "application/x-authorware-bin=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aam" 
   RETURN "application/x-authorware-map=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aas" 
   RETURN "application/x-authorware-seg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.air" 
   RETURN "application/vnd.adobe.air-application-installer-package+zip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.swf" 
   RETURN "application/x-shockwave-flash=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fxp" 
   RETURN "application/vnd.adobe.fxp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pdf" 
   RETURN "application/pdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ppd" 
   RETURN "application/vnd.cups-ppd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dir" 
   RETURN "application/x-director=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xdp" 
   RETURN "application/vnd.adobe.xdp+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xfdf" 
   RETURN "application/vnd.adobe.xfdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aac" 
   RETURN "audio/x-aac=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ahead" 
   RETURN "application/vnd.ahead.space=" + cFname + cFext
ELSEIF cFile LIKE ".+\.azf" 
   RETURN "application/vnd.airzip.filesecure.azf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.azs" 
   RETURN "application/vnd.airzip.filesecure.azs=" + cFname + cFext
ELSEIF cFile LIKE ".+\.azw" 
   RETURN "application/vnd.amazon.ebook=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ami" 
   RETURN "application/vnd.amiga.ami=" + cFname + cFext
ELSEIF cFile LIKE ".+\.apk" 
   RETURN "application/vnd.android.package-archive=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cii" 
   RETURN "application/vnd.anser-web-certificate-issue-initiation=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fti" 
   RETURN "application/vnd.anser-web-funds-transfer-initiation=" + cFname + cFext
ELSEIF cFile LIKE ".+\.atx" 
   RETURN "application/vnd.antix.game-component=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpkg" 
   RETURN "application/vnd.apple.installer+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aw " 
   RETURN "application/applixware=" + cFname + cFext
ELSEIF cFile LIKE ".+\.les" 
   RETURN "application/vnd.hhe.lesson-player=" + cFname + cFext
ELSEIF cFile LIKE ".+\.swi" 
   RETURN "application/vnd.aristanetworks.swi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.s" 
   RETURN "text/x-asm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.atomcat" 
   RETURN "application/atomcat+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.atomsvc" 
   RETURN "application/atomsvc+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.atom" 
   RETURN "application/atom+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ac" 
   RETURN "application/pkix-attr-cert=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aif" 
   RETURN "audio/x-aiff=" + cFname + cFext
ELSEIF cFile LIKE ".+\.avi" 
   RETURN "video/x-msvideo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aep" 
   RETURN "application/vnd.audiograph=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dxf" 
   RETURN "image/vnd.dxf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dwf" 
   RETURN "model/vnd.dwf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.par" 
   RETURN "text/plain-bas=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bcpio" 
   RETURN "application/x-bcpio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bin" 
   RETURN "application/octet-stream=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bmp" 
   RETURN "image/bmp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.torrent " 
   RETURN "application/x-bittorrent=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cod" 
   RETURN "application/vnd.rim.cod=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpm" 
   RETURN "application/vnd.blueice.multipass=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bmi" 
   RETURN "application/vnd.bmi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sh" 
   RETURN "application/x-sh=" + cFname + cFext
ELSEIF cFile LIKE ".+\.btif" 
   RETURN "image/prs.btif=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rep" 
   RETURN "application/vnd.businessobjects=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bz" 
   RETURN "application/x-bzip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bz2" 
   RETURN "application/x-bzip2=" + cFname + cFext
ELSEIF cFile LIKE ".+\.csh" 
   RETURN "application/x-csh=" + cFname + cFext
ELSEIF cFile LIKE ".+\.c" 
   RETURN "text/x-c=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdxml" 
   RETURN "application/vnd.chemdraw+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.css" 
   RETURN "text/css=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdx" 
   RETURN "chemical/x-cdx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cml" 
   RETURN "chemical/x-cml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.csml" 
   RETURN "chemical/x-csml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdbcmsg " 
   RETURN "application/vnd.contact.cmsg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cla" 
   RETURN "application/vnd.claymore=" + cFname + cFext
ELSEIF cFile LIKE ".+\.c4g" 
   RETURN "application/vnd.clonk.c4group=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sub" 
   RETURN "image/vnd.dvb.subtitle=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdmia" 
   RETURN "application/cdmi-capability=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdmic" 
   RETURN "application/cdmi-container=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdmid" 
   RETURN "application/cdmi-domain=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdmio" 
   RETURN "application/cdmi-object=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdmiq" 
   RETURN "application/cdmi-queue=" + cFname + cFext
ELSEIF cFile LIKE ".+\.c11amc" 
   RETURN "application/vnd.cluetrust.cartomobile-config=" + cFname + cFext
ELSEIF cFile LIKE ".+\.c11amz" 
   RETURN "application/vnd.cluetrust.cartomobile-config-pkg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ras" 
   RETURN "image/x-cmu-raster=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dae" 
   RETURN "model/vnd.collada+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.csv" 
   RETURN "text/csv=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cpt" 
   RETURN "application/mac-compactpro=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmlc" 
   RETURN "application/vnd.wap.wmlc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cgm" 
   RETURN "image/cgm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ice" 
   RETURN "x-conference/x-cooltalk=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cmx" 
   RETURN "image/x-cmx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xar" 
   RETURN "application/vnd.xara=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cmc" 
   RETURN "application/vnd.cosmocaller=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cpio" 
   RETURN "application/x-cpio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.clkx" 
   RETURN "application/vnd.crick.clicker=" + cFname + cFext
ELSEIF cFile LIKE ".+\.clkk" 
   RETURN "application/vnd.crick.clicker.keyboard=" + cFname + cFext
ELSEIF cFile LIKE ".+\.clkp" 
   RETURN "application/vnd.crick.clicker.palette=" + cFname + cFext
ELSEIF cFile LIKE ".+\.clkt" 
   RETURN "application/vnd.crick.clicker.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.clkw" 
   RETURN "application/vnd.crick.clicker.wordbank=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wbs" 
   RETURN "application/vnd.criticaltools.wbs+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cryptonote" 
   RETURN "application/vnd.rig.cryptonote=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cif" 
   RETURN "chemical/x-cif=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cmdf" 
   RETURN "chemical/x-cmdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cu" 
   RETURN "application/cu-seeme=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cww" 
   RETURN "application/prs.cww=" + cFname + cFext
ELSEIF cFile LIKE ".+\.curl" 
   RETURN "text/vnd.curl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dcurl" 
   RETURN "text/vnd.curl.dcurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mcurl" 
   RETURN "text/vnd.curl.mcurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.scurl" 
   RETURN "text/vnd.curl.scurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.car" 
   RETURN "application/vnd.curl.car=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pcurl" 
   RETURN "application/vnd.curl.pcurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cmp" 
   RETURN "application/vnd.yellowriver-custom-menu=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dssc" 
   RETURN "application/dssc+der=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xdssc" 
   RETURN "application/dssc+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.deb" 
   RETURN "application/x-debian-package=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uva" 
   RETURN "audio/vnd.dece.audio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uvi" 
   RETURN "image/vnd.dece.graphic=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uvh" 
   RETURN "video/vnd.dece.hd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uvm" 
   RETURN "video/vnd.dece.mobile=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uvu" 
   RETURN "video/vnd.uvvu.mp4=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uvp" 
   RETURN "video/vnd.dece.pd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uvs" 
   RETURN "video/vnd.dece.sd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uvv" 
   RETURN "video/vnd.dece.video=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dvi" 
   RETURN "application/x-dvi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.seed" 
   RETURN "application/vnd.fdsn.seed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dtb" 
   RETURN "application/x-dtbook+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.res" 
   RETURN "application/x-dtbresource+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ait" 
   RETURN "application/vnd.dvb.ait=" + cFname + cFext
ELSEIF cFile LIKE ".+\.svc" 
   RETURN "application/vnd.dvb.service=" + cFname + cFext
ELSEIF cFile LIKE ".+\.eol" 
   RETURN "audio/vnd.digital-winds=" + cFname + cFext
ELSEIF cFile LIKE ".+\.djvu" 
   RETURN "image/vnd.djvu=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dtd" 
   RETURN "application/xml-dtd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mlp" 
   RETURN "application/vnd.dolby.mlp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wad" 
   RETURN "application/x-doom=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dpg" 
   RETURN "application/vnd.dpgraph=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dra" 
   RETURN "audio/vnd.dra=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dfac" 
   RETURN "application/vnd.dreamfactory=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dts" 
   RETURN "audio/vnd.dts=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dtshd" 
   RETURN "audio/vnd.dts.hd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dwg" 
   RETURN "image/vnd.dwg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.geo" 
   RETURN "application/vnd.dynageo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.es" 
   RETURN "application/ecmascript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mag" 
   RETURN "application/vnd.ecowin.chart=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mmr" 
   RETURN "image/vnd.fujixerox.edmics-mmr=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rlc" 
   RETURN "image/vnd.fujixerox.edmics-rlc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.exi" 
   RETURN "application/exi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mgz" 
   RETURN "application/vnd.proteus.magazine=" + cFname + cFext
ELSEIF cFile LIKE ".+\.epub" 
   RETURN "application/epub+zip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.eml" 
   RETURN "message/rfc822=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nml" 
   RETURN "application/vnd.enliven=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xpr" 
   RETURN "application/vnd.is-xpr=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xif" 
   RETURN "image/vnd.xiff=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xfdl" 
   RETURN "application/vnd.xfdl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.emma" 
   RETURN "application/emma+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ez2" 
   RETURN "application/vnd.ezpix-album=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ez3" 
   RETURN "application/vnd.ezpix-package=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fst" 
   RETURN "image/vnd.fst=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fvt" 
   RETURN "video/vnd.fvt=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fbs" 
   RETURN "image/vnd.fastbidsheet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fe_launch" 
   RETURN "application/vnd.denovo.fcselayout-link=" + cFname + cFext
ELSEIF cFile LIKE ".+\.f4v" 
   RETURN "video/x-f4v=" + cFname + cFext
ELSEIF cFile LIKE ".+\.flv" 
   RETURN "video/x-flv=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fpx" 
   RETURN "image/vnd.fpx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.npx" 
   RETURN "image/vnd.net-fpx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.flx" 
   RETURN "text/vnd.fmi.flexstor=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fli" 
   RETURN "video/x-fli=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ftc" 
   RETURN "application/vnd.fluxtime.clip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fdf" 
   RETURN "application/vnd.fdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.f" 
   RETURN "text/x-fortran=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mif" 
   RETURN "application/vnd.mif=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fm" 
   RETURN "application/vnd.framemaker=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fh" 
   RETURN "image/x-freehand=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fsc" 
   RETURN "application/vnd.fsc.weblaunch=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fnc" 
   RETURN "application/vnd.frogans.fnc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ltf" 
   RETURN "application/vnd.frogans.ltf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ddd" 
   RETURN "application/vnd.fujixerox.ddd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xdw" 
   RETURN "application/vnd.fujixerox.docuworks=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xbd" 
   RETURN "application/vnd.fujixerox.docuworks.binder=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oas" 
   RETURN "application/vnd.fujitsu.oasys=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oa2" 
   RETURN "application/vnd.fujitsu.oasys2=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oa3" 
   RETURN "application/vnd.fujitsu.oasys3=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fg5" 
   RETURN "application/vnd.fujitsu.oasysgp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bh2" 
   RETURN "application/vnd.fujitsu.oasysprs=" + cFname + cFext
ELSEIF cFile LIKE ".+\.spl" 
   RETURN "application/x-futuresplash=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fzs" 
   RETURN "application/vnd.fuzzysheet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.g3" 
   RETURN "image/g3fax=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gmx" 
   RETURN "application/vnd.gmx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gtw" 
   RETURN "model/vnd.gtw=" + cFname + cFext
ELSEIF cFile LIKE ".+\.txd" 
   RETURN "application/vnd.genomatix.tuxedo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ggb" 
   RETURN "application/vnd.geogebra.file=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ggt" 
   RETURN "application/vnd.geogebra.tool=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gdl" 
   RETURN "model/vnd.gdl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gex" 
   RETURN "application/vnd.geometry-explorer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gxt" 
   RETURN "application/vnd.geonext=" + cFname + cFext
ELSEIF cFile LIKE ".+\.g2w" 
   RETURN "application/vnd.geoplan=" + cFname + cFext
ELSEIF cFile LIKE ".+\.g3w" 
   RETURN "application/vnd.geospace=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gsf" 
   RETURN "application/x-font-ghostscript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bdf" 
   RETURN "application/x-font-bdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gtar" 
   RETURN "application/x-gtar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.texinfo " 
   RETURN "application/x-texinfo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gnumeric" 
   RETURN "application/x-gnumeric=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kml" 
   RETURN "application/vnd.google-earth.kml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kmz" 
   RETURN "application/vnd.google-earth.kmz=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gqf" 
   RETURN "application/vnd.grafeq=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gif" 
   RETURN "image/gif=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gv" 
   RETURN "text/vnd.graphviz=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gac" 
   RETURN "application/vnd.groove-account=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ghf" 
   RETURN "application/vnd.groove-help=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gim" 
   RETURN "application/vnd.groove-identity-message=" + cFname + cFext
ELSEIF cFile LIKE ".+\.grv" 
   RETURN "application/vnd.groove-injector=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gtm" 
   RETURN "application/vnd.groove-tool-message=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tpl" 
   RETURN "application/vnd.groove-tool-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vcg" 
   RETURN "application/vnd.groove-vcard=" + cFname + cFext
ELSEIF cFile LIKE ".+\.h261" 
   RETURN "video/h261=" + cFname + cFext
ELSEIF cFile LIKE ".+\.h263" 
   RETURN "video/h263=" + cFname + cFext
ELSEIF cFile LIKE ".+\.h264" 
   RETURN "video/h264=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hpid" 
   RETURN "application/vnd.hp-hpid=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hps" 
   RETURN "application/vnd.hp-hps=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hdf" 
   RETURN "application/x-hdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rip" 
   RETURN "audio/vnd.rip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hbci" 
   RETURN "application/vnd.hbci=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jlt" 
   RETURN "application/vnd.hp-jlyt=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pcl" 
   RETURN "application/vnd.hp-pcl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hpgl" 
   RETURN "application/vnd.hp-hpgl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hvs" 
   RETURN "application/vnd.yamaha.hv-script=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hvd" 
   RETURN "application/vnd.yamaha.hv-dic=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hvp" 
   RETURN "application/vnd.yamaha.hv-voice=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sfd-hdstx" 
   RETURN "application/vnd.hydrostatix.sof-data=" + cFname + cFext
ELSEIF cFile LIKE ".+\.stk" 
   RETURN "application/hyperstudio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hal" 
   RETURN "application/vnd.hal+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.html" 
   RETURN "text/html=" + cFname + cFext
ELSEIF cFile LIKE ".+\.irm" 
   RETURN "application/vnd.ibm.rights-management=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sc" 
   RETURN "application/vnd.ibm.secure-container=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ics" 
   RETURN "text/calendar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.icc" 
   RETURN "application/vnd.iccprofile=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ico" 
   RETURN "image/x-icon=" + cFname + cFext
ELSEIF cFile LIKE ".+\.igl" 
   RETURN "application/vnd.igloader=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ief" 
   RETURN "image/ief=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ivp" 
   RETURN "application/vnd.immervision-ivp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ivu" 
   RETURN "application/vnd.immervision-ivu=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rif" 
   RETURN "application/reginfo+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.3dml" 
   RETURN "text/vnd.in3d.3dml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.spot" 
   RETURN "text/vnd.in3d.spot=" + cFname + cFext
ELSEIF cFile LIKE ".+\.igs" 
   RETURN "model/iges=" + cFname + cFext
ELSEIF cFile LIKE ".+\.i2g" 
   RETURN "application/vnd.intergeo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdy" 
   RETURN "application/vnd.cinderella=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xpw" 
   RETURN "application/vnd.intercon.formnet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fcs" 
   RETURN "application/vnd.isac.fcs=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ipfix" 
   RETURN "application/ipfix=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cer" 
   RETURN "application/pkix-cert=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pki" 
   RETURN "application/pkixcmp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.crl" 
   RETURN "application/pkix-crl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pkipath " 
   RETURN "application/pkix-pkipath=" + cFname + cFext
ELSEIF cFile LIKE ".+\.igm" 
   RETURN "application/vnd.insors.igm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rcprofile" 
   RETURN "application/vnd.ipunplugged.rcprofile=" + cFname + cFext
ELSEIF cFile LIKE ".+\.irp" 
   RETURN "application/vnd.irepository.package+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jad" 
   RETURN "text/vnd.sun.j2me.app-descriptor=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jar" 
   RETURN "application/java-archive=" + cFname + cFext
ELSEIF cFile LIKE ".+\.class" 
   RETURN "application/java-vm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jnlp" 
   RETURN "application/x-java-jnlp-file=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ser" 
   RETURN "application/java-serialized-object=" + cFname + cFext
ELSEIF cFile LIKE ".+\.java" 
   RETURN "text/x-java-source=" + cFname + cFext
ELSEIF cFile LIKE ".+\.js" 
   RETURN "application/javascript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.json" 
   RETURN "application/json=" + cFname + cFext
ELSEIF cFile LIKE ".+\.joda" 
   RETURN "application/vnd.joost.joda-archive=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jpm" 
   RETURN "video/jpm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jpgv" 
   RETURN "video/jpeg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ktz" 
   RETURN "application/vnd.kahootz=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mmd" 
   RETURN "application/vnd.chipnuts.karaoke-mmd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.karbon" 
   RETURN "application/vnd.kde.karbon=" + cFname + cFext
ELSEIF cFile LIKE ".+\.chrt" 
   RETURN "application/vnd.kde.kchart=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kfo" 
   RETURN "application/vnd.kde.kformula=" + cFname + cFext
ELSEIF cFile LIKE ".+\.flw" 
   RETURN "application/vnd.kde.kivio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kon" 
   RETURN "application/vnd.kde.kontour=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kpr" 
   RETURN "application/vnd.kde.kpresenter=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ksp" 
   RETURN "application/vnd.kde.kspread=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kwd" 
   RETURN "application/vnd.kde.kword=" + cFname + cFext
ELSEIF cFile LIKE ".+\.htke" 
   RETURN "application/vnd.kenameaapp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kia" 
   RETURN "application/vnd.kidspiration=" + cFname + cFext
ELSEIF cFile LIKE ".+\.kne" 
   RETURN "application/vnd.kinar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sse" 
   RETURN "application/vnd.kodak-descriptor=" + cFname + cFext
ELSEIF cFile LIKE ".+\.lasxml" 
   RETURN "application/vnd.las.las+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.latex" 
   RETURN "application/x-latex=" + cFname + cFext
ELSEIF cFile LIKE ".+\.lbd" 
   RETURN "application/vnd.llamagraphics.life-balance.desktop=" + cFname + cFext
ELSEIF cFile LIKE ".+\.lbe" 
   RETURN "application/vnd.llamagraphics.life-balance.exchange+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jam" 
   RETURN "application/vnd.jam=" + cFname + cFext
ELSEIF cFile LIKE ".+\.123" 
   RETURN "application/vnd.lotus-1-2-3=" + cFname + cFext
ELSEIF cFile LIKE ".+\.apr" 
   RETURN "application/vnd.lotus-approach=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pre" 
   RETURN "application/vnd.lotus-freelance=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nsf" 
   RETURN "application/vnd.lotus-notes=" + cFname + cFext
ELSEIF cFile LIKE ".+\.org" 
   RETURN "application/vnd.lotus-organizer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.scm" 
   RETURN "application/vnd.lotus-screencam=" + cFname + cFext
ELSEIF cFile LIKE ".+\.lwp" 
   RETURN "application/vnd.lotus-wordpro=" + cFname + cFext
ELSEIF cFile LIKE ".+\.lvp" 
   RETURN "audio/vnd.lucent.voice=" + cFname + cFext
ELSEIF cFile LIKE ".+\.m3u" 
   RETURN "audio/x-mpegurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.m4v" 
   RETURN "video/x-m4v=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hqx" 
   RETURN "application/mac-binhex40=" + cFname + cFext
ELSEIF cFile LIKE ".+\.portpkg " 
   RETURN "application/vnd.macports.portpkg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mgp" 
   RETURN "application/vnd.osgeo.mapguide.package=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mrc" 
   RETURN "application/marc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mrcx" 
   RETURN "application/marcxml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mxf" 
   RETURN "application/mxf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nbp" 
   RETURN "application/vnd.wolfram.player=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ma" 
   RETURN "application/mathematica=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mathml" 
   RETURN "application/mathml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mbox" 
   RETURN "application/mbox=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mc1" 
   RETURN "application/vnd.medcalcdata=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mscml" 
   RETURN "application/mediaservercontrol+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cdkey" 
   RETURN "application/vnd.mediastation.cdkey=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mwf" 
   RETURN "application/vnd.mfer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mfm" 
   RETURN "application/vnd.mfmp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.msh" 
   RETURN "model/mesh=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mads" 
   RETURN "application/mads+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mets" 
   RETURN "application/mets+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mods" 
   RETURN "application/mods+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.meta4" 
   RETURN "application/metalink4+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.potm" 
   RETURN "application/vnd.ms-powerpoint.template.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.docm" 
   RETURN "application/vnd.ms-word.document.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dotm" 
   RETURN "application/vnd.ms-word.template.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mcd" 
   RETURN "application/vnd.mcd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.flo" 
   RETURN "application/vnd.micrografx.flo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.igx" 
   RETURN "application/vnd.micrografx.igx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.es3" 
   RETURN "application/vnd.eszigno3+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mdb" 
   RETURN "application/x-msaccess=" + cFname + cFext
ELSEIF cFile LIKE ".+\.asf" 
   RETURN "video/x-ms-asf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.exe" 
   RETURN "application/x-msdownload=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cil" 
   RETURN "application/vnd.ms-artgalry=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cab" 
   RETURN "application/vnd.ms-cab-compressed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ims" 
   RETURN "application/vnd.ms-ims=" + cFname + cFext
ELSEIF cFile LIKE ".+\.application" 
   RETURN "application/x-ms-application=" + cFname + cFext
ELSEIF cFile LIKE ".+\.clp" 
   RETURN "application/x-msclip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mdi" 
   RETURN "image/vnd.ms-modi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.eot" 
   RETURN "application/vnd.ms-fontobject=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xls" 
   RETURN "application/vnd.ms-excel=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xlam" 
   RETURN "application/vnd.ms-excel.addin.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xlsb" 
   RETURN "application/vnd.ms-excel.sheet.binary.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xltm" 
   RETURN "application/vnd.ms-excel.template.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xlsm" 
   RETURN "application/vnd.ms-excel.sheet.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.chm" 
   RETURN "application/vnd.ms-htmlhelp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.crd" 
   RETURN "application/x-mscardfile=" + cFname + cFext
ELSEIF cFile LIKE ".+\.lrm" 
   RETURN "application/vnd.ms-lrm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mvb" 
   RETURN "application/x-msmediaview=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mny" 
   RETURN "application/x-msmoney=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pptx" 
   RETURN "application/vnd.openxmlformats-officedocument.presentationml.presentation=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sldx" 
   RETURN "application/vnd.openxmlformats-officedocument.presentationml.slide=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ppsx" 
   RETURN "application/vnd.openxmlformats-officedocument.presentationml.slideshow=" + cFname + cFext
ELSEIF cFile LIKE ".+\.potx" 
   RETURN "application/vnd.openxmlformats-officedocument.presentationml.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xlsx" 
   RETURN "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xltx" 
   RETURN "application/vnd.openxmlformats-officedocument.spreadsheetml.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.docx" 
   RETURN "application/vnd.openxmlformats-officedocument.wordprocessingml.document=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dotx" 
   RETURN "application/vnd.openxmlformats-officedocument.wordprocessingml.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.obd" 
   RETURN "application/x-msbinder=" + cFname + cFext
ELSEIF cFile LIKE ".+\.thmx" 
   RETURN "application/vnd.ms-officetheme=" + cFname + cFext
ELSEIF cFile LIKE ".+\.onetoc" 
   
   RETURN "application/onenote=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pya" 
   RETURN "audio/vnd.ms-playready.media.pya=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pyv" 
   RETURN "video/vnd.ms-playready.media.pyv=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ppt" 
   RETURN "application/vnd.ms-powerpoint=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ppam" 
   RETURN "application/vnd.ms-powerpoint.addin.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sldm" 
   RETURN "application/vnd.ms-powerpoint.slide.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pptm" 
   RETURN "application/vnd.ms-powerpoint.presentation.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ppsm" 
   RETURN "application/vnd.ms-powerpoint.slideshow.macroenabled.12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpp" 
   RETURN "application/vnd.ms-project=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pub" 
   RETURN "application/x-mspublisher=" + cFname + cFext
ELSEIF cFile LIKE ".+\.scd" 
   RETURN "application/x-msschedule=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xap" 
   RETURN "application/x-silverlight-app=" + cFname + cFext
ELSEIF cFile LIKE ".+\.stl" 
   RETURN "application/vnd.ms-pki.stl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.cat" 
   RETURN "application/vnd.ms-pki.seccat=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vsd" 
   RETURN "application/vnd.visio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wm" 
   RETURN "video/x-ms-wm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wma" 
   RETURN "audio/x-ms-wma=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wax" 
   RETURN "audio/x-ms-wax=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmx" 
   RETURN "video/x-ms-wmx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmd" 
   RETURN "application/x-ms-wmd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wpl" 
   RETURN "application/vnd.ms-wpl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmz" 
   RETURN "application/x-ms-wmz=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmv" 
   RETURN "video/x-ms-wmv=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wvx" 
   RETURN "video/x-ms-wvx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmf" 
   RETURN "application/x-msmetafile=" + cFname + cFext
ELSEIF cFile LIKE ".+\.trm" 
   RETURN "application/x-msterminal=" + cFname + cFext
ELSEIF cFile LIKE ".+\.doc" 
   RETURN "application/msword=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wri" 
   RETURN "application/x-mswrite=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wps" 
   RETURN "application/vnd.ms-works=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xbap" 
   RETURN "application/x-ms-xbap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xps" 
   RETURN "application/vnd.ms-xpsdocument=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mid" 
   RETURN "audio/midi=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpy" 
   RETURN "application/vnd.ibm.minipay=" + cFname + cFext
ELSEIF cFile LIKE ".+\.afp" 
   RETURN "application/vnd.ibm.modcap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rms" 
   RETURN "application/vnd.jcp.javame.midlet-rms=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tmo" 
   RETURN "application/vnd.tmobile-livetv=" + cFname + cFext
ELSEIF cFile LIKE ".+\.prc" 
   RETURN "application/x-mobipocket-ebook=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mbk" 
   RETURN "application/vnd.mobius.mbk=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dis" 
   RETURN "application/vnd.mobius.dis=" + cFname + cFext
ELSEIF cFile LIKE ".+\.plc" 
   RETURN "application/vnd.mobius.plc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mqy" 
   RETURN "application/vnd.mobius.mqy=" + cFname + cFext
ELSEIF cFile LIKE ".+\.msl" 
   RETURN "application/vnd.mobius.msl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.txf" 
   RETURN "application/vnd.mobius.txf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.daf" 
   RETURN "application/vnd.mobius.daf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fly" 
   RETURN "text/vnd.fly=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpc" 
   RETURN "application/vnd.mophun.certificate=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpn" 
   RETURN "application/vnd.mophun.application=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mj2" 
   RETURN "video/mj2=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpga" 
   RETURN "audio/mpeg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mxu" 
   RETURN "video/vnd.mpegurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mpeg" 
   RETURN "video/mpeg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.m21" 
   RETURN "application/mp21=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mp4a" 
   RETURN "audio/mp4=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mp4" 
   RETURN "video/mp4=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mp4" 
   RETURN "application/mp4=" + cFname + cFext
ELSEIF cFile LIKE ".+\.m3u8" 
   RETURN "application/vnd.apple.mpegurl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mus" 
   RETURN "application/vnd.musician=" + cFname + cFext
ELSEIF cFile LIKE ".+\.msty" 
   RETURN "application/vnd.muvee.style=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mxml" 
   RETURN "application/xv+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ngdat" 
   RETURN "application/vnd.nokia.n-gage.data=" + cFname + cFext
ELSEIF cFile LIKE ".+\.n-gage" 

   RETURN "application/vnd.nokia.n-gage.symbian.install=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ncx" 
   RETURN "application/x-dtbncx+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nc" 
   RETURN "application/x-netcdf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nlu" 
   RETURN "application/vnd.neurolanguage.nlu=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dna" 
   RETURN "application/vnd.dna=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nnd" 
   RETURN "application/vnd.noblenet-directory=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nns" 
   RETURN "application/vnd.noblenet-sealer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.nnw" 
   RETURN "application/vnd.noblenet-web=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rpst" 
   RETURN "application/vnd.nokia.radio-preset=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rpss" 
   RETURN "application/vnd.nokia.radio-presets=" + cFname + cFext
ELSEIF cFile LIKE ".+\.n3" 
   RETURN "text/n3=" + cFname + cFext
ELSEIF cFile LIKE ".+\.edm" 
   RETURN "application/vnd.novadigm.edm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.edx" 
   RETURN "application/vnd.novadigm.edx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ext" 
   RETURN "application/vnd.novadigm.ext=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gph" 
   RETURN "application/vnd.flographit=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ecelp4800" 
   RETURN "audio/vnd.nuera.ecelp4800=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ecelp7470" 
   RETURN "audio/vnd.nuera.ecelp7470=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ecelp9600" 
   RETURN "audio/vnd.nuera.ecelp9600=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oda" 
   RETURN "application/oda=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ogx" 
   RETURN "application/ogg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oga" 
   RETURN "audio/ogg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ogv" 
   RETURN "video/ogg=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dd2" 
   RETURN "application/vnd.oma.dd2+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oth" 
   RETURN "application/vnd.oasis.opendocument.text-web=" + cFname + cFext
ELSEIF cFile LIKE ".+\.opf" 
   RETURN "application/oebps-package+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.qbo" 
   RETURN "application/vnd.intu.qbo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oxt" 
   RETURN "application/vnd.openofficeorg.extension=" + cFname + cFext
ELSEIF cFile LIKE ".+\.osf" 
   RETURN "application/vnd.yamaha.openscoreformat=" + cFname + cFext
ELSEIF cFile LIKE ".+\.weba" 
   RETURN "audio/webm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.webm" 
   RETURN "video/webm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odc" 
   RETURN "application/vnd.oasis.opendocument.chart=" + cFname + cFext
ELSEIF cFile LIKE ".+\.otc" 
   RETURN "application/vnd.oasis.opendocument.chart-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odb" 
   RETURN "application/vnd.oasis.opendocument.database=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odf" 
   RETURN "application/vnd.oasis.opendocument.formula=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odft" 
   RETURN "application/vnd.oasis.opendocument.formula-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odg" 
   RETURN "application/vnd.oasis.opendocument.graphics=" + cFname + cFext
ELSEIF cFile LIKE ".+\.otg" 
   RETURN "application/vnd.oasis.opendocument.graphics-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odi" 
   RETURN "application/vnd.oasis.opendocument.image=" + cFname + cFext
ELSEIF cFile LIKE ".+\.oti" 
   RETURN "application/vnd.oasis.opendocument.image-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odp" 
   RETURN "application/vnd.oasis.opendocument.presentation=" + cFname + cFext
ELSEIF cFile LIKE ".+\.otp" 
   RETURN "application/vnd.oasis.opendocument.presentation-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ods" 
   RETURN "application/vnd.oasis.opendocument.spreadsheet=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ots" 
   RETURN "application/vnd.oasis.opendocument.spreadsheet-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odt" 
   RETURN "application/vnd.oasis.opendocument.text=" + cFname + cFext
ELSEIF cFile LIKE ".+\.odm" 
   RETURN "application/vnd.oasis.opendocument.text-master=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ott" 
   RETURN "application/vnd.oasis.opendocument.text-template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ktx" 
   RETURN "image/ktx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sxc" 
   RETURN "application/vnd.sun.xml.calc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.stc" 
   RETURN "application/vnd.sun.xml.calc.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sxd" 
   RETURN "application/vnd.sun.xml.draw=" + cFname + cFext
ELSEIF cFile LIKE ".+\.std" 
   RETURN "application/vnd.sun.xml.draw.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sxi" 
   RETURN "application/vnd.sun.xml.impress=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sti" 
   RETURN "application/vnd.sun.xml.impress.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sxm" 
   RETURN "application/vnd.sun.xml.math=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sxw" 
   RETURN "application/vnd.sun.xml.writer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sxg" 
   RETURN "application/vnd.sun.xml.writer.global=" + cFname + cFext
ELSEIF cFile LIKE ".+\.stw" 
   RETURN "application/vnd.sun.xml.writer.template=" + cFname + cFext
ELSEIF cFile LIKE ".+\.otf" 
   RETURN "application/x-font-otf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.osfpvg" 
   RETURN "application/vnd.yamaha.openscoreformat.osfpvg+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dp" 
   RETURN "application/vnd.osgi.dp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pdb" 
   RETURN "application/vnd.palm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p" 
   RETURN "text/x-pascal=" + cFname + cFext
ELSEIF cFile LIKE ".+\.paw" 
   RETURN "application/vnd.pawaafile=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pclxl" 
   RETURN "application/vnd.hp-pclxl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.efif" 
   RETURN "application/vnd.picsel=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pcx" 
   RETURN "image/x-pcx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.psd" 
   RETURN "image/vnd.adobe.photoshop=" + cFname + cFext
ELSEIF cFile LIKE ".+\.prf" 
   RETURN "application/pics-rules=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pic" 
   RETURN "image/x-pict=" + cFname + cFext
ELSEIF cFile LIKE ".+\.chat" 
   RETURN "application/x-chat=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p10" 
   RETURN "application/pkcs10=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p12" 
   RETURN "application/x-pkcs12=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p7m" 
   RETURN "application/pkcs7-mime=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p7s" 
   RETURN "application/pkcs7-signature=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p7r" 
   RETURN "application/x-pkcs7-certreqresp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p7b" 
   RETURN "application/x-pkcs7-certificates=" + cFname + cFext
ELSEIF cFile LIKE ".+\.p8" 
   RETURN "application/pkcs8=" + cFname + cFext
ELSEIF cFile LIKE ".+\.plf" 
   RETURN "application/vnd.pocketlearn=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pnm" 
   RETURN "image/x-portable-anymap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pbm" 
   RETURN "image/x-portable-bitmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pcf" 
   RETURN "application/x-font-pcf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pfr" 
   RETURN "application/font-tdpfr=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pgn" 
   RETURN "application/x-chess-pgn=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pgm" 
   RETURN "image/x-portable-graymap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.png" 
   RETURN "image/png=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ppm" 
   RETURN "image/x-portable-pixmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pskcxml" 
   RETURN "application/pskc+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pml" 
   RETURN "application/vnd.ctc-posml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ai" 
   RETURN "application/postscript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pfa" 
   RETURN "application/x-font-type1=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pbd" 
   RETURN "application/vnd.powerbuilder6=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pgp" 
   RETURN "application/pgp-signature=" + cFname + cFext
ELSEIF cFile LIKE ".+\.box" 
   RETURN "application/vnd.previewsystems.box=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ptid" 
   RETURN "application/vnd.pvi.ptid1=" + cFname + cFext
ELSEIF cFile LIKE ".+\.pls" 
   RETURN "application/pls+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.str" 
   RETURN "application/vnd.pg.format=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ei6" 
   RETURN "application/vnd.pg.osasli=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dsc" 
   RETURN "text/prs.lines.tag=" + cFname + cFext
ELSEIF cFile LIKE ".+\.psf" 
   RETURN "application/x-font-linux-psf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.qps" 
   RETURN "application/vnd.publishare-delta-tree=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wg" 
   RETURN "application/vnd.pmi.widget=" + cFname + cFext
ELSEIF cFile LIKE ".+\.qxd" 
   RETURN "application/vnd.quark.quarkxpress=" + cFname + cFext
ELSEIF cFile LIKE ".+\.esf" 
   RETURN "application/vnd.epson.esf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.msf" 
   RETURN "application/vnd.epson.msf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ssf" 
   RETURN "application/vnd.epson.ssf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.qam" 
   RETURN "application/vnd.epson.quickanime=" + cFname + cFext
ELSEIF cFile LIKE ".+\.qfx" 
   RETURN "application/vnd.intu.qfx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.qt" 
   RETURN "video/quicktime=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rar" 
   RETURN "application/x-rar-compressed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ram" 
   RETURN "audio/x-pn-realaudio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rmp" 
   RETURN "audio/x-pn-realaudio-plugin=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rsd" 
   RETURN "application/rsd+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rm" 
   RETURN "application/vnd.rn-realmedia=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bed" 
   RETURN "application/vnd.realvnc.bed=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mxl" 
   RETURN "application/vnd.recordare.musicxml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.musicxml" 
   RETURN "application/vnd.recordare.musicxml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rnc" 
   RETURN "application/relax-ng-compact-syntax=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rdz" 
   RETURN "application/vnd.data-vision.rdz=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rdf" 
   RETURN "application/rdf+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rp9" 
   RETURN "application/vnd.cloanto.rp9=" + cFname + cFext
ELSEIF cFile LIKE ".+\.jisp" 
   RETURN "application/vnd.jisp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rtf" 
   RETURN "application/rtf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rtx" 
   RETURN "text/richtext=" + cFname + cFext
ELSEIF cFile LIKE ".+\.link66" 
   RETURN "application/vnd.route66.link66+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rss" 
   RETURN "application/rss+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.shf" 
   RETURN "application/shf+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.st" 
   RETURN "application/vnd.sailingtracker.track=" + cFname + cFext
ELSEIF cFile LIKE ".+\.svg" 
   RETURN "image/svg+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sus" 
   RETURN "application/vnd.sus-calendar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sru" 
   RETURN "application/sru+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.setpay" 
   RETURN "application/set-payment-initiation=" + cFname + cFext
ELSEIF cFile LIKE ".+\.setreg" 
   RETURN "application/set-registration-initiation=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sema" 
   RETURN "application/vnd.sema=" + cFname + cFext
ELSEIF cFile LIKE ".+\.semd" 
   RETURN "application/vnd.semd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.semf" 
   RETURN "application/vnd.semf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.see" 
   RETURN "application/vnd.seemail=" + cFname + cFext
ELSEIF cFile LIKE ".+\.snf" 
   RETURN "application/x-font-snf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.spq" 
   RETURN "application/scvp-vp-request=" + cFname + cFext
ELSEIF cFile LIKE ".+\.spp" 
   RETURN "application/scvp-vp-response=" + cFname + cFext
ELSEIF cFile LIKE ".+\.scq" 
   RETURN "application/scvp-cv-request=" + cFname + cFext
ELSEIF cFile LIKE ".+\.scs" 
   RETURN "application/scvp-cv-response=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sdp" 
   RETURN "application/sdp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.etx" 
   RETURN "text/x-setext=" + cFname + cFext
ELSEIF cFile LIKE ".+\.movie" 
   RETURN "video/x-sgi-movie=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ifm" 
   RETURN "application/vnd.shana.informed.formdata=" + cFname + cFext
ELSEIF cFile LIKE ".+\.itp" 
   RETURN "application/vnd.shana.informed.formtemplate=" + cFname + cFext
ELSEIF cFile LIKE ".+\.iif" 
   RETURN "application/vnd.shana.informed.interchange=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ipk" 
   RETURN "application/vnd.shana.informed.package=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tfi" 
   RETURN "application/thraud+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.shar" 
   RETURN "application/x-shar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rgb" 
   RETURN "image/x-rgb=" + cFname + cFext
ELSEIF cFile LIKE ".+\.slt" 
   RETURN "application/vnd.epson.salt=" + cFname + cFext
ELSEIF cFile LIKE ".+\.aso" 
   RETURN "application/vnd.accpac.simply.aso=" + cFname + cFext
ELSEIF cFile LIKE ".+\.imp" 
   RETURN "application/vnd.accpac.simply.imp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.twd" 
   RETURN "application/vnd.simtech-mindmapper=" + cFname + cFext
ELSEIF cFile LIKE ".+\.csp" 
   RETURN "application/vnd.commonspace=" + cFname + cFext
ELSEIF cFile LIKE ".+\.saf" 
   RETURN "application/vnd.yamaha.smaf-audio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mmf" 
   RETURN "application/vnd.smaf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.spf" 
   RETURN "application/vnd.yamaha.smaf-phrase=" + cFname + cFext
ELSEIF cFile LIKE ".+\.teacher" 
   RETURN "application/vnd.smart.teacher=" + cFname + cFext
ELSEIF cFile LIKE ".+\.svd" 
   RETURN "application/vnd.svd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rq" 
   RETURN "application/sparql-query=" + cFname + cFext
ELSEIF cFile LIKE ".+\.srx" 
   RETURN "application/sparql-results+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.gram" 
   RETURN "application/srgs=" + cFname + cFext
ELSEIF cFile LIKE ".+\.grxml" 
   RETURN "application/srgs+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ssml" 
   RETURN "application/ssml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.skp" 
   RETURN "application/vnd.koan=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sgml" 
   RETURN "text/sgml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sdc" 
   RETURN "application/vnd.stardivision.calc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sda" 
   RETURN "application/vnd.stardivision.draw=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sdd" 
   RETURN "application/vnd.stardivision.impress=" + cFname + cFext
ELSEIF cFile LIKE ".+\.smf" 
   RETURN "application/vnd.stardivision.math=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sdw" 
   RETURN "application/vnd.stardivision.writer=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sgl" 
   RETURN "application/vnd.stardivision.writer-global=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sm" 
   RETURN "application/vnd.stepmania.stepchart=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sit" 
   RETURN "application/x-stuffit=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sitx" 
   RETURN "application/x-stuffitx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sdkm" 
   RETURN "application/vnd.solent.sdkm+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xo" 
   RETURN "application/vnd.olpc-sugar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.au" 
   RETURN "audio/basic=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wqd" 
   RETURN "application/vnd.wqd=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sis" 
   RETURN "application/vnd.symbian.install=" + cFname + cFext
ELSEIF cFile LIKE ".+\.smi" 
   RETURN "application/smil+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xsm" 
   RETURN "application/vnd.syncml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.bdm" 
   RETURN "application/vnd.syncml.dm+wbxml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xdm" 
   RETURN "application/vnd.syncml.dm+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sv4cpio" 
   RETURN "application/x-sv4cpio=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sv4crc" 
   RETURN "application/x-sv4crc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sbml" 
   RETURN "application/sbml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tsv" 
   RETURN "text/tab-separated-values=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tiff" 
   RETURN "image/tiff=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tao" 
   RETURN "application/vnd.tao.intent-module-archive=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tar" 
   RETURN "application/x-tar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tcl" 
   RETURN "application/x-tcl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tex" 
   RETURN "application/x-tex=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tfm" 
   RETURN "application/x-tex-tfm=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tei" 
   RETURN "application/tei+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.txt" 
   RETURN "text/plain=" + cFname + cFext
ELSEIF cFile LIKE ".+\.dxp" 
   RETURN "application/vnd.spotfire.dxp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.sfs" 
   RETURN "application/vnd.spotfire.sfs=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tsd" 
   RETURN "application/timestamped-data=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tpt" 
   RETURN "application/vnd.trid.tpt=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mxs" 
   RETURN "application/vnd.triscape.mxs=" + cFname + cFext
ELSEIF cFile LIKE ".+\.t" 
   RETURN "text/troff=" + cFname + cFext
ELSEIF cFile LIKE ".+\.tra" 
   RETURN "application/vnd.trueapp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ttf" 
   RETURN "application/x-font-ttf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ttl" 
   RETURN "text/turtle=" + cFname + cFext
ELSEIF cFile LIKE ".+\.umj" 
   RETURN "application/vnd.umajin=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uoml" 
   RETURN "application/vnd.uoml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.unityweb" 
   RETURN "application/vnd.unity=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ufd" 
   RETURN "application/vnd.ufdl=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uri" 
   RETURN "text/uri-list=" + cFname + cFext
ELSEIF cFile LIKE ".+\.utz" 
   RETURN "application/vnd.uiq.theme=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ustar" 
   RETURN "application/x-ustar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.uu" 
   RETURN "text/x-uuencode=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vcs" 
   RETURN "text/x-vcalendar=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vcf" 
   RETURN "text/x-vcard=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vcd" 
   RETURN "application/x-cdlink=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vsf" 
   RETURN "application/vnd.vsf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wrl" 
   RETURN "model/vrml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vcx" 
   RETURN "application/vnd.vcx=" + cFname + cFext
ELSEIF cFile LIKE ".+\.mts" 
   RETURN "model/vnd.mts=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vtu" 
   RETURN "model/vnd.vtu=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vis" 
   RETURN "application/vnd.visionary=" + cFname + cFext
ELSEIF cFile LIKE ".+\.viv" 
   RETURN "video/vnd.vivo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.ccxml" 
   RETURN "application/ccxml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.vxml" 
   RETURN "application/voicexml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.src" 
   RETURN "application/x-wais-source=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wbxml" 
   RETURN "application/vnd.wap.wbxml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wbmp" 
   RETURN "image/vnd.wap.wbmp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wav" 
   RETURN "audio/x-wav=" + cFname + cFext
ELSEIF cFile LIKE ".+\.davmount" 
   RETURN "application/davmount+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.woff" 
   RETURN "application/x-font-woff=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wspolicy" 
   RETURN "application/wspolicy+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.webp" 
   RETURN "image/webp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wtb" 
   RETURN "application/vnd.webturbo=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wgt" 
   RETURN "application/widget=" + cFname + cFext
ELSEIF cFile LIKE ".+\.hlp" 
   RETURN "application/winhlp=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wml" 
   RETURN "text/vnd.wap.wml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmls" 
   RETURN "text/vnd.wap.wmlscript=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wmlsc" 
   RETURN "application/vnd.wap.wmlscriptc=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wpd" 
   RETURN "application/vnd.wordperfect=" + cFname + cFext
ELSEIF cFile LIKE ".+\.stf" 
   RETURN "application/vnd.wt.stf=" + cFname + cFext
ELSEIF cFile LIKE ".+\.wsdl" 
   RETURN "application/wsdl+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xbm" 
   RETURN "image/x-xbitmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xpm" 
   RETURN "image/x-xpixmap=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xwd" 
   RETURN "image/x-xwindowdump=" + cFname + cFext
ELSEIF cFile LIKE ".+\.der" 
   RETURN "application/x-x509-ca-cert=" + cFname + cFext
ELSEIF cFile LIKE ".+\.fig" 
   RETURN "application/x-xfig=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xhtml" 
   RETURN "application/xhtml+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xml" 
   RETURN "application/xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xdf" 
   RETURN "application/xcap-diff+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xenc" 
   RETURN "application/xenc+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xer" 
   RETURN "application/patch-ops-error+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rl" 
   RETURN "application/resource-lists+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rs" 
   RETURN "application/rls-services+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.rld" 
   RETURN "application/resource-lists-diff+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xslt" 
   RETURN "application/xslt+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xop" 
   RETURN "application/xop+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xpi" 
   RETURN "application/x-xpinstall=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xspf" 
   RETURN "application/xspf+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xul" 
   RETURN "application/vnd.mozilla.xul+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.xyz" 
   RETURN "chemical/x-xyz=" + cFname + cFext
ELSEIF cFile LIKE ".+\.yang" 
   RETURN "application/yang=" + cFname + cFext
ELSEIF cFile LIKE ".+\.yin" 
   RETURN "application/yin+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.zir" 
   RETURN "application/vnd.zul=" + cFname + cFext
ELSEIF cFile LIKE ".+\.zip" 
   RETURN "application/zip=" + cFname + cFext
ELSEIF cFile LIKE ".+\.zmm" 
   RETURN "application/vnd.handheld-entertainment+xml=" + cFname + cFext
ELSEIF cFile LIKE ".+\.zaz" 
   RETURN "application/vnd.zzazz.deck+xml=" + cFname + cFext
   ENDIF

RETURN "text/plain;filename=" + cFname + cFext