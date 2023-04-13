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


FUNCTION HB_SendMail( cServer, nPort, cFrom, aTo, aCC, aBCC, cBody, cSubject, aFiles, cUser, cPass, cPopServer, nPriority, lRead, lTrace, lPopAuth, lNoAuth, nTimeOut, cReplyTo, lSSL, CaFile, CAPath,lTls)
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
   DEFAULT lTls        TO .F.
   
   IF HB_ISSTRING( aFiles )
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
   IF HB_ISARRAY( aTo )
      IF Len( aTo ) > 1
         FOR EACH cTo IN aTo
            IF HB_EnumIndex() != 1 .AND. !Empty( cTo )
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
   IF HB_ISARRAY( aCC )
      IF Len(aCC) >0
         FOR EACH cTmp IN aCC
            IF !Empty( cTmp )
               cCC += cTmp + ","
            ENDIF
         NEXT
         cCC := Substr( cCC, 1, Len( cCC ) - 1 )
      ENDIF
   ELSEIF HB_ISSTRING( aCC )
      cCC := Alltrim( aCC )
   ENDIF


   // BCC (Blind Carbon Copy)
   IF HB_ISARRAY( aBCC )
      IF Len(aBCC)>0
         FOR EACH cTmp IN aBCC
            IF !Empty( cTmp )
               cBCC += cTmp + ","
            ENDIF
         NEXT
         cBCC := Substr( cBCC, 1, Len( cBCC ) - 1 )
      ENDIF
   ELSEIF HB_ISSTRING( aBCC )
      cBCC := Alltrim( aBCC )
   ENDIF

   IF cPopServer != NIL .AND. lPopAuth
      TRY
         oUrl1 := tUrl():New( "pop://" + cUser + ":" + cPass + "@" + cPopServer + "/" )
         oUrl1:cUserid := Strtran( cUser, "&at;", "@" )
         opop:= tIPClientPOP():New( oUrl1, lTrace, , CaFile, CAPath  )
         if !oPop:lSSL  .and. lSSL
//            oPop:lSSL := .T.
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

      IF oInMail:Opensecure(,lSSl)

         IF oInmail:lAuthLogin
            IF !oInMail:Auth( StrTran( cUser, "&at;", "@" ), cPass )
               lConnect := .F.
            ELSE
               lConnectPlain := .T.
            ENDIF
         ENDIF

         IF oInmail:lAuthPlain .AND. !lConnect
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
         IF oInMail:cReply == NIL  .or. Left( oInMail:cReply, 3 ) == '250'  // culik HELO return only 250 from server 
            EXIT
         ENDIF
      ENDDO

   ENDIF

   oInMail:oUrl:cUserid := cFrom
   oMail:hHeaders[ "To" ]      := cTo
   oMail:hHeaders[ "Subject" ] := cSubject

   FOR EACH aThisFile IN AFiles

      IF HB_ISSTRING( aThisFile )
         cFile := aThisFile
         Memoread( cFile )
      ELSEIF HB_ISARRAY( aThisFile ) .AND. Len( aThisFile ) >= 2
         cFile := aThisFile[ 1 ]
      ELSE
         EXIT
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


FUNCTION HB_SetMimeType( cFile)

   cFile := Lower( cFile )

   IF cFile LIKE ".+\.123" ; RETURN "application/vnd.lotus-1-2-3"
   ELSEIF cFile LIKE ".+\.3dml" ; RETURN "text/vnd.in3d.3dml"
   ELSEIF cFile LIKE ".+\.3g2" ; RETURN "video/3gpp2"
   ELSEIF cFile LIKE ".+\.3gp" ; RETURN "video/3gpp"
   ELSEIF cFile LIKE ".+\.(a|bin|bpk|deploy|dist|distz|dmg|dms|dump|elc|iso|lha|lrf|lzh|o|obj|pkg|so)" ; RETURN "application/octet-stream"
   ELSEIF cFile LIKE ".+\.(aab|u32|vox|x32)" ; RETURN "application/x-authorware-bin"
   ELSEIF cFile LIKE ".+\.aac" ; RETURN "audio/x-aac"
   ELSEIF cFile LIKE ".+\.aam" ; RETURN "application/x-authorware-map"
   ELSEIF cFile LIKE ".+\.aas" ; RETURN "application/x-authorware-seg"
   ELSEIF cFile LIKE ".+\.abw" ; RETURN "application/x-abiword"
   ELSEIF cFile LIKE ".+\.acc" ; RETURN "application/vnd.americandynamics.acc"
   ELSEIF cFile LIKE ".+\.ace" ; RETURN "application/x-ace-compressed"
   ELSEIF cFile LIKE ".+\.acu" ; RETURN "application/vnd.acucobol"
   ELSEIF cFile LIKE ".+\.(acutc|atc)" ; RETURN "application/vnd.acucorp"   
   ELSEIF cFile LIKE ".+\.adp" ; RETURN "audio/adpcm"
   ELSEIF cFile LIKE ".+\.aep" ; RETURN "application/vnd.audiograph"
   ELSEIF cFile LIKE ".+\.(afm|pfa|pfb|pfm)" ; RETURN "application/x-font-type1"
   ELSEIF cFile LIKE ".+\.(afp|list3820|listafp)" ; RETURN "application/vnd.ibm.modcap"
   ELSEIF cFile LIKE ".+\.(ai|eps|ps)" ; RETURN "application/postscript"
   ELSEIF cFile LIKE ".+\.(aif|aifc|aiff)" ; RETURN "audio/x-aiff"
   ELSEIF cFile LIKE ".+\.air" ; RETURN "application/vnd.adobe.air-application-installer-package+zip"
   ELSEIF cFile LIKE ".+\.ami" ; RETURN "application/vnd.amiga.ami"
   ELSEIF cFile LIKE ".+\.apk" ; RETURN "application/vnd.android.package-archive"
   ELSEIF cFile LIKE ".+\.application" ; RETURN "application/x-ms-application"
   ELSEIF cFile LIKE ".+\.apr" ; RETURN "application/vnd.lotus-approach"
   ELSEIF cFile LIKE ".+\.(asc|sig)" ; RETURN "application/pgp-signature"
   ELSEIF cFile LIKE ".+\.(asf|asx)" ; RETURN "video/x-ms-asf"
   ELSEIF cFile LIKE ".+\.(asm|s)" ; RETURN "text/x-asm"
   ELSEIF cFile LIKE ".+\.aso" ; RETURN "application/vnd.accpac.simply.aso"
   ELSEIF cFile LIKE ".+\.atom" ; RETURN "application/atom+xml"
   ELSEIF cFile LIKE ".+\.atomcat" ; RETURN "application/atomcat+xml"
   ELSEIF cFile LIKE ".+\.atomsvc" ; RETURN "application/atomsvc+xml"
   ELSEIF cFile LIKE ".+\.atx" ; RETURN "application/vnd.antix.game-component"
   ELSEIF cFile LIKE ".+\.(au|snd)" ; RETURN "audio/basic"
   ELSEIF cFile LIKE ".+\.avi" ; RETURN "video/x-msvideo"
   ELSEIF cFile LIKE ".+\.aw" ; RETURN "application/applixware"
   ELSEIF cFile LIKE ".+\.azf" ; RETURN "application/vnd.airzip.filesecure.azf"
   ELSEIF cFile LIKE ".+\.azs" ; RETURN "application/vnd.airzip.filesecure.azs"
   ELSEIF cFile LIKE ".+\.azw" ; RETURN "application/vnd.amazon.ebook"
   ELSEIF cFile LIKE ".+\.(bat|com|dll|exe|msi)" ; RETURN "application/x-msdownload"
   ELSEIF cFile LIKE ".+\.bcpio" ; RETURN "application/x-bcpio"
   ELSEIF cFile LIKE ".+\.bdf" ; RETURN "application/x-font-bdf"
   ELSEIF cFile LIKE ".+\.bdm" ; RETURN "application/vnd.syncml.dm+wbxml"
   ELSEIF cFile LIKE ".+\.bh2" ; RETURN "application/vnd.fujitsu.oasysprs"
   ELSEIF cFile LIKE ".+\.bmi" ; RETURN "application/vnd.bmi"
   ELSEIF cFile LIKE ".+\.bmp" ; RETURN "image/bmp"
   ELSEIF cFile LIKE ".+\.(book|fm|frame|maker)" ; RETURN "application/vnd.framemaker"
   ELSEIF cFile LIKE ".+\.box" ; RETURN "application/vnd.previewsystems.box"
   ELSEIF cFile LIKE ".+\.(boz|bz2)" ; RETURN "application/x-bzip2"
   ELSEIF cFile LIKE ".+\.btif" ; RETURN "image/prs.btif"
   ELSEIF cFile LIKE ".+\.bz" ; RETURN "application/x-bzip"
   ELSEIF cFile LIKE ".+\.(c|cc|cpp|cxx|dic|h|hh)" ; RETURN "text/x-c"
   ELSEIF cFile LIKE ".+\.(c4d|c4f|c4g|c4p|c4u)" ; RETURN "application/vnd.clonk.c4group"
   ELSEIF cFile LIKE ".+\.cab" ; RETURN "application/vnd.ms-cab-compressed"
   ELSEIF cFile LIKE ".+\.car" ; RETURN "application/vnd.curl.car"
   ELSEIF cFile LIKE ".+\.cat" ; RETURN "application/vnd.ms-pki.seccat"
   ELSEIF cFile LIKE ".+\.(cct|cst|cxt|dcr|dir|dxr|fgd|swa|w3d)" ; RETURN "application/x-director"
   ELSEIF cFile LIKE ".+\.ccxml" ; RETURN "application/ccxml+xml"
   ELSEIF cFile LIKE ".+\.cdbcmsg" ; RETURN "application/vnd.contact.cmsg"
   ELSEIF cFile LIKE ".+\.(cdf|nc)" ; RETURN "application/x-netcdf"
   ELSEIF cFile LIKE ".+\.cdkey" ; RETURN "application/vnd.mediastation.cdkey"
   ELSEIF cFile LIKE ".+\.cdx" ; RETURN "chemical/x-cdx"
   ELSEIF cFile LIKE ".+\.cdxml" ; RETURN "application/vnd.chemdraw+xml"
   ELSEIF cFile LIKE ".+\.cdy" ; RETURN "application/vnd.cinderella"
   ELSEIF cFile LIKE ".+\.cer" ; RETURN "application/pkix-cert"
   ELSEIF cFile LIKE ".+\.cgm" ; RETURN "image/cgm"
   ELSEIF cFile LIKE ".+\.chat" ; RETURN "application/x-chat"
   ELSEIF cFile LIKE ".+\.chm" ; RETURN "application/vnd.ms-htmlhelp"
   ELSEIF cFile LIKE ".+\.chrt" ; RETURN "application/vnd.kde.kchart"
   ELSEIF cFile LIKE ".+\.cif" ; RETURN "chemical/x-cif"
   ELSEIF cFile LIKE ".+\.cii" ; RETURN "application/vnd.anser-web-certificate-issue-initiation"
   ELSEIF cFile LIKE ".+\.cil" ; RETURN "application/vnd.ms-artgalry"
   ELSEIF cFile LIKE ".+\.cla" ; RETURN "application/vnd.claymore"
   ELSEIF cFile LIKE ".+\.class" ; RETURN "application/java-vm"
   ELSEIF cFile LIKE ".+\.clkk" ; RETURN "application/vnd.crick.clicker.keyboard"
   ELSEIF cFile LIKE ".+\.clkp" ; RETURN "application/vnd.crick.clicker.palette"
   ELSEIF cFile LIKE ".+\.clkt" ; RETURN "application/vnd.crick.clicker.template"
   ELSEIF cFile LIKE ".+\.clkw" ; RETURN "application/vnd.crick.clicker.wordbank"
   ELSEIF cFile LIKE ".+\.clkx" ; RETURN "application/vnd.crick.clicker"
   ELSEIF cFile LIKE ".+\.clp" ; RETURN "application/x-msclip"
   ELSEIF cFile LIKE ".+\.cmc" ; RETURN "application/vnd.cosmocaller"
   ELSEIF cFile LIKE ".+\.cmdf" ; RETURN "chemical/x-cmdf"
   ELSEIF cFile LIKE ".+\.cml" ; RETURN "chemical/x-cml"
   ELSEIF cFile LIKE ".+\.cmp" ; RETURN "application/vnd.yellowriver-custom-menu"
   ELSEIF cFile LIKE ".+\.cmx" ; RETURN "image/x-cmx"
   ELSEIF cFile LIKE ".+\.cod" ; RETURN "application/vnd.rim.cod"
   ELSEIF cFile LIKE ".+\.(conf|def|diff|in|ksh|list|log|pl|text|txt)" ; RETURN "text/plain"
   ELSEIF cFile LIKE ".+\.cpio" ; RETURN "application/x-cpio"
   ELSEIF cFile LIKE ".+\.cpt" ; RETURN "application/mac-compactpro"
   ELSEIF cFile LIKE ".+\.crd" ; RETURN "application/x-mscardfile"
   ELSEIF cFile LIKE ".+\.crl" ; RETURN "application/pkix-crl"
   ELSEIF cFile LIKE ".+\.(crt|der)" ; RETURN "application/x-x509-ca-cert"
   ELSEIF cFile LIKE ".+\.csh" ; RETURN "application/x-csh"
   ELSEIF cFile LIKE ".+\.csml" ; RETURN "chemical/x-csml"
   ELSEIF cFile LIKE ".+\.csp" ; RETURN "application/vnd.commonspace"
   ELSEIF cFile LIKE ".+\.css" ; RETURN "text/css"
   ELSEIF cFile LIKE ".+\.csv" ; RETURN "text/csv"
   ELSEIF cFile LIKE ".+\.cu" ; RETURN "application/cu-seeme"
   ELSEIF cFile LIKE ".+\.curl" ; RETURN "text/vnd.curl"
   ELSEIF cFile LIKE ".+\.cww" ; RETURN "application/prs.cww"
   ELSEIF cFile LIKE ".+\.daf" ; RETURN "application/vnd.mobius.daf"
   ELSEIF cFile LIKE ".+\.(dataless|seed)" ; RETURN "application/vnd.fdsn.seed"
   ELSEIF cFile LIKE ".+\.davmount" ; RETURN "application/davmount+xml"
   ELSEIF cFile LIKE ".+\.dcurl" ; RETURN "text/vnd.curl.dcurl"
   ELSEIF cFile LIKE ".+\.dd2" ; RETURN "application/vnd.oma.dd2+xml"
   ELSEIF cFile LIKE ".+\.ddd" ; RETURN "application/vnd.fujixerox.ddd"
   ELSEIF cFile LIKE ".+\.(deb|udeb)" ; RETURN "application/x-debian-package"
   ELSEIF cFile LIKE ".+\.dfac" ; RETURN "application/vnd.dreamfactory"
   ELSEIF cFile LIKE ".+\.dis" ; RETURN "application/vnd.mobius.dis"
   ELSEIF cFile LIKE ".+\.(djv|djvu)" ; RETURN "image/vnd.djvu"
   ELSEIF cFile LIKE ".+\.dna" ; RETURN "application/vnd.dna"
   ELSEIF cFile LIKE ".+\.(doc|dot|wiz)" ; RETURN "application/msword"
   ELSEIF cFile LIKE ".+\.docm" ; RETURN "application/vnd.ms-word.document.macroenabled.12"
   ELSEIF cFile LIKE ".+\.docx" ; RETURN "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
   ELSEIF cFile LIKE ".+\.dotm" ; RETURN "application/vnd.ms-word.template.macroenabled.12"
   ELSEIF cFile LIKE ".+\.dotx" ; RETURN "application/vnd.openxmlformats-officedocument.wordprocessingml.template"
   ELSEIF cFile LIKE ".+\.dp" ; RETURN "application/vnd.osgi.dp"
   ELSEIF cFile LIKE ".+\.dpg" ; RETURN "application/vnd.dpgraph"
   ELSEIF cFile LIKE ".+\.dsc" ; RETURN "text/prs.lines.tag"
   ELSEIF cFile LIKE ".+\.dtb" ; RETURN "application/x-dtbook+xml"
   ELSEIF cFile LIKE ".+\.dtd" ; RETURN "application/xml-dtd"
   ELSEIF cFile LIKE ".+\.dts" ; RETURN "audio/vnd.dts"
   ELSEIF cFile LIKE ".+\.dtshd" ; RETURN "audio/vnd.dts.hd"
   ELSEIF cFile LIKE ".+\.dvi" ; RETURN "application/x-dvi"
   ELSEIF cFile LIKE ".+\.dwf" ; RETURN "model/vnd.dwf"
   ELSEIF cFile LIKE ".+\.dwg" ; RETURN "image/vnd.dwg"
   ELSEIF cFile LIKE ".+\.dxf" ; RETURN "image/vnd.dxf"
   ELSEIF cFile LIKE ".+\.dxp" ; RETURN "application/vnd.spotfire.dxp"
   ELSEIF cFile LIKE ".+\.ecelp4800" ; RETURN "audio/vnd.nuera.ecelp4800"
   ELSEIF cFile LIKE ".+\.ecelp7470" ; RETURN "audio/vnd.nuera.ecelp7470"
   ELSEIF cFile LIKE ".+\.ecelp9600" ; RETURN "audio/vnd.nuera.ecelp9600"
   ELSEIF cFile LIKE ".+\.ecma" ; RETURN "application/ecmascript"
   ELSEIF cFile LIKE ".+\.edm" ; RETURN "application/vnd.novadigm.edm"
   ELSEIF cFile LIKE ".+\.edx" ; RETURN "application/vnd.novadigm.edx"
   ELSEIF cFile LIKE ".+\.efif" ; RETURN "application/vnd.picsel"
   ELSEIF cFile LIKE ".+\.ei6" ; RETURN "application/vnd.pg.osasli"
   ELSEIF cFile LIKE ".+\.(eml|mht|mhtml|mime|nws)" ; RETURN "message/rfc822"
   ELSEIF cFile LIKE ".+\.emma" ; RETURN "application/emma+xml"
   ELSEIF cFile LIKE ".+\.eol" ; RETURN "audio/vnd.digital-winds"
   ELSEIF cFile LIKE ".+\.eot" ; RETURN "application/vnd.ms-fontobject"
   ELSEIF cFile LIKE ".+\.epub" ; RETURN "application/epub+zip"
   ELSEIF cFile LIKE ".+\.(es3|et3)" ; RETURN "application/vnd.eszigno3+xml"
   ELSEIF cFile LIKE ".+\.esf" ; RETURN "application/vnd.epson.esf"
   ELSEIF cFile LIKE ".+\.etx" ; RETURN "text/x-setext"
   ELSEIF cFile LIKE ".+\.ext" ; RETURN "application/vnd.novadigm.ext"
   ELSEIF cFile LIKE ".+\.ez" ; RETURN "application/andrew-inset"
   ELSEIF cFile LIKE ".+\.ez2" ; RETURN "application/vnd.ezpix-album"
   ELSEIF cFile LIKE ".+\.ez3" ; RETURN "application/vnd.ezpix-package"
   ELSEIF cFile LIKE ".+\.(f|f77|f90|for)" ; RETURN "text/x-fortran"
   ELSEIF cFile LIKE ".+\.f4v" ; RETURN "video/x-f4v"
   ELSEIF cFile LIKE ".+\.fbs" ; RETURN "image/vnd.fastbidsheet"
   ELSEIF cFile LIKE ".+\.fdf" ; RETURN "application/vnd.fdf"
   ELSEIF cFile LIKE ".+\.fe_launch" ; RETURN "application/vnd.denovo.fcselayout-link"
   ELSEIF cFile LIKE ".+\.fg5" ; RETURN "application/vnd.fujitsu.oasysgp"
   ELSEIF cFile LIKE ".+\.(fh|fh4|fh5|fh7|fhc)" ; RETURN "image/x-freehand"
   ELSEIF cFile LIKE ".+\.fig" ; RETURN "application/x-xfig"
   ELSEIF cFile LIKE ".+\.fli" ; RETURN "video/x-fli"
   ELSEIF cFile LIKE ".+\.flo" ; RETURN "application/vnd.micrografx.flo"
   ELSEIF cFile LIKE ".+\.flv" ; RETURN "video/x-flv"
   ELSEIF cFile LIKE ".+\.flw" ; RETURN "application/vnd.kde.kivio"
   ELSEIF cFile LIKE ".+\.flx" ; RETURN "text/vnd.fmi.flexstor"
   ELSEIF cFile LIKE ".+\.fly" ; RETURN "text/vnd.fly"
   ELSEIF cFile LIKE ".+\.fnc" ; RETURN "application/vnd.frogans.fnc"
   ELSEIF cFile LIKE ".+\.fpx" ; RETURN "image/vnd.fpx"
   ELSEIF cFile LIKE ".+\.fsc" ; RETURN "application/vnd.fsc.weblaunch"
   ELSEIF cFile LIKE ".+\.fst" ; RETURN "image/vnd.fst"
   ELSEIF cFile LIKE ".+\.ftc" ; RETURN "application/vnd.fluxtime.clip"
   ELSEIF cFile LIKE ".+\.fti" ; RETURN "application/vnd.anser-web-funds-transfer-initiation"
   ELSEIF cFile LIKE ".+\.fvt" ; RETURN "video/vnd.fvt"
   ELSEIF cFile LIKE ".+\.fzs" ; RETURN "application/vnd.fuzzysheet"
   ELSEIF cFile LIKE ".+\.g3" ; RETURN "image/g3fax"
   ELSEIF cFile LIKE ".+\.gac" ; RETURN "application/vnd.groove-account"
   ELSEIF cFile LIKE ".+\.gdl" ; RETURN "model/vnd.gdl"
   ELSEIF cFile LIKE ".+\.geo" ; RETURN "application/vnd.dynageo"
   ELSEIF cFile LIKE ".+\.(gex|gre)" ; RETURN "application/vnd.geometry-explorer"
   ELSEIF cFile LIKE ".+\.ggb" ; RETURN "application/vnd.geogebra.file"
   ELSEIF cFile LIKE ".+\.ggt" ; RETURN "application/vnd.geogebra.tool"
   ELSEIF cFile LIKE ".+\.ghf" ; RETURN "application/vnd.groove-help"
   ELSEIF cFile LIKE ".+\.gif" ; RETURN "image/gif"
   ELSEIF cFile LIKE ".+\.gim" ; RETURN "application/vnd.groove-identity-message"
   ELSEIF cFile LIKE ".+\.gmx" ; RETURN "application/vnd.gmx"
   ELSEIF cFile LIKE ".+\.gnumeric" ; RETURN "application/x-gnumeric"
   ELSEIF cFile LIKE ".+\.gph" ; RETURN "application/vnd.flographit"
   ELSEIF cFile LIKE ".+\.(gqf|gqs)" ; RETURN "application/vnd.grafeq"
   ELSEIF cFile LIKE ".+\.gram" ; RETURN "application/srgs"
   ELSEIF cFile LIKE ".+\.grv" ; RETURN "application/vnd.groove-injector"
   ELSEIF cFile LIKE ".+\.grxml" ; RETURN "application/srgs+xml"
   ELSEIF cFile LIKE ".+\.gsf" ; RETURN "application/x-font-ghostscript"
   ELSEIF cFile LIKE ".+\.gtar" ; RETURN "application/x-gtar"
   ELSEIF cFile LIKE ".+\.gtm" ; RETURN "application/vnd.groove-tool-message"
   ELSEIF cFile LIKE ".+\.gtw" ; RETURN "model/vnd.gtw"
   ELSEIF cFile LIKE ".+\.gv" ; RETURN "text/vnd.graphviz"
   ELSEIF cFile LIKE ".+\.(gz|tgz)" ; RETURN "application/x-gzip"
   ELSEIF cFile LIKE ".+\.h261" ; RETURN "video/h261"
   ELSEIF cFile LIKE ".+\.h263" ; RETURN "video/h263"
   ELSEIF cFile LIKE ".+\.h264" ; RETURN "video/h264"
   ELSEIF cFile LIKE ".+\.hbci" ; RETURN "application/vnd.hbci"
   ELSEIF cFile LIKE ".+\.hdf" ; RETURN "application/x-hdf"
   ELSEIF cFile LIKE ".+\.hlp" ; RETURN "application/winhlp"
   ELSEIF cFile LIKE ".+\.hpgl" ; RETURN "application/vnd.hp-hpgl"
   ELSEIF cFile LIKE ".+\.hpid" ; RETURN "application/vnd.hp-hpid"
   ELSEIF cFile LIKE ".+\.hps" ; RETURN "application/vnd.hp-hps"
   ELSEIF cFile LIKE ".+\.hqx" ; RETURN "application/mac-binhex40"
   ELSEIF cFile LIKE ".+\.htke" ; RETURN "application/vnd.kenameaapp"
   ELSEIF cFile LIKE ".+\.(htm|html)" ; RETURN "text/html"
   ELSEIF cFile LIKE ".+\.hvd" ; RETURN "application/vnd.yamaha.hv-dic"
   ELSEIF cFile LIKE ".+\.hvp" ; RETURN "application/vnd.yamaha.hv-voice"
   ELSEIF cFile LIKE ".+\.hvs" ; RETURN "application/vnd.yamaha.hv-script"
   ELSEIF cFile LIKE ".+\.(icc|icm)" ; RETURN "application/vnd.iccprofile"
   ELSEIF cFile LIKE ".+\.ice" ; RETURN "x-conference/x-cooltalk"
   ELSEIF cFile LIKE ".+\.ico" ; RETURN "image/x-icon"
   ELSEIF cFile LIKE ".+\.(ics|ifb)" ; RETURN "text/calendar"
   ELSEIF cFile LIKE ".+\.ief" ; RETURN "image/ief"
   ELSEIF cFile LIKE ".+\.ifm" ; RETURN "application/vnd.shana.informed.formdata"
   ELSEIF cFile LIKE ".+\.(iges|igs)" ; RETURN "model/iges"
   ELSEIF cFile LIKE ".+\.igl" ; RETURN "application/vnd.igloader"
   ELSEIF cFile LIKE ".+\.igx" ; RETURN "application/vnd.micrografx.igx"
   ELSEIF cFile LIKE ".+\.iif" ; RETURN "application/vnd.shana.informed.interchange"
   ELSEIF cFile LIKE ".+\.imp" ; RETURN "application/vnd.accpac.simply.imp"
   ELSEIF cFile LIKE ".+\.ims" ; RETURN "application/vnd.ms-ims"
   ELSEIF cFile LIKE ".+\.ipk" ; RETURN "application/vnd.shana.informed.package"
   ELSEIF cFile LIKE ".+\.irm" ; RETURN "application/vnd.ibm.rights-management"
   ELSEIF cFile LIKE ".+\.irp" ; RETURN "application/vnd.irepository.package+xml"
   ELSEIF cFile LIKE ".+\.itp" ; RETURN "application/vnd.shana.informed.formtemplate"
   ELSEIF cFile LIKE ".+\.ivp" ; RETURN "application/vnd.immervision-ivp"
   ELSEIF cFile LIKE ".+\.ivu" ; RETURN "application/vnd.immervision-ivu"
   ELSEIF cFile LIKE ".+\.jad" ; RETURN "text/vnd.sun.j2me.app-descriptor"
   ELSEIF cFile LIKE ".+\.jam" ; RETURN "application/vnd.jam"
   ELSEIF cFile LIKE ".+\.jar" ; RETURN "application/java-archive"
   ELSEIF cFile LIKE ".+\.java" ; RETURN "text/x-java-source"
   ELSEIF cFile LIKE ".+\.jisp" ; RETURN "application/vnd.jisp"
   ELSEIF cFile LIKE ".+\.jlt" ; RETURN "application/vnd.hp-jlyt"
   ELSEIF cFile LIKE ".+\.jnlp" ; RETURN "application/x-java-jnlp-file"
   ELSEIF cFile LIKE ".+\.joda" ; RETURN "application/vnd.joost.joda-archive"
   ELSEIF cFile LIKE ".+\.(jpe|jpeg|jpg)" ; RETURN "image/jpeg"
   ELSEIF cFile LIKE ".+\.(jpgm|jpm)" ; RETURN "video/jpm"
   ELSEIF cFile LIKE ".+\.jpgv" ; RETURN "video/jpeg"
   ELSEIF cFile LIKE ".+\.js" ; RETURN "application/javascript"
   ELSEIF cFile LIKE ".+\.json" ; RETURN "application/json"
   ELSEIF cFile LIKE ".+\.(kar|mid|midi|rmi)" ; RETURN "audio/midi"
   ELSEIF cFile LIKE ".+\.karbon" ; RETURN "application/vnd.kde.karbon"
   ELSEIF cFile LIKE ".+\.kfo" ; RETURN "application/vnd.kde.kformula"
   ELSEIF cFile LIKE ".+\.kia" ; RETURN "application/vnd.kidspiration"
   ELSEIF cFile LIKE ".+\.kil" ; RETURN "application/x-killustrator"
   ELSEIF cFile LIKE ".+\.kml" ; RETURN "application/vnd.google-earth.kml+xml"
   ELSEIF cFile LIKE ".+\.kmz" ; RETURN "application/vnd.google-earth.kmz"
   ELSEIF cFile LIKE ".+\.(kne|knp)" ; RETURN "application/vnd.kinar"
   ELSEIF cFile LIKE ".+\.kon" ; RETURN "application/vnd.kde.kontour"
   ELSEIF cFile LIKE ".+\.(kpr|kpt)" ; RETURN "application/vnd.kde.kpresenter"
   ELSEIF cFile LIKE ".+\.ksp" ; RETURN "application/vnd.kde.kspread"
   ELSEIF cFile LIKE ".+\.(ktr|ktz)" ; RETURN "application/vnd.kahootz"
   ELSEIF cFile LIKE ".+\.(kwd|kwt)" ; RETURN "application/vnd.kde.kword"
   ELSEIF cFile LIKE ".+\.latex" ; RETURN "application/x-latex"
   ELSEIF cFile LIKE ".+\.lbd" ; RETURN "application/vnd.llamagraphics.life-balance.desktop"
   ELSEIF cFile LIKE ".+\.lbe" ; RETURN "application/vnd.llamagraphics.life-balance.exchange+xml"
   ELSEIF cFile LIKE ".+\.les" ; RETURN "application/vnd.hhe.lesson-player"
   ELSEIF cFile LIKE ".+\.link66" ; RETURN "application/vnd.route66.link66+xml"
   ELSEIF cFile LIKE ".+\.lostxml" ; RETURN "application/lost+xml"
   ELSEIF cFile LIKE ".+\.lrm" ; RETURN "application/vnd.ms-lrm"
   ELSEIF cFile LIKE ".+\.ltf" ; RETURN "application/vnd.frogans.ltf"
   ELSEIF cFile LIKE ".+\.lvp" ; RETURN "audio/vnd.lucent.voice"
   ELSEIF cFile LIKE ".+\.lwp" ; RETURN "application/vnd.lotus-wordpro"
   ELSEIF cFile LIKE ".+\.(m13|m14|mvb)" ; RETURN "application/x-msmediaview"
   ELSEIF cFile LIKE ".+\.(m1v|m2v|mpa|mpe|mpeg|mpg)" ; RETURN "video/mpeg"
   ELSEIF cFile LIKE ".+\.(m2a|m3a|mp2|mp2a|mp3|mpga)" ; RETURN "audio/mpeg"
   ELSEIF cFile LIKE ".+\.m3u" ; RETURN "audio/x-mpegurl"
   ELSEIF cFile LIKE ".+\.(m4u|mxu)" ; RETURN "video/vnd.mpegurl"
   ELSEIF cFile LIKE ".+\.m4v" ; RETURN "video/x-m4v"
   ELSEIF cFile LIKE ".+\.(ma|mb|nb)" ; RETURN "application/mathematica"
   ELSEIF cFile LIKE ".+\.mag" ; RETURN "application/vnd.ecowin.chart"
   ELSEIF cFile LIKE ".+\.(man|me|ms|roff|t|tr)" ; RETURN "text/troff"
   ELSEIF cFile LIKE ".+\.mathml" ; RETURN "application/mathml+xml"
   ELSEIF cFile LIKE ".+\.mbk" ; RETURN "application/vnd.mobius.mbk"
   ELSEIF cFile LIKE ".+\.mbox" ; RETURN "application/mbox"
   ELSEIF cFile LIKE ".+\.mc1" ; RETURN "application/vnd.medcalcdata"
   ELSEIF cFile LIKE ".+\.mcd" ; RETURN "application/vnd.mcd"
   ELSEIF cFile LIKE ".+\.mcurl" ; RETURN "text/vnd.curl.mcurl"
   ELSEIF cFile LIKE ".+\.mdb" ; RETURN "application/x-msaccess"
   ELSEIF cFile LIKE ".+\.mdi" ; RETURN "image/vnd.ms-modi"
   ELSEIF cFile LIKE ".+\.(mesh|msh|silo)" ; RETURN "model/mesh"
   ELSEIF cFile LIKE ".+\.mfm" ; RETURN "application/vnd.mfmp"
   ELSEIF cFile LIKE ".+\.mgz" ; RETURN "application/vnd.proteus.magazine"
   ELSEIF cFile LIKE ".+\.mif" ; RETURN "application/vnd.mif"
   ELSEIF cFile LIKE ".+\.(mj2|mjp2)" ; RETURN "video/mj2"
   ELSEIF cFile LIKE ".+\.mlp" ; RETURN "application/vnd.dolby.mlp"
   ELSEIF cFile LIKE ".+\.mmd" ; RETURN "application/vnd.chipnuts.karaoke-mmd"
   ELSEIF cFile LIKE ".+\.mmf" ; RETURN "application/vnd.smaf"
   ELSEIF cFile LIKE ".+\.mmr" ; RETURN "image/vnd.fujixerox.edmics-mmr"
   ELSEIF cFile LIKE ".+\.mny" ; RETURN "application/x-msmoney"
   ELSEIF cFile LIKE ".+\.(mobi|prc)" ; RETURN "application/x-mobipocket-ebook"
   ELSEIF cFile LIKE ".+\.(mov|qt)" ; RETURN "video/quicktime"
   ELSEIF cFile LIKE ".+\.movie" ; RETURN "video/x-sgi-movie"
   ELSEIF cFile LIKE ".+\.(mp4|mp4v|mpg4)" ; RETURN "video/mp4"
   ELSEIF cFile LIKE ".+\.mp4a" ; RETURN "audio/mp4"
   ELSEIF cFile LIKE ".+\.mp4s" ; RETURN "application/mp4"
   ELSEIF cFile LIKE ".+\.mpc" ; RETURN "application/vnd.mophun.certificate"
   ELSEIF cFile LIKE ".+\.mpkg" ; RETURN "application/vnd.apple.installer+xml"
   ELSEIF cFile LIKE ".+\.mpm" ; RETURN "application/vnd.blueice.multipass"
   ELSEIF cFile LIKE ".+\.mpn" ; RETURN "application/vnd.mophun.application"
   ELSEIF cFile LIKE ".+\.(mpp|mpt)" ; RETURN "application/vnd.ms-project"
   ELSEIF cFile LIKE ".+\.mpy" ; RETURN "application/vnd.ibm.minipay"
   ELSEIF cFile LIKE ".+\.mqy" ; RETURN "application/vnd.mobius.mqy"
   ELSEIF cFile LIKE ".+\.mrc" ; RETURN "application/marc"
   ELSEIF cFile LIKE ".+\.mscml" ; RETURN "application/mediaservercontrol+xml"
   ELSEIF cFile LIKE ".+\.mseed" ; RETURN "application/vnd.fdsn.mseed"
   ELSEIF cFile LIKE ".+\.mseq" ; RETURN "application/vnd.mseq"
   ELSEIF cFile LIKE ".+\.msf" ; RETURN "application/vnd.epson.msf"
   ELSEIF cFile LIKE ".+\.msl" ; RETURN "application/vnd.mobius.msl"
   ELSEIF cFile LIKE ".+\.msty" ; RETURN "application/vnd.muvee.style"
   ELSEIF cFile LIKE ".+\.mts" ; RETURN "model/vnd.mts"
   ELSEIF cFile LIKE ".+\.mus" ; RETURN "application/vnd.musician"
   ELSEIF cFile LIKE ".+\.musicxml" ; RETURN "application/vnd.recordare.musicxml+xml"
   ELSEIF cFile LIKE ".+\.mwf" ; RETURN "application/vnd.mfer"
   ELSEIF cFile LIKE ".+\.mxf" ; RETURN "application/mxf"
   ELSEIF cFile LIKE ".+\.mxl" ; RETURN "application/vnd.recordare.musicxml"
   ELSEIF cFile LIKE ".+\.(mxml|xhvml|xvm|xvml)" ; RETURN "application/xv+xml"
   ELSEIF cFile LIKE ".+\.mxs" ; RETURN "application/vnd.triscape.mxs"
   ELSEIF cFile LIKE ".+\.n-gage" ; RETURN "application/vnd.nokia.n-gage.symbian.install"
   ELSEIF cFile LIKE ".+\.ncx" ; RETURN "application/x-dtbncx+xml"
   ELSEIF cFile LIKE ".+\.ngdat" ; RETURN "application/vnd.nokia.n-gage.data"
   ELSEIF cFile LIKE ".+\.nlu" ; RETURN "application/vnd.neurolanguage.nlu"
   ELSEIF cFile LIKE ".+\.nml" ; RETURN "application/vnd.enliven"
   ELSEIF cFile LIKE ".+\.nnd" ; RETURN "application/vnd.noblenet-directory"
   ELSEIF cFile LIKE ".+\.nns" ; RETURN "application/vnd.noblenet-sealer"
   ELSEIF cFile LIKE ".+\.nnw" ; RETURN "application/vnd.noblenet-web"
   ELSEIF cFile LIKE ".+\.npx" ; RETURN "image/vnd.net-fpx"
   ELSEIF cFile LIKE ".+\.nsf" ; RETURN "application/vnd.lotus-notes"
   ELSEIF cFile LIKE ".+\.oa2" ; RETURN "application/vnd.fujitsu.oasys2"
   ELSEIF cFile LIKE ".+\.oa3" ; RETURN "application/vnd.fujitsu.oasys3"
   ELSEIF cFile LIKE ".+\.oas" ; RETURN "application/vnd.fujitsu.oasys"
   ELSEIF cFile LIKE ".+\.obd" ; RETURN "application/x-msbinder"
   ELSEIF cFile LIKE ".+\.oda" ; RETURN "application/oda"
   ELSEIF cFile LIKE ".+\.odb" ; RETURN "application/vnd.oasis.opendocument.database"
   ELSEIF cFile LIKE ".+\.odc" ; RETURN "application/vnd.oasis.opendocument.chart"
   ELSEIF cFile LIKE ".+\.odf" ; RETURN "application/vnd.oasis.opendocument.formula"
   ELSEIF cFile LIKE ".+\.odft" ; RETURN "application/vnd.oasis.opendocument.formula-template"
   ELSEIF cFile LIKE ".+\.odg" ; RETURN "application/vnd.oasis.opendocument.graphics"
   ELSEIF cFile LIKE ".+\.odi" ; RETURN "application/vnd.oasis.opendocument.image"
   ELSEIF cFile LIKE ".+\.odp" ; RETURN "application/vnd.oasis.opendocument.presentation"
   ELSEIF cFile LIKE ".+\.ods" ; RETURN "application/vnd.oasis.opendocument.spreadsheet"
   ELSEIF cFile LIKE ".+\.odt" ; RETURN "application/vnd.oasis.opendocument.text"
   ELSEIF cFile LIKE ".+\.(oga|ogg|spx)" ; RETURN "audio/ogg"
   ELSEIF cFile LIKE ".+\.ogv" ; RETURN "video/ogg"
   ELSEIF cFile LIKE ".+\.ogx" ; RETURN "application/ogg"
   ELSEIF cFile LIKE ".+\.(onepkg|onetmp|onetoc|onetoc2)" ; RETURN "application/onenote"
   ELSEIF cFile LIKE ".+\.opf" ; RETURN "application/oebps-package+xml"
   ELSEIF cFile LIKE ".+\.(oprc|pdb|pqa)" ; RETURN "application/vnd.palm"
   ELSEIF cFile LIKE ".+\.org" ; RETURN "application/vnd.lotus-organizer"
   ELSEIF cFile LIKE ".+\.osf" ; RETURN "application/vnd.yamaha.openscoreformat"
   ELSEIF cFile LIKE ".+\.osfpvg" ; RETURN "application/vnd.yamaha.openscoreformat.osfpvg+xml"
   ELSEIF cFile LIKE ".+\.otc" ; RETURN "application/vnd.oasis.opendocument.chart-template"
   ELSEIF cFile LIKE ".+\.otf" ; RETURN "application/x-font-otf"
   ELSEIF cFile LIKE ".+\.otg" ; RETURN "application/vnd.oasis.opendocument.graphics-template"
   ELSEIF cFile LIKE ".+\.oth" ; RETURN "application/vnd.oasis.opendocument.text-web"
   ELSEIF cFile LIKE ".+\.oti" ; RETURN "application/vnd.oasis.opendocument.image-template"
   ELSEIF cFile LIKE ".+\.otm" ; RETURN "application/vnd.oasis.opendocument.text-master"
   ELSEIF cFile LIKE ".+\.otp" ; RETURN "application/vnd.oasis.opendocument.presentation-template"
   ELSEIF cFile LIKE ".+\.ots" ; RETURN "application/vnd.oasis.opendocument.spreadsheet-template"
   ELSEIF cFile LIKE ".+\.ott" ; RETURN "application/vnd.oasis.opendocument.text-template"
   ELSEIF cFile LIKE ".+\.oxt" ; RETURN "application/vnd.openofficeorg.extension"
   ELSEIF cFile LIKE ".+\.(p|pas)" ; RETURN "text/x-pascal"
   ELSEIF cFile LIKE ".+\.p10" ; RETURN "application/pkcs10"
   ELSEIF cFile LIKE ".+\.(p12|pfx)" ; RETURN "application/x-pkcs12"
   ELSEIF cFile LIKE ".+\.p7b" ; RETURN "application/x-pkcs7-certificates"
   ELSEIF cFile LIKE ".+\.(p7c|p7m)" ; RETURN "application/pkcs7-mime"
   ELSEIF cFile LIKE ".+\.p7r" ; RETURN "application/x-pkcs7-certreqresp"
   ELSEIF cFile LIKE ".+\.p7s" ; RETURN "application/pkcs7-signature"
   ELSEIF cFile LIKE ".+\.pbd" ; RETURN "application/vnd.powerbuilder6"
   ELSEIF cFile LIKE ".+\.pbm" ; RETURN "image/x-portable-bitmap"
   ELSEIF cFile LIKE ".+\.pcf" ; RETURN "application/x-font-pcf"
   ELSEIF cFile LIKE ".+\.pcl" ; RETURN "application/vnd.hp-pcl"
   ELSEIF cFile LIKE ".+\.pclxl" ; RETURN "application/vnd.hp-pclxl"
   ELSEIF cFile LIKE ".+\.(pct|pic)" ; RETURN "image/x-pict"
   ELSEIF cFile LIKE ".+\.pcurl" ; RETURN "application/vnd.curl.pcurl"
   ELSEIF cFile LIKE ".+\.pcx" ; RETURN "image/x-pcx"
   ELSEIF cFile LIKE ".+\.pdf" ; RETURN "application/pdf"
   ELSEIF cFile LIKE ".+\.pfr" ; RETURN "application/font-tdpfr"
   ELSEIF cFile LIKE ".+\.pgm" ; RETURN "image/x-portable-graymap"
   ELSEIF cFile LIKE ".+\.pgn" ; RETURN "application/x-chess-pgn"
   ELSEIF cFile LIKE ".+\.pgp" ; RETURN "application/pgp-encrypted"
   ELSEIF cFile LIKE ".+\.pki" ; RETURN "application/pkixcmp"
   ELSEIF cFile LIKE ".+\.pkipath" ; RETURN "application/pkix-pkipath"
   ELSEIF cFile LIKE ".+\.plb" ; RETURN "application/vnd.3gpp.pic-bw-large"
   ELSEIF cFile LIKE ".+\.plc" ; RETURN "application/vnd.mobius.plc"
   ELSEIF cFile LIKE ".+\.plf" ; RETURN "application/vnd.pocketlearn"
   ELSEIF cFile LIKE ".+\.pls" ; RETURN "application/pls+xml"
   ELSEIF cFile LIKE ".+\.pml" ; RETURN "application/vnd.ctc-posml"
   ELSEIF cFile LIKE ".+\.png" ; RETURN "image/png"
   ELSEIF cFile LIKE ".+\.pnm" ; RETURN "image/x-portable-anymap"
   ELSEIF cFile LIKE ".+\.portpkg" ; RETURN "application/vnd.macports.portpkg"
   ELSEIF cFile LIKE ".+\.(pot|ppa|pps|ppt|pwz)" ; RETURN "application/vnd.ms-powerpoint"
   ELSEIF cFile LIKE ".+\.potm" ; RETURN "application/vnd.ms-powerpoint.template.macroenabled.12"
   ELSEIF cFile LIKE ".+\.potx" ; RETURN "application/vnd.openxmlformats-officedocument.presentationml.template"
   ELSEIF cFile LIKE ".+\.ppam" ; RETURN "application/vnd.ms-powerpoint.addin.macroenabled.12"
   ELSEIF cFile LIKE ".+\.ppd" ; RETURN "application/vnd.cups-ppd"
   ELSEIF cFile LIKE ".+\.ppm" ; RETURN "image/x-portable-pixmap"
   ELSEIF cFile LIKE ".+\.ppsm" ; RETURN "application/vnd.ms-powerpoint.slideshow.macroenabled.12"
   ELSEIF cFile LIKE ".+\.ppsx" ; RETURN "application/vnd.openxmlformats-officedocument.presentationml.slideshow"
   ELSEIF cFile LIKE ".+\.pptm" ; RETURN "application/vnd.ms-powerpoint.presentation.macroenabled.12"
   ELSEIF cFile LIKE ".+\.pptx" ; RETURN "application/vnd.openxmlformats-officedocument.presentationml.presentation"
   ELSEIF cFile LIKE ".+\.pre" ; RETURN "application/vnd.lotus-freelance"
   ELSEIF cFile LIKE ".+\.prf" ; RETURN "application/pics-rules"
   ELSEIF cFile LIKE ".+\.psb" ; RETURN "application/vnd.3gpp.pic-bw-small"
   ELSEIF cFile LIKE ".+\.psd" ; RETURN "image/vnd.adobe.photoshop"
   ELSEIF cFile LIKE ".+\.psf" ; RETURN "application/x-font-linux-psf"
   ELSEIF cFile LIKE ".+\.ptid" ; RETURN "application/vnd.pvi.ptid1"
   ELSEIF cFile LIKE ".+\.pub" ; RETURN "application/x-mspublisher"
   ELSEIF cFile LIKE ".+\.pvb" ; RETURN "application/vnd.3gpp.pic-bw-var"
   ELSEIF cFile LIKE ".+\.pwn" ; RETURN "application/vnd.3m.post-it-notes"
   ELSEIF cFile LIKE ".+\.py" ; RETURN "text/x-python"
   ELSEIF cFile LIKE ".+\.pya" ; RETURN "audio/vnd.ms-playready.media.pya"
   ELSEIF cFile LIKE ".+\.(pyc|pyo)" ; RETURN "application/x-python-code"
   ELSEIF cFile LIKE ".+\.pyv" ; RETURN "video/vnd.ms-playready.media.pyv"
   ELSEIF cFile LIKE ".+\.qam" ; RETURN "application/vnd.epson.quickanime"
   ELSEIF cFile LIKE ".+\.qbo" ; RETURN "application/vnd.intu.qbo"
   ELSEIF cFile LIKE ".+\.qfx" ; RETURN "application/vnd.intu.qfx"
   ELSEIF cFile LIKE ".+\.qps" ; RETURN "application/vnd.publishare-delta-tree"
   ELSEIF cFile LIKE ".+\.(qwd|qwt|qxb|qxd|qxl|qxt)" ; RETURN "application/vnd.quark.quarkxpress"
   ELSEIF cFile LIKE ".+\.(ra|ram)" ; RETURN "audio/x-pn-realaudio"
   ELSEIF cFile LIKE ".+\.rar" ; RETURN "application/x-rar-compressed"
   ELSEIF cFile LIKE ".+\.ras" ; RETURN "image/x-cmu-raster"
   ELSEIF cFile LIKE ".+\.rcprofile" ; RETURN "application/vnd.ipunplugged.rcprofile"
   ELSEIF cFile LIKE ".+\.rdf" ; RETURN "application/rdf+xml"
   ELSEIF cFile LIKE ".+\.rdz" ; RETURN "application/vnd.data-vision.rdz"
   ELSEIF cFile LIKE ".+\.rep" ; RETURN "application/vnd.businessobjects"
   ELSEIF cFile LIKE ".+\.res" ; RETURN "application/x-dtbresource+xml"
   ELSEIF cFile LIKE ".+\.rgb" ; RETURN "image/x-rgb"
   ELSEIF cFile LIKE ".+\.rif" ; RETURN "application/reginfo+xml"
   ELSEIF cFile LIKE ".+\.rl" ; RETURN "application/resource-lists+xml"
   ELSEIF cFile LIKE ".+\.rlc" ; RETURN "image/vnd.fujixerox.edmics-rlc"
   ELSEIF cFile LIKE ".+\.rld" ; RETURN "application/resource-lists-diff+xml"
   ELSEIF cFile LIKE ".+\.rm" ; RETURN "application/vnd.rn-realmedia"
   ELSEIF cFile LIKE ".+\.rmp" ; RETURN "audio/x-pn-realaudio-plugin"
   ELSEIF cFile LIKE ".+\.rms" ; RETURN "application/vnd.jcp.javame.midlet-rms"
   ELSEIF cFile LIKE ".+\.rnc" ; RETURN "application/relax-ng-compact-syntax"
   ELSEIF cFile LIKE ".+\.rpm" ; RETURN "application/x-rpm"
   ELSEIF cFile LIKE ".+\.rpss" ; RETURN "application/vnd.nokia.radio-presets"
   ELSEIF cFile LIKE ".+\.rpst" ; RETURN "application/vnd.nokia.radio-preset"
   ELSEIF cFile LIKE ".+\.rq" ; RETURN "application/sparql-query"
   ELSEIF cFile LIKE ".+\.rs" ; RETURN "application/rls-services+xml"
   ELSEIF cFile LIKE ".+\.rsd" ; RETURN "application/rsd+xml"
   ELSEIF cFile LIKE ".+\.rss" ; RETURN "application/rss+xml"
   ELSEIF cFile LIKE ".+\.rtf" ; RETURN "application/rtf"
   ELSEIF cFile LIKE ".+\.rtx" ; RETURN "text/richtext"
   ELSEIF cFile LIKE ".+\.saf" ; RETURN "application/vnd.yamaha.smaf-audio"
   ELSEIF cFile LIKE ".+\.sbml" ; RETURN "application/sbml+xml"
   ELSEIF cFile LIKE ".+\.sc" ; RETURN "application/vnd.ibm.secure-container"
   ELSEIF cFile LIKE ".+\.scd" ; RETURN "application/x-msschedule"
   ELSEIF cFile LIKE ".+\.scm" ; RETURN "application/vnd.lotus-screencam"
   ELSEIF cFile LIKE ".+\.scq" ; RETURN "application/scvp-cv-request"
   ELSEIF cFile LIKE ".+\.scs" ; RETURN "application/scvp-cv-response"
   ELSEIF cFile LIKE ".+\.scurl" ; RETURN "text/vnd.curl.scurl"
   ELSEIF cFile LIKE ".+\.sda" ; RETURN "application/vnd.stardivision.draw"
   ELSEIF cFile LIKE ".+\.sdc" ; RETURN "application/vnd.stardivision.calc"
   ELSEIF cFile LIKE ".+\.sdd" ; RETURN "application/vnd.stardivision.impress"
   ELSEIF cFile LIKE ".+\.(sdkd|sdkm)" ; RETURN "application/vnd.solent.sdkm+xml"
   ELSEIF cFile LIKE ".+\.sdp" ; RETURN "application/sdp"
   ELSEIF cFile LIKE ".+\.(sdw|vor)" ; RETURN "application/vnd.stardivision.writer"
   ELSEIF cFile LIKE ".+\.see" ; RETURN "application/vnd.seemail"
   ELSEIF cFile LIKE ".+\.sema" ; RETURN "application/vnd.sema"
   ELSEIF cFile LIKE ".+\.semd" ; RETURN "application/vnd.semd"
   ELSEIF cFile LIKE ".+\.semf" ; RETURN "application/vnd.semf"
   ELSEIF cFile LIKE ".+\.ser" ; RETURN "application/java-serialized-object"
   ELSEIF cFile LIKE ".+\.setpay" ; RETURN "application/set-payment-initiation"
   ELSEIF cFile LIKE ".+\.setreg" ; RETURN "application/set-registration-initiation"
   ELSEIF cFile LIKE ".+\.sfd-hdstx" ; RETURN "application/vnd.hydrostatix.sof-data"
   ELSEIF cFile LIKE ".+\.sfs" ; RETURN "application/vnd.spotfire.sfs"
   ELSEIF cFile LIKE ".+\.sgl" ; RETURN "application/vnd.stardivision.writer-global"
   ELSEIF cFile LIKE ".+\.(sgm|sgml)" ; RETURN "text/sgml"
   ELSEIF cFile LIKE ".+\.sh" ; RETURN "application/x-sh"
   ELSEIF cFile LIKE ".+\.shar" ; RETURN "application/x-shar"
   ELSEIF cFile LIKE ".+\.shf" ; RETURN "application/shf+xml"
   ELSEIF cFile LIKE ".+\.si" ; RETURN "text/vnd.wap.si"
   ELSEIF cFile LIKE ".+\.sic" ; RETURN "application/vnd.wap.sic"
   ELSEIF cFile LIKE ".+\.(sis|sisx)" ; RETURN "application/vnd.symbian.install"
   ELSEIF cFile LIKE ".+\.sit" ; RETURN "application/x-stuffit"
   ELSEIF cFile LIKE ".+\.sitx" ; RETURN "application/x-stuffitx"
   ELSEIF cFile LIKE ".+\.(skd|skm|skp|skt)" ; RETURN "application/vnd.koan"
   ELSEIF cFile LIKE ".+\.sl" ; RETURN "text/vnd.wap.sl"
   ELSEIF cFile LIKE ".+\.slc" ; RETURN "application/vnd.wap.slc"
   ELSEIF cFile LIKE ".+\.sldm" ; RETURN "application/vnd.ms-powerpoint.slide.macroenabled.12"
   ELSEIF cFile LIKE ".+\.sldx" ; RETURN "application/vnd.openxmlformats-officedocument.presentationml.slide"
   ELSEIF cFile LIKE ".+\.slt" ; RETURN "application/vnd.epson.salt"
   ELSEIF cFile LIKE ".+\.smf" ; RETURN "application/vnd.stardivision.math"
   ELSEIF cFile LIKE ".+\.(smi|smil)" ; RETURN "application/smil+xml"
   ELSEIF cFile LIKE ".+\.snf" ; RETURN "application/x-font-snf"
   ELSEIF cFile LIKE ".+\.spf" ; RETURN "application/vnd.yamaha.smaf-phrase"
   ELSEIF cFile LIKE ".+\.spl" ; RETURN "application/x-futuresplash"
   ELSEIF cFile LIKE ".+\.spot" ; RETURN "text/vnd.in3d.spot"
   ELSEIF cFile LIKE ".+\.spp" ; RETURN "application/scvp-vp-response"
   ELSEIF cFile LIKE ".+\.spq" ; RETURN "application/scvp-vp-request"
   ELSEIF cFile LIKE ".+\.src" ; RETURN "application/x-wais-source"
   ELSEIF cFile LIKE ".+\.srx" ; RETURN "application/sparql-results+xml"
   ELSEIF cFile LIKE ".+\.sse" ; RETURN "application/vnd.kodak-descriptor"
   ELSEIF cFile LIKE ".+\.ssf" ; RETURN "application/vnd.epson.ssf"
   ELSEIF cFile LIKE ".+\.ssml" ; RETURN "application/ssml+xml"
   ELSEIF cFile LIKE ".+\.stc" ; RETURN "application/vnd.sun.xml.calc.template"
   ELSEIF cFile LIKE ".+\.std" ; RETURN "application/vnd.sun.xml.draw.template"
   ELSEIF cFile LIKE ".+\.stf" ; RETURN "application/vnd.wt.stf"
   ELSEIF cFile LIKE ".+\.sti" ; RETURN "application/vnd.sun.xml.impress.template"
   ELSEIF cFile LIKE ".+\.stk" ; RETURN "application/hyperstudio"
   ELSEIF cFile LIKE ".+\.stl" ; RETURN "application/vnd.ms-pki.stl"
   ELSEIF cFile LIKE ".+\.str" ; RETURN "application/vnd.pg.format"
   ELSEIF cFile LIKE ".+\.stw" ; RETURN "application/vnd.sun.xml.writer.template"
   ELSEIF cFile LIKE ".+\.(sus|susp)" ; RETURN "application/vnd.sus-calendar"
   ELSEIF cFile LIKE ".+\.sv4cpio" ; RETURN "application/x-sv4cpio"
   ELSEIF cFile LIKE ".+\.sv4crc" ; RETURN "application/x-sv4crc"
   ELSEIF cFile LIKE ".+\.svd" ; RETURN "application/vnd.svd"
   ELSEIF cFile LIKE ".+\.(svg|svgz)" ; RETURN "image/svg+xml"
   ELSEIF cFile LIKE ".+\.swf" ; RETURN "application/x-shockwave-flash"
   ELSEIF cFile LIKE ".+\.swi" ; RETURN "application/vnd.arastra.swi"
   ELSEIF cFile LIKE ".+\.sxc" ; RETURN "application/vnd.sun.xml.calc"
   ELSEIF cFile LIKE ".+\.sxd" ; RETURN "application/vnd.sun.xml.draw"
   ELSEIF cFile LIKE ".+\.sxg" ; RETURN "application/vnd.sun.xml.writer.global"
   ELSEIF cFile LIKE ".+\.sxi" ; RETURN "application/vnd.sun.xml.impress"
   ELSEIF cFile LIKE ".+\.sxm" ; RETURN "application/vnd.sun.xml.math"
   ELSEIF cFile LIKE ".+\.sxw" ; RETURN "application/vnd.sun.xml.writer"
   ELSEIF cFile LIKE ".+\.tao" ; RETURN "application/vnd.tao.intent-module-archive"
   ELSEIF cFile LIKE ".+\.tar" ; RETURN "application/x-tar"
   ELSEIF cFile LIKE ".+\.tcap" ; RETURN "application/vnd.3gpp2.tcap"
   ELSEIF cFile LIKE ".+\.tcl" ; RETURN "application/x-tcl"
   ELSEIF cFile LIKE ".+\.teacher" ; RETURN "application/vnd.smart.teacher"
   ELSEIF cFile LIKE ".+\.tex" ; RETURN "application/x-tex"
   ELSEIF cFile LIKE ".+\.(texi|texinfo)" ; RETURN "application/x-texinfo"
   ELSEIF cFile LIKE ".+\.tfm" ; RETURN "application/x-tex-tfm"
   ELSEIF cFile LIKE ".+\.(tif|tiff)" ; RETURN "image/tiff"
   ELSEIF cFile LIKE ".+\.tmo" ; RETURN "application/vnd.tmobile-livetv"
   ELSEIF cFile LIKE ".+\.torrent" ; RETURN "application/x-bittorrent"
   ELSEIF cFile LIKE ".+\.tpl" ; RETURN "application/vnd.groove-tool-template"
   ELSEIF cFile LIKE ".+\.tpt" ; RETURN "application/vnd.trid.tpt"
   ELSEIF cFile LIKE ".+\.tra" ; RETURN "application/vnd.trueapp"
   ELSEIF cFile LIKE ".+\.trm" ; RETURN "application/x-msterminal"
   ELSEIF cFile LIKE ".+\.tsv" ; RETURN "text/tab-separated-values"
   ELSEIF cFile LIKE ".+\.(ttc|ttf)" ; RETURN "application/x-font-ttf"
   ELSEIF cFile LIKE ".+\.(twd|twds)" ; RETURN "application/vnd.simtech-mindmapper"
   ELSEIF cFile LIKE ".+\.txd" ; RETURN "application/vnd.genomatix.tuxedo"
   ELSEIF cFile LIKE ".+\.txf" ; RETURN "application/vnd.mobius.txf"
   ELSEIF cFile LIKE ".+\.(ufd|ufdl)" ; RETURN "application/vnd.ufdl"
   ELSEIF cFile LIKE ".+\.umj" ; RETURN "application/vnd.umajin"
   ELSEIF cFile LIKE ".+\.unityweb" ; RETURN "application/vnd.unity"
   ELSEIF cFile LIKE ".+\.uoml" ; RETURN "application/vnd.uoml+xml"
   ELSEIF cFile LIKE ".+\.(uri|uris|urls)" ; RETURN "text/uri-list"
   ELSEIF cFile LIKE ".+\.ustar" ; RETURN "application/x-ustar"
   ELSEIF cFile LIKE ".+\.utz" ; RETURN "application/vnd.uiq.theme"
   ELSEIF cFile LIKE ".+\.uu" ; RETURN "text/x-uuencode"
   ELSEIF cFile LIKE ".+\.vcd" ; RETURN "application/x-cdlink"
   ELSEIF cFile LIKE ".+\.vcf" ; RETURN "text/x-vcard"
   ELSEIF cFile LIKE ".+\.vcg" ; RETURN "application/vnd.groove-vcard"
   ELSEIF cFile LIKE ".+\.vcs" ; RETURN "text/x-vcalendar"
   ELSEIF cFile LIKE ".+\.vcx" ; RETURN "application/vnd.vcx"
   ELSEIF cFile LIKE ".+\.vis" ; RETURN "application/vnd.visionary"
   ELSEIF cFile LIKE ".+\.viv" ; RETURN "video/vnd.vivo"
   ELSEIF cFile LIKE ".+\.(vrml|wrl)" ; RETURN "model/vrml"
   ELSEIF cFile LIKE ".+\.(vsd|vss|vst|vsw)" ; RETURN "application/vnd.visio"
   ELSEIF cFile LIKE ".+\.vsf" ; RETURN "application/vnd.vsf"
   ELSEIF cFile LIKE ".+\.vtu" ; RETURN "model/vnd.vtu"
   ELSEIF cFile LIKE ".+\.vxml" ; RETURN "application/voicexml+xml"
   ELSEIF cFile LIKE ".+\.wad" ; RETURN "application/x-doom"
   ELSEIF cFile LIKE ".+\.wav" ; RETURN "audio/x-wav"
   ELSEIF cFile LIKE ".+\.wax" ; RETURN "audio/x-ms-wax"
   ELSEIF cFile LIKE ".+\.wbmp" ; RETURN "image/vnd.wap.wbmp"
   ELSEIF cFile LIKE ".+\.wbs" ; RETURN "application/vnd.criticaltools.wbs+xml"
   ELSEIF cFile LIKE ".+\.wbxml" ; RETURN "application/vnd.wap.wbxml"
   ELSEIF cFile LIKE ".+\.(wcm|wdb|wks|wps)" ; RETURN "application/vnd.ms-works"
   ELSEIF cFile LIKE ".+\.wm" ; RETURN "video/x-ms-wm"
   ELSEIF cFile LIKE ".+\.wma" ; RETURN "audio/x-ms-wma"
   ELSEIF cFile LIKE ".+\.wmd" ; RETURN "application/x-ms-wmd"
   ELSEIF cFile LIKE ".+\.wmf" ; RETURN "application/x-msmetafile"
   ELSEIF cFile LIKE ".+\.wml" ; RETURN "text/vnd.wap.wml"
   ELSEIF cFile LIKE ".+\.wmlc" ; RETURN "application/vnd.wap.wmlc"
   ELSEIF cFile LIKE ".+\.wmls" ; RETURN "text/vnd.wap.wmlscript"
   ELSEIF cFile LIKE ".+\.wmlsc" ; RETURN "application/vnd.wap.wmlscriptc"
   ELSEIF cFile LIKE ".+\.wmv" ; RETURN "video/x-ms-wmv"
   ELSEIF cFile LIKE ".+\.wmx" ; RETURN "video/x-ms-wmx"
   ELSEIF cFile LIKE ".+\.wmz" ; RETURN "application/x-ms-wmz"
   ELSEIF cFile LIKE ".+\.wpd" ; RETURN "application/vnd.wordperfect"
   ELSEIF cFile LIKE ".+\.wpl" ; RETURN "application/vnd.ms-wpl"
   ELSEIF cFile LIKE ".+\.wqd" ; RETURN "application/vnd.wqd"
   ELSEIF cFile LIKE ".+\.wri" ; RETURN "application/x-mswrite"
   ELSEIF cFile LIKE ".+\.wsdl" ; RETURN "application/wsdl+xml"
   ELSEIF cFile LIKE ".+\.wspolicy" ; RETURN "application/wspolicy+xml"
   ELSEIF cFile LIKE ".+\.wtb" ; RETURN "application/vnd.webturbo"
   ELSEIF cFile LIKE ".+\.wvx" ; RETURN "video/x-ms-wvx"
   ELSEIF cFile LIKE ".+\.x3d" ; RETURN "application/vnd.hzn-3d-crossword"
   ELSEIF cFile LIKE ".+\.xap" ; RETURN "application/x-silverlight-app"
   ELSEIF cFile LIKE ".+\.xar" ; RETURN "application/vnd.xara"
   ELSEIF cFile LIKE ".+\.xbap" ; RETURN "application/x-ms-xbap"
   ELSEIF cFile LIKE ".+\.xbd" ; RETURN "application/vnd.fujixerox.docuworks.binder"
   ELSEIF cFile LIKE ".+\.xbm" ; RETURN "image/x-xbitmap"
   ELSEIF cFile LIKE ".+\.xdm" ; RETURN "application/vnd.syncml.dm+xml"
   ELSEIF cFile LIKE ".+\.xdp" ; RETURN "application/vnd.adobe.xdp+xml"
   ELSEIF cFile LIKE ".+\.xdw" ; RETURN "application/vnd.fujixerox.docuworks"
   ELSEIF cFile LIKE ".+\.xenc" ; RETURN "application/xenc+xml"
   ELSEIF cFile LIKE ".+\.xer" ; RETURN "application/patch-ops-error+xml"
   ELSEIF cFile LIKE ".+\.xfdf" ; RETURN "application/vnd.adobe.xfdf"
   ELSEIF cFile LIKE ".+\.xfdl" ; RETURN "application/vnd.xfdl"
   ELSEIF cFile LIKE ".+\.(xht|xhtml)" ; RETURN "application/xhtml+xml"
   ELSEIF cFile LIKE ".+\.xif" ; RETURN "image/vnd.xiff"
   ELSEIF cFile LIKE ".+\.(xla|xlb|xlc|xlm|xls|xlt|xlw)" ; RETURN "application/vnd.ms-excel"
   ELSEIF cFile LIKE ".+\.xlam" ; RETURN "application/vnd.ms-excel.addin.macroenabled.12"
   ELSEIF cFile LIKE ".+\.xlsb" ; RETURN "application/vnd.ms-excel.sheet.binary.macroenabled.12"
   ELSEIF cFile LIKE ".+\.xlsm" ; RETURN "application/vnd.ms-excel.sheet.macroenabled.12"
   ELSEIF cFile LIKE ".+\.xlsx" ; RETURN "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
   ELSEIF cFile LIKE ".+\.xltm" ; RETURN "application/vnd.ms-excel.template.macroenabled.12"
   ELSEIF cFile LIKE ".+\.xltx" ; RETURN "application/vnd.openxmlformats-officedocument.spreadsheetml.template"
   ELSEIF cFile LIKE ".+\.(xml|xpdl|xsl)" ; RETURN "application/xml"
   ELSEIF cFile LIKE ".+\.xo" ; RETURN "application/vnd.olpc-sugar"
   ELSEIF cFile LIKE ".+\.xop" ; RETURN "application/xop+xml"
   ELSEIF cFile LIKE ".+\.xpi" ; RETURN "application/x-xpinstall"
   ELSEIF cFile LIKE ".+\.xpm" ; RETURN "image/x-xpixmap"
   ELSEIF cFile LIKE ".+\.xpr" ; RETURN "application/vnd.is-xpr"
   ELSEIF cFile LIKE ".+\.xps" ; RETURN "application/vnd.ms-xpsdocument"
   ELSEIF cFile LIKE ".+\.(xpw|xpx)" ; RETURN "application/vnd.intercon.formnet"
   ELSEIF cFile LIKE ".+\.xslt" ; RETURN "application/xslt+xml"
   ELSEIF cFile LIKE ".+\.xsm" ; RETURN "application/vnd.syncml+xml"
   ELSEIF cFile LIKE ".+\.xspf" ; RETURN "application/xspf+xml"
   ELSEIF cFile LIKE ".+\.xul" ; RETURN "application/vnd.mozilla.xul+xml"
   ELSEIF cFile LIKE ".+\.xwd" ; RETURN "image/x-xwindowdump"
   ELSEIF cFile LIKE ".+\.xyz" ; RETURN "chemical/x-xyz"
   ELSEIF cFile LIKE ".+\.z"   ; RETURN "application/x-compress"
   ELSEIF cFile LIKE ".+\.zaz" ; RETURN "application/vnd.zzazz.deck+xml"
   ELSEIF cFile LIKE ".+\.zip" ; RETURN "application/zip"
   ELSEIF cFile LIKE ".+\.(zir|zirz)" ; RETURN "application/vnd.zul"
   ELSEIF cFile LIKE ".+\.zmm" ; RETURN "application/vnd.handheld-entertainment+xml"   
   ENDIF

RETURN "text/plain"
