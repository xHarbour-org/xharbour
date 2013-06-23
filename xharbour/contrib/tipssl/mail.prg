/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * www - http://www.harbour-project.org
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

/* 2007-04-11, Hannes Ziegler <hz AT knowlexbase.com>
   Added method :setHeader()
   Added method :attachFile()
   Added method :detachFile()
   Added method :getFileName()
   Added method :isMultiPart()
   Added method :getMultiParts()
*/

#include "hbclass.ch"

CLASS TipMail

   DATA hHeaders                 //
   DATA aReceived INIT {}        // received fields may be more than once.

   METHOD New(cBody, oEncoder )    Constructor
   METHOD SetBody( cBody )
   METHOD GetBody()
   METHOD GetRawBody()              INLINE ::cBody
   METHOD SetEncoder( cEncoder )

   METHOD FromString( cMail, cBoundary, nPos )
   METHOD ToString()

   METHOD GetFieldPart( cField )
   METHOD GetFieldOption( cField, cOption )
   METHOD SetFieldPart( cField, cValue )
   METHOD SetFieldOption( cField, cOption, cValue )

   METHOD GetContentType() INLINE ::GetFieldPart( "Content-Type" )
   METHOD GetCharEncoding() INLINE ::GetFieldOption( "Content-Type", "encoding" )

   METHOD Attach( oSubPart )
   METHOD NextAttachment()
   METHOD CountAttachments()  INLINE Len( ::aAttachments )
   METHOD GetAttachment()
   METHOD ResetAttachment()   INLINE ::nAttachPos := 1

   METHOD MakeBoundary()

   METHOD isMultiPart()
   METHOD getMultiParts()

   METHOD setHeader( cSubject, cFrom, cTo, cCC, cBCC ) 
   METHOD attachFile( cFileName )
   METHOD detachFile( cPath )
   METHOD getFileName()
HIDDEN:
   DATA cBody
   Data lBodyEncoded init .f.
   DATA oEncoder
   DATA aAttachments
   DATA nAttachPos   INIT 1

ENDCLASS

METHOD New( cBody, oEncoder ) CLASS TipMail

   // Set header fileds to non-sensitive
   ::hHeaders := HSetCaseMatch( {=>}, .F. )
   ::aAttachments := {}

   IF HB_ISSTRING( oEncoder ) .OR. HB_ISOBJECT( oEncoder )
      ::setEncoder( oEncoder )
   ENDIF

   IF cBody != NIL
      IF ::oEncoder != NIL
         ::cBody := ::oEncoder:Encode( cBody )
         ::hHeaders[ "Content-Transfer-Encoding" ] := ::oEncoder:cName
      ELSE
         ::cBody := cBody
      ENDIF
      ::hHeaders[ "Content-Length" ] := Ltrim( Str( Len( ::cBody ) ) )
   ENDIF

RETURN Self

METHOD SetEncoder( cEnc ) CLASS TipMail

   if HB_IsString( cEnc )
      ::oEncoder := TIp_GetEncoder( cEnc )
   ELSE
      ::oEncoder := cEnc
   ENDIF
   ::hHeaders[ "Content-transfer-encoding" ] := ::oEncoder:cName
RETURN .T.

METHOD SetBody( cBody ) CLASS TipMail

   IF ::oEncoder != NIL
      ::cBody := ::oEncoder:Encode( cBody )
      ::lBodyEncoded:=.t.  //GD needed to prevent an extra crlf from being appended
   ELSE
      ::cBody := cBody
   ENDIF
   //::hHeaders[ "Content-Length" ] := Ltrim( Str( Len( cBody ) ) )  //GD -not needed
RETURN .T.

METHOD GetBody() CLASS TipMail

   IF ::cBody == NIL
      RETURN NIL
   ELSEIF ::oEncoder != NIL
      RETURN ::oEncoder:Decode( ::cBody )
   ENDIF
RETURN ::cBody

METHOD GetFieldPart( cPart ) CLASS TipMail

   LOCAL nPos, cEnc

   nPos := HGetPos( ::hHeaders, cPart )
   IF nPos == 0
      RETURN ""
   ELSE
      cEnc := HGetValueAt( ::hHeaders, nPos )
      nPos := At( ";", cEnc )
      IF nPos != 0
         cEnc := Substr( cEnc, 1, nPos - 1)
      ENDIF
   ENDIF

RETURN cEnc

METHOD GetFieldOption( cPart, cOption ) CLASS TipMail

   LOCAL nPos, aMatch
   LOCAL cEnc

   nPos := HGetPos( ::hHeaders, cPart )
   IF nPos == 0
      RETURN ""
   ELSE
      cEnc := HGetValueAt( ::hHeaders, nPos )
      // Case insensitive check
      aMatch := HB_Regex( ";\s*" + cOption +"\s*=\s*([^;]*)", cEnc, .F. )
      IF aMatch != NIL
         cEnc := aMatch[2]
      ELSE
         RETURN ""
      ENDIF
   ENDIF
RETURN cEnc

METHOD SetFieldPart( cPart, cValue ) CLASS TipMail

   LOCAL nPos, cEnc

   nPos := HGetPos( ::hHeaders, cPart )
   IF nPos == 0
      ::hHeaders[ cPart ] := cValue
   ELSE
      cEnc := HGetValueAt( ::hHeaders, nPos )
      nPos := At( ";", cEnc )
      IF nPos == 0
         ::hHeaders[ cPart ] := cValue
      ELSE
         ::hHeaders[ cPart ] := cValue + Substr( cEnc, nPos )
      ENDIF
   ENDIF

RETURN .T.

METHOD SetFieldOption( cPart, cOption, cValue ) CLASS TipMail

   LOCAL nPos, aMatch
   LOCAL cEnc

   nPos := HGetPos( ::hHeaders, cPart )
   IF nPos == 0
      Return .F.
   ELSE
      cEnc := HGetValueAt( ::hHeaders, nPos )
      aMatch := HB_Regex( "(.*?;\s*)" + cOption +"\s*=[^;]*(.*)?", cEnc, .F. )
      IF Empty( aMatch )
         ::hHeaders[ cPart ] := cEnc += "; "+ cOption + '="' + cValue + '"'
      ELSE
         ::hHeaders[ cPart ] := aMatch[2] + cOption + '="' +;
                cValue + '"' + aMatch[3]
      ENDIF
   ENDIF

RETURN .T.


METHOD Attach( oSubPart ) CLASS TipMail

   IF HB_IsObject( oSubPart ) .and. oSubPart:ClassName == "TIPMAIL"
      // reset wrong content-type
      IF At( "multipart/", Lower( ::GetFieldPart("Content-Type")) ) == 0
         ::hHeaders[ "Content-Type" ] := "multipart/mixed"
      ENDIF

      AAdd( ::aAttachments, oSubPart )
      RETURN .T.
   ELSE
      Alert( "TipMail:Attach() must be called with another TipMail object" )
   ENDIF

RETURN .F.


METHOD NextAttachment() CLASS TipMail

   IF ::nAttachPos > Len( ::aAttachments )
      RETURN NIL
   ELSE
      ::nAttachPos ++
   ENDIF

RETURN ::aAttachments[ ::nAttachPos - 1 ]


METHOD GetAttachment() CLASS TipMail

   IF ::nAttachPos > Len( ::aAttachments )
      RETURN NIL
   ENDIF

RETURN ::aAttachments[ ::nAttachPos ]

METHOD ToString() CLASS TipMail

   LOCAL cBoundary, cElem, i
   LOCAL cRet := ""
   // this is a multipart message; we need a boundary
    IF Len( ::aAttachments ) > 0
      ::hHeaders[ "Mime-Version" ] :="1.0"
    endif

   IF Len( ::aAttachments ) > 0
      //Reset failing content type
      IF At( "multipart/", Lower( ::GetFieldPart("Content-Type")) ) == 0
         ::hHeaders[ "Content-Type" ] := "multipart/mixed"
      ENDIF

      // have we got it already?
      cBoundary := ::GetFieldOption( "Content-Type", "Boundary" )
      IF Empty( cBoundary )
         cBoundary := ::MakeBoundary()
         IF ! ::SetFieldOption( "Content-Type", "Boundary", cBoundary )
            ::hHeaders[ "Content-Type" ] := ;
               'multipart/mixed; boundary="' + cBoundary + '"'
         ENDIF
      ENDIF
   ENDIF

   // Begin output the fields
   // Presenting them in a "well-known" order
   IF "Return-Path" IN ::hHeaders
      cRet += "Return-Path: "+::hHeaders[ "Return-Path" ] + e"\r\n"
   ENDIF
   IF "Delivered-To" IN ::hHeaders
      cRet += "Delivered-To: "+::hHeaders[ "Delivered-To" ] + e"\r\n"
   ENDIF
   FOR EACH cElem IN ::aReceived
      cRet += "Received: "+ cElem+ e"\r\n"
   NEXT
   IF "Date" IN ::hHeaders
      cRet += "Date: "+::hHeaders[ "Date" ] + e"\r\n"
   ENDIF
   IF "From" IN ::hHeaders
      cRet += "From: "+::hHeaders[ "From" ] + e"\r\n"
   ENDIF
   IF "To" IN ::hHeaders
      cRet += "To: "+::hHeaders[ "To" ] + e"\r\n"
   ENDIF
   IF "Subject" IN ::hHeaders
      cRet += "Subject: "+ ::hHeaders[ "Subject" ] + e"\r\n"
   ENDIF
   IF Len( ::aAttachments ) > 0
      cRet += "Mime-Version:" + ::hHeaders[ "Mime-Version" ] + e"\r\n"
   ENDIF

   FOR i := 1 TO Len( ::hHeaders )
      cElem := Lower(HGetKeyAt( ::hHeaders, i ))
      IF !( cElem == "return-path" ) .and.;
         !( cElem == "delivered-to" ) .and.;
         !( cElem == "date" ) .and.;
         !( cElem == "from" ) .and.;
         !( cElem == "to" ) .and.;
         !( cElem == "subject" ) .and.;
         !( cElem == "mime-version" )
         cRet += HGetKeyAt( ::hHeaders, i ) + ": " +;
                 HGetValueAt( ::hHeaders, i ) + e"\r\n"
      ENDIF
   NEXT

   // end of Header
   cRet += e"\r\n"

   //Body
   IF ! Empty( ::cBody )
      IF empty(::aAttachments)
         //cRet += ::cBody +if(lAttachment,'', e"\r\n")
         cRet += ::cBody + if(::lBodyEncoded,'', e"\r\n")
      else
         //GD - if there are attachements the body of the message has to be treated as an attachment.
         cRet += "--" + cBoundary + e"\r\n"
         cRet+= "Content-Type: text/plain; charset=ISO-8859-1; format=flowed"+ e"\r\n"
         cRet+= "Content-Transfer-Encoding: 7bit"+ e"\r\n"
         cRet+= "Content-Disposition: inline"+ e"\r\n"+ e"\r\n"
         cRet += ::cBody+ e"\r\n"
      ENDIF

   ENDIF

   IF ! Empty( ::aAttachments )
      //Eventually go with mime multipart
      FOR i := 1 TO Len(::aAttachments )
         cRet += "--" + cBoundary + e"\r\n"
         cRet += ::aAttachments[i]:ToString()
      NEXT
      cRet += "--" + cBoundary + "--" + e"\r\n"
   ENDIF

RETURN cRet

METHOD FromString( cMail, cBoundary, nPos ) CLASS TipMail

   LOCAL oSubSection, cSubBoundary
   LOCAL nLinePos, nSplitPos, nBodyPos
   LOCAL cValue, cLastField

   IF Len( ::aAttachments ) > 0
      ::aAttachments := {}
   ENDIF

   IF Len( ::hHeaders ) > 0
      ::hHeaders := HSetCaseMatch( {=>} , .F. )
   ENDIF

   IF Len( ::aReceived ) > 0
      ::aReceived := {}
   ENDIF

   // Part 1: parsing header
   IF nPos == NIL
      nPos := 1
   ENDIF

   IF cMail == NIL
      cMail := ""
   ENDIF

   nLinePos := At( e"\r\n", cMail, nPos )
   DO WHILE nLinePos > nPos
      // going on with last field?
      IF (cMail[ nPos ] == " " .or. cMail[ nPos ] == e"\t" );
               .and. cLastField != NIL
         cValue := Ltrim(Substr( cMail, nPos, nLinePos - nPos ))
         IF Lower(cLastField) == "received"
            ::aReceived[Len(::aReceived)] += " " + cValue
         ELSE
            ::hHeaders[ cLastField ] += " " +cValue
         ENDIF

      ELSE
         nSplitPos := At( ":", cMail, nPos )
         cLastField := Substr( cMail, nPos, nSplitPos - nPos)
         cValue := Ltrim(Substr( cMail, nSplitPos +1, nLinePos - nSplitPos -1))
         IF Lower(cLastField) == "received"
            AAdd( ::aReceived, cValue )
         ELSE
            ::hHeaders[ cLastField ] := cValue
         ENDIF
      ENDIF

      nPos := nLinePos + 2
      nLinePos := At( e"\r\n", cMail, nPos )
      //Prevents malformed body to affect us
      IF cBoundary != NIL .and. At( "--"+cBoundary, cMail, nPos ) == 1
         RETURN 0
      ENDIF
   ENDDO

   // now we may have a body or a multipart message; multipart
   // messages may also have a "fake" body, that is usually not
   // displayed, between their headers and the first multipart
   // boundary.

   IF "Content-Transfer-Encoding" IN ::hHeaders
      ::oEncoder := TIp_GetEncoder( ::hHeaders["Content-Transfer-Encoding"] )
   ENDIF

   // se if we have subparts:
   IF At( "multipart/", Lower( ::GetFieldPart("Content-Type")) ) > 0
      cSubBoundary := ::GetFieldOption( "Content-Type", "Boundary" )
      //strip " on boundary
      IF cSubBoundary[1] == '"'
         cSubBoundary := Substr( cSubBoundary, 2, Len( cSubBoundary ) - 2)
      ENDIF
   ENDIF

   nPos := nLinePos + 2
   nBodyPos := nPos
   nLinePos := At( e"\r\n", cMail, nPos )

   DO WHILE nLinePos >= nPos
      // Avoid useless tests for empty lines
      IF nLinePos == nPos
         nPos += 2
         nLinePos := At( e"\r\n", cMail, nPos )
         LOOP
      ENDIF

      //have we met the boundary?
      IF cBoundary != NIL .and. At( "--"+cBoundary, cMail, nPos ) == nPos
         EXIT
      ENDIF

      //Have we met a section?
      IF cSubBoundary != NIL .and.;
            At( "--" + cSubBoundary, cMail, nPos ) == nPos

         //is it the last subsection?
         IF At( "--", cMail, nPos + Len(cSubBoundary)+2, nLinePos) > 0
            EXIT
         ENDIF

         // set our body
         IF nBodyPos > 0
            ::cBody := Substr( cMail, nBodyPos, nPos - nBodyPos )
            nBodyPos := 0
         ENDIF

         // Add our subsection
         oSubSection := TipMail():New()
         nPos := oSubSection:FromString( cMail, cSubBoundary, nLinePos + 2 )

         IF nPos > 0
            AAdd( ::aAttachments, oSubSection )
         ELSE
            RETURN 0
         ENDIF
         // I must stay on the boundary found by the subsection to
         // enter in this part of the loop again.

      ELSE
         //nPos := nLinePos + 2
         /* 04/05/2004 - <maurilio.longo@libero.it>
            Instead of testing every single line of mail until we find next boundary, if there is a boundary we
            jump to it immediatly, this saves thousands of EOL test and makes splitting of a string fast
         */
         nPos := iif( ! Empty( cSubBoundary ), At( "--" + cSubBoundary, cMail, nPos ), ;
                      iif( ! Empty(cBoundary ), At( "--" + cBoundary, cMail, nPos ), nLinePos + 2 ) )
      ENDIF

      nLinePos := At( e"\r\n", cMail, nPos )
   ENDDO

   // set our body if needed
   IF nBodyPos > 0
      ::cBody := Substr( cMail, nBodyPos, nPos - nBodyPos )
   ENDIF


RETURN nPos

METHOD MakeBoundary() CLASS TipMail

   LOCAL cBound := "=_0" + Space(17)
   LOCAL i

   FOR i := 4 TO 20
      cBound[i] := Chr( HB_Random(0, 25 ) + Asc("A") )
   NEXT

   cBound += "_TIP_" + StrTran( DToC( Date() ), "/", "" ) + "_" + StrTran( Time(), ":", "" )

RETURN cBound

METHOD setHeader( cSubject, cFrom, cTo, cCC, cBCC ) CLASS TipMail

   LOCAL aTo, aCC, aBCC, i, imax

   IF ! HB_ISSTRING( cSubject )
      cSubject := ""
   ENDIF

   IF ! HB_ISSTRING( cFrom )
      RETURN .F.
   ENDIF

   IF HB_ISSTRING( cTo )
      aTo := { cTo }
   ELSEIF HB_ISARRAY( cTo )
      aTo := cTo
   ENDIF

   IF HB_ISSTRING( cCC )
      aCC := { cCC }
   ELSEIF HB_ISARRAY( cCC )
      aCC := cCC
   ENDIF

   IF HB_ISSTRING( cBCC )
      aBCC := { cBCC }
   ELSEIF HB_ISARRAY( cBCC )
      aBCC := cBCC
   ENDIF

   IF aTO == NIL
      RETURN .F.
   ENDIF

   IF ! ::SetFieldPart( "Subject", cSubject )
      RETURN .F.
   ENDIF

   IF ! ::SetFieldPart( "From", cFrom )
      RETURN .F.
   ENDIF

   cTo  := aTO[1]
   imax := Len( aTO )
   FOR i:=2 TO imax
      cTo += "," + InetCrlf() + Chr(9) + aTo[i]
   NEXT

   IF ! ::SetFieldPart( "To", cTo )
      RETURN .F.
   ENDIF

   IF aCC != NIL
      cCC  := aCC[1]
      imax := Len( aCC )
      FOR i:=2 TO imax
        cCC += "," + InetCrlf() + Chr(9) + aCC[i]
      NEXT

      IF ! ::SetFieldPart( "Cc", cCC )
         RETURN .F.
      ENDIF
   ENDIF

   IF aBCC != NIL
      cBCC  := aBCC[1]
      imax := Len( aBCC )
      FOR i:=2 TO imax
        cBCC += "," + InetCrlf() + Chr(9) + aBCC[i]
      NEXT

      IF ! ::SetFieldPart( "Bcc", cBCC )
         RETURN .F.
      ENDIF
   ENDIF

RETURN .T.

METHOD attachFile( cFileName ) CLASS TipMail

   LOCAL cContent := MemoRead( cFileName )
   LOCAL cFname, cFext
   LOCAL cMimeType
   LOCAL cDelim := HB_OsPathSeparator()
   LOCAL oAttach

   IF Empty( cContent )
      RETURN .F.
   ENDIF

   hb_FNameSplit( cFileName,, @cFname, @cFext )
   cMimeType := HB_SetMimeType( cFileName, cFname, cFext )

   IF ".html" in lower( cFext ) .OR. ".htm" in lower( cFext )
      cMimeType += "; charset=ISO-8859-1"
   ENDIF

//   oAttach   := TIPMail():new( cContent, "base64" )
   IF Lower( cFileName ) LIKE ".+\.(123|3dm|3dmf|3dml|3g2|3gp|7z|aab|aabaam)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(aac|aam|aas|abw|ac|acc|ace|acu|adp)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(adr|aep|afl|afp|ahead|ai|aif|aifc|aiff)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(air|ait|alt|ami|apk|application|apr|arj|asd)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(asf|asn|aso|asp|asx|asz|atc|atom|atomcat)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(atomsvc|atx|au|avi|aw|axs|azf|azs|azw)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(bcpio|bdf|bdm|bed|bh2|bin|bmi|bmp|box)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(btif|bz|bz2|c|c11amc|c11amz|c4g|cab|car)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(cat|ccxml|cdbcmsg|cdf|cdkey|cdmia|cdmic|cdmid|cdmio)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(cdmiq|cdx|cdxml|cdy|cer|cgm|chat|che|chm)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(chrt|cht|cif|cii|cil|cla|class|clkk|clkp)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(clkt|clkw|clkx|clp|cmc|cmdf|cml|cmp|cmx)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(cnc|cod|coda|con|cpi|cpio|cpt|crd|crl)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(cryptonote|csh|csm|csml|csp|css|csv|cu|curl)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(cww|dae|daf|davmount|dbf|dbt|dcr|dcurl|dd2)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(ddd|deb|der|dfac|dig|dir|dis|djvu|dna)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(doc|docm|docx|dotm|dotx|dp|dpg|dra|dsc)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(dsf|dssc|dst|dtb|dtd|dts|dtshd|dus|dvi)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(dwf|dwg|dxf|dxp|dxr|ebkgtar|ecelp4800|ecelp7470|ecelp9600)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(edm|edx|efif|ei6|eml|emma|eol|eot|eps)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(epub|es|es3|esf|etf|etx|evy|exe|exi)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(ext|ez2|ez3|f|f4v|fbs|fcs|fdf|fe_launch)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(fg5|fh|fh4|fh5|fhc|fif|fig|fli|flo)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(flv|flw|flx|fly|fm|fnc|fpt|fpx|frl)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(fsc|fst|ftc|fti|fvt|fxp|fzs|g2w|g3)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(g3w|gac|gdl|geo|gex|ggb|ggt|ghf|gif)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(gim|gmx|gnumeric|gph|gqf|gram|grv|grxml|gsd)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(gsf|gsm|gtar|gtm|gtw|gv|gxt|gz|h261)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(h263|h264|hal|hbci|hdf|hlp|hpgl|hpid|hps)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(hqx|htke|html|hvd|hvp|hvs|i2g|ica|icc)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(ice|ico|ics|ief|ifm|igl|igm|igs|igx)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(iif|imp|ims|ins|ipfix|ipk|ips|ipx|irm)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(irp|itp|ivp|ivr|ivu|jad|jam|jar|java)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(jisp|jlt|jnlp|joda|jpe|jpeg|jpg|jpgv|jpm)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(js|json|karbon|kfo|kia|kml|kmz|kne|kon)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(kpr|ksp|ktx|ktz|kwd|lasxml|latex|lbd|lbe)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(les|lha|link66|lrm|ltf|lvp|lwp|lzh|lzx)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(m21|m3u|m3u8|m4v|ma|mads|mag|man|map)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(mathml|mbd|mbk|mbox|mc1|mcd|mcf|mcurl|mdb)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(mdi|mems|meta4|mets|mfm|mfp|mgp|mgz|mid)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(midi|mif|mj2|mlp|mmd|mmf|mmr|mny|mods)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(mol|mov|movie|mp2|mp3|mp4|mp4a|mpc|mpe)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(mpeg|mpg|mpga|mpire|mpkg|mpl|mpm|mpn|mpp)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(mpy|mqy|mrc|mrcx|mscml|mseq|msf|msh|msl)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(msty|mts|mus|musicxml|mvb|mwf|mxf|mxl|mxml)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(mxs|mxu|n-gage|n2p|n3|nbp|nc|ncx|ngdat)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(nlu|nml|nnd|nns|nnw|npx|nsc|nsf|ntx)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(oa2|oa3|oas|obd|oda|odb|odc|odf|odft)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(odg|odi|odm|odp|ods|odt|ofml|oga|ogv)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(ogx|onetoc|opf|org|osf|osfpvg|otc|otf|otg)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(oth|oti|otp|ots|ott|oxt|p|p10|p12)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(p7b|p7m|p7r|p7s|p8|page|par|paw|pbd)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(pbm|pcf|pcl|pclxl|pcurl|pcx|pdb|pdf|pfa)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(pfr|pgm|pgn|pgp|php3|phtml|pic|pki|pkipath)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(plb|plc|plf|pls|pml|png|pnm|portpkg|pot)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(potm|potx|ppam|ppd|ppm|pps|ppsm|ppsx|ppt)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(pptm|pptx|ppz|pqf|pqi|prc|pre|prf|ps)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(psb|psd|psf|pskcxml|ptid|ptlk|pub|push|pvb)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(pwn|pya|pyv|qam|qbo|qd3|qd3d|qfx|qps)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(qrt|qt|qxd|ra|ram|rar|ras|rcprofile|rdf)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(rdz|rep|res|rgb|rif|rip|rl|rlc|rld)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(rm|rmf|rmp|rms|rnc|roff|rp9|rpm|rpss)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(rpst|rq|rrf|rs|rsd|rss|rtc|rtf|rtx)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(s|saf|sbml|sc|sca|scd|scm|scq|scs)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(scurl|sda|sdc|sdd|sdkm|sdp|sdw|see|seed)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(sema|semd|semf|ser|setpay|setreg|sfd-hdstx|sfs|sgl)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(sgml|sh|shar|shf|shw|sis|sit|sitx|skp)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(sldm|sldx|slt|sm|smf|smi|sml|smp|snd)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(snf|spf|spl|spot|spp|spq|spr|sprite|src)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(sru|srx|sse|ssf|ssml|st|stc|std|stf)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(sti|stk|stl|str|stream|stw|sub|sus|sv4cpio)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(sv4crc|svc|svd|svf|svg|svh|svr|swa|swf)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(swi|sxc|sxd|sxg|sxi|sxm|sxw|t|talk)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(tao|tar|tbk|tcap|tcl|teacher|tei|tex|texi)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(texinfo|tfi|tfm|thmx|tif|tiff|tlk|tmo|tmv)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(torrent|tpl|tpt|tr|tra|trm|tsd|tsi|tsp)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(tsv|ttf|ttl|twd|txd|txf|ufd|umj)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(unityweb|uoml|uri|ustar|utz|uu|uva|uvh|uvi)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(uvm|uvp|uvs|uvu|uvv|vbd|vcd|vcf|vcg)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(vcs|vcx|vgm|vgp|vgx|vis|viv|vivo|vmd)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(vmf|vox|vqe|vqf|vql|vrt|vsd|vsf|vts)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(vtts|vtu|vxml|wad|waf|wan|wav|wax|wbmp)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(wbs|wbxml|weba|webm|webp|wg|wgt|wi|wid)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(wis|wlt|wm|wma|wmd|wmf|wml|wmlc|wmls)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(wmlsc|wmv|wmx|wmz|woff|wpd|wpl|wps|wqd)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(wri|wrl|wrz|wsdl|wspolicy|wtb|wtx|wvx|x3d)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(xap|xar|xbap|xbd|xbm|xdf|xdm|xdp|xdr)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(xdssc|xdw|xenc|xer|xfdf|xfdl|xhtml|xif|xlam)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(xls|xlsb|xlsm|xlsx|xlt|xltm|xltx|xml|xo)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(xop|xpi|xpm|xpr|xps|xpw|xslt|xsm|xspf)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(xul|xwd|xyz|yang|yin|z|zaz|zip|zir)"    .OR. ;
      Lower( cFileName ) LIKE ".+\.(zmm|zpa)" .OR. Empty( cFExt )
         
   oAttach   := TIPMail():new( cContent, "base64" )
      ELSE
      oAttach := TipMail():New( cContent, "7bit" )
      ENDIF

   cFileName := SubStr( cFileName, Rat( cFileName, cDelim ) + 1 )

   oAttach:setFieldPart  ( "Content-Type", cMimeType )
   oAttach:setFieldOption( "Content-Type", "name", cFname + cFext )

   oAttach:setFieldPart  ( "Content-Disposition", "attachment" )
   oAttach:setFieldOption( "Content-Disposition", "filename", cFname + cFext )

RETURN ::attach( oAttach )

METHOD detachFile( cPath ) CLASS TipMail

   LOCAL cContent  := ::getBody()
   LOCAL cFileName := ::getFileName()
   LOCAL cDelim    := HB_OsPathSeparator()
   LOCAL nFileHandle

   IF Empty( cFileName )
      RETURN .F.
   ENDIF

   IF HB_ISSTRING( cPath )
      cFileName := StrTran( cPath + cDelim + cFileName, cDelim+cDelim, cDelim )
   ENDIF

   nFileHandle := FCreate( cFileName )
   IF FError() != 0
      RETURN .F.
   ENDIF

   FWrite( nFileHandle, cContent )

   FClose( nFileHandle )
RETURN FError() == 0

METHOD getFileName() CLASS TipMail

   LOCAL cName

   IF "attachment" $ Lower( ::GetFieldPart( "Content-Disposition" ) )
      cName := ::GetFieldOption( "Content-Disposition", "filename" )
   ELSE
      cName := ::GetFieldOption( "Content-Type", "name" )
   ENDIF

   RETURN StrTran( cName, '"', '' )

METHOD isMultiPart CLASS TipMail
RETURN "multipart/" $ Lower( ::GetFieldPart("Content-Type") )

METHOD getMultiParts( aParts ) CLASS TipMail

   LOCAL oSubPart, lReset := .F.

   ::resetAttachment()

   IF aParts == NIL
      aParts := {}
   ENDIF

   DO WHILE ( oSubPart := ::nextAttachment() ) != NIL
      lReset := .T.
      AAdd( aParts, oSubPart )
      IF oSubPart:countAttachments() > 0
         oSubPart:getMultiparts( aParts )
      ENDIF
   ENDDO

   IF lReset
      ::resetAttachment()
   ENDIF
RETURN aParts
