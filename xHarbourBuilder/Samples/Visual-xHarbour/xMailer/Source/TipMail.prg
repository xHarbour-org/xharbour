/*
 * $Id: TipMail.prg,v 1.1 2008/09/15 23:02:02 Ella Stern Exp $
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

#include "hbclass.ch"

CLASS TipMail
   DATA hHeaders
   // received fields may be more than once.
   DATA aReceived INIT {}

   METHOD New(cBody, oEncoder )    Constructor
   METHOD SetBody( cBody )
   METHOD GetBody()
   METHOD GetRawBody()              INLINE ::cBody
   METHOD SetEncoder( cEncoder )

   /*
   METHOD FWrite( nFile )
   METHOD FRead( nFile )
   METHOD Send( sSocket )
   METHOD Recv( sSocket )
   */
   METHOD FromString( cString )
   METHOD ToString()

   METHOD GetFieldPart( cField )
   METHOD GetFieldOption( cField )
   METHOD SetFieldPart( cField, cValue )
   METHOD SetFieldOption( cField, cValue )

   METHOD GetContentType() INLINE ::GetFieldPart( "Content-Type" )
   METHOD GetCharEncoding() INLINE ::GetFieldOption( "Content-Type", "encoding" )

   METHOD Attach( oSubPart )
   METHOD NextAttachment()
   METHOD CountAttachments()  INLINE Len( ::aAttachments )
   METHOD GetAttachment()
   METHOD ResetAttachment()   INLINE ::nAttachPos := 1

   METHOD MakeBoundary()
HIDDEN:
   DATA cBody
   DATA oEncoder
   DATA aAttachments
   DATA nAttachPos   INIT 1

ENDCLASS

METHOD New( cBody, oEncoder ) CLASS TipMail

   // Set header fileds to non-sensitive
   ::hHeaders := HSetCaseMatch( {=>}, .F. )
   ::aAttachments := {}

   IF HB_IsString( oEncoder )
      ::oEncoder := TIPEncoder():New( oEncoder )
      IF ::oEncoder == NIL
         Alert( "Invalid encoder " + oEncoder )
         QUIT
      ENDIF
   ELSEIF HB_IsObject( oEncoder ) .and. At("TIPEncoder", oEncoder:ClassName() ) != 0
      ::oEncoder := oEncoder
      ::hHeaders[ "Content-transfer-encoding" ] := oEncoder:cName
   ENDIF

   IF cBody != NIL
      IF ::oEncoder != NIL
         ::cBody := ::oEncoder:Encode( cBody )
      ELSE
         ::cBody := cBody
      ENDIF
      ::hHeaders[ "Content-Length" ] := Ltrim( Str( Len( cBody ) ) )
   ENDIF

RETURN Self


METHOD SetEncoder( cEnc ) CLASS TipMail
   if HB_IsString( cEnc )
      ::oEncoder := TipEncoder():New( cEnc )
   ELSE
      ::oEncoder := cEnc
   ENDIF
   ::hHeaders[ "Content-transfer-encoding" ] := ::oEncoder:cName
RETURN .T.



METHOD SetBody( cBody ) CLASS TipMail
   IF ::oEncoder != NIL
      ::cBody := ::oEncoder:Encode( cBody )
   ELSE
      ::cBody := cBody
   ENDIF
   ::hHeaders[ "Content-Length" ] := Ltrim( Str( Len( cBody ) ) )
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
         ::hHeaders["Content-Type"] := "multipart/mixed"
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
   LOCAL oSubPart
   LOCAL cRet := ""

   // this is a multipart message; we need a boundary
    IF Len( ::aAttachments ) > 0
      ::hHeaders["Mime-Version"] :="1.0"
    endif

   IF Len( ::aAttachments ) > 0
      //Reset failing content type
      IF At( "multipart/", Lower( ::GetFieldPart("Content-Type")) ) == 0
         ::hHeaders["Content-Type"] := "multipart/mixed"
      ENDIF

      // have we got it already?
      cBoundary := ::GetFieldOption( "Content-Type", "Boundary" )
      IF Empty( cBoundary )
         cBoundary := ::MakeBoundary()
         IF .not. ::SetFieldOption( "Content-Type", "Boundary", cBoundary )
            ::hHeaders[ "Content-Type" ] := ;
               'multipart/mixed; boundary="' + cBoundary + '"'
         ENDIF
      ENDIF
   ENDIF

   // Begin output the fields
   // Presenting them in a "well-known" order
   IF "Return-Path" IN ::hHeaders
      cRet+= "Return-Path: "+::hHeaders[ "Return-Path"] + e"\r\n"
   ENDIF
   IF "Delivered-To" IN ::hHeaders
      cRet+= "Delivered-To: "+::hHeaders[ "Delivered-To"] + e"\r\n"
   ENDIF
   FOR EACH cElem IN ::aReceived
      cRet+= "Received: "+ cElem+ e"\r\n"
   NEXT
   IF "Date" IN ::hHeaders
      cRet+= "Date: "+::hHeaders[ "Date"] + e"\r\n"
   ENDIF
   IF "From" IN ::hHeaders
      cRet+= "From: "+::hHeaders[ "From"] + e"\r\n"
   ENDIF
   IF "To" IN ::hHeaders
      cRet+= "To: "+::hHeaders[ "To"] + e"\r\n"
   ENDIF
   IF "Subject" IN ::hHeaders
      cRet+= "Subject: "+ ::hHeaders[ "Subject"] + e"\r\n"
   ENDIF
    IF Len( ::aAttachments ) > 0
      cRet+= "Mime-Version:" + ::hHeaders["Mime-Version"] + e"\r\n"
    endif

   FOR i := 1 TO Len( ::hHeaders )
      cElem := Lower(HGetKeyAt( ::hHeaders, i ))
      IF cElem != "return-path" .and. cElem != "delivered-to" .and.;
            cElem != "date" .and. cElem != "from" .and.;
            cElem != "to" .and. cElem != "subject" .and. cElem !="mime-version"
         cRet += HGetKeyAt( ::hHeaders, i ) + ": " +;
                HGetValueAt( ::hHeaders, i ) + e"\r\n"
      ENDIF
   NEXT

   // end of Header
   cRet += e"\r\n"

   //Body
   IF .not. Empty( ::cBody )
      cRet += ::cBody + e"\r\n"
   ENDIF

   IF .not. Empty( ::aAttachments )
      //Eventually go with mime multipart
      FOR i := 1 TO Len(::aAttachments )
         cRet += "--" + cBoundary + e"\r\n"
         cRet += ::aAttachments[i]:ToString() + e"\r\n"
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
      ::oEncoder := TipEncoder():New( ::hHeaders["Content-Transfer-Encoding"] )
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
         nPos := oSubSection:FromString( cMail, cSubBoundary,;
                   nLinePos + 2 )

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
         nPos := iif( ! Empty(cSubBoundary), At("--" + cSubBoundary, cMail, nPos ), iif( ! Empty(cBoundary), At("--" + cBoundary, cMail, nPos ), nLinePos + 2 ))
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

   cBound += "_TIP_" + StrTran(Dtoc( Date() ),"/","") +;
       "_" + StrTran(Time(), ":", "" )

RETURN cBound
