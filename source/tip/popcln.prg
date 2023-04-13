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

#include "hbclass.ch"

/**
* Inet service manager: pop3
*/

CLASS TIPClientPOP FROM TIPClient

   METHOD New( oUrl, lTrace, oCredentials )
   METHOD Open( cUrl )
   METHOD CLOSE()
   METHOD READ( nLen )
   METHOD STAT()
   METHOD LIST()
   METHOD Retrieve( nId, nLen )
   METHOD DELETE( nId )
   METHOD QUIT()
   METHOD NoOp()                 // Can be called repeatedly to keep-alive the connection
   METHOD Top( nMsgId )          // Get Headers of mail (no body) to be able to quickly handle a message
   METHOD UIDL( nMsgId )         // Returns Unique ID of message n or list of unique IDs of all message inside maildrop
   METHOD GetOK()
   METHOD countMail()
   METHOD retrieveAll( lDelete, bAllBlock, bEachBlock )
   DESTRUCTOR PopClnDestructor()

ENDCLASS

PROCEDURE PopClnDestructor() CLASS TIPClientPOP

   IF ::lTrace .AND. ::nHandle > 0
      FClose( ::nHandle )
      ::nHandle := - 1
   ENDIF

   RETURN

METHOD New( oUrl, lTrace, oCredentials ) CLASS TIPClientPOP

   LOCAL cFile := "pop3"
   LOCAL n := 0

   ::super:New( oUrl, lTrace, oCredentials )

   ::nDefaultPort := 110
   ::nConnTimeout := 10000

   if ::lTrace
      IF ! File( "pop3.log" )
         ::nHandle := FCreate( "pop3.log" )
      ELSE
         WHILE File( cFile + AllTrim( Str(n,4 ) ) + ".log" )
            n++
         ENDDO
         ::nHandle := FCreate( cFile + AllTrim( Str(n,4 ) ) + ".log" )
      ENDIF
   ENDIF

   RETURN Self

METHOD Open( cUrl ) CLASS TIPClientPOP

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   IF Empty ( ::oUrl:cUserId ) .OR. Empty ( ::oUrl:cPassword )
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF ::GetOK()
      ::InetSendAll( ::SocketCon, "USER " + ::oUrl:cUserId + ::cCRLF )
      IF ::GetOK()
         ::InetSendAll( ::SocketCon, "PASS " + ::oUrl:cPassword + ::cCRLF )
         IF ::GetOK()
            ::isOpen := .T.
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

METHOD GetOK() CLASS TIPClientPOP

   LOCAL nLen

   ::cReply := ::InetRecvLine( ::SocketCon, @nLen, 128 )
   IF ::InetErrorCode( ::SocketCon ) != 0 .OR. ::cReply[1] != '+'
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD NoOp() CLASS TIPClientPOP

   ::InetSendAll( ::SocketCon, "NOOP" + ::cCRLF )

   RETURN ::GetOK()

METHOD CLOSE() CLASS TIPClientPOP

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   if ::lTrace
      FClose( ::nHandle )
      ::nHandle := - 1
   ENDIF

   ::QUIT()

   RETURN ::super:CLOSE()

METHOD QUIT() CLASS TIPClientPOP

   ::InetSendAll( ::SocketCon, "QUIT" + ::cCRLF )

   RETURN ::GetOK()

METHOD STAT() CLASS TIPClientPOP

   LOCAL nRead

   ::InetSendAll( ::SocketCon, "STAT" + ::cCRLF )

   RETURN ::InetRecvLine( ::SocketCon, @nRead, 128 )

METHOD READ( nLen ) CLASS TIPClientPOP

   // Set what to read for
   IF Empty( ::oUrl:cFile )
      RETURN ::LIST()
   ENDIF

   IF Val ( ::oUrl:cFile ) < 0
      IF ::DELETE( - Val ( ::oUrl:cFile ) )
         RETURN ::QUIT()
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   RETURN ::Retrieve( Val ( ::oUrl:cFile ), nLen )

METHOD Top( nMsgId ) CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   ::InetSendAll( ::SocketCon, "TOP " + Str( nMsgId ) + " 0 " + ::cCRLF )
   IF ! ::GetOK()
      RETURN NIL
   ENDIF

   cRet := ""
   DO WHILE cStr != "." .AND. ::InetErrorCode( ::SocketCon ) == 0
      cStr := ::InetRecvLine( ::SocketCon, @nPos, 256 )
      IF cStr != "."
         cRet += cStr + ::cCRLF
      ELSE
         ::bEof := .T.
      ENDIF

   ENDDO

   IF ::InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   RETURN cRet

METHOD LIST() CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   ::InetSendAll( ::SocketCon, "LIST" + ::cCRLF )
   IF ! ::GetOK()
      RETURN ""
   ENDIF

   cRet := ""
   DO WHILE cStr != "." .AND. ::InetErrorCode( ::SocketCon ) == 0
      cStr := ::InetRecvLine( ::SocketCon, @nPos, 256 )
      IF cStr != "."
         cRet += cStr + ::cCRLF
      ELSE
         ::bEof := .T.
      ENDIF

   ENDDO

   IF ::InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   RETURN cRet

METHOD UIDL( nMsgId ) CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   IF ! Empty( nMsgId )
      ::InetSendAll( ::SocketCon, "UIDL " + Str( nMsgId ) + ::cCRLF )
   ELSE
      ::InetSendAll( ::SocketCon, "UIDL" + ::cCRLF )
   ENDIF

   IF ! ::GetOK()
      RETURN NIL
   ENDIF

   IF ! Empty( nMsgId )

      // +OK Space(1) nMsg Space(1) UID
      RETURN SubStr( ::cReply, RAt( Space(1 ), ::cReply ) + 1 )

   ELSE

      cRet := ""
      DO WHILE cStr != "." .AND. ::InetErrorCode( ::SocketCon ) == 0
         cStr := ::InetRecvLine( ::SocketCon, @nPos, 256 )
         IF cStr != "."
            cRet += cStr + ::cCRLF
         ELSE
            ::bEof := .T.
         ENDIF

      ENDDO

   ENDIF

   IF ::InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   RETURN cRet

METHOD Retrieve( nId, nLen ) CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cRet, nRetLen, cBuffer, nRead
   LOCAL cEOM := ::cCRLF + "." + ::cCRLF        // End Of Mail

   IF ! ::bInitialized
      ::InetSendAll( ::SocketCon, "RETR " + Str( nId ) + ::cCRLF )
      IF ! ::GetOK()
         ::bEof := .T.
         RETURN NIL
      ENDIF
      ::bInitialized := .T.
   ENDIF

   cRet    := ""
   nRetLen := 0

   /* 04/05/2004 - <maurilio.longo@libero.it>
      Instead of receiving a single char at a time until after we have the full mail, let's receive as
      much as we can and stop when we reach EOM (end of mail :)) sequence. This way is _a lot_ faster
   */
   DO WHILE ::InetErrorCode( ::SocketCon ) == 0 .AND. ! ::bEof

      cBuffer := Space( 1024 )
      nRead   := ::InetRecv( ::SocketCon, @cBuffer, 1024 )

      cRet += Left( cBuffer, nRead )

      /* 24/11/2005 - <maurilio.longo@libero.it>
         "- Len( cEOM )" to be sure to always find a full EOM,
         otherwise if response breaks EOM in two, it will never be found
      */
      IF ( nPos := At( cEOM, cRet, Max( nRetLen - Len( cEOM ), 1 ) ) ) <> 0
         // Remove ".CRLF"
         cRet   := Left( cRet, nPos + 1 )
         ::bEof := .T.

      ELSEIF ! Empty( nLen ) .AND. nLen < Len( cRet )
         EXIT

      ELSE
         nRetLen += nRead

      ENDIF

   ENDDO

   IF ::InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   // Remove byte-stuffed termination octet(s) if any
   RETURN StrTran( cRet, ::cCRLF + "..", ::cCRLF + "." )

METHOD DELETE( nId ) CLASS TIPClientPOP

   ::InetSendAll( ::SocketCon, "DELE " + AllTrim( Str( nId ) ) +  ::cCRLF )

   RETURN ::GetOK()

METHOD countMail() CLASS TIPClientPop

   LOCAL aMails

   IF ::isOpen
      ::Reset()
      aMails := hb_ATokens( StrTran( ::LIST(), Chr(13 ),'' ), Chr( 10 ) )
      RETURN Len( aMails )
   ENDIF

   RETURN - 1

METHOD retrieveAll( lDelete, bAllBlock, bEachBlock ) CLASS TIPClientPop

   LOCAL aMails, i, imax, cMail

   IF ! HB_ISLOGICAL( lDelete )
      lDelete := .F.
   ENDIF

   IF ! ::isOpen
      RETURN NIL
   ENDIF

   imax   := ::countMail()
   aMails := Array( imax )

   FOR i := 1 TO imax
      ::Reset()
      cMail     := ::Retrieve( i )
      aMails[i] := TipMail():New()
      aMails[i]:fromString( cMail, , , bEachBlock )

      IF ValType( bAllBlock ) == 'B'
         Eval( bAllBlock, i, iMax, aMails )
      ENDIF

      IF lDelete
         ::Reset()
         ::DELETE( i )
      ENDIF
   NEXT

   RETURN aMails
