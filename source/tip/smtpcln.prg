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
#include "tip.ch"

/**
* Inet service manager: smtp
*/

CLASS TIPClientSMTP FROM TIPClient

   METHOD New( oUrl, lTrace, oCredentials )
   METHOD Open( cUrl )
   METHOD CLOSE()
   METHOD Write( cData, nLen, bCommit )
   METHOD Mail( cFrom )
   METHOD Rcpt( cRcpt )
   METHOD DATA( cData )
   METHOD COMMIT()
   METHOD QUIT()
   METHOD GetOK()
   METHOD OpenSecure( cUrl )                   // Method for smtp server that require login
   METHOD AUTH( cUser, cPass )                 // Auth by login method
   METHOD AUTHplain( cUser, cPass )            // Auth by plain method
   METHOD ServerSuportSecure( lAuthp, lAuthl )
   METHOD sendMail( oTipMail )
   DESTRUCTOR SmtpClnDestructor()

   HIDDEN:
   DATA isAuth INIT .F.

ENDCLASS

PROCEDURE SmtpClnDestructor() CLASS TIPClientSMTP

   IF ::lTrace .AND. ::nHandle > 0
      FClose( ::nHandle )
      ::nHandle := - 1
   ENDIF

   RETURN

METHOD New( oUrl, lTrace, oCredentials ) CLASS TIPClientSMTP

   LOCAL cFile := "sendmail"
   LOCAL n := 1

   ::super:New( oUrl, lTrace, oCredentials )

   ::nDefaultPort := 25
   ::nConnTimeout := 5000
   ::nAccessMode  := TIP_WO    // a write only

   if ::lTrace
      IF ! File( "sendmail.log" )
         ::nHandle := FCreate( "sendmail.log" )
      ELSE
         WHILE File( cFile + AllTrim( Str(n,4 ) ) + ".log" )
            n++
         ENDDO
         ::nHandle := FCreate( cFile + AllTrim( Str(n,4 ) ) + ".log" )
      ENDIF
   ENDIF

   RETURN Self

METHOD Open( cUrl ) CLASS TIPClientSMTP

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF ! Empty ( ::oUrl:cUserId )
      ::InetSendAll( ::SocketCon, "HELO " +  ::oUrl:cUserId + ::cCRLF )
   ELSE
      ::InetSendAll( ::SocketCon, "HELO TIPClientSMTP" + ::cCRLF )
   ENDIF

   RETURN ::GetOK()

METHOD GetOK() CLASS TIPClientSMTP

   LOCAL nLen

   ::cReply := ::InetRecvLine( ::SocketCon, @nLen, 512 )
   IF ::InetErrorCode( ::SocketCon ) != 0 .OR. SubStr( ::cReply, 1, 1 ) == '5'
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD CLOSE() CLASS TIPClientSMTP

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   if ::lTrace
      FClose( ::nHandle )
      ::nHandle := - 1
   ENDIF
   ::QUIT()

   RETURN ::super:CLOSE()

METHOD COMMIT() CLASS TIPClientSMTP

   ::InetSendAll( ::SocketCon, ::cCRLF + "." + ::cCRLF )

   RETURN ::GetOK()

METHOD QUIT() CLASS TIPClientSMTP

   ::InetSendAll( ::SocketCon, "QUIT" + ::cCRLF )
   ::isAuth := .F.

   RETURN ::GetOK()

METHOD Mail( cFrom ) CLASS TIPClientSMTP

   ::InetSendAll( ::SocketCon, "MAIL FROM: <" + cFrom + ">" + ::cCRLF )

   RETURN ::GetOK()

METHOD Rcpt( cTo ) CLASS TIPClientSMTP

   ::InetSendAll( ::SocketCon, "RCPT TO: <" + cTo + ">" + ::cCRLF )

   RETURN ::GetOK()

METHOD DATA( cData ) CLASS TIPClientSMTP

   ::InetSendAll( ::SocketCon, "DATA" + ::cCRLF )
   IF ! ::GetOK()
      RETURN .F.
   ENDIF
   ::InetSendAll( ::SocketCon, cData + ::cCRLF + "." + ::cCRLF )

   RETURN ::GetOK()

METHOD OpenSecure( cUrl ) CLASS TIPClientSMTP

   LOCAL cUser

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )

   cUser := ::oUrl:cUserId

   IF ! Empty ( ::oUrl:cUserId )
      ::InetSendAll( ::SocketCon, "EHLO " +  cUser + ::cCRLF )
   ELSE
      ::InetSendAll( ::SocketCon, "EHLO TIPClientSMTP" + ::cCRLF )
   ENDIF

   RETURN ::GetOK()

METHOD AUTH( cUser, cPass ) CLASS TIPClientSMTP

   LOCAL cEncodedUser
   LOCAL cEncodedPAss

   cUser := StrTran( cUser, "&at;", "@" )

   cEncodedUser := AllTrim( HB_BASE64( cuser,Len(cuser ) ) )
   cEncodedPAss := AllTrim( HB_BASE64( cPass,Len(cpass ) ) )


   ::InetSendAll( ::SocketCon, "AUTH LOGIN" + ::cCRLF )

   if ::GetOK()
      ::InetSendAll( ::SocketCon, cEncodedUser + ::cCRLF )
      if ::GetOK()
         ::InetSendAll( ::SocketCon, cEncodedPass + ::cCRLF )
      ENDIF
   ENDIF

   RETURN ( ::isAuth := ::GetOK() )

METHOD AUTHplain( cUser, cPass ) CLASS TIPClientSMTP

   LOCAL cBase := BUILDUSERPASSSTRING( cUser, cPass )
   LOCAL cen   := HB_BASE64( cBase, 2 + Len( cUser ) + Len( cPass ) )

   ::InetSendAll( ::SocketCon, "AUTH PLAIN" + cen + ::cCRLF )

   RETURN ( ::isAuth := ::GetOK() )

METHOD Write( cData, nLen, bCommit ) CLASS TIPClientSMTP

   LOCAL aTo, cRecpt

   IF ! ::bInitialized
      // IF Empty( ::oUrl:cUserId ) .or. Empty( ::oUrl:cFile )
      IF Empty( ::oUrl:cFile )            // GD user id not needed if we did not auth
         RETURN - 1
      ENDIF

      IF ! ::Mail( ::oUrl:cUserId )
         RETURN - 1
      ENDIF
      aTo := hb_regexSplit( ",", ::oUrl:cFile )

      FOR EACH cRecpt in aTo
         IF ! ::Rcpt( cRecpt )
            RETURN - 1
         ENDIF
      NEXT

      ::InetSendAll( ::SocketCon, "DATA" + ::cCRLF )
      IF ! ::GetOK()
         RETURN - 1
      ENDIF
      ::bInitialized := .T.
   ENDIF

   ::nLastWrite := ::super:Write( cData, nLen, bCommit )

   RETURN ::nLastWrite

METHOD ServerSuportSecure( lAuthp, lAuthl ) CLASS TIPClientSMTP

   LOCAL lAuthLogin := .F., lAuthPlain := .F.

   IF ::OpenSecure()
      WHILE .T.
         ::GetOK()
         IF ::cReply == NIL
            EXIT
         ELSEIF "LOGIN" IN ::cReply
            lAuthLogin := .T.
         ELSEIF "PLAIN" IN ::cReply
            lAuthPlain := .T.
         ENDIF
      ENDDO
      ::CLOSE()
   ENDIF

   lAuthp := lAuthPlain
   lAuthl := lAuthLogin

   RETURN lAuthLogin .OR. lAuthPlain

METHOD sendMail( oTipMail ) CLASS TIPClientSMTP

   LOCAL cFrom, cTo, aTo
 
   IF ! ::isOpen
      RETURN .F.
   ENDIF

   IF ! ::isAuth
      ::AUTH( ::oUrl:cUserId, ::oUrl:cPassword )
      IF ! ::isAuth
         RETURN .F.
      ENDIF
   ENDIF

   cFrom := oTipMail:GetFieldPart( "From" )
   cTo   := oTipMail:GetFieldPart( "To" )

   cTo   := StrTran( cTo, InetCrLf(), "" )
   cTo   := StrTran( cTo, Chr(  9 ) , "" )
   cTo   := StrTran( cTo, Chr( 32 ) , "" )

   aTo   := hb_regexSplit( ",", cTo )

   ::Mail( cFrom )
   FOR EACH cTo IN aTo
      ::Rcpt( cTo )
   NEXT

   RETURN ::DATA( oTipMail:ToString() )
