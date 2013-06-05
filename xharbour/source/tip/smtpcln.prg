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

CLASS tIPClientSMTP FROM tIPClient

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

PROCEDURE SmtpClnDestructor() CLASS tIPClientSMTP

   IF ::lTrace .AND. ::nHandle > 0
      FClose( ::nHandle )
      ::nHandle := - 1
   ENDIF

   RETURN

METHOD New( oUrl, lTrace, oCredentials ) CLASS tIPClientSMTP

   LOCAL cFile := "sendmail"
   LOCAL n := 1

   ::super:New( oUrl, lTrace, oCredentials )

   ::nDefaultPort := 25
   ::nConnTimeout := 5000
   ::nAccessMode  := TIP_WO    // a write only

   if ::lTrace
      IF !File( "sendmail.log" )
         ::nHandle := FCreate( "sendmail.log" )
      ELSE
         WHILE File( cFile + AllTrim( Str(n,4 ) ) + ".log" )
            n++
         ENDDO
         ::nHandle := FCreate( cFile + AllTrim( Str(n,4 ) ) + ".log" )
      ENDIF
   ENDIF

   RETURN Self

METHOD Open( cUrl ) CLASS tIPClientSMTP

   IF .NOT. ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF .NOT. Empty ( ::oUrl:cUserId )
      ::InetSendall( ::SocketCon, "HELO " +  ::oUrl:cUserId + ::cCRLF )
   ELSE
      ::InetSendall( ::SocketCon, "HELO tipClientSMTP" + ::cCRLF )
   ENDIF

   RETURN ::GetOK()

METHOD GetOK() CLASS tIPClientSMTP

   LOCAL nLen

   ::cReply := ::InetRecvLine( ::SocketCon, @nLen, 512 )
   IF ::InetErrorCode( ::SocketCon ) != 0 .OR. SubStr( ::cReply, 1, 1 ) == '5'
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD CLOSE() CLASS tIPClientSMTP

   InetSetTimeOut( ::SocketCon, ::nConnTimeout )
   if ::lTrace
      FClose( ::nHandle )
      ::nHandle := - 1
   ENDIF
   ::QUIT()

   RETURN ::super:CLOSE()

METHOD COMMIT() CLASS tIPClientSMTP

   ::InetSendall( ::SocketCon, ::cCRLF + "." + ::cCRLF )

   RETURN ::GetOK()

METHOD QUIT() CLASS tIPClientSMTP

   ::InetSendall( ::SocketCon, "QUIT" + ::cCRLF )
   ::isAuth := .F.

   RETURN ::GetOK()

METHOD Mail( cFrom ) CLASS tIPClientSMTP

   ::InetSendall( ::SocketCon, "MAIL FROM: <" + cFrom + ">" + ::cCRLF )

   RETURN ::GetOK()

METHOD Rcpt( cTo ) CLASS tIPClientSMTP

   ::InetSendall( ::SocketCon, "RCPT TO: <" + cTo + ">" + ::cCRLF )

   RETURN ::GetOK()

METHOD DATA( cData ) CLASS tIPClientSMTP

   ::InetSendall( ::SocketCon, "DATA" + ::cCRLF )
   IF .NOT. ::GetOK()
      RETURN .F.
   ENDIF
   ::InetSendall( ::SocketCon, cData + ::cCRLF + "." + ::cCRLF )

   RETURN ::GetOK()

METHOD OpenSecure( cUrl ) CLASS tIPClientSMTP

   LOCAL cUser

   IF .NOT. ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )

   cUser := ::oUrl:cUserId

   IF .NOT. Empty ( ::oUrl:cUserId )
      ::InetSendall( ::SocketCon, "EHLO " +  cUser + ::cCRLF )
   ELSE
      ::InetSendall( ::SocketCon, "EHLO tipClientSMTP" + ::cCRLF )
   ENDIF

   RETURN ::GetOK()

METHOD AUTH( cUser, cPass ) CLASS tIPClientSMTP

   LOCAL cEncodedUser
   LOCAL cEncodedPAss

   cUser := StrTran( cUser, "&at;", "@" )

   cEncodedUser := AllTrim( HB_BASE64( cuser,Len(cuser ) ) )
   cEncodedPAss := AllTrim( HB_BASE64( cPass,Len(cpass ) ) )


   ::InetSendall( ::SocketCon, "AUTH LOGIN" + ::cCRLF )

   if ::GetOK()
      ::InetSendall( ::SocketCon, cEncodedUser + ::cCRLF )
      if ::GetOK()
         ::InetSendall( ::SocketCon, cEncodedPass + ::cCRLF )
      ENDIF
   ENDIF

   RETURN ( ::isAuth := ::GetOK() )

METHOD AUTHplain( cUser, cPass ) CLASS tIPClientSMTP

   LOCAL cBase := BUILDUSERPASSSTRING( cUser, cPass )
   LOCAL cen   := HB_BASE64( cBase, 2 + Len( cUser ) + Len( cPass ) )

   ::InetSendall( ::SocketCon, "AUTH PLAIN" + cen + ::cCRLF )

   RETURN ( ::isAuth := ::GetOK() )

METHOD Write( cData, nLen, bCommit ) CLASS tIPClientSMTP

   LOCAL aTo, cRecpt

   IF .NOT. ::bInitialized
      //IF Empty( ::oUrl:cUserId ) .or. Empty( ::oUrl:cFile )
      IF Empty( ::oUrl:cFile )  //GD user id not needed if we did not auth
         RETURN - 1
      ENDIF

      IF .NOT. ::Mail( ::oUrl:cUserId )
         RETURN - 1
      ENDIF
      aTo := hb_regexSplit( ",", ::oUrl:cFile )

      FOR EACH cRecpt in Ato
         IF .NOT.   ::Rcpt( cRecpt )
            RETURN - 1
         ENDIF
      NEXT

      ::InetSendall( ::SocketCon, "DATA" + ::cCRLF )
      IF .NOT. ::GetOK()
         RETURN - 1
      ENDIF
      ::bInitialized := .T.
   ENDIF

   ::nLastWrite := ::super:Write( cData, nLen, bCommit )

   RETURN ::nLastWrite

METHOD ServerSuportSecure( lAuthp, lAuthl ) CLASS tIPClientSMTP

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

METHOD sendMail( oTipMail ) CLASS tIPClientSMTP

   LOCAL cFrom, cTo, aTo
 
   IF .NOT. ::isOpen
      RETURN .F.
   ENDIF

   IF .NOT. ::isAuth
      ::AUTH( ::oUrl:cUserId, ::oUrl:cPassword )
      IF .NOT. ::isAuth
         RETURN .F.
      ENDIF
   ENDIF

   cFrom := oTipMail:GetFieldPart( "From" )
   cTo   := oTipMail:GetFieldPart( "To" )

   cTo   := StrTran( cTo, InetCRLF(), "" )
   cTo   := StrTran( cTo, Chr(  9 ) , "" )
   cTo   := StrTran( cTo, Chr( 32 ) , "" )

   aTo   := hb_regexSplit( ",", cTo )

   ::Mail( cFrom )
   FOR EACH cTo IN aTo
      ::Rcpt( cTo )
   NEXT

   RETURN ::DATA( oTipMail:ToString() )
