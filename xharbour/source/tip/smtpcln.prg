/*
 * $Id: tipmail.prg,v 1.26 2004/04/08 13:26:53 druzus Exp $
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
* Inet service manager: pop3
*/

CLASS tIPClientSMTP FROM tIPClient

   METHOD New()
   METHOD Open()
   METHOD Close()
   METHOD Write( cData, nLen, bCommit )
   METHOD Mail( cFrom )
   METHOD Rcpt( cRcpt )
   METHOD Data( cData )
   METHOD Commit()
   METHOD Quit()
   METHOD GetOK()
   /* Method for smtp server that require login */
   METHOD OpenSecure()
   METHOD AUTH( cUser, cPass) // Auth by login method
   METHOD AUTHplain( cUser, cPass) // Auth by plain method
   METHOD ServerSuportSecure(lAuthp,lAuthl) 

ENDCLASS

METHOD New() CLASS tIPClientSMTP
   ::nDefaultPort := 25
   ::nConnTimeout := 5000
   ::nAccessMode := TIP_WO  // a write only
RETURN Self

METHOD Open() CLASS tIPClientSMTP

   IF .not. ::super:Open()
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF .not. Empty ( ::oUrl:cUserid )
      InetSendAll( ::SocketCon, "HELO " +  ::oUrl:cUserid + ::cCRLF )
   ELSE
      InetSendAll( ::SocketCon, "HELO tipClientSMTP" + ::cCRLF )
   ENDIF

RETURN ::GetOk()


METHOD GetOk() CLASS tIPClientSMTP
   LOCAL nLen

   ::cReply := InetRecvLine( ::SocketCon, @nLen, 128 )
   IF InetErrorCode( ::SocketCon ) != 0 .or. Substr( ::cReply, 1, 1 ) == '5'
      RETURN .F.
   ENDIF
RETURN .T.


METHOD Close() CLASS tIPClientSMTP
   InetSetTimeOut( ::SocketCon, ::nConnTimeout )
   ::Quit()
RETURN ::super:Close()

METHOD Commit() CLASS tIPClientSMTP
   InetSendAll( ::SocketCon, ::cCRLF + "." + ::cCRLF )
RETURN ::GetOk()


METHOD Quit() CLASS tIPClientSMTP
   InetSendAll( ::SocketCon, "QUIT" + ::cCRLF )
RETURN ::GetOk()


METHOD Mail( cFrom ) CLASS tIPClientSMTP
   InetSendAll( ::SocketCon, "MAIL FROM: <" + cFrom +">" + ::cCRLF )
RETURN ::GetOk()


METHOD Rcpt( cTo ) CLASS tIPClientSMTP
   InetSendAll( ::SocketCon, "RCPT TO: <" + cTo + ">" + ::cCRLF )
RETURN ::GetOk()


METHOD Data( cData ) CLASS tIPClientSMTP
   InetSendAll( ::SocketCon, "DATA" + ::cCRLF )
   IF .not. ::GetOk()
      RETURN .F.
   ENDIF
   InetSendAll(::SocketCon, cData + ::cCRLF + "." + ::cCRLF )
RETURN ::GetOk()



METHOD OpenSecure( ) CLASS tIPClientSMTP

   Local cUser

   IF .not. ::super:Open()
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )

   cUser := ::oUrl:cUserid

   IF .not. Empty ( ::oUrl:cUserid )
      InetSendAll( ::SocketCon, "EHLO " +  cUser + ::cCRLF )
   ELSE
      InetSendAll( ::SocketCon, "EHLO tipClientSMTP" + ::cCRLF )
   ENDIF

RETURN ::getOk()

METHOD AUTH( cUser, cPass) CLASS tIPClientSMTP

   Local cs:=''
   Local cEncodedUser
   Local cEncodedPAss

   cUser := StrTran( cUser,"&at;", "@")

   cEncodedUser := alltrim(HB_BASE64(cuser,len(cuser)))
   cEncodedPAss :=alltrim(HB_BASE64(cPass,len(cpass)))


   InetSendAll( ::SocketCon, "AUTH LOGIN " +::ccrlf )

   if ::GetOk()
      InetSendAll( ::SocketCon, cEncodedUser+::cCrlf  )
      if ::Getok()
         InetSendAll( ::SocketCon, cEncodedPass +::cCrlf )
      endif
   endif

   return ::GetOk()

METHOD AuthPlain( cUser, cPass) CLASS tIPClientSMTP

   Local cBase := BUILDUSERPASSSTRING( cUser, cPass )
   Local cen   := HB_BASE64( cBase, 2 + Len( cUser ) + Len( cPass ) )

   InetSendAll( ::SocketCon, "AUTH PLAIN " + cen + ::cCrlf)
   return ::GetOk()

METHOD Write( cData, nLen, bCommit ) CLASS tIPClientSMTP
Local aTo,cRecpt
   IF .not. ::bInitialized
      IF Empty( ::oUrl:cUserid ) .or. Empty( ::oUrl:cFile )
         RETURN -1
      ENDIF

      IF .not. ::Mail( ::oUrl:cUserid )
         RETURN -1
      ENDIF
      aTo:= HB_RegexSplit(",", ::oUrl:cFile )

      FOR each cRecpt in Ato
         IF .not.   ::Rcpt(cRecpt)
            RETURN -1
         ENDIF
      NEXT

      InetSendAll( ::SocketCon, "DATA" + ::cCRLF )
      IF .not. ::GetOk()
         RETURN -1
      ENDIF
      ::bInitialized := .T.
   ENDIF

   ::nLastWrite := ::super:Write( cData, nLen, bCommit )
RETURN ::nLastWrite

METHOD ServerSuportSecure(lAuthp,lAuthl) CLASS  tIPClientSMTP
   Local lAuthLogin := .F.,lAuthPlain :=.F.

   IF ::OPENSECURE()
      WHILE .T.
         ::GetOk()
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

   lAuthp:=lAuthPlain
   lAuthl:=lAuthLogin

RETURN  lAuthLogin .OR. lAuthPlain
