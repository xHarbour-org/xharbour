/**********************************************
* tipclientsmpt.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclientsmtp.prg,v 1.4 2004/02/07 12:53:55 lculik Exp $
************************************************/
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
