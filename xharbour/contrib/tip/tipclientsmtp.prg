/**********************************************
* tipclientsmpt.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclientsmtp.prg,v 1.2 2003/07/13 19:18:38 jonnymind Exp $
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


METHOD Write( cData, nLen, bCommit ) CLASS tIPClientSMTP
   IF .not. ::bInitialized
      IF Empty( ::oUrl:cUserid ) .or. Empty( ::oUrl:cFile )
         RETURN -1
      ENDIF
      IF .not. ::Mail( ::oUrl:cUserid ) .or. .not. ::Rcpt( ::oUrl:cFile )
         RETURN -1
      ENDIF
      InetSendAll( ::SocketCon, "DATA" + ::cCRLF )
      IF .not. ::GetOk()
         RETURN -1
      ENDIF
      ::bInitialized := .T.
   ENDIF

   ::nLastWrite := ::super:Write( cData, nLen, bCommit )
RETURN ::nLastWrite

