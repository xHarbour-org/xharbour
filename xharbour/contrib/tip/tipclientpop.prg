/**********************************************
* tIPClientpop.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclientpop.prg,v 1.1 2003/02/22 16:44:46 jonnymind Exp $
************************************************/
#include "hbclass.ch"

/**
* Inet service manager: pop3
*/

CLASS tIPClientPOP FROM tIPClient

   METHOD New()
   METHOD Open()
   METHOD Close()
   METHOD Read( iLen )
   METHOD Stat()
   METHOD List()
   METHOD Retreive( nMsgId )
   METHOD Delete()
   METHOD Quit()

   METHOD GetOK()

ENDCLASS


METHOD New() CLASS tIPClientPOP
   ::nDefaultPort := 110
   ::nConnTimeout := 10000
RETURN Self


METHOD Open() CLASS tIPClientPOP
   IF .not. ::super:Open()
      RETURN .F.
   ENDIF

   IF Empty ( ::oUrl:cUserid ) .or. Empty ( ::oUrl:cPassword )
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF ::GetOk()
      InetSendAll( ::SocketCon, "USER " + ::oUrl:cUserid + ::cCRLF )
      IF ::GetOK()
         InetSendAll( ::SocketCon, "PASS " + ::oUrl:cPassword + ::cCRLF )
         IF ::GetOK()
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF
RETURN .F.


METHOD GetOk() CLASS tIPClientPOP
   LOCAL nLen

   ::cReply := InetRecvLine( ::SocketCon, @nLen, 128 )
   IF InetErrorCode( ::SocketCon ) != 0 .or. ::cReply[1] != '+'
      RETURN .F.
   ENDIF
RETURN .T.


METHOD Close() CLASS tIPClientPOP
   InetSetTimeOut( ::SocketCon, ::nConnTimeout )
   ::Quit()
RETURN ::super:Close()


METHOD Quit() CLASS tIPClientPOP
   InetSendAll( ::SocketCon, "QUIT" + ::cCRLF )
RETURN ::GetOk()


METHOD Stat() CLASS tIPClientPOP
   LOCAL nRead
   InetSendAll( ::SocketCon, "STAT" + ::cCRLF )
RETURN InetRecvLine( ::SocketCon, @nRead, 128)


METHOD Read( nLen ) CLASS tIPClientPOP
   /** Set what to read for */
   IF Empty( ::oUrl:cFile )
      RETURN ::List()
   ENDIF

   IF Val (::oUrl:cFile ) < 0
      IF ::Delete( -  Val (::oUrl:cFile ) )
         RETURN ::Quit()
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

RETURN ::Retreive( Val (::oUrl:cFile ), nLen )



METHOD List() CLASS tIPClientPOP
   LOCAL nPos
   LOCAL cStr, cRet

   InetSendAll( ::SocketCon, "LIST" + ::cCRLF )
   IF .not. ::GetOk()
      RETURN NIL
   ENDIF

   cRet := ""
   DO WHILE cStr != "." .and. InetErrorCode( ::SocketCon ) == 0
      cStr := InetRecvLine( ::SocketCon, @nPos, 256 )
      IF cStr != "."
         cRet += cStr + ::cCRLF
      ELSE
         ::bEof := .T.
      ENDIF

   ENDDO

   IF InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

RETURN cRet



METHOD Retreive( nId, nLen ) CLASS tIPClientPOP
   LOCAL nPos
   LOCAL cStr, cRet

   IF .not. ::bInitialized
      InetSendAll( ::SocketCon, "RETR "+ Str( nId ) + ::cCRLF )
      IF .not. ::GetOk()
         ::bEof := .T.
         RETURN NIL
      ENDIF
      ::bInitialized := .T.
   ENDIF

   cRet := ""
   DO WHILE InetErrorCode( ::SocketCon ) == 0
      cStr := InetRecvLine( ::SocketCon, @nPos, 1024 )
      IF cStr != NIL
         IF cStr == "."
            ::bEof := .T.
            EXIT
         ELSE
            cRet += cStr + ::cCRLF
            IF .not. Empty( nLen ) .and. nLen < Len( cRet )
               EXIT
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   IF InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

RETURN cRet


METHOD Delete( nId ) CLASS tIPClientPOP
   InetSendAll( ::SocketCon, "DELE " + AllTrim( Str( nId ) ) +  ::cCRLF )
RETURN ::GetOk()

