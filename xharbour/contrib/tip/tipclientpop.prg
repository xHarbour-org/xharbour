/**********************************************
* tIPClientpop.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclientpop.prg,v 1.2 2003/11/05 11:06:41 jonnymind Exp $
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
   METHOD Noop()                 // Can be called repeatedly to keep-alive the connection
   METHOD Top( nMsgId )          // Get Headers of mail (no body) to be able to quickly handle a message
   METHOD UIDL( nMsgId )         // Returns Unique ID of message n or list of unique IDs of all message inside maildrop

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


METHOD Noop() CLASS tIPClientPOP
   InetSendAll( ::SocketCon, "NOOP" + ::cCRLF )
RETURN ::GetOk()


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



METHOD Top( nMsgId ) CLASS tIPClientPOP
   LOCAL nPos
   LOCAL cStr, cRet

   InetSendAll( ::SocketCon, "TOP " + Str( nMsgId ) + " 0 " + ::cCRLF )
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



METHOD UIDL( nMsgId ) CLASS tIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   IF ! Empty( nMsgId )
      InetSendAll( ::SocketCon, "UIDL " + Str( nMsgId ) + ::cCRLF )
   ELSE
      InetSendAll( ::SocketCon, "UIDL" + ::cCRLF )
   ENDIF

   IF .not. ::GetOk()
      RETURN NIL
   ENDIF

   IF ! Empty( nMsgId )

      // +OK Space(1) nMsg Space(1) UID
      RETURN SubStr(::cReply, Rat(Space(1), ::cReply) + 1)

   ELSE

      cRet := ""
      DO WHILE cStr != "." .and. InetErrorCode( ::SocketCon ) == 0
         cStr := InetRecvLine( ::SocketCon, @nPos, 256 )
         IF cStr != "."
            cRet += cStr + ::cCRLF
         ELSE
            ::bEof := .T.
         ENDIF

      ENDDO

   ENDIF

   IF InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

RETURN cRet


METHOD Retreive( nId, nLen ) CLASS tIPClientPOP
   LOCAL nPos
   LOCAL cStr, cRet, nRetLen, cBuffer, nRead

   IF .not. ::bInitialized
      InetSendAll( ::SocketCon, "RETR "+ Str( nId ) + ::cCRLF )
      IF .not. ::GetOk()
         ::bEof := .T.
         RETURN NIL
      ENDIF
      ::bInitialized := .T.
   ENDIF

   /* old code, one char at a time, slow
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
   ENDDO*/

   cRet := ""
   nRetLen := 1
   nRead := 0

   /* 04/05/2004 - <maurilio.longo@libero.it>
      Instead of receiving a single char at a time until after we have the full mail, let's receive as
      much as we can and stop when we reach EOM (end of mail :)) sequence. This way is _a lot_ faster
   */
   DO WHILE InetErrorCode( ::SocketCon ) == 0 .AND. iif(! Empty( nLen ), nLen < Len( cRet ), .T.)

      cBuffer := Space(1024)

      nRead := InetRecv( ::SocketCon, @cBuffer, 1024 )

      cRet += Left(cBuffer, nRead)

      IF At(::cCRLF + "." + ::cCRLF, cRet, nRetLen) <> 0
         // Remove ".CRLF"
         cRet := Left(cRet, Len(cRet) - 3)
         EXIT
      ELSE
         nRetLen += nRead
      ENDIF
   ENDDO

   IF InetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   // Remove byte-stuffed termination octet(s) if any
   cRet := StrTran(cRet, ::cCRLF + "..", ::cCRLF + ".")

RETURN cRet


METHOD Delete( nId ) CLASS tIPClientPOP
   InetSendAll( ::SocketCon, "DELE " + AllTrim( Str( nId ) ) +  ::cCRLF )
RETURN ::GetOk()

