/**********************************************
* tIPClienthttp.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclientftp.prg,v 1.5 2003/11/20 16:44:33 jonnymind Exp $
************************************************/
#include "hbclass.ch"
#include "tip.ch"


/**
* Inet service manager: ftp
*/

CLASS tIPClientFTP FROM tIPClient
   DATA nDataPort
   DATA cDataServer
   DATA bUsePasv
   DATA RegBytes
   DATA RegPasv
   // Socket opened in respose to a port command
   DATA SocketControl

   METHOD New()
   METHOD Open()
   METHOD Read( nLen )
   METHOD Write( nLen )
   METHOD Close()
   METHOD TransferStart()
   METHOD Commit()

   METHOD GetReply()
   METHOD Pasv()
   METHOD TypeI()
   METHOD TypeA()
   METHOD List()
   METHOD Cwd()
   METHOD Dele()
   //METHOD Port()
   //METHOD SendPort()
   METHOD Retr()
   METHOD Stor()
   METHOD Quit()
   METHOD ScanLength()

ENDCLASS


METHOD New() CLASS tIPClientFTP
   ::nDefaultPort := 21
   ::nConnTimeout := 3000
   ::bUsePasv := .T.
   ::nAccessMode := TIP_RW  // a read-write protocol

   // precompilation of regex for better prestations
   ::RegBytes := HB_RegexComp( "\(([0-9]+)[ )a-zA-Z]" )
   ::RegPasv :=  HB_RegexComp( ;
      "([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*)" )
RETURN Self


METHOD Open() CLASS tIPClientFTP

   IF Len( ::oUrl:cUserid ) == 0 .or. Len( ::oUrl:cPassword ) == 0
      RETURN .F.
   ENDIF

   IF .not. ::super:Open()
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF ::GetReply()
      InetSendAll( ::SocketCon, "USER " + ::oUrl:cUserid + ::cCRLF )
      IF ::GetReply()
         InetSendAll( ::SocketCon, "PASS " + ::oUrl:cPassword + ::cCRLF )
         // set binary by default
         IF ::GetReply() .and. ::TypeI()
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF
RETURN .F.


METHOD GetReply() CLASS tIPClientFTP
   LOCAL nLen
   LOCAL cRep

   ::cReply := InetRecvLine( ::SocketCon, @nLen, 128 )
   IF ::cReply == NIL
      RETURN .F.
   ENDIF

   // now, if the reply has a '-' as fuorth character, we need to proceed...
   cRep := ::cReply
   DO WHILE .not. Empty(cRep) .and. cRep[4] == '-'
      cRep := InetRecvLine( ::SocketCon, @nLen, 128 )
   ENDDO

   // 4 and 5 are error codes
   IF InetErrorCode( ::SocketCon ) != 0 .or. ::cReply[1] >= '4'
      RETURN .F.
   ENDIF
RETURN .T.


METHOD Pasv() CLASS tIPClientFTP
   LOCAL aRep

   InetSendAll( ::SocketCon, "PASV" + ::cCRLF )
   IF .not. ::GetReply()
      RETURN .F.
   ENDIF
   aRep := HB_Regex( ::RegPasv, ::cReply )

   IF Empty(aRep)
      RETURN .F.
   ENDIF

   ::cDataServer := aRep[2] + "." + aRep[3] + "." + aRep[4] + "." + aRep[5]
   ::nDataPort := Val(aRep[6]) *256 + Val( aRep[7] )

RETURN .T.


METHOD Close() CLASS tIPClientFTP
   InetSetTimeOut( ::SocketCon, ::nConnTimeout )
   ::Quit()
RETURN ::super:Close()


METHOD Quit() CLASS tIPClientFTP
   InetSendAll( ::SocketCon, "QUIT" + ::cCRLF )
RETURN ::GetReply()


METHOD TypeI() CLASS tIPClientFTP
   InetSendAll( ::SocketCon, "TYPE I" + ::cCRLF )
RETURN ::GetReply()


METHOD TypeA() CLASS tIPClientFTP
   InetSendAll( ::SocketCon, "TYPE A" + ::cCRLF )
RETURN ::GetReply()


METHOD CWD( cPath ) CLASS tIPClientFTP
   InetSendAll( ::SocketCon, "CWD " + cPath + ::cCRLF )
RETURN ::GetReply()

METHOD DELE( cPath ) CLASS tIPClientFTP
   InetSendAll( ::SocketCon, "DELE " + cPath + ::cCRLF )
RETURN ::GetReply()


// scan last reply for an hint of lenght
METHOD ScanLength() CLASS tIPClientFTP
   LOCAL aBytes
   aBytes := HB_Regex( ::RegBytes, ::cReply )
   IF .not. Empty(aBytes)
      ::nLength = Val( aBytes[2] )
   ENDIF
RETURN .T.


METHOD TransferStart() CLASS tIPClientFTP
   ::SocketControl := ::SocketCon

   IF ::bUsePasv
      ::SocketCon := InetConnectIP( ::cDataServer, ::nDataPort )
      InetSetTimeout( ::SocketCon, ::nConnTimeout )
   /*ELSE
      ::SocketCon := InetAccept( ::SocketPortServer )*/
   ENDIF

   IF InetErrorCode( ::SocketCon ) == 0
      RETURN .T.
   ENDIF
RETURN .F.

METHOD Commit() CLASS tIPClientFTP
   ::SocketCon := ::SocketControl
   IF ::GetReply() .and. ::GetReply()
      RETURN .T.
   ENDIF
RETURN .F.


METHOD List() CLASS tIPClientFTP
   LOCAL cStr

   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

/*   IF .not. ::bUsePasv
      IF .not. ::Port()
         RETURN .F.
      ENDIF
   ENDIF
*/
   InetSendAll( ::SocketCon, "LIST" + ::cCRLF )
   cStr := ::ReadAuxPort()
   IF ::GetReply()
      IF .not. ::GetReply()
         RETURN NIL
      ENDIF
   ENDIF
RETURN cStr


METHOD Retr( cFile ) CLASS tIPClientFTP
   LOCAL nTimeout

   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

   InetSendAll( ::SocketCon, "RETR " + cFile+ ::cCRLF )
   nTimeout := InetGetTimeout( ::SocketCon )
   InetSetTimeout( ::SocketCon, 100 )
   // have we got a timeout? then the server is waiting for us on the
   // other line...
   IF .not. ::GetReply() .and. InetErrorCode( ::SocketCon ) == -1
      InetSetTimeout( ::SocketCon, nTimeout )
      ::TransferStart()
      RETURN .T.
   ENDIF
   InetSetTimeout( ::SocketCon, nTimeout )
RETURN .F.


METHOD Stor( cFile ) CLASS tIPClientFTP
   LOCAL nTimeout

   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

   InetSendAll( ::SocketCon, "STOR " + cFile+ ::cCRLF )
   nTimeout := InetGetTimeout( ::SocketCon )
   InetSetTimeout( ::SocketCon, 100 )
   // have we got a timeout? then the server is waiting for us on the
   // other line...
   IF .not. ::GetReply() .and. InetErrorCode( ::SocketCon ) == -1
      InetSetTimeout( ::SocketCon, nTimeout )
      ::TransferStart()
      RETURN .T.
   ENDIF
   InetSetTimeout( ::SocketCon, nTimeout )
RETURN .F.

/*
METHOD Port() CLASS tIPClientFTP
   LOCAL nPort := 16000

   ::SocketPortServer := InetCreate( ::nConnTimeout )
   DO WHILE nPort < 24000
      InetServer( nPort, ::SocketPortServer )
      IF InetErrorCode( ::SocketPortServer ) == 0
         RETURN ::SendPort()
      ENDIF
      nPort ++
   ENDDO
   ::SocketPortServer := NIL

RETURN .F.

METHOD SendPort() CLASS tIPClientFTP
   LOCAL cAddr
   LOCAL cPort, nPort

   cAddr := InetAddress( ::SocketPortServer )
   cAddr := StrTran( cAddr, ".", "," )
   nPort := InetPort( ::SocketPortServer )
   cPort := "," + AllTrim( Str ( Int( nPort / 256 ) ) ) +  "," + AllTrim( Str ( nPort % 256 ) )

   ? "PORT " + cAddr + cPort
   InetSendAll( ::SocketCom, "PORT " + cAddr + cPort  + ::cCRLF )
RETURN ::GetReply()
*/

METHOD Read( nLen ) CLASS tIPClientFTP
   LOCAL cRet

   IF .not. ::bInitialized
      IF .not. Empty( ::oUrl:cPath )
         IF .not. ::CWD( ::oUrl:cPath )
            ::bEof = .T.  // no data for this transaction
            RETURN .F.
         ENDIF
      ENDIF
      IF Empty( ::oUrl:cFile )
         RETURN ::List()
      ENDIF

      IF .not. ::Retr( ::oUrl:cFile )
         ::bEof = .T.  // no data for this transaction
         RETURN .F.
      ENDIF
      // now channel is open
      ::bInitialized := .T.
   ENDIF

   cRet := ::super:Read( nLen )
   IF cRet == NIL
      ::Commit()
      ::bEof := .T.
   ENDIF
RETURN cRet


METHOD Write( cData, nLen, bCommit ) CLASS tIPClientFTP
   IF .not. ::bInitialized

      IF Empty( ::oUrl:cFile )
         RETURN -1
      ENDIF

      IF .not. Empty( ::oUrl:cPath )
         IF .not. ::CWD( ::oUrl:cPath )
            RETURN -1
         ENDIF
      ENDIF

      IF .not. ::Stor( ::oUrl:cFile )
         RETURN -1
      ENDIF
      // now channel is open
      ::bInitialized := .T.
   ENDIF

RETURN ::super:Write( cData, nLen, bCommit )
