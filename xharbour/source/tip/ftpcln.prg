/*
 * $Id: ftpcln.prg,v 1.3 2005/02/12 10:56:00 jonnymind Exp $
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
* Inet service manager: ftp
*/

CLASS tIPClientFTP FROM tIPClient
   DATA nDataPort
   DATA cDataServer
   DATA bUsePasv
   DATA RegBytes
   DATA RegPasv
   // Socket opened in response to a port command
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
   METHOD ReadAuxPort()

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

   // now, if the reply has a '-' as fourth character, we need to proceed...
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


// scan last reply for an hint of length
METHOD ScanLength() CLASS tIPClientFTP
   LOCAL aBytes
   aBytes := HB_Regex( ::RegBytes, ::cReply )
   IF .not. Empty(aBytes)
      ::nLength = Val( aBytes[2] )
   ENDIF
RETURN .T.


METHOD TransferStart() CLASS tIPClientFTP
   LOCAL skt
   ::SocketControl := ::SocketCon

   IF ::bUsePasv
      skt := InetConnectIP( ::cDataServer, ::nDataPort )
      IF skt != NIL .and. InetErrorCode( skt ) == 0
         // Get the start message from the control connection
         IF .not. ::GetReply()
            InetClose( skt )
            RETURN .F.
         ENDIF

         InetSetTimeout( skt, ::nConnTimeout )
         ::SocketCon := skt
      ENDIF
   /*ELSE
      ::SocketCon := InetAccept( ::SocketPortServer )*/
   ENDIF

RETURN .T.



METHOD Commit() CLASS tIPClientFTP
   InetClose( ::SocketCon )
   ::SocketCon := ::SocketControl
   IF .not. ::GetReply()
      RETURN .F.
   ENDIF

   // error code?
   IF ::cReply[1] == "5"
      RETURN .F.
   ENDIF
/*
   IF ::GetReply() .and. ::cReply[1] != "5"
      RETURN .T.
   ENDIF*/
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

RETURN cStr

METHOD ReadAuxPort() CLASS tIPClientFTP
   LOCAL cRet, cList := ""

   IF .not. ::TransferStart()
      RETURN NIL
   END

   cRet := ::super:Read( 512 )
   WHILE cRet != NIL .and. len( cRet ) > 0
      cList += cRet
      cRet := ::super:Read( 512 )
   END

   InetClose( ::SocketCon )
   ::SocketCon := ::SocketControl
   IF ::GetReply()
      RETURN cList
   ENDIF

RETURN NIL

METHOD Retr( cFile ) CLASS tIPClientFTP
   LOCAL nTimeout

   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

   InetSendAll( ::SocketCon, "RETR " + cFile+ ::cCRLF )
   IF ::TransferStart()
      RETURN .T.
   ENDIF
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
RETURN ::TransferStart()

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

*
* FTP transfer wants commit only at end.
*
METHOD Write( cData, nLen ) CLASS tIPClientFTP
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

RETURN ::super:Write( cData, nLen, .F. )
