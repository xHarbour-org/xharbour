/*
* $Id$
*/

/*
* xHarbour Project source code:
* Versatile logging system - Logger sending log message to e-mail
*
* Copyright 2003 Giancarlo Niccolai [gian@niccolai.ws]
* www - http://www.xharbour.org
*
* this program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General public License as published by
* the Free Software Foundation; either version 2, or (at your option)
* any later version.
*
* this program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
* GNU General public License for more details.
*
* You should have received a copy of the GNU General public License
* along with this software; see the file COPYING.  if not, write to
* the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
* Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
*
* As a special exception, xHarbour license gives permission for
* additional uses of the text contained in its release of xHarbour.
*
* The exception is that, if you link the xHarbour libraries with other
* files to produce an executable, this does not by itself cause the
* resulting executable to be covered by the GNU General public License.
* Your use of that executable is in no way restricted on account of
* linking the xHarbour library code into it.
*
* this exception does not however invalidate any other reasons why
* the executable file might be covered by the GNU General public License.
*
* this exception applies only to the code released with this xHarbour
* explicit exception.  if you add/copy code from other sources,
* as the General public License permits, the above exception does
* not apply to the code that you add in this way.  To avoid misleading
* anyone as to the status of such modified files, you must delete
* this exception notice from them.
*
* if you write modifications of your own for xHarbour, it is your choice
* whether to permit this exception to apply to your modifications.
* if you do not wish that, delete this exception notice.
*
*/

#include "hbclass.ch"

CLASS HB_LogEmail FROM HB_LogChannel

   DATA cServer
   DATA cAddress        INIT "log@xharbour.org"
   DATA cSubject        INIT "Log message from xharbour application"
   DATA cSendTo
   DATA cHelo           INIT "XHarbour E-mail Logger"
   DATA nPort           INIT 25

   DATA cPrefix
   DATA cPostfix

   METHOD New( nLevel, cHelo, cServer, cSendTo, cSubject, cFrom )
   METHOD Open()
   METHOD CLOSE()

   PROTECTED:
   METHOD Send( nStyle, cMessage, cProgName, nPrio )

   HIDDEN:
   METHOD GetOk()
   METHOD Prepare( nStyle, cMessage, cProgName, nPrio )

ENDCLASS

METHOD New(  nLevel, cHelo, cServer, cSendTo, cSubject, cFrom ) CLASS HB_LogEmail

   LOCAL nPos

   ::Super:New( nLevel )

   nPos := At( ":", cServer )
   IF nPos > 0
      ::nPort := Val( SubStr( cServer, nPos + 1 ) )
      cServer := SubStr( cServer , 1, nPos - 1 )
   ENDIF

   ::cServer := cServer
   ::cSendTo := cSendTo

   IF cHelo != NIL
      ::cHelo := cHelo
   ENDIF

   IF cSubject != NIL
      ::cSubject := cSubject
   ENDIF

   IF cFrom != NIL
      ::cAddress := cFrom
   ENDIF

   RETURN SELF

/**
* Inet init must be called here
*/

METHOD Open( ) CLASS HB_LogEmail

   InetInit()

   RETURN .T.

/**
* InetCleanup to be called here
*/

METHOD CLOSE() CLASS HB_LogEmail

   InetCleanup()

   RETURN .T.


/**
* Sends the real message in e-mail
*/

METHOD Send( nStyle, cMessage, cName, nPrio ) CLASS HB_LogEmail

   LOCAL skCon := InetCreate()

   InetSetTimeout( skCon, 10000 )

   InetConnect( ::cServer, ::nPort, skCon )

   IF InetErrorCode( skCon ) != 0 .OR. .NOT. ::GetOk( skCon )
      RETURN .F.
   ENDIF

   InetSendAll( skCon, "HELO " + ::cHelo + InetCRLF() )
   IF .NOT. ::GetOk( skCon )
      RETURN .F.
   ENDIF

   InetSendAll( skCon, "MAIL FROM: <" + ::cAddress + ">" + InetCRLF() )
   IF .NOT. ::GetOk( skCon )
      RETURN .F.
   ENDIF

   InetSendAll( skCon, "RCPT TO: <" + ::cSendTo + ">" + InetCRLF() )
   IF .NOT. ::GetOk( skCon )
      RETURN .F.
   ENDIF

   InetSendAll( skCon, "DATA" + InetCRLF() )
   IF .NOT. ::GetOk( skCon )
      RETURN .F.
   ENDIF

   cMessage := ::Prepare( nStyle, cMessage, cName, nPrio )

   InetSendAll( skCon,  cMessage + InetCRLF() + "." + InetCRLF() )
   IF .NOT. ::GetOk( skCon )
      RETURN .F.
   ENDIF

   InetSendAll( skCon, "QUIT" + InetCRLF() )

   RETURN ::GetOk( skCon )  // if quit fails, the mail does not go!

/**
* Get the reply and returns true if it is allright
*/

METHOD GetOk( skCon ) CLASS HB_LogEmail

   LOCAL nLen, cReply

   cReply := InetRecvLine( skCon, @nLen, 128 )
   IF InetErrorCode( skcon ) != 0 .OR. SubStr( cReply, 1, 1 ) == '5'
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD Prepare( nStyle, cMessage, cName, nPrio ) CLASS HB_LogEmail

   LOCAL cPre

   cPre := "FROM: " + ::cAddress + InetCRLF() + ;
      "TO: " + ::cSendTo + InetCRLF() + ;
      "Subject:" + ::cSubject + InetCRLF() + InetCRLF()

   IF .NOT. Empty( ::cPrefix )
      cPre += ::cPrefix + InetCRLF() + InetCRLF()
   ENDIF

   cPre += ::Format( nStyle, cMessage, cName, nPrio )

   IF .NOT. Empty( ::cPostfix )
      cPre += InetCRLF() + InetCRLF() + ::cPostfix + InetCRLF()
   ENDIF

   RETURN cPre



/************************************************
* Channel for monitors listening on a port
*************************************************/

CLASS HB_LogInetPort FROM HB_LogChannel

   DATA nPort           INIT 7761
   DATA aListeners      INIT {}
   DATA skIn
   DATA bOnConnect

   DATA bTerminate      INIT .F.
   DATA nThread
   DATA mtxBusy

   METHOD New( nLevel, nPort )
   METHOD Open( cName )
   METHOD CLOSE( cName )
   METHOD Connections()
   METHOD BroadcastMessage( cMessage )
   METHOD AcceptCon()

   PROTECTED:
   METHOD Send( nStyle, cMessage, cName, nPrio )

/* HIDDEN:
   METHOD AcceptCon()
*/

ENDCLASS

METHOD New( nLevel, nPort ) CLASS HB_LogInetPort

   ::Super:New( nLevel )

   IF nPort != NIL
      ::nPort := nPort
   ENDIF

   RETURN Self

METHOD Open() CLASS HB_LogInetPort

   InetInit()

   ::skIn := InetServer( ::nPort )

   IF ::skIn == NIL
      RETURN .F.
   ENDIF

   HB_LOGOPEN( self )

   RETURN .T.

METHOD CLOSE( ) CLASS HB_LogInetPort

   LOCAL sk

   IF ::skIn == NIL
      RETURN .F.
   ENDIF

// kind termination request
   ::bTerminate := .T.
   JoinThread( ::nThread )

   InetClose( ::skIn )

// we now are sure that incoming thread index is not used.

   DO WHILE  Len( ::aListeners ) > 0
      sk := ATail( ::aListeners )
      ASize( ::aListeners, Len( ::aListeners ) - 1 )
      InetClose( sk )
   ENDDO

   InetCleanup()

   RETURN .T.

METHOD Send( nStyle, cMessage, cName, nPrio ) CLASS HB_LogInetPort

// now we transmit the message to all the available channels
   cMessage := ::Format( nStyle, cMessage, cName, nPrio )
   ::BroadcastMessage( cMessage + InetCRLF() )

   RETURN .T.

METHOD BroadcastMessage( cMessage ) CLASS HB_LogInetPort

   LOCAL sk, nCount

   IF cMessage == NIL
      cMessage := ""
   ENDIF

   HB_LOGBROADCASTMESSAGE( self )

   nCount := 1
   DO WHILE nCount <= Len( ::aListeners )
      sk := ::aListeners[ nCount ]
      InetSendAll( sk, cMessage )
      // if there is an error, we remove the listener
      IF InetErrorCode( sk ) != 0
         ADel( ::aListeners, nCount )
         ASize( ::aListeners , Len( ::aListeners ) - 1 )
      ELSE
         nCount ++
      ENDIF
   ENDDO

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD Connections() CLASS HB_LogInetPort

   LOCAL nCount

   ::BroadcastMessage( Chr( 0 ) )

// be sure thread is not busy now
   hb_mutexLock( ::mtxBusy )

   nCount := Len( ::aListeners )

   hb_mutexUnlock( ::mtxBusy )

   RETURN nCount

METHOD AcceptCon() CLASS HB_LogInetPort

   HB_LOGACCEPTCON( self )

   RETURN .T.
