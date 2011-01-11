/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// WinSock.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include "vxh.ch"
#include "debug.ch"

//206.175.128.3
//10198

//-------------------------------------------------------------------------------------------------------
CLASS WinSock INHERIT Component
   DATA ClsName         EXPORTED  INIT "Timer"
   DATA Events          EXPORTED  INIT {  {"Socket", { { "OnAccepting" , "", "" },;
                                                       { "OnReceive"   , "", "" },;
                                                       { "OnError"     , "", "" },;
                                                       { "OnConnected" , "", "" }} } }

   DATA pCallBack       PROTECTED
   DATA nRecId          EXPORTED
   DATA lListener       PROTECTED INIT .F.
   DATA Handle          EXPORTED
   DATA RemoteHandle    EXPORTED
   DATA LocalIP         EXPORTED INIT 0
   
   DATA RecData         EXPORTED INIT ""
   
   DATA LocalPort       PUBLISHED INIT 0
   DATA RemoteIP        PUBLISHED
   DATA RemotePort      PUBLISHED INIT 0
   
   DATA EnumProtocol    EXPORTED INIT { __GetSystem():SockProtocol:Keys, {1,2} }
   DATA Protocol        PUBLISHED INIT 1
   
   DATA Connected       EXPORTED INIT .F.
   
   ACCESS Status  INLINE IIF( ::Handle != NIL, InetErrorDesc( ::Handle ), "Failed to connect" )
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD SockControlProc()
   METHOD Destroy()    INLINE ::Disconnect(), ::Super:Destroy(), IIF( ::pCallBack != NIL, FreeCallBackPointer( ::pCallBack ), ), NIL

   METHOD Connect()
   METHOD Send()
   METHOD Close()      INLINE ::Disconnect( .T. )
   METHOD Listen()
   METHOD Disconnect()
   METHOD SetTimeOut(n) INLINE InetSetTimeout( ::Handle, n )
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner, lCallBack ) CLASS WinSock
   DEFAULT lCallBack TO .T.
   ::__xCtrlName   := "WinSock"
   ::ClsName       := "WinSock"
   ::ComponentType := "Socket"
   ::Super:Init( oOwner )
   ::nRecId := oOwner:__Timers ++
   IF !::Application:__SocketInit
      ::Application:__SocketInit := .T.
      InetInit()
   ENDIF
   IF ::__ClassInst == NIL .AND. lCallBack
      ::pCallBack := WinCallBackPointer( HB_ObjMsgPtr( Self, "SockControlProc" ), Self )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create() CLASS WinSock
   LOCAL oComp
   IF ::__ClassInst == NIL
      ::Handle := InetCreate( 250 )
   ENDIF
   ::lCreated := .T.
   Super:Create()
   FOR EACH oComp IN ::Form:Components
       IF oComp:HasMessage( "Socket" ) .AND. VALTYPE( oComp:Socket ) == "C" .AND. UPPER( oComp:Socket ) == UPPER( ::Name )

          oComp:Socket := Self
          IF ::__ClassInst == NIL
             oComp:Connector := SocketRdd( oComp )
             oComp:Connector:Create()
          ENDIF
       ENDIF
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Connect() CLASS WinSock
   IF ::Protocol == ::System:SockProtocol:TCP
      DEFAULT ::Handle TO InetCreate( 250 )
      InetConnect( ::RemoteIP, ::RemotePort, ::Handle )
      IF ( ::Connected := InetErrorCode( ::Handle ) == 0 )
         ExecuteEvent( "OnConnected", Self )
         SetTimer( ::Owner:hWnd, ::nRecId, 250, ::pCallBack )
      ENDIF
   ENDIF
RETURN ::Connected

//-------------------------------------------------------------------------------------------------------
METHOD Disconnect( lAll ) CLASS WinSock
   IF ::__ClassInst == NIL
      DEFAULT lAll TO .F.
      IF ::RemoteHandle != NIL
         InetClose( ::RemoteHandle )
         ::RemoteHandle := NIL
         IF !lAll
            RETURN Self
         ENDIF
      ENDIF
      IF ::Handle != NIL
         InetClose( ::Handle )
         ::Handle := NIL
      ENDIF
      KillTimer( ::Owner:hWnd, ::nRecId )
      ::Connected := .F.
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Send( cData, aFields ) CLASS WinSock
   LOCAL nSent, oDataSource, n, cField, hSocket
   IF ::Protocol == ::System:SockProtocol:TCP
      IF VALTYPE( cData ) == "O"
         oDataSource := cData
         cData := ""
         FOR n := 1 TO LEN( oDataSource:Structure )
             cField := oDataSource:Structure[n][1]
             IF EMPTY( aFields ) .OR. cField $ aFields
                cData += "<"+lower(cField)+">" + ALLTRIM( oDataSource:Fields:&cField ) + "</"+lower(cField)+">"
             ENDIF
         NEXT
      ENDIF
      IF ( hSocket := IIF( ::RemoteHandle != NIL, ::RemoteHandle, ::Handle ) ) != NIL
         nSent := InetSend( hSocket, cData + InetCRLF() )
      ENDIF
   ENDIF
RETURN nSent

//-------------------------------------------------------------------------------------------------------
METHOD SockControlProc() CLASS WinSock
   LOCAL pSocket, cData, n
   KillTimer( ::Owner:hWnd, ::nRecId )
   IF ::lListener .AND. ::RemoteHandle == NIL
      ExecuteEvent( "OnAccepting", Self )
      ::RemoteHandle := InetAccept( ::Handle )
      IF ::RemoteHandle != NIL
         ::RemoteIP   := InetAddress( ::RemoteHandle )
         ::RemotePort := InetPort( ::RemoteHandle )
         
         ExecuteEvent( "OnConnected", Self )
      ENDIF
      SetTimer( ::Owner:hWnd, ::nRecId, 250, ::pCallBack )
      RETURN 0
   ENDIF
   
   IF ::Handle == NIL .OR. InetErrorCode( IIF( ::RemoteHandle != NIL, ::RemoteHandle, ::Handle ) ) <> 0
      ::Connected := .F.
      ExecuteEvent( "OnError", Self )
    ELSE
      ::RecData := ""
      WHILE InetDataReady( IIF( ::RemoteHandle != NIL, ::RemoteHandle, ::Handle ) ) > 0
         cData := SPACE(1024)
         InetRecv( IIF( ::RemoteHandle != NIL, ::RemoteHandle, ::Handle ), @cData, 1024 )
         IF ( n := AT( InetCRLF(), ALLTRIM( cData ) ) ) > 0
            cData := LEFT( ALLTRIM( cData ), n-1 )
         ENDIF
         IF EMPTY( cData )
            EXIT
         ENDIF
         ::RecData += cData
      ENDDO
      IF !EMPTY( ::RecData )
         ExecuteEvent( "OnReceive", Self )
      ENDIF

   ENDIF
   
   SetTimer( ::Owner:hWnd, ::nRecId, 250, ::pCallBack )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Listen() CLASS WinSock
   LOCAL hRemote
   DEFAULT ::Handle TO InetCreate( 250 )
   InetServer( ::LocalPort, ::Handle )

   ::lListener := .T.

   ExecuteEvent( "OnAccepting", Self )
   ::RemoteHandle := InetAccept( ::Handle )
   
   IF ::RemoteHandle != NIL
      ::RemoteIP   := InetAddress( ::RemoteHandle )
      ::RemotePort := InetPort( ::RemoteHandle )

      ExecuteEvent( "OnConnected", Self )
   ENDIF
   SetTimer( ::Owner:hWnd, ::nRecId, 250, ::pCallBack )
RETURN Self

#endif
