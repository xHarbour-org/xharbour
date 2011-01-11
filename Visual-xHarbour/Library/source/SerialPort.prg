/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// SerialPort.prg                                                                                         *
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

#define BITFBINARY                    1
#define BITFPARITY                    2
#define BITFOUTXCTSFLOW               3
#define BITFOUTXDSRFLOW               4
#define BITFDTRCONTROL                5
#define BITFDSRSENSITIVITY            7
#define BITFTXCONTINUEONXOFF          8
#define BITFOUTX                      9
#define BITFINX                      10
#define BITFERRORCHAR                11
#define BITFNULL                     12
#define BITFRTSCONTROL               13
#define BITFABORTONERROR             15

#define GENERIC_READ        -2147483648
#define GENERIC_WRITE        1073741824
#define OPEN_EXISTING                 3
#define INVALID_HANDLE_VALUE         -1

#define PURGE_TXABORT                 1
#define PURGE_RXABORT                 2
#define PURGE_TXCLEAR                 4
#define PURGE_RXCLEAR                 8

#define WAIT_OBJECT_0       0x00000000L
#define INFINITE                     -1
#define FILE_FLAG_OVERLAPPED 0x40000000

#define MS_CTS_ON                    16
#define MS_DSR_ON                    32
#define MS_RING_ON                   64
#define MS_RLSD_ON                  128

#define SETXOFF                       1
#define SETXON                        2
#define SETRTS                        3
#define CLRRTS                        4
#define SETDTR                        5
#define CLRDTR                        6
#define RESETDEV                      7
#define SETBREAK                      8
#define CLRBREAK                      9


//-------------------------------------------------------------------------------------------------------
CLASS SerialPort INHERIT Component
   DATA Events                 EXPORTED  INIT {  {"Serial", { { "OnDataReceived"  , "", "" },;
                                                              { "OnError"         , "", "" },;
                                                              { "OnPinChanged"    , "", "" }} } }

   DATA Handle                 EXPORTED

   DATA PortName               PUBLISHED INIT "COM1"
   DATA BaudRate               PUBLISHED INIT 9600
   DATA HandShake              PUBLISHED INIT __GetSystem():HandShake:None
   DATA Parity                 PUBLISHED INIT __GetSystem():Parity:None
   DATA StopBits               PUBLISHED INIT __GetSystem():StopBits:One
   DATA DataBits               PUBLISHED INIT 8
   DATA ReadBufferSize         PUBLISHED INIT 4096
   DATA ReadTimeOut            PUBLISHED INIT 500

   PROPERTY DtrEnabled INDEX 2 READ xDtrEnabled WRITE __SetCommData DEFAULT .F.
   PROPERTY RtsEnabled INDEX 1 READ xRtsEnabled WRITE __SetCommData DEFAULT .F.

   ACCESS Cts                  INLINE ::GetStatus( MS_CTS_ON )
   ACCESS Dsr                  INLINE ::GetStatus( MS_DSR_ON )
   ACCESS Ring                 INLINE ::GetStatus( MS_RING_ON )
   ACCESS Rlsd                 INLINE ::GetStatus( MS_RLSD_ON )
   ACCESS IsOpen               INLINE ::Handle != NIL

   //DATA ReceivedBytesThreshold PUBLISHED INIT 1
   //DATA DiscardNull            PUBLISHED INIT .F.
   //DATA WriteTimeOut           PUBLISHED INIT -1
   //DATA ParityReplace          PUBLISHED INIT 63

   DATA __pCallBackPtr         PROTECTED
   DATA __nTimID               PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Destroy()

   METHOD Open()
   METHOD Close()
   METHOD Write()
   METHOD Read()
   METHOD Listen()
   METHOD CountIn()
   METHOD CountOut()
   METHOD Properties()
   METHOD GetStatus()
   METHOD Clear()              INLINE PurgeComm( ::Handle, PURGE_RXCLEAR )
   METHOD Escape(n)            INLINE EscapeCommFunction( ::Handle, n )

   METHOD __SerialControlProc()
   METHOD __SetCommData()

ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS SerialPort
   ::__xCtrlName   := "SerialPort"
   ::ClsName       := "SerialPort"
   ::ComponentType := "SerialPort"
   ::Super:Init( oOwner )
   IF ::__ClassInst == NIL
      ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, "__SerialControlProc" ), Self )
   ENDIF
   ::__nTimID := ::Owner:__Timers ++
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Open() CLASS SerialPort
   LOCAL nEvent := -1, dcb := (struct DCB), cto := (struct COMMTIMEOUTS)
   ::Owner:CallWindowProc()
   IF ::IsOpen
      ::Close()
   ENDIF
   ::Handle := CreateFile( ::PortName+":", GENERIC_READ | GENERIC_WRITE, 0,, OPEN_EXISTING, 0, )
   IF !GetCommState( ::Handle, @dcb )
      RETURN .F.
   ENDIF

   dcb:DCBlength := dcb:sizeOf()
   dcb:BaudRate  := ::BaudRate
   dcb:Parity    := ::Parity
   dcb:StopBits  := ::StopBits
   dcb:ByteSize  := ::DataBits

   SetBit( dcb:data, BITFPARITY, 1 )

   IF !SetCommState( ::Handle, dcb )
      RETURN .F.
   ENDIF

   cto:ReadIntervalTimeout         := 15
   cto:ReadTotalTimeoutMultiplier  := 1
   cto:ReadTotalTimeoutConstant    := 250

   cto:WriteTotalTimeoutMultiplier := 1
   cto:WriteTotalTimeoutConstant   := 750
   IF !SetCommTimeouts( ::Handle, cto )
      RETURN .F.
   ENDIF
   ::DtrEnabled( ::DtrEnabled )
   ::RtsEnabled( ::RtsEnabled )
RETURN .T.

//-------------------------------------------------------------------------------------------------------
METHOD Close() CLASS SerialPort
   KillTimer( ::Owner:hWnd, ::__nTimID )
   ::Clear()
   CloseHandle( ::Handle )
   ::Handle := NIL
RETURN .T.

//-------------------------------------------------------------------------------------------------------
METHOD Write( cStr ) CLASS SerialPort
   LOCAL nSent
   IF ::IsOpen
      WriteFile( ::Handle, cStr, LEN(cStr), @nSent )

      //_WriteFile( ::Handle, @cStr, @nSent )

      FlushFileBuffers( ::Handle )
   ENDIF
RETURN nSent

//-------------------------------------------------------------------------------------------------------
METHOD Read( nRead ) CLASS SerialPort
   LOCAL cBuffer := ""
   IF ::IsOpen
      DEFAULT nRead TO ::ReadBufferSize
      cBuffer   := SPACE( nRead )

      ReadFile( ::Handle, @cBuffer, nRead, @nRead )
      //nRead     := fRead( ::Handle, @cBuffer, nRead )

      cBuffer := cBuffer := LEFT( cBuffer, nRead )
   ENDIF
RETURN cBuffer

//-------------------------------------------------------------------------------------------------------
METHOD CountOut() CLASS SerialPort
   LOCAL nError, nCount := -1, cs := (struct COMSTAT)
   IF ::IsOpen .AND. ClearCommError( ::Handle, @nError, @cs )
      nCount := cs:cbOutQue
   ENDIF
RETURN nCount

//-------------------------------------------------------------------------------------------------------
METHOD __SetCommData( nBit, lValue ) CLASS SerialPort
   LOCAL lStatus, nSet, nClr, nError, cs := (struct COMSTAT)
   IF ::IsOpen
      IF ClearCommError( ::Handle, @nError, @cs )
         lStatus := CheckBit( cs:data, nBit )
      ENDIF
      IF VALTYPE( lValue ) == "L"
         IF nBit == 1
            nSet := SETRTS
            nClr := CLRRTS
          ELSE
            nSet := SETDTR
            nClr := CLRDTR
         ENDIF
         EscapeCommFunction( ::Handle, IIF( lValue, nSet, nClr ) )
      ENDIF
   ENDIF
RETURN lStatus

//-------------------------------------------------------------------------------------------------------
METHOD CountIn() CLASS SerialPort
   LOCAL nError, nCount := -1, cs := (struct COMSTAT)
   IF ClearCommError( ::Handle, @nError, @cs )
      nCount := cs:cbInQue
   ENDIF
RETURN nCount

//-------------------------------------------------------------------------------------------------------
METHOD __SerialControlProc() CLASS SerialPort
   LOCAL nError, cBuffer, nRead := 0
   ::Application:Yield()
   KillTimer( ::Owner:hWnd, ::__nTimID )

   IF ::CountIn() > 0
      ExecuteEvent( "OnDataReceived", Self )
   ENDIF

   SetTimer( ::Owner:hWnd, ::__nTimID, ::ReadTimeOut, ::__pCallBackPtr )
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD Listen() CLASS SerialPort
   IF ::IsOpen
      SetTimer( ::Owner:hWnd, ::__nTimID, ::ReadTimeOut, ::__pCallBackPtr )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Properties() CLASS SerialPort
   LOCAL cp := (struct COMMPROP)
   IF ::IsOpen
      GetCommProperties( ::Handle, @cp )
   ENDIF
RETURN cp

//-------------------------------------------------------------------------------------------------------
METHOD Destroy() CLASS SerialPort
   ::Close()
   IF ::__pCallBackPtr != NIL
      FreeCallBackPointer( ::__pCallBackPtr )
      ::__pCallBackPtr := NIL
   ENDIF
   ::Super:Destroy()
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD GetStatus( nType ) CLASS SerialPort
   LOCAL nStatus := 0
   IF ::IsOpen
      GetCommModemStatus( ::Handle, @nStatus )
   ENDIF
RETURN ( ( nStatus & nType ) == nType )
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

#pragma BEGINDUMP
#include <windows.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "tchar.h"

//-----------------------------------------------------------------------------
HB_FUNC_STATIC( SETBIT )
{
   if( hb_pcount() < 3 || hb_parni( 3 ) )
   {
      hb_retnl( hb_parnl(1) | ( 1 << ( hb_parni( 2 )-1 ) ) );
   }
   else
   {
      hb_retnl( hb_parnl(1) & ~( 1 << ( hb_parni( 2 )-1 ) ) );
   }
}


//-----------------------------------------------------------------------------
HB_FUNC_STATIC( CHECKBIT )
{
   hb_retl( hb_parnl( 1 ) & ( 1 << ( hb_parni( 2 ) - 1 ) ) );
}
#pragma ENDDUMP
#endif

