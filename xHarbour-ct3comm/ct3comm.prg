//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                Pritpal Bedi <pritpal@vouchcac.com>
//                  Serial Communications Routines
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#include            "hbClass.ch"
#include             "fileio.ch"
#Include            "WinUser.ch"
#include            "cstruct.ch"
#include           "wintypes.ch"
#include             "common.ch"

//-------------------------------------------------------------------//

#define GENERIC_READ       -2147483648 // (0x80000000)  // -2147483648
#define GENERIC_WRITE       1073741824 // (0x40000000)  //  1073741824
#define OPEN_EXISTING          3
#define INVALID_HANDLE_VALUE  -1

//-------------------------------------------------------------------//

#define NOPARITY               0
#define ODDPARITY              1
#define EVENPARITY             2
#define MARKPARITY             3
#define SPACEPARITY            4

#define ONESTOPBIT             0
#define ONE5STOPBITS           1
#define TWOSTOPBITS            2

//-------------------------------------------------------------------//
//
// Baud rates at which the communication device operates
//
#define CBR_110              110
#define CBR_300              300
#define CBR_600              600
#define CBR_1200            1200
#define CBR_2400            2400
#define CBR_4800            4800
#define CBR_9600            9600
#define CBR_14400          14400
#define CBR_19200          19200
#define CBR_38400          38400
#define CBR_56000          56000
#define CBR_57600          57600
#define CBR_115200        115200
#define CBR_128000        128000
#define CBR_256000        256000

// Error Flags
//
#define CE_RXOVER              1   // Receive Queue overflow
#define CE_OVERRUN             2   // Receive Overrun Error
#define CE_RXPARITY            4   // Receive Parity Error
#define CE_FRAME               8   // Receive Framing error
#define CE_BREAK              16   // Break Detected
#define CE_TXFULL            256   // TX Queue is full
#define CE_PTO               512   // LPTx Timeout
#define CE_IOE              1024   // LPTx I/O Error
#define CE_DNS              2048   // LPTx Device not selected
#define CE_OOP              4096   // LPTx Out-Of-Paper
#define CE_MODE            32768   // Requested mode unsupported

#define IE_BADID             (-1)  // Invalid or unsupported id
#define IE_OPEN              (-2)  // Device Already Open
#define IE_NOPEN             (-3)  // Device Not Open
#define IE_MEMORY            (-4)  // Unable to allocate queues
#define IE_DEFAULT           (-5)  // Error in default parameters
#define IE_HARDWARE         (-10)  // Hardware Not Present
#define IE_BYTESIZE         (-11)  // Illegal Byte Size
#define IE_BAUDRATE         (-12)  // Unsupported BaudRate

// Events
//
#define EV_RXCHAR              1    // Any Character received
#define EV_RXFLAG              2    // Received certain character
#define EV_TXEMPTY             4    // Transmitt Queue Empty
#define EV_CTS                 8    // CTS changed state
#define EV_DSR                16    // DSR changed state
#define EV_RLSD               32    // RLSD changed state
#define EV_BREAK              64    // BREAK received
#define EV_ERR               128    // Line status error occurred
#define EV_RING              256    // Ring signal detected
#define EV_PERR              512    // Printer error occured
#define EV_RX80FULL         1024    // Receive buffer is 80 percent full
#define EV_EVENT1           2048    // Provider specific event 1
#define EV_EVENT2           4096    // Provider specific event 2

// Escape Functions
//
#define SETXOFF                1    // Simulate XOFF received
#define SETXON                 2    // Simulate XON received
#define SETRTS                 3    // Set RTS high
#define CLRRTS                 4    // Set RTS low
#define SETDTR                 5    // Set DTR high
#define CLRDTR                 6    // Set DTR low
#define RESETDEV               7    // Reset device if possible
#define SETBREAK               8    // Set the device break line.
#define CLRBREAK               9    // Clear the device break line.

// PURGE function flags.
//
#define PURGE_TXABORT          1    // Kill the pending/current writes to the comm port.
#define PURGE_RXABORT          2    // Kill the pending/current reads to the comm port.
#define PURGE_TXCLEAR          4    // Kill the transmit queue if there.
#define PURGE_RXCLEAR          8    // Kill the typeahead buffer if there.

// Modem Status Flags
//
#define MS_CTS_ON             16
#define MS_DSR_ON             32
#define MS_RING_ON            64
#define MS_RLSD_ON           128

// DTR Control Flow Values.
//
#define DTR_CONTROL_DISABLE    0
#define DTR_CONTROL_ENABLE     1
#define DTR_CONTROL_HANDSHAKE  2

// RTS Control Flow Values
//
#define RTS_CONTROL_DISABLE    0
#define RTS_CONTROL_ENABLE     1
#define RTS_CONTROL_HANDSHAKE  2
#define RTS_CONTROL_TOGGLE     3

//-------------------------------------------------------------------//
//
//  aDcb array structure
//
#define dcbBaudRate            1
#define dcbFBinary             2
#define dcbFParity             3
#define dcbFOutxCtsFlow        4
#define dcbFOutxDsrFlow        5
#define dcbFDtrControl         6
#define dcbFDsrSensitivity     7
#define dcbFTxContinueOnXoff   8
#define dcbFOutX               9
#define dcbFInX               10
#define dcbFErrorChar         11
#define dcbFNull              12
#define dcbFRtsControl        13
#define dcbFAbortOnError      14
#define dcbXonLim             15
#define dcbXoffLim            16
#define dcbByteSize           17
#define dcbParity             18
#define dcbStopBits           19
#define dcbXonChar            20
#define dcbXoffChar           21
#define dcbErrorChar          22
#define dcbEofChar            23
#define dcbEvtChar            24

#define dcbVariables          24

//-------------------------------------------------------------------//
//  dcb:data bits
//
#define BitFBinary             1
#define BitFParity             2
#define BitFOutxCtsFlow        3
#define BitFOutxDsrFlow        4
#define BitFDtrControl         5
#define BitFDsrSensitivity     7
#define BitFTxContinueOnXoff   8
#define BitFOutX               9
#define BitFInX               10
#define BitFErrorChar         11
#define BitFNull              12
#define BitFRtsControl        13
#define BitFAbortOnError      15

//-------------------------------------------------------------------//
//
//  comstat:data bits  Com_stat()
//  Provides information about why communication is suspended
//
#define BitFCtsHold           1  // Specifies whether transmission is waiting for the CTS (clear-to-send) signal to be sent.
                                 // If this member is TRUE, transmission is waiting.
#define BitFDsrHold           2  // Specifies whether transmission is waiting for the DSR (data-set-ready) signal to be sent.
                                 // If this member is TRUE, transmission is waiting.
#define BitFRlsdHold          3  // Specifies whether transmission is waiting for the RLSD (receive-line-signal-detect) signal to be sent.
                                 // If this member is TRUE, transmission is waiting.
#define BitFXoffHold          4  // Specifies whether transmission is waiting because the XOFF character was received.
                                 // If this member is TRUE, transmission is waiting.
#define BitFXoffSent          5  // Specifies whether transmission is waiting because the XOFF character was transmitted.
                                 // If this member is TRUE, transmission is waiting.
                                 // Transmission halts when the XOFF character is transmitted to a system that takes the next character as XON, regardless of the actual character
#define BitFEof               6  // Specifies whether the end-of-file (EOF) character has been received.
                                 // If this member is TRUE, the EOF character has been received.
#define BitFTxim              7  // If this member is TRUE, there is a character queued for transmission that has come to the ;
                                 // communications device by way of the TransmitCommChar function.
                                 // The communications device transmits such a character ahead of other characters in the device's output buffer.

//-------------------------------------------------------------------//

static ports_:= { { '', 0 } }

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

pragma pack( 4 )

typedef struct _DCB {               ;
  DWORD DCBlength                   ; // sizeof(DCB)
  DWORD BaudRate                    ; // current baud rate
  DWORD data                        ; //  Original structure members reduced to DWORD to be used in .PRG code
  WORD  wReserved                   ; // not currently used
  WORD  XonLim                      ; // transmit XON threshold
  WORD  XoffLim                     ; // transmit XOFF threshold
  BYTE  ByteSize                    ; // number of bits/byte, 4-8
  BYTE  Parity                      ; // 0-4=no,odd,even,mark,space
  BYTE  StopBits                    ; // 0,1,2 = 1, 1.5, 2
  BYTE  XonChar                     ; // Tx and Rx XON character
  BYTE  XoffChar                    ; // Tx and Rx XOFF character
  BYTE  ErrorChar                   ; // error replacement character
  BYTE  EofChar                     ; // end of input character
  BYTE  EvtChar                     ; // received event character
  WORD  wReserved1                  ; // reserved; do not use
} DCB

typedef struct _COMM_CONFIG {       ;
  DWORD dwSize                      ;
  WORD  wVersion                    ;
  WORD  wReserved                   ;
  DCB   DCB                         ;
  DWORD dwProviderSubType           ;
  DWORD dwProviderOffset            ;
  DWORD dwProviderSize              ;
  WCHAR wcProviderData[1]           ;
} COMMCONFIG

typedef struct _COMMPROP {          ;
  WORD  wPacketLength               ;
  WORD  wPacketVersion              ;
  DWORD dwServiceMask               ;
  DWORD dwReserved1                 ;
  DWORD dwMaxTxQueue                ;
  DWORD dwMaxRxQueue                ;
  DWORD dwMaxBaud                   ;
  DWORD dwProvSubType               ;
  DWORD dwProvCapabilities          ;
  DWORD dwSettableParams            ;
  DWORD dwSettableBaud              ;
  WORD  wSettableData               ;
  WORD  wSettableStopParity         ;
  DWORD dwCurrentTxQueue            ;
  DWORD dwCurrentRxQueue            ;
  DWORD dwProvSpec1                 ;
  DWORD dwProvSpec2                 ;
} COMMPROP
//   WCHAR wcProvChar[1];

typedef struct _COMMTIMEOUTS {      ;
  DWORD ReadIntervalTimeout         ;
  DWORD ReadTotalTimeoutMultiplier  ;
  DWORD ReadTotalTimeoutConstant    ;
  DWORD WriteTotalTimeoutMultiplier ;
  DWORD WriteTotalTimeoutConstant   ;
} COMMTIMEOUTS

typedef struct _COMSTAT {           ;
  DWORD data                        ;
  DWORD cbInQue                     ;
  DWORD cbOutQue                    ;
} COMSTAT

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

static function GetValidHandle( ncPort )
   local n, cPort

   if empty( ports_ )
      return 0
   endif

   cPort := Com_PortName( ncPort )
   if ( n := ascan( ports_, {|e_| e_[ 1 ] == cPort } ) ) == 0
      return 0
   endif

   return ports_[ n,2 ]

//-------------------------------------------------------------------//

static function com_PortName( ncPort )
   local cPort := ncPort

   if valtype( ncPort ) == 'N'
      cPort := 'COM' + ltrim( str( ncPort ) )
   endif

   return cPort

//-------------------------------------------------------------------//

function Com_Open( ncPort, nInBuffer, nOutBuffer )
   local nHandle, cPort,n

   if ncPort == NIL
      return .F.
   endif
   cPort := com_PortName( ncPort )

   nHandle := CreateFile( cPort, ;
                          GENERIC_READ + GENERIC_WRITE, ;
                          0,             ; /* comm devices must be opened with exclusive-access */
                           ,             ; /* no security attrs */
                          OPEN_EXISTING, ; /* comm devices must use OPEN_EXISTING */
                          0,             ; /* not overlapped I/O */
                                         ; /* hTemplate must be NULL for comm devices */
                          )

   if nHandle == INVALID_HANDLE_VALUE
      return .f.
   endif

   if valtype( nInBuffer ) == 'N'
      DEFAULT nOutBuffer TO 1024
      if !SetupComm( nHandle, nInBuffer, nOutBuffer )
         CloseHandle( nHandle )
         return .f.
      endif
   endif

   n := ascan( ports_, {|e_| e_[ 1 ] == cPort } )
   if n == 0
      aadd( ports_, { cPort, nHandle } )
   else
      ports_[ n,2 ] := nHandle
   endif

   return .t.

//-------------------------------------------------------------------//

function Com_Init( ncPort, nBaud, cParity, nDatabits, nStopbit )
   local nHandle, a_ :={}
   local dcb IS DCB
   local cDcb := dcb:value
   local TimeOuts IS COMMTIMEOUTS

   if ( nHandle := GetValidHandle( ncPort ) ) == 0
      return .f.
   endif

   DEFAULT nBaud     TO 9600
   DEFAULT cParity   TO 'N'
   DEFAULT nDatabits TO 8
   DEFAULT nStopbit  TO 1

   cParity := upper( substr( cParity,1,1 ) )

   dcb:reset()
   dcb:DCBLength := dcb:sizeof
   cDcb := dcb:value
   if !GetCommState( nHandle, @cDcb )
      return .f.
   endif
   dcb:buffer( cDcb )

   dcb:BaudRate := nBaud
   dcb:Parity   := if( cParity == 'O', ODDPARITY,  if( cParity == 'E', EVENPARITY, ;
                   if( cParity == 'M', MARKPARITY, if( cParity == 'S', SPACEPARITY, NOPARITY ) ) ) )
   dcb:StopBits := if( nStopbit == 2, TWOSTOPBITS, if( nStopbit == 1.5, ONE5STOPBITS, ONESTOPBIT ) )
   dcb:ByteSize := nDatabits

   SetBit( dcb:data, BitFParity, 1 )

   cDcb := dcb:value
   If !SetCommState( nHandle, cDcb )
      return .f.
   endif

   TimeOuts:ReadIntervalTimeout         := 15
   TimeOuts:ReadTotalTimeoutMultiplier  := 1
   TimeOuts:ReadTotalTimeoutConstant    := 250
   TimeOuts:WriteTotalTimeoutMultiplier := 1
   TimeOuts:WriteTotalTimeoutConstant   := 750
   if !SetCommTimeouts( nHandle, TimeOuts:Value )
      return .f.
   endif

   return .t.

//-------------------------------------------------------------------//
//
//     nComFlush is xBase++ compatible
//
function Com_Close( ncPort, nComFlush )
   local nHandle,n
   local cPort := Com_PortName( ncPort )

   DEFAULT nComFlush TO 0   // Flush the receive buffer

   if ( nHandle := GetValidHandle( cPort ) ) == 0
      return .f.
   endif

   if nComFlush == 0
      com_flush( ncPort )
   endif
   CloseHandle( nHandle )

   n := ascan( ports_, {|e_| e_[ 1 ] == cPort } )
   ports_[ n,2 ] := 0

   return .t.

//-------------------------------------------------------------------//

function Com_Send( nPort, cString )
   local nHandle, nBytes := 0, nBytesWritten

   if ( nHandle := GetValidHandle( nPort ) ) <> 0
      if !WriteFile( nHandle, @cString, @nBytesWritten )
         nBytes := len( cString ) - nBytesWritten
      endif
   else
      nBytes := len( cString )
   endif

   return nBytes

//-------------------------------------------------------------------//

function com_Read( nPort, nBytesToRead, lDelete )
   local nBytesRead := 0, nHandle, cBuffer

   DEFAULT nBytesToRead TO 1024
   DEFAULT lDelete      TO .t.

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return ''
   endif

   cBuffer := space( nBytesToRead )
   if !ReadFile( nHandle, @cBuffer, nBytesToRead, @nBytesRead )
      cBuffer := ''
   endif

   return cBuffer

//-------------------------------------------------------------------//
//
//  Returns the number of bytes in receiving buffer
//
//  if ( nCount := com_count( 1 ) ) > 0
//    cStr := Com_Read( 1,nCount )
//  endif
//
function com_count( nPort )
   local nCount := -1
   local CommStat IS COMSTAT
   local cCommStat := CommStat:value
   local nHandle, nError

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return nCount
   endif

   if ClearCommError( nHandle, @nError, @cCommStat )
      CommStat:buffer( cCommStat )
      nCount := CommStat:cbInQue
   endif

   return nCount

//-------------------------------------------------------------------//

function com_scount( nPort )
   local nCount := -1
   local CommStat IS COMSTAT
   local cCommStat := CommStat:value
   local nHandle, nError

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return nCount
   endif

   if ClearCommError( nHandle, @nError, @cCommStat )
      CommStat:buffer( cCommStat )
      nCount := CommStat:cbOutQue
   endif

   return nCount

//-------------------------------------------------------------------//
//
//  Empty the In buffer at once
//
//  Com_send( 1, 'ATZ' )
//  Com_Flush( 1 )       // Discard modem's receipt messages
//
function com_flush( nPort )
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   return PurgeComm( nHandle, PURGE_RXCLEAR )

//-------------------------------------------------------------------//

function com_sflush( nPort )
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   return PurgeComm( nHandle, PURGE_TXCLEAR )

//-------------------------------------------------------------------//

function com_break( nPort, nDuration )
   local nHandle

   DEFAULT nDuration TO 100

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if SetCommBreak( nHandle )
      Sleep( nDuration )
      ClearCommBreak( nHandle )
      return .t.
   endif

   return .F.

//-------------------------------------------------------------------//
//
//  Data Carrier Detect state   ??
//
function com_dcd()
   return .F.

//-------------------------------------------------------------------//
//
//  Checks whether Clear To Send ( CTS ) signal is active
//
//  do while !com_cts( 1 )
//  enddo
//
//  Com_Send( 1, 'ATZ' )
//
function com_cts( nPort )
   local lActive := .F., nStatus
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if GetCommModemStatus( nHandle, @nStatus )
      lActive := ( and( nStatus, MS_CTS_ON ) == MS_CTS_ON )
   endif

   return lActive

//-------------------------------------------------------------------//
//
//   Retrievs the status of Data-set-Ready signal
//
function com_dsr( nPort )
   local lActive := .F., nStatus
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if GetCommModemStatus( nHandle, @nStatus )
      lActive := ( and( nStatus, MS_DSR_ON ) == MS_DSR_ON )
   endif

   return lActive

//-------------------------------------------------------------------//
//
//  Retreives whether Ring Indicator Signal is ON
//
function com_ring( nPort )
   local lActive := .F., nStatus
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if GetCommModemStatus( nHandle, @nStatus )
      lActive := ( and( nStatus, MS_RING_ON ) == MS_RING_ON )
   endif

   return lActive

//-------------------------------------------------------------------//
//
//   Receive Line Signal Detect
//
function com_RLSD( nPort )
   local lActive := .F., nStatus
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if GetCommModemStatus( nHandle, @nStatus )
      lActive := ( and( nStatus, MS_RLSD_ON ) == MS_RLSD_ON )
   endif

   return lActive

//-------------------------------------------------------------------//
//
//   Queries or sets Data Terminal Ready status
//
FUNCTION com_dtr( nPort, lNewStatus )
   local lOldStatus
   local CommStat IS COMSTAT
   local cCommStat := CommStat:value
   local nHandle, nError

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return nil
   endif

   if ClearCommError( nHandle, @nError, @cCommStat )
      CommStat:buffer( cCommStat )
      lOldStatus := CheckBit( CommStat:data, 2 )    // fDtrHold
   endif
   if valtype( lNewStatus ) == 'L'
      EscapeCommFunction( nHandle, if( lNewStatus, SETDTR, CLRDTR ) )
   endif

   return lOldStatus

//-------------------------------------------------------------------//
//
//   Queries or sets Ready-to-Send status
//
FUNCTION com_rts( nPort, lNewStatus )
   local lOldStatus := .f.
   local CommStat IS COMSTAT
   local cCommStat := CommStat:value
   local nHandle, nError

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return nil
   endif

   if ClearCommError( nHandle, @nError, @cCommStat )
      CommStat:buffer( cCommStat )
      lOldStatus := CheckBit( CommStat:data, 1 )   // fCtsHold
   endif

   if valtype( lNewStatus ) == 'L'
      EscapeCommFunction( nHandle, if( lNewStatus, SETRTS, CLRRTS ) )
   endif

   return lOldStatus

//-------------------------------------------------------------------//

function com_Soft( nPort, lOnOff, ncONChar, ncOFFChar )
   local lOldOnOff := .f.
   local DCB IS DCB
   local cDcb, nHandle, nOnChar, nOffChar, cType

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   cType := valtype( ncONChar )
   if     cType == 'U'
      nOnChar := 19
   elseif cType == 'N'
      nOnChar := ncONChar
   elseif cType == 'C'
      nOnChar := asc( ncONChar )
   endif

   cType := valtype( ncOFFChar )
   if     cType == 'U'
      nOffChar := 17
   elseif cType == 'N'
      nOffChar := ncOFFChar
   elseif cType == 'C'
      nOffChar := asc( ncOFFChar )
   endif

   dcb:reset()
   dcb:DCBLength := dcb:sizeof
   cDcb := dcb:value
   if GetCommState( nHandle, @cDcb )
      dcb:buffer( cDcb )
      lOldOnOff := CheckBit( DCB:data, BitFOutX )

      if valtype( lOnOff ) == 'L'
         DCB:data     := SetBit( DCB:data, BitFOutX, if( lOnOff, 1, 0 ) )
         DCB:XonChar  := nOnChar
         DCB:XoffChar := nOffChar
         if !SetCommState( nHandle, DCB:value )
//            V32Debug( { 'Could not Set Software Handshake!' } )
            lOldOnOff := .f.
         endif
      endif
   endif

   return lOldOnOff

//-------------------------------------------------------------------//

function com_hard( nPort, lNewSetting, lDsr )
   local lOldSetting := .f.
   local DCB IS DCB
   local cDcb, nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   DEFAULT lDsr TO .F.   // CTS/RTS Handshake to be taken care of

   dcb:reset()
   dcb:DCBLength := dcb:sizeof
   cDcb := dcb:value
   if GetCommState( nHandle, @cDcb )
      dcb:buffer( cDcb )
      lOldSetting := CheckBit( DCB:data, if( lDsr, BitFOutxDsrFlow, BitFOutxCtsFlow ) )
   endif

   if valtype( lNewSetting ) == 'L'
      if lDsr
         DCB:data := SetBit( DCB:data, BitFOutxDsrFlow, if( lNewSetting, 1, 0 ) )
         DCB:data := SetBit( DCB:data, 5, if( lNewSetting, 1, 0 ) )
         DCB:data := SetBit( DCB:data, 6, 0 )
      else
         DCB:data := SetBit( DCB:data, BitFOutxCtsFlow, if( lNewSetting, 1, 0 ) )
         DCB:data := SetBit( DCB:data,13, if( lNewSetting, 1, 0 ) )
         DCB:data := SetBit( DCB:data,14, 0 )
      endif
      if !SetCommState( nHandle, DCB:value )
         lOldSetting := .F.                   //  an error condition
      endif
   endif

   return lOldSetting

//-------------------------------------------------------------------//

function Com_Soft_R( nPort, lNewStatus )
   local lOldStatus
   local CommStat IS COMSTAT
   local cCommStat := CommStat:value
   local nHandle, nError

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if ClearCommError( nHandle, @nError, @cCommStat )
      CommStat:buffer( cCommStat )
      lOldStatus := CheckBit( CommStat:data, BitFXoffHold )
   endif
   if valtype( lNewStatus ) == 'L'
      EscapeCommFunction( nHandle, if( lNewStatus, SETXON, SETXOFF ) )
   endif

   return lOldStatus

//-------------------------------------------------------------------//

function Com_Soft_S( nPort )
   local lOldStatus
   local CommStat IS COMSTAT
   local cCommStat := CommStat:value
   local nHandle, nError

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if ClearCommError( nHandle, @nError, @cCommStat )
      CommStat:buffer( cCommStat )
      lOldStatus := CheckBit( CommStat:data, BitFXoffSent )
   endif

   return lOldStatus

//-------------------------------------------------------------------//
//
//  Extension . Executes EscapeCommFunction()
//  nFunction := aNYoNE( CLRRTS, CLRDTR, SETRTS, SETDTR, SETXOFF, SETXON, SETBREAK, CLRBREAK )
//
function com_escape( nPort, nFunction )
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   return EscapeCommFunction( nHandle, nFunction )

//-------------------------------------------------------------------//
//
//  nMask := Any combination of the following
//  EV_BREAK, EV_CTS, EV_DSR,  ...
//
function com_SetMask( nPort, nMask )
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   return SetCommMask( nHandle, nMask )

//-------------------------------------------------------------------//
//
//
function com_GetMask( nPort )
   local nHandle, nEvMask := -1

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return -1
   endif

   if !GetCommMask( nHandle, @nEvMask )
      nEvMask := -1
   endif

   return nEvMask

//-------------------------------------------------------------------//
//
//  Waits until one of the specified event occurs
//
function Com_WaitEvent( nPort )
   local nHandle, nEvent := -1

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return -1
   endif

   if !WaitCommEvent( nHandle, @nEvent )
      nEvent := -1
   endif

   return nEvent

//-------------------------------------------------------------------//
//
//
//
function Com_Transmit( nPort, ncChar )
   local cChar
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if valtype( ncChar ) == 'N'
      cChar := chr( ncChar )
   elseif valtype( ncChar ) == 'C'
      cChar := ncChar
   else
      return .F.
   endif

   return TransmitCommChar( nHandle, cChar )

//-------------------------------------------------------------------//

function com_dcb( nPort, aDcb )
   local DCB IS DCB
   local cDcb := DCB:value
   local dcb_:={}, nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return dcb_
   endif

   GetCommState( nHandle, @cDcb )
   dcb:buffer( cDcb )
   dcb_:= Com_Struct2Array( DCB,'dcb' )

   if aDcb <> NIL .AND. valtype( aDcb ) == 'A'
      DCB:BaudRate  := aDcb[ dcbBaudRate  ]

      DCB:data      := SetBit( DCB:data, BitFBinary          , if( aDcb[ dcbFBinary           ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFParity          , if( aDcb[ dcbFParity           ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFOutxCtsFlow     , if( aDcb[ dcbFOutxCtsFlow      ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFOutxDsrFlow     , if( aDcb[ dcbFOutxDsrFlow      ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFDtrControl      , if( aDcb[ dcbFDtrControl       ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFDsrSensitivity  , if( aDcb[ dcbFDsrSensitivity   ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFTxContinueOnXoff, if( aDcb[ dcbFTxContinueOnXoff ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFOutX            , if( aDcb[ dcbFOutX             ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFInX             , if( aDcb[ dcbFInX              ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFErrorChar       , if( aDcb[ dcbFErrorChar        ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFNull            , if( aDcb[ dcbFNull             ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFRtsControl      , if( aDcb[ dcbFRtsControl       ], 1, 0 ) )
      DCB:data      := SetBit( DCB:data, BitFAbortOnError    , if( aDcb[ dcbFAbortOnError     ], 1, 0 ) )

      DCB:XonLim    := aDcb[ dcbXonLim    ]
      DCB:XoffLim   := aDcb[ dcbXoffLim   ]
      DCB:ByteSize  := aDcb[ dcbByteSize  ]
      DCB:Parity    := aDcb[ dcbParity    ]
      DCB:StopBits  := aDcb[ dcbStopBits  ]
      DCB:XonChar   := aDcb[ dcbXonChar   ]
      DCB:XoffChar  := aDcb[ dcbXoffChar  ]
      DCB:ErrorChar := aDcb[ dcbErrorChar ]
      DCB:EofChar   := aDcb[ dcbEofChar   ]
      DCB:EvtChar   := aDcb[ dcbEvtChar   ]

      SetCommState( nHandle, DCB:value )
   endif

   return dcb_

//-------------------------------------------------------------------//
//
//   cDevice := 'Standard Modem over IR link #4'  Device name as in Device Manager
//
function Com_Dialog( cDevice, hWnd )
   local CommConfig IS COMMCONFIG
   local cCommConfig
   local dcb_:={}

   CommConfig:dwSize := CommConfig:sizeof
   cCommConfig       := CommConfig:value
   if CommConfigDialog( cDevice, hWnd, @cCommConfig )
      CommConfig:buffer( cCommConfig )
      dcb_:= com_struct2array( CommConfig:DCB, 'dcb' )
   endif

   return dcb_

//-------------------------------------------------------------------//

function Com_DefaultConfig( cDevice )
   local CommConfig IS COMMCONFIG
   local cCommConfig
   local dcb_:={}

   CommConfig:dwSize := CommConfig:sizeof
   cCommConfig       := CommConfig:value
   if GetDefaultCommConfig( cDevice, @cCommConfig )
      CommConfig:buffer( cCommConfig )
      dcb_:= com_struct2array( CommConfig:DCB, 'dcb' )
   endif

   return dcb_

//-------------------------------------------------------------------//

function Com_Config( nPort )
   local CommConfig IS COMMCONFIG
   local cCommConfig
   local dcb_:={}
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return dcb_
   endif

   CommConfig:dwSize := CommConfig:sizeof
   cCommConfig       := CommConfig:value
   if GetCommConfig( nHandle, @cCommConfig )
      CommConfig:buffer( cCommConfig )
      dcb_:= com_struct2array( CommConfig:DCB, 'dcb' )
   endif

   return dcb_

//-------------------------------------------------------------------//

function Com_stat( nPort )
   local stat_:={}
   local CommStat IS COMSTAT
   local cCommStat := CommStat:value
   local nHandle, nError

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return stat_
   endif

   if ClearCommError( nHandle, @nError, @cCommStat )
      CommStat:buffer( cCommStat )
      stat_:= com_struct2array( CommStat, 'stat' )
   endif

   return stat_

//-------------------------------------------------------------------//

function com_properties( nPort )
   local prop_:={}
   local CommProp IS COMMPROP
   local cCommProp := CommProp:value
   local nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   CommProp:wPacketLength := CommProp:sizeof
   if GetCommProperties( nHandle, @cCommProp )
      CommProp:Buffer( cCommProp )
      prop_:= com_struct2array( CommProp, 'properties' )
   endif

   return prop_

//-------------------------------------------------------------------//

function com_timeouts( nPort, aTimeOuts )
   local TimeOuts IS COMMTIMEOUTS
   local cTimeOuts := TimeOuts:Value
   local tme_:={}, nHandle

   if ( nHandle := GetValidHandle( nPort ) ) == 0
      return .f.
   endif

   if GetCommTimeouts( nHandle, @cTimeOuts )
      TimeOuts:buffer( cTimeOuts )
      tme_:= com_struct2array( TimeOuts, 'timeouts' )
   endif

   if valtype( aTimeOuts ) == 'A' .AND. len( aTimeOuts ) == 5
      TimeOuts:ReadIntervalTimeout         := aTimeOuts[ 1 ]
      TimeOuts:ReadTotalTimeoutMultiplier  := aTimeOuts[ 2 ]
      TimeOuts:ReadTotalTimeoutConstant    := aTimeOuts[ 3 ]
      TimeOuts:WriteTotalTimeoutMultiplier := aTimeOuts[ 4 ]
      TimeOuts:WriteTotalTimeoutConstant   := aTimeOuts[ 5 ]
      SetCommTimeouts( nHandle, TimeOuts:value )
   endif

   return tme_

//-------------------------------------------------------------------//

function com_error( nPort )
   (nPort)
   return GetLastError()

//-------------------------------------------------------------------//

function Com_SendString( nPort, cString )
   local nBytes

   cString += chr( 13 )

   nBytes := Com_Send( nPort, cString )

   return ( nBytes == 0 )

//-------------------------------------------------------------------//

function Com_ReadString( nPort, nTimeout )
   local cStr  := '', s
   local nTime := seconds()

   DEFAULT nTimeout TO 10

   do while .T.
      s := com_read( nPort,1 )
      if s == chr( 10 )
         exit
      endif
      if s <> chr( 13 )
         cStr += s
      endif
      if seconds()-nTime >= nTimeout
         exit
      endif
   enddo

   return cStr

//-------------------------------------------------------------------//

static function com_struct2array( cStruct, cName )
   local a_:={}, bit5, bit6, bit13, bit14, nDtr, nRts

   do case

   case cName == 'dcb'
      bit5  := CheckBit( cStruct:data,  5   )
      bit6  := CheckBit( cStruct:data,  6   )
      bit13 := CheckBit( cStruct:data, 13   )
      bit14 := CheckBit( cStruct:data, 14   )
      nDtr  := if( bit5  .AND. bit6,  3, if( bit5  .AND. !bit6,  2, if( !bit5  .AND. bit6,  1, 0 ) ) )
      nRts  := if( bit13 .AND. bit14, 3, if( bit13 .AND. !bit14, 2, if( !bit13 .AND. bit14, 1, 0 ) ) )

      aadd( a_, cStruct:BaudRate            )  //  1 current baud rate
      aadd( a_, CheckBit( cStruct:data, 1 ) )  //  2 fBinary        . binary mode, no EOF check
      aadd( a_, CheckBit( cStruct:data, 2 ) )  //  3 fParity        . enable parity checking
      aadd( a_, CheckBit( cStruct:data, 3 ) )  //  4 fOutxCtsFlow   . CTS output flow control
      aadd( a_, CheckBit( cStruct:data, 4 ) )  //  5 fOutxDsrFlow   . DSR output flow control
      aadd( a_, nDtr                        )  //  6 fDtrControl    . +6  // DTR flow control type
      aadd( a_, CheckBit( cStruct:data, 7 ) )  //  7 fDSRSensitivity   . DSR sensitivity
      aadd( a_, CheckBit( cStruct:data, 8 ) )  //  8 fTXContinueOnXoff . XOFF continues Tx
      aadd( a_, CheckBit( cStruct:data, 9 ) )  //  9 fOutX          . XON/XOFF out flow control
      aadd( a_, CheckBit( cStruct:data, 10) )  // 10 fInx           . XON/XOFF in flow control
      aadd( a_, CheckBit( cStruct:data, 11) )  // 11 fErrorChar     . enable error replacement
      aadd( a_, CheckBit( cStruct:data, 12) )  // 12 fNull          . enable null stripping
      aadd( a_, nRts                        )  // 13 fRtsControl    . +14  // RTS flow control
      aadd( a_, CheckBit( cStruct:data, 15) )  // 14 fAbortOnError  . abort on error
      aadd( a_, cStruct:XonLim              )  // 15 transmit XON threshold
      aadd( a_, cStruct:XoffLim             )  // 16 transmit XOFF threshold
      aadd( a_, cStruct:ByteSize            )  // 17 number of bits/byte, 4-8
      aadd( a_, cStruct:Parity              )  // 18 0-4=no,odd,even,mark,space
      aadd( a_, cStruct:StopBits            )  // 19 0,1,2 = 1, 1.5, 2
      aadd( a_, cStruct:XonChar             )  // 20 Tx and Rx XON character
      aadd( a_, cStruct:XoffChar            )  // 21 Tx and Rx XOFF character
      aadd( a_, cStruct:ErrorChar           )  // 22 error replacement character
      aadd( a_, cStruct:EofChar             )  // 23 end of input character
      aadd( a_, cStruct:EvtChar             )  // 24 received event character

   case cName == 'config'
      aadd( a_, cStruct:wVersion            )
      aadd( a_, cStruct:dwProviderSubType   )
      aadd( a_, cStruct:dwProviderOffset    )
      aadd( a_, cStruct:dwProviderSize      )

   case cName == 'properties'
      aadd( a_, cStruct:wPacketLength       )
      aadd( a_, cStruct:wPacketVersion      )
      aadd( a_, cStruct:dwServiceMask       )
      aadd( a_, cStruct:dwReserved1         )
      aadd( a_, cStruct:dwMaxTxQueue        )
      aadd( a_, cStruct:dwMaxRxQueue        )
      aadd( a_, cStruct:dwMaxBaud           )
      aadd( a_, cStruct:dwProvSubType       )
      aadd( a_, cStruct:dwProvCapabilities  )
      aadd( a_, cStruct:dwSettableParams    )
      aadd( a_, cStruct:dwSettableBaud      )
      aadd( a_, cStruct:wSettableData       )
      aadd( a_, cStruct:wSettableStopParity )
      aadd( a_, cStruct:dwCurrentTxQueue    )
      aadd( a_, cStruct:dwCurrentRxQueue    )
      aadd( a_, cStruct:dwProvSpec1         )
      aadd( a_, cStruct:dwProvSpec2         )

   case cName == 'timeouts'
      aadd( a_,cStruct:ReadIntervalTimeout         )
      aadd( a_,cStruct:ReadTotalTimeoutMultiplier  )
      aadd( a_,cStruct:ReadTotalTimeoutConstant    )
      aadd( a_,cStruct:WriteTotalTimeoutMultiplier )
      aadd( a_,cStruct:WriteTotalTimeoutConstant   )

   case cName == 'stat'
      aadd( a_, CheckBit( cStruct:data, BitFCtsHold   ) )
      aadd( a_, CheckBit( cStruct:data, BitFDsrHold   ) )
      aadd( a_, CheckBit( cStruct:data, BitFRlsdHold  ) )
      aadd( a_, CheckBit( cStruct:data, BitFXoffHold  ) )
      aadd( a_, CheckBit( cStruct:data, BitFXoffSent  ) )
      aadd( a_, CheckBit( cStruct:data, BitFEof       ) )
      aadd( a_, CheckBit( cStruct:data, BitFTxim      ) )
      aadd( a_, cStruct:cbInQue                         )
      aadd( a_, cStruct:cbOutQue                        )

   endcase

   return a_

//-------------------------------------------------------------------//
#pragma BEGINDUMP
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                 Pritpal Bedi <pritpal@vouchcac.com>
//                Serial Communication WinApi Functions
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#define _WIN32_WINNT   0x0400

//-------------------------------------------------------------------//

#include <windows.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "tchar.h"

//-------------------------------------------------------------------//

HB_FUNC_STATIC( AND )
{
  hb_retnl( hb_parnl(1) & hb_parnl(2) ) ;
}

//-------------------------------------------------------------------//

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

//-------------------------------------------------------------------//

HB_FUNC_STATIC( GETLASTERROR )
{
  hb_retnl( ( LONG ) GetLastError() ) ;
}

//-------------------------------------------------------------------//

HB_FUNC_STATIC( SLEEP )
{
   Sleep( (DWORD) hb_parnl( 1 ) ) ;
}

//-------------------------------------------------------------------//
/*
 HANDLE CreateFile(
  LPCTSTR lpFileName,          // pointer to name of the file
  DWORD dwDesiredAccess,       // access (read-write) mode
  DWORD dwShareMode,           // share mode
  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
                               // pointer to security attributes
  DWORD dwCreationDisposition,  // how to create
  DWORD dwFlagsAndAttributes,  // file attributes
  HANDLE hTemplateFile         // handle to file with attributes to
                               // copy
                  );
*/
HB_FUNC_STATIC( CREATEFILE )
{
   SECURITY_ATTRIBUTES *sa ;

   if( ISCHAR( 4 ) )
      sa = ( SECURITY_ATTRIBUTES *) hb_param( 4, HB_IT_STRING )->item.asString.value ;

   hb_retnl( (LONG) CreateFile( (LPCTSTR) hb_parcx(1),
                                (DWORD)   hb_parnl(2),
                                (DWORD)   hb_parnl(3),
                                ISCHAR( 4 ) ? (SECURITY_ATTRIBUTES *) sa : NULL ,
                                (DWORD) hb_parnl(5),
                                (DWORD) hb_parnl(6),
                                ISNIL( 7 ) ? NULL : (HANDLE) hb_parnl(7) ) ) ;
}

//-------------------------------------------------------------------//
/*
BOOL CloseHandle(
  HANDLE hObject   // handle to object to close
);
*/
HB_FUNC_STATIC( CLOSEHANDLE )
{
  hb_retl( CloseHandle( (HANDLE) hb_parnl(1) ) );
}

//-------------------------------------------------------------------//
/*
 BOOL ReadFile(
  HANDLE hFile,                // handle of file to read
  LPVOID lpBuffer,             // pointer to buffer that receives data
  DWORD nNumberOfBytesToRead,  // number of bytes to read
  LPDWORD lpNumberOfBytesRead, // pointer to number of bytes read
  LPOVERLAPPED lpOverlapped    // pointer to structure for data
              );
*/
HB_FUNC_STATIC( READFILE )
{
   char * Buffer = ( char * ) hb_xgrab( hb_parnl( 3 ) ) ;
   DWORD nRead   = 0      ;
   BOOL  bRet             ;
   OVERLAPPED *Overlapped ;

   if( ISCHAR( 5 ) )
      Overlapped = ( OVERLAPPED *) hb_param( 5, HB_IT_STRING )->item.asString.value ;


   bRet = ReadFile( (HANDLE) hb_parnl( 1 ) ,
                    Buffer                 ,
                    (DWORD)  hb_parnl( 3 ) ,
                    &nRead        ,
                    ISCHAR( 5 ) ? Overlapped : NULL ) ;

   if ( bRet )
        hb_storclen( ( char * ) Buffer, nRead, 2 ) ;

   hb_stornl( nRead, 4 ) ;
   hb_retl( bRet ) ;
}

//-------------------------------------------------------------------//
/*
BOOL WriteFile(
  HANDLE hFile,                    // handle to file to write to
  LPCVOID lpBuffer,                // pointer to data to write to file
  DWORD nNumberOfBytesToWrite,     // number of bytes to write
  LPDWORD lpNumberOfBytesWritten,  // pointer to number of bytes written
  LPOVERLAPPED lpOverlapped        // pointer to structure for overlapped I/O
                );
*/
HB_FUNC_STATIC( WRITEFILE )
{
   DWORD nWritten = 0     ;
   OVERLAPPED *Overlapped ;

   if( ISCHAR( 4 ))
     Overlapped = ( OVERLAPPED *) hb_param( 4, HB_IT_STRING )->item.asString.value ;

   hb_retl ( WriteFile( (HANDLE)  hb_parnl( 1 )   ,
                     hb_parcx( 2 )       ,
                     hb_parclen( 2 )    ,
                     &nWritten          ,
                     ISCHAR( 4 ) ? Overlapped : NULL ) ) ;

   hb_stornl( nWritten, 3 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL BuildCommDCB(
  LPCTSTR lpDef,  // device-control string                           IN
  LPDCB   lpDCB   // device-control block                           OUT
);
//
local dcb IS DCB
local cComParam := 'COM1: baud=9600 parity=N data=8 stop=1'
local dcbInfo   := dcb:value

BuildComm( cComParam, @dcbInfo )
dcb:buffer( dcbInfo )
*/
HB_FUNC_STATIC( BUILDCOMMDCB )
{
   DCB dcb ;

   hb_retl( BuildCommDCB( ( LPCTSTR ) hb_parcx( 1 ), &dcb ) );

   hb_storclen( ( char * ) &dcb, sizeof( DCB ), 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL BuildCommDCBAndTimeouts(
  LPCTSTR         lpDef,          // device-control string           IN
  LPDCB           lpDCB,          // device-control block           OUT
  LPCOMMTIMEOUTS  lpCommTimeouts  // device time-out values          IN
);
local dcb IS DCB
local CommTimeOuts IS COMMTIMEOUTS
local dcbInfo := dcb:value
local cComParam := 'COM1: baud=9600 parity=N data=8 stop=1 to=ON'

BuildComDCBAndTimeouts( cCommParam, @dcbInfo, CommTimeOuts:value )
dcb:buffer( dcbInfo )
*/
//
HB_FUNC_STATIC( BUILDCOMMDCBANDTIMEOUTS )
{
   DCB dcb ;
   LPCOMMTIMEOUTS lptimeouts = ( LPCOMMTIMEOUTS ) hb_parcx( 3 );
   hb_retl( BuildCommDCBAndTimeouts( ( LPCTSTR ) hb_parcx( 1 ), &dcb, lptimeouts ) ) ;

   hb_storclen( ( char * ) &dcb, sizeof( DCB ), 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL ClearCommBreak(
  HANDLE hFile   // handle to communications device                  IN
);
//
local hFile := CreateFile( ... )
if ClearCommBreak( hFile )
   // Your code goes here
endif
*/
HB_FUNC_STATIC( CLEARCOMMBREAK )
{
   hb_retl( ClearCommBreak( ( HANDLE ) hb_parnl( 1 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL ClearCommError(
  HANDLE     hFile,     // handle to communications device           IN
  LPDWORD    lpErrors,  // error codes                              OUT
  LPCOMSTAT  lpStat     // communications status                    OUT
);
if ClearCommError( hFile, @nError, @cComStat )
   // Proceed with fresh i/o
endif
*/
HB_FUNC_STATIC( CLEARCOMMERROR )
{
   DWORD   err = 0 ;
   COMSTAT Stat ;

   hb_retl( ClearCommError( ( HANDLE ) hb_parnl( 1 ), &err, &Stat ) );

   hb_stornl( err, 2 );
   hb_storclen( ( char * ) &Stat, sizeof( COMSTAT ), 3 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL CommConfigDialog(
  LPCTSTR lpszName,   // device name string                          IN
  HWND hWnd,          // handle to window                            IN
  LPCOMMCONFIG lpCC   // configuration information               IN/OUT
);
local cDeviceName := 'Standard Modem over IR link #4'
local hWnd        := nil
local CommConfig IS COMMCONFIG
local cCommConfig := CommConfig:value

if CommConfigDialog( cDeviceName, hWnd, @cCommConfig )
   ? 'Hurray'
   CommConfig:buffer( cCommConfig )
endif
*/
HB_FUNC_STATIC( COMMCONFIGDIALOG )
{
   LPCTSTR      lpszName = ( LPCTSTR ) hb_parcx( 1 );
   HWND         hwnd     = ISNIL( 2 ) ? NULL : ( HWND ) hb_parnl( 2 );
   LPCOMMCONFIG lpCC     = ( LPCOMMCONFIG ) hb_parcx( 3 ) ;

   hb_retl( CommConfigDialog( lpszName, hwnd, lpCC ) );

   hb_storclen( ( char * ) lpCC, sizeof( COMMCONFIG ), 3 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL EscapeCommFunction(
  HANDLE hFile,   // handle to communications device                 IN
  DWORD  dwFunc   // extended function to perform                    IN
);
local nFunc := CLRDTR  // CLRRTS, SETDTR, SETRTS, SETXOFF, SETXON, SETBREAK, CLRBREAK - one of these values

if EscapeCommFunction( hFile, nFunc )
   // ok
endif
*/
HB_FUNC_STATIC( ESCAPECOMMFUNCTION )
{
   hb_retl( EscapeCommFunction( ( HANDLE ) hb_parnl( 1 ), hb_parnl( 2 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL GetCommConfig(
  HANDLE hCommDev,    // handle to communications service            IN
  LPCOMMCONFIG lpCC,  // configuration information                  OUT
  LPDWORD lpdwSize    // size of buffer                          IN/OUT
);
if GetCommConfig( hFile, @cCommConfig )
   CommConfig:buffer( cCommConfig )
endif
*/
HB_FUNC_STATIC( GETCOMMCONFIG )
{
   COMMCONFIG lpCC ; // = ( LPCOMMCONFIG ) hb_parcx( 2 );
   DWORD        size = sizeof( COMMCONFIG );

   hb_retl( GetCommConfig( ( HANDLE ) hb_parnl( 1 ), &lpCC, &size ) ) ;

   hb_storclen( ( char * ) &lpCC, size, 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL GetCommMask(
  HANDLE  hFile,      // handle to communications device             IN
  LPDWORD lpEvtMask   // event mask                                 OUT
);
if GetCommMask( hFile, @nMask )
   if nMask == EV_BREAK + EV_CTS + ....
   endif
endif
*/
HB_FUNC_STATIC( GETCOMMMASK )
{
   DWORD mask;
   hb_retl( GetCommMask( ( HANDLE ) hb_parnl( 1 ), &mask ) ) ;
   hb_stornl( ( ULONG ) mask, 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL GetCommModemStatus(
  HANDLE  hFile,        // handle to communications device           IN
  LPDWORD lpModemStat   // control-register values                  OUT
);
if GetCommModemStatus( hFile, @nStat )
   if nStat == MS_CTS_ON + MS_DSR_ON ...
   endif
endif
*/
HB_FUNC_STATIC( GETCOMMMODEMSTATUS )
{
   DWORD modemStat ;
   hb_retl( GetCommModemStatus( ( HANDLE ) hb_parnl( 1 ), &modemStat ) ) ;
   hb_stornl( ( ULONG ) modemStat, 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL GetCommProperties(
  HANDLE     hFile,       // handle to comm device                   IN
  LPCOMMPROP lpCommProp   // communications properties              OUT
);
local CommProp IS COMMPROP
local cCommProp := CommProp:value
GetCommProperties( hFile, @cCommProp )
CommProp:buffer( cCommProp )
*/
HB_FUNC_STATIC( GETCOMMPROPERTIES )
{
   COMMPROP CommProp ;
   CommProp.wPacketLength = sizeof( COMMPROP );

   hb_retl( GetCommProperties( ( HANDLE ) hb_parnl( 1 ), &CommProp ) );

   hb_storclen( ( char * ) &CommProp, sizeof( COMMPROP ), 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL GetCommState(
  HANDLE hFile,  // handle to communications device                  IN
  LPDCB  lpDCB   // device-control block                            OUT
);
GetCommState( hFile, @cDcb )
dcb:buffer( cDcb )
*/
HB_FUNC_STATIC( GETCOMMSTATE )
{
   DCB dcb ;
   dcb.DCBlength = sizeof( DCB ) ;

   hb_retl( GetCommState( ( HANDLE ) hb_parnl( 1 ), &dcb ) );

   hb_storclen( ( char * ) &dcb, sizeof( DCB ), 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL GetCommTimeouts(
  HANDLE         hFile,          // handle to comm device            IN
  LPCOMMTIMEOUTS lpCommTimeouts  // time-out values                 OUT
);
GetCommTimeouts( cFile, @cCommTimeouts )
CommTimeouts:buffer( cCommTimeouts )
*/
HB_FUNC_STATIC( GETCOMMTIMEOUTS )
{
   COMMTIMEOUTS Timeouts ;

   hb_retl( GetCommTimeouts( ( HANDLE ) hb_parnl( 1 ), &Timeouts ) );

   hb_storclen( ( char * ) &Timeouts, sizeof( COMMTIMEOUTS ), 2 ) ;
}

//-------------------------------------------------------------------//
/*
BOOL GetDefaultCommConfig(
  LPCTSTR      lpszName,    // device name string                    IN
  LPCOMMCONFIG lpCC,        // configuration information            OUT
  LPDWORD      lpdwSize     // size of buffer                        IN
);
GetDefaultCommConfig( 'Standard Modem over IR link #4', @cCommConfig )
CommConfig:buffer( cCommConfig )
*/
HB_FUNC_STATIC( GETDEFAULTCOMMCONFIG )
{
   char *Buffer = (char *) hb_xgrab( sizeof( COMMCONFIG ) );
   DWORD size = sizeof( COMMCONFIG );

   if ( GetDefaultCommConfig( ( LPCTSTR ) hb_parcx( 1 ), ( COMMCONFIG * ) Buffer, &size ) == 0 )
   {
      hb_xfree( (void *) Buffer ) ;
      Buffer = (char *) hb_xgrab( size ) ;
      if ( GetDefaultCommConfig( ( LPCTSTR ) hb_parcx( 1 ), ( COMMCONFIG * ) Buffer, &size ) == 0 )
      {
         hb_xfree( Buffer ) ;
         hb_retl( FALSE ) ;
         return ;
      }
   }
   hb_retl( TRUE );
   hb_storclen( ( char * ) Buffer, size, 2 ) ;
   hb_xfree( Buffer );
}
//-------------------------------------------------------------------//
/*
BOOL PurgeComm(
  HANDLE hFile,  // handle to communications resource                IN
  DWORD dwFlags  // action to perform                                IN
);
local nActions := PURGE_TXABORT + PURGE_RXABORT // + ... ANY COMBINATION
if PurgeComm( hFile, nActions )
endif
*/
HB_FUNC_STATIC( PURGECOMM )
{
   hb_retl( PurgeComm( ( HANDLE ) hb_parnl( 1 ), hb_parnl( 2 ) ) ) ;
}

//-------------------------------------------------------------------//
/*
BOOL SetCommBreak(
  HANDLE hFile   // handle to communications device                  IN
);
*/
HB_FUNC_STATIC( SETCOMMBREAK )
{
   hb_retl( SetCommBreak( ( HANDLE ) hb_parnl( 1 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommConfig(
  HANDLE hCommDev,    // handle to communications device             IN
  LPCOMMCONFIG lpCC,  // configuration services                      IN
  DWORD dwSize        // size of structure                           IN
);
SetCommConfig( hFile, CommConfig:Value, nSize )
*/
HB_FUNC_STATIC( SETCOMMCONFIG )
{
   LPCOMMCONFIG lpCC = ( LPCOMMCONFIG ) hb_parcx( 2 );
   DWORD        size = ISNIL( 3 ) ? sizeof( COMMCONFIG ) : hb_parnl( 3 );

   hb_retl( SetCommConfig( ( HANDLE ) hb_parnl( 1 ), lpCC, size ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommMask(
  HANDLE hFile,     // handle to communications device               IN
  DWORD  dwEvtMask  // mask that identifies enabled events           IN
);
if SetCommMask( hFile, nEvtMask )
endif
*/
HB_FUNC_STATIC( SETCOMMMASK )
{
   hb_retl( SetCommMask( ( HANDLE ) hb_parnl( 1 ), hb_parnl( 2 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommState(
  HANDLE hFile,  // handle to communications device                  IN
  LPDCB lpDCB    // device-control block                             IN
);
*/
HB_FUNC_STATIC( SETCOMMSTATE )
{
   LPDCB lpDCB = ( LPDCB ) hb_parcx( 2 );

   hb_retl( SetCommState( ( HANDLE ) hb_parnl( 1 ), lpDCB ) ) ;
}

//-------------------------------------------------------------------//
/*
BOOL SetCommTimeouts(
  HANDLE         hFile,          // handle to comm device            IN
  LPCOMMTIMEOUTS lpCommTimeouts  // time-out values                  IN
);
*/
HB_FUNC_STATIC( SETCOMMTIMEOUTS )
{
   LPCOMMTIMEOUTS lptimeouts = ( LPCOMMTIMEOUTS ) hb_parcx( 2 ) ;

   hb_retl( SetCommTimeouts( ( HANDLE ) hb_parnl( 1 ), lptimeouts ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetDefaultCommConfig(
  LPCTSTR      lpszName, // device name string                       IN
  LPCOMMCONFIG lpCC,     // configuration information                IN
  DWORD        dwSize    // size of structure                        IN
);
*/
HB_FUNC_STATIC( SETDEFAULTCOMMCONFIG )
{
   LPCOMMCONFIG lpCC = ( LPCOMMCONFIG ) hb_parcx( 2 );
   DWORD        size = sizeof( COMMCONFIG ) ;

   hb_retl( SetDefaultCommConfig( ( LPCTSTR ) hb_parcx( 1 ), lpCC, size ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetupComm(
  HANDLE hFile,      // handle to communications device              IN
  DWORD  dwInQueue,  // size of input buffer                         IN
  DWORD  dwOutQueue  // size of output buffer                        IN
);
*/
HB_FUNC_STATIC( SETUPCOMM )
{
   hb_retl( SetupComm( ( HANDLE ) hb_parnl( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL TransmitCommChar(
  HANDLE hFile,  // handle to communications device                  IN
  char   cChar   // character to transmit                            IN
);
*/
HB_FUNC_STATIC( TRANSMITCOMMCHAR )
{
   hb_retl( TransmitCommChar( ( HANDLE ) hb_parnl( 1 ), ( char ) hb_parcx( 2 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL WaitCommEvent(
  HANDLE hFile,                // handle to comm device              IN
  LPDWORD lpEvtMask,           // event type mask                   OUT
  LPOVERLAPPED lpOverlapped,   // overlapped structure               IN    Not used here
);
if WaitCommEvent( hFile, @nEvent )
   if nEvent == EV_RSCHAR
      // do the needful
   endif
endif
*/
HB_FUNC_STATIC( WAITCOMMEVENT )
{
   DWORD evMask ;

   hb_retl( WaitCommEvent( ( HANDLE ) hb_parnl( 1 ), &evMask, NULL ) );
   hb_stornl( ( ULONG ) evMask, 2 ) ;
}

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
#pragma ENDDUMP
//-------------------------------------------------------------------//

