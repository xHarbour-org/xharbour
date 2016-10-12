/*
 * $Id$
 */

/*
* Tcomm Class for xBuilder
* Copyright (c) 2005 - Luiz Rafael Culik Guimaraes <culikr@bturbo.com.br>
* All Rights Reserved
*/
#define __TCOMM_PRG_
#include "hbclass.ch"
#include "common.ch"

#include "tcomm.ch"
CREATE CLASS TCOMM

   EXPORT:
      DATA nHandle
      DATA nBuffer AS Numeric
      DATA cPort
      DATA nBaudRate AS Numeric
      DATA nDatabits AS Numeric
      DATA nParity AS Numeric
      DATA nStopBits AS Numeric
      DATA nBufferSize AS Numeric
      Data cBuffer AS Character INIT ""
      DATA lCts INIT .F.
      DATA lDSR INIT .F.
      DATA lRing INIT .F.
      DATA lRLSD INIT .F.


      METHOD NEW( cPort, nBaudrate, nDatabits, nParity, nStopbits, nBuffersize )
      METHOD NEW2( cPort ) CONSTRUCTOR
      METHOD ISWORKING() INLINE Inkey( 0.2 ), ISWORKING( ::nHandle )
      METHOD CLOSE() INLINE UNINT_PORT( ::nHandle )
      METHOD OUTBUFSIZE() INLINE OUTBUFSIZE( ::nHandle )
      METHOD INBUFSIZE() INLINE INBUFSIZE( ::nHandle )
      METHOD WRITEPORT( cData, nLen )
      METHOD READPORT( iSize, xData )
      METHOD CLEAROUTBUFF() INLINE OUTBUFCLR( ::nHandle )
      METHOD GetModemStatus() inline GetModemStatus(::nHandle,@::lCts,@::lDSR,@::lRing,@::lRLSD)
      METHOD ISCTS()   inline ::lCts
      METHOD ISDSR()   inline ::lDSR
      METHOD ISRING()  inline ::lRing
      METHOD SETRTS()  inline SETCOMOPTIONS( ::nHandle, HSETRTS )
      METHOD CLRRTS()  inline SETCOMOPTIONS( ::nHandle, HCLRRTS )
      METHOD SETDTR()  inline SETCOMOPTIONS( ::nHandle, HSETDTR )
      METHOD CLRCTR()  inline SETCOMOPTIONS( ::nHandle, HCLRDTR )
      METHOD SETXOFF() inline SETCOMOPTIONS( ::nHandle, HSETXOFF )
      METHOD SETXON()  inline SETCOMOPTIONS( ::nHandle, HSETXON ) 

      METHOD SETHANDSHAKEOFF() inline SETHANDSHAKE( ::nHandle, EHANDSHAKEOFF)
      METHOD SETHANDSHAKEHARDWARE() inline SETHANDSHAKE( ::nHandle, EHANDSHAKEHARDWARE )
      METHOD SETHANDSHAKESOFTWARE() inline SETHANDSHAKE( ::nHandle, EHANDSHAKESOFTWARE )


ENDCLASS

METHOD NEW( cPort, nBaudrate, nDatabits, nParity, nStopbits, nBufferSize ) CLASS tComm

   DEFAULT cPort TO "COM1"
   DEFAULT nBaudrate TO 9600
   DEFAULT nDatabits TO 8
   DEFAULT nParity TO 0
   DEFAULT nStopbits TO 0
   DEFAULT nBuffersize TO 4000

   ::cPort       := cPort
   ::nBaudRate   := nBaudrate
   ::nDatabits   := nDatabits
   ::nParity     := nParity
   ::nStopBits   := nStopbits
   ::nBufferSize := nBufferSize

   ::nHandle := INIT_PORT( ::cPort, ::nBaudRate, ::nByteSize, ::nParity, ::nStopBits, ::nBufferSize )

RETURN Self

METHOD NEW2( cPort ) CLASS tComm

   ::cPort   := cPort
   ::nHandle := Init_Port2( ::cPort )

RETURN Self

METHOD READPORT( iSize, xData ) CLASS tComm

   LOCAL xTemp := Space( iSize )
   LOCAL nRet  := 0

   nRet := INCHR( ::nHandle, iSize, @xTemp )

   IF nRet >= 0
      xData := Substr( xTemp, 1, nRet )
   ELSE
      xData := ""
   ENDIF

RETURN nRet

METHOD WRITEPORT( cData, nLen ) CLASS tComm

   DEFAULT nLen TO Len( cData )

RETURN OUTCHR( ::nHandle, cData, nLen )


