/*
 * $Id$
 */

/*
* Tcomm Class for xBuilder
* Comm Commands and defines
* Copyright (c) 2005 - Luiz Rafael Culik Guimaraes <culikr@bturbo.com.br>
* All Rights Reserved
*/

#ifndef __TCOMM_CH__

#define CBR_110             110
#define CBR_300             300
#define CBR_600             600
#define CBR_1200            1200
#define CBR_2400            2400
#define CBR_4800            4800
#define CBR_9600            9600
#define CBR_14400           14400
#define CBR_19200           19200
#define CBR_38400           38400
#define CBR_56000           56000
#define CBR_57600           57600
#define CBR_115200          115200
#define CBR_128000          128000
#define CBR_256000          256000
#define NOPARITY            0
#define ODDPARITY           1
#define MARKPARITY          2
#define EVENPARITY          3
#define ONESTOPBIT          0
#define ONE5STOPBITS        1
#define TWOSTOPBITS         2

#xcommand START COMM <oPort>  PORT <cPort>  [ BAUD <nBaud>] [STOP <nStop> ] [PARITY <nParity>] [DATASIZE <nDataSize>] [BUFFERSIZE <nBufferSize>] => ;
    <oPort> :=tComm():New(<cPort>[,<nBaud>][, <nDataSize>][, <nParity>][, <nStop>][, <nBufferSize>] )

#xcommand START COMMDIALOG <oPort>  PORT <cPort>  => ;
    <oPort> :=tComm():New2(<cPort>)

#xcommand WRITE COMM <oComm> DATA <xData> [ Len <nLen>] => <oComm>:WRITEPORT(<xData>[,<nLen>])
#xcommand READ COMM <oComm> DATA <xData>  Len <nLen> => <oComm>:READPORT(<nLen> ,<xData>)
#xcommand CLOSE COMM <oComm> => <oComm>:CLOSE()
#xcommand ISWORKING <oComm> => <oComm>:ISWORKING()
#xcommand GETINBUFFER <oComm> into <xVal> => <xVal> := <oComm>:INBUFSIZE()
#xcommand GETOUTBUFFER <oComm> into <xVal> => <xVal> := <oComm>:OUTBUFSIZE()
#xcommand CLEAR OUT BUFFER <oComm> => <oComm>:CLEAROUTBUFF()

#ifndef __TCOMM_PRG_
#define SETXOFF             1       // Simulate XOFF received
#define SETXON              2       // Simulate XON received
#define SETRTS              3       // Set RTS high
#define CLRRTS              4       // Set RTS low
#define SETDTR              5       // Set DTR high
#define CLRDTR              6       // Set DTR low

#else
#define HSETXOFF             1       // Simulate XOFF received
#define HSETXON              2       // Simulate XON received
#define HSETRTS              3       // Set RTS high
#define HCLRRTS              4       // Set RTS low
#define HSETDTR              5       // Set DTR high
#define HCLRDTR              6       // Set DTR low
#endif
#define RESETDEV            7       // Reset device if possible
#define SETBREAK            8       // Set the device break line.
#define CLRBREAK            9       // Clear the device break line.

#define       EHANDSHAKEOFF       0
#define       EHANDSHAKEHARDWARE  1
#define       EHANDSHAKESOFTWARE  2


#define       EHANDSHAKEHARDWAREDTR  3
#define       EHANDSHAKEHARDWARERTS  4

#define MS_CTS_ON           16
#define MS_DSR_ON           32
#define MS_RING_ON          64
#define MS_RLSD_ON          128

#DEFINE __TCOMM_CH__
#ENDIF
