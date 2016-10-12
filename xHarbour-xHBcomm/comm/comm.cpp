///// comm.cpp
/////     purpose :  member function code for TCommPort, serial communictaions API
/////                encapsulation
/////    copyright: Harold Howe, bcbdev.com 1996-1999.
/////    notice   : This file provides an object that encapsulates the win32 serial port routines.
/////               This file may be distributed and used freely for program development,
/////               but it may not be resold as part of a software development library.
/////               In other words, don't take the source and attempt to sell it to other developers.

// For pre-compiled headers, we need to include vcl.h and list a
// hdrstop pragma. Do this only for BCB
#ifdef __BORLANDC__
//#include <vcl.h>
#pragma hdrstop
#endif

#include "comm.h"

ECommError::ECommError( ErrorType error)
   : Error(error),
     Errno(GetLastError())
{
}

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::TCommPort() (constructor)
/////
/////       scope:  TCommPort public constructor function.
/////    purpose :  Initialize serial port settings (baud rate et al) to
/////               reasonable values
/////       args :  none
/////    returns :  none, its a constructor.
/////    written :  10/31/96 by H Howe
/////    remarks :  TCommPort is a stand alone class (not derived from anybody).
/////               All the constructor needs to do is initialize the private
/////               data members that are being hidden from the user.
/////    methods :  use auto assignment (that : stuff) to initialize bool open
/////               flag and the comm port handle.  Then fill in the DCB with
/////               decent initial values in case the comm port is opened before
/////               any of the values are set by the user.
TCommPort::TCommPort()
  :m_CommOpen(false),
   m_CommPort("COM1"),
   m_hCom(0)
{
    // initialize the comm port to  N81 9600 baud communications.  These values
    // will be used to initialize the port if OpenCommPort is called before any
    // of the SetXXXX functions are called.
    m_dcb.DCBlength = sizeof(DCB);
    m_dcb.BaudRate  =9600;
    m_dcb.ByteSize  =8;
    m_dcb.Parity    =NOPARITY;    //NOPARITY and friends are #defined in windows.h
    m_dcb.StopBits  =ONESTOPBIT;  //ONESTOPBIT is also from windows.h
}
///// end of TCommPort::TCommPort (constructor)
////////////////////////////////////////////////////////////////////////////////

TCommPort::~TCommPort()
{
    if(m_CommOpen)
      CloseCommPort();
}


////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::OpenCommPort()
/////
/////       scope:  TCommPort public function.
/////    purpose :  Open the comm port and configure it with the current
/////               settings.
/////       args :  void
/////    returns :  bool indicating success (true) or failure.
/////    written :  10/31/96 by H Howe
/////    remarks :  Opening a comm port in windows involves getting a handle
/////               to a port (comm 1 or 2), getting the current port properties,
/////               and changing the properties to suit your needs. Each of the
/////               steps can fail. This function bundles all of these tasks
/////               into one place.
/////    methods :  use CreateFile as described in the win32 help file. Check
/////               for errors.  Then get the DCB properties.  Our private DCB
/////               properties are then copied into the 4 major settings.
void TCommPort::OpenCommPort(void)
{
    if(m_CommOpen)   // if already open, don't bother
        return;

    // we need to get the default settings while preserving the settings
    // that we override. The DCB struct has 20 or so members. We override 4.
    // Make of copy of the settings we care about.
    DCB tempDCB;
    tempDCB.BaudRate  =  m_dcb.BaudRate;
    tempDCB.ByteSize  =  m_dcb.ByteSize;
    tempDCB.Parity    =  m_dcb.Parity;
    tempDCB.StopBits  =  m_dcb.StopBits;

    m_hCom = CreateFile(m_CommPort.c_str(),
                        GENERIC_READ | GENERIC_WRITE,
                        0,    /* comm devices must be opened w/exclusive-access */
                        NULL, /* no security attrs */
                        OPEN_EXISTING, /* comm devices must use OPEN_EXISTING */
                        0,    /* not overlapped I/O */
                        NULL  /* hTemplate must be NULL for comm devices */
                        );

    // If CreateFile fails, throw an exception. CreateFile will fail if the
    // port is already open, or if the com port does not exist.
    if(m_hCom == INVALID_HANDLE_VALUE)
        throw ECommError(ECommError::OPEN_ERROR);


    // Now get the DCB properties of the port we just opened
    if(!GetCommState(m_hCom,&m_dcb))
    {
        // something is hay wire, close the port and return
        CloseHandle(m_hCom);
        throw ECommError(ECommError::GETCOMMSTATE);
    }

    // dcb contains the actual port properties.  Now copy our settings into this dcb
    m_dcb.BaudRate  =  tempDCB.BaudRate;
    m_dcb.ByteSize  =  tempDCB.ByteSize;
    m_dcb.Parity    =  tempDCB.Parity;
    m_dcb.StopBits  =  tempDCB.StopBits;

    // now we can set the properties of the port with our settings.
    if(!SetCommState(m_hCom,&m_dcb))
    {
        // something is hay wire, close the port and return
        CloseHandle(m_hCom);
        throw ECommError(ECommError::SETCOMMSTATE);
    }

    // set the intial size of the transmit and receive queues. These are
    // not exposed to outside clients of the class either. Perhaps they should be?
    // I set the receive buffer to 32k, and the transmit buffer to 9k (a default).
    if(!SetupComm(m_hCom, 1024*32, 1024*9))
    {
        // something is hay wire, close the port and return
        CloseHandle(m_hCom);
        throw ECommError(ECommError::SETUPCOMM);
    }

    // These values are just default values that I determined empirically.
    // Adjust as necessary. I don't expose these to the outside because
    // most people aren't sure how they work (uhhh, like me included).
    m_TimeOuts.ReadIntervalTimeout         = 15;
    m_TimeOuts.ReadTotalTimeoutMultiplier  = 1;
    m_TimeOuts.ReadTotalTimeoutConstant    = 250;
    m_TimeOuts.WriteTotalTimeoutMultiplier = 1;
    m_TimeOuts.WriteTotalTimeoutConstant   = 250;
    if(!SetCommTimeouts(m_hCom, &m_TimeOuts))
    {
        // something is hay wire, close the port and return
        CloseHandle(m_hCom);
        throw ECommError(ECommError::SETCOMMTIMEOUTS);
    }

    // if we made it to here then success
    m_CommOpen = true;
}
///// end of TCommPort::OpenCommPort
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::CloseCommPort()
/////
/////       scope:  TCommPort public function.
/////    purpose :  Closes the comm port.
/////       args :  void
/////    returns :
/////    written :
/////    remarks :
/////    methods :  use API function to close the comm handle.
void TCommPort::CloseCommPort(void)
{
    if(!m_CommOpen)       // if already closed, return
      return;

    if(CloseHandle(m_hCom) != 0) // CloseHandle is non-zero on success
    {
        m_CommOpen = false;
    }
    else
        throw ECommError(ECommError::CLOSE_ERROR);
}
///// end of TCommPort::CloseCommPort
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::SetCommDCBProperties()
/////
/////       scope:  TCommPort public function.
/////    purpose :  Set the comm port properties
/////       args :  DCB structure with the new properties
/////    returns :  bool indicating success (true) or failure.
/////    written :  10/31/96 by H Howe
/////    remarks :  This function provides the ability to set the comm
/////               properties using a DCB structure.  This does not really
/////               encapsulate anything.  Avoid using this function.
/////    methods :  Call win32 api to set the new properties.  If success, copy
/////               the new properties into the private DCB member.
void TCommPort::SetCommDCBProperties(DCB &properties)
{
    // we need to act differently based on whether the port is already open or
    // not.  If it is open, then attempt to change the ports properties.
    // If not open, then just make a copy of the new properties.
    if(m_CommOpen)  // port is open
    {
        if(!SetCommState(m_hCom,&properties))  // try to set the state directly
            throw ECommError(ECommError::SETCOMMSTATE);          // bomb out if failed
        else                                       // copy the new properties
            m_dcb = properties;                      // if success
    }
    else                                         // comm port is not open
        m_dcb = properties;                        // best we can do is save a copy
}
///// end of TCommPort::SetCommProperties()
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::GetCommDCBProperties()
/////
/////       scope:  TCommPort public function.
/////    purpose :  Get the comm port properties
/////       args :  DCB structure that holds the return value
/////    returns :  void
/////    written :  10/31/96 by H Howe
/////    remarks :  Again, this function need not be used.
/////    methods :  copy private dcb structure into the arg.
void TCommPort::GetCommDCBProperties(DCB &properties)
{
  properties = m_dcb;
}
///// end of TCommPort::GetCommDCBProperties()
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::SetBaudRate()
/////
/////       scope:  TCommPort public function.
/////    purpose :  set the comm port baud rate
/////       args :  int of new baud rate
/////    returns :  bool to indicate success (true) or failure/////    written :  10/31/96 by H Howe
/////    remarks :  If the port is open, SetCommState will be used to try and
/////               set the baud rate immediately.  If the comm port is closed
/////               the best we can do is save the new baud rate in our private
/////               data member.
/////    methods :  first, make a copy of the old baud rate in case we fail at
/////               some point. Then change the baud rate member of dcb.  If the
/////               port is open, try to set the new baud rate.
void TCommPort::SetBaudRate(unsigned int newBaud)
{
    unsigned int oldBaudRate = m_dcb.BaudRate; // make a backup of the old baud rate
    m_dcb.BaudRate = newBaud;                // assign new rate

    if(m_CommOpen)                     // check for open comm port
    {
        if(!SetCommState(m_hCom,&m_dcb))   // try to set the new comm settings
        {                                      // if failure
            m_dcb.BaudRate = oldBaudRate;        // restore old baud rate
            throw ECommError(ECommError::BAD_BAUD_RATE);     // bomb out
        }
    }
}
///// end of TCommPort::SetBaudRate()
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::SetByteSize()
/////
/////       scope:  TCommPort public function.
/////    purpose :  set the comm port byte size
/////       args :  int of new byte size
/////    returns :  bool to indicate success (true) or failure
/////    written :  10/31/96 by H Howe
/////    remarks :  If the port is open, SetCommState will be used to try and
/////               set the byte size immediately.  If the comm port is closed
/////               the best we can do is save the new byte size in our private
/////               data member.
/////    methods :  first, make a copy of the old byte size in case we fail at
/////               some point. Then change the byte size member of dcb.  If the
/////               port is open, try to set the new byte size.
void TCommPort::SetByteSize(BYTE newByteSize)
{
    BYTE oldByteSize = m_dcb.ByteSize; // make a backup of the old byte size
    m_dcb.ByteSize = newByteSize;              // assign new size

    if(m_CommOpen)                     // check for open comm port
    {
        if(!SetCommState(m_hCom,&m_dcb))     // try to set the new comm settings
        {                                      // if failure
            m_dcb.ByteSize = oldByteSize;          // restore old byte size
            throw ECommError (ECommError::BAD_BYTESIZE);                        // bomb out
        }
    }
}
///// end of TCommPort::SetByteSize()
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::SetParity ()
/////
/////       scope:  TCommPort public function.
/////    purpose :  set the comm port parity
/////       args :  int of new parity
/////    returns :  bool to indicate success (true) or failure
/////    written :  10/31/96 by H Howe
/////    remarks :  If the port is open, SetCommState will be used to try and
/////               set the parity immediately.  If the comm port is closed the
/////               best we can do is save the new parity in our private data
/////               member.  Note, the arg to this function should be one of the
/////               windows #defines for parity (NOPARITY,ODDPARITY,EVENPARITY)
/////    methods :  first, make a copy of the old parity in case we fail at some
/////               point. Then change the parity member of dcb.  If the port is
/////               open, try to set the new parity.
void TCommPort::SetParity(BYTE newParity)
{
    BYTE oldParity = m_dcb.Parity;     // make a backup of the old parity
    m_dcb.Parity = newParity;                  // assign new parity

    if(m_CommOpen)                     // check for open comm port
    {
        if(!SetCommState(m_hCom,&m_dcb))     // try to set the new comm settings
        {                                    // if failure
            m_dcb.Parity = oldParity;              // restore old parity
            throw ECommError(ECommError::BAD_PARITY);  // bomb out
        }
    }
}
///// end of TCommPort::SetParity()
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/////  TCommPort::SetStopBits()
/////
/////       scope:  TCommPort public function.
/////    purpose :  set the number of stop bits.
/////       args :  int arg for number of stopbits.
/////    returns :  bool to indicate success (true) or failure
/////    written :  10/31/96 by H Howe
/////    remarks :  If the port is open, SetCommState will be used to try and
/////               set the # of stop bits immediately.  Otherwise the best we
/////               can do is save the new parity in our private data member.
/////               Note, the arg to this function should be one of the windows
/////               #defines for stop bits(ONESTOPBIT ONE5STOPBITS TWOSTOPBITS)
/////    methods :  first, make a copy of the old stopbits # in case we fail.
/////               Then change the stop bit member of dcb.  If the port is open
/////               open, try to set the new stop bit number.
void TCommPort::SetStopBits(BYTE newStopBits)
{
    BYTE oldStopBits = m_dcb.StopBits; // make a backup of old #of stop bits
    m_dcb.StopBits = newStopBits;              // assign new # of stop bits

    if(m_CommOpen)                     // check for open comm port
    {
        if(!SetCommState(m_hCom,&m_dcb))     // try to set the new comm settings
        {                                    // if failure
            m_dcb.StopBits = oldStopBits;          // restore old # of stop bits
            throw ECommError(ECommError::BAD_STOP_BITS);                        // bomb out
        }
    }
}
///// end of TCommPort::SetParity()
////////////////////////////////////////////////////////////////////////////////

unsigned int TCommPort::GetBaudRate(void)
{
  return m_dcb.BaudRate;
}

BYTE TCommPort::GetByteSize(void)
{
  return m_dcb.ByteSize;
}

BYTE TCommPort::GetParity(void)
{
  return m_dcb.Parity;
}

BYTE TCommPort::GetStopBits(void)
{
  return m_dcb.StopBits;
}

void TCommPort::WriteBuffer(BYTE *buffer, unsigned int ByteCount)
{
    VerifyOpen();
    DWORD dummy;
    if( (ByteCount == 0) || (buffer == NULL))
        return;

    if(!WriteFile(m_hCom,buffer,ByteCount,&dummy,NULL))
        throw ECommError(ECommError::WRITE_ERROR);
}

void TCommPort::WriteBufferSlowly(BYTE *buffer, unsigned int ByteCount)
{
    VerifyOpen();
    DWORD dummy;
    BYTE *ptr = buffer;

    for (unsigned int j=0; j<ByteCount; j++)
    {
        if(!WriteFile(m_hCom,ptr,1,&dummy,NULL))
            throw ECommError(ECommError::WRITE_ERROR);

        // Use FlushCommPort to wait until the character has been sent.
        FlushCommPort();
        ++ptr;
    }
}

void TCommPort::WriteString(const char *outString)
{
    VerifyOpen();

    DWORD dummy;
    if(!WriteFile(m_hCom,outString, strlen(outString),&dummy,NULL))
        throw ECommError(ECommError::WRITE_ERROR);
}

int TCommPort::ReadBytes(BYTE *buffer, unsigned int MaxBytes)
{
    VerifyOpen();
    DWORD bytes_read;

    if(!ReadFile(m_hCom,buffer,MaxBytes,&bytes_read,NULL))
        throw ECommError(ECommError::READ_ERROR);

    // add a null terminate if bytes_read < byteCount
    // if the two are equal, there is no space to put a null terminator
    if(bytes_read < MaxBytes)
        buffer[bytes_read]='\0';

    return bytes_read;
}

int TCommPort::ReadString(char *str, unsigned int MaxBytes)
{
    VerifyOpen();

    if(MaxBytes == 0u)
        return 0;
    str[0]='\0';
    if(BytesAvailable() ==0)
        return 0;

    BYTE NewChar;
    unsigned int Index=0;
    while(Index < MaxBytes)
    {
        NewChar = GetByte();

        // if the byte is a \r or \n, don't add it to the string
        if( (NewChar != '\r') && (NewChar != '\n'))
        {
            str[Index] = (char) NewChar;
            Index++;
        }

        // when /r is received, we are done reading the string, so return
        // don't forget to terminate the string with a \0.
        if(NewChar == '\r')
        {
            str[Index] = '\0';
            return Index +1;
        }
    }

    // if the while loop false through, then MaxBytes were received without
    // receiveing a \n. Add null terminator to the string and return the number
    str[MaxBytes-1]='\0';
    return MaxBytes;
}

void TCommPort::DiscardBytes(unsigned int MaxBytes)
{
    VerifyOpen();
    if(MaxBytes == 0)
        return;

    BYTE *dummy= new BYTE[MaxBytes];
    ReadBytes(dummy, MaxBytes);
    delete []dummy;
}

void TCommPort::PurgeCommPort(void)
{
    VerifyOpen();
    if(!PurgeComm(m_hCom,PURGE_RXCLEAR))
        throw ECommError(ECommError::PURGECOMM);

}

void TCommPort::PurgeOCommPort(void)
{
    VerifyOpen();
    if(!PurgeComm(m_hCom,PURGE_TXCLEAR))
        throw ECommError(ECommError::PURGECOMM);

}

void TCommPort::FlushCommPort(void)
{
    VerifyOpen();

    if(!FlushFileBuffers(m_hCom))
        throw ECommError(ECommError::FLUSHFILEBUFFERS);
}

void TCommPort::PutByte(BYTE value)
{
    VerifyOpen();

    DWORD dummy;
    if(!WriteFile(m_hCom,&value,1,&dummy,NULL))
        throw ECommError(ECommError::WRITE_ERROR);
}


BYTE TCommPort::GetByte()
{
    VerifyOpen();

    DWORD dummy;
    BYTE  value;
    if(!ReadFile(m_hCom,&value,1,&dummy,NULL))
        throw ECommError(ECommError::READ_ERROR);

    return value;
}


unsigned int TCommPort::BytesAvailable(void)
{
    VerifyOpen();

    COMSTAT comstat;
    DWORD dummy;

    if(!ClearCommError(m_hCom, &dummy, &comstat))
        throw ECommError(ECommError::CLEARCOMMERROR);
    return comstat.cbInQue;
}
unsigned int TCommPort::BytesOAvailable(void)
{
    VerifyOpen();

    COMSTAT comstat;
    DWORD dummy;

    if(!ClearCommError(m_hCom, &dummy, &comstat))
        throw ECommError(ECommError::CLEARCOMMERROR);
    return comstat.cbOutQue;
}


void TCommPort::SetCommPort(const std::string & port)
{
    VerifyClosed();   // can't change comm port once comm is open
                      // could close and reopen, but don't want to

    m_CommPort = port;
}

std::string TCommPort::GetCommPort(void)
{
    return m_CommPort;
}

void TCommPort::GetCommProperties(COMMPROP &properties)
{
    VerifyOpen();

    COMMPROP prop;
    ZeroMemory(&prop, sizeof(COMMPROP));
    ::GetCommProperties(m_hCom, &prop);

    properties = prop;
}


void TCommPort::SetDtr(unsigned int newBaud)
{
   unsigned int oldDtr = m_dcb.fDtrControl; // make a backup of the old baud rate
   if ( newBaud == DTR_CONTROL_DISABLE )
   {
      m_dcb.fOutxCtsFlow = false;               // Enable CTS monitoring
      m_dcb.fOutxDsrFlow = false;               // Enable DSR monitoring
   }
   else
   { 
      m_dcb.fOutxCtsFlow = false;               // Enable CTS monitoring
       m_dcb.fOutxDsrFlow = true;               // Enable DSR monitoring
   }
   m_dcb.fDtrControl = newBaud;                // assign new rate

    if(m_CommOpen)                     // check for open comm port
    {
        if(!SetCommState(m_hCom,&m_dcb))   // try to set the new comm settings
        {                                      // if failure
            m_dcb.fDtrControl = oldDtr;        // restore old baud rate
      
            throw ECommError(ECommError::BAD_BAUD_RATE);     // bomb out
        }
    }
}

void TCommPort::SetRts(unsigned int newBaud)
{
    unsigned int oldDtr = m_dcb.fRtsControl; // make a backup of the old baud rate
    m_dcb.fRtsControl = newBaud;                // assign new rate

    if(m_CommOpen)                     // check for open comm port
    {
        if(!SetCommState(m_hCom,&m_dcb))   // try to set the new comm settings
        {                                      // if failure
            m_dcb.fRtsControl = oldDtr;        // restore old baud rate
            throw ECommError(ECommError::BAD_BAUD_RATE);     // bomb out
        }
    }
}

BOOL  TCommPort::ConfigDialog(void)
{


	DWORD size = sizeof( m_config );
   GetDefaultCommConfig( m_CommPort.c_str(), &m_config, &size );

   if (CommConfigDialog( m_CommPort.c_str(), NULL, &m_config ))
	{
      SetDefaultCommConfig( m_CommPort.c_str(), &m_config, sizeof(m_config) );
		return TRUE;
	}
	return FALSE;
}

void TCommPort::OpenPort(void)
{
   DWORD size = 0;
   COMMTIMEOUTS   m_TimeOuts;

   m_hCom = CreateFile(m_CommPort.c_str(),
                        GENERIC_READ | GENERIC_WRITE,
                        0,    /* comm devices must be opened w/exclusive-access */
                        NULL, /* no security attrs */
                        OPEN_EXISTING, /* comm devices must use OPEN_EXISTING */
                        0,    /* not overlapped I/O */
                        NULL  /* hTemplate must be NULL for comm devices */
                        );

   if(m_hCom == INVALID_HANDLE_VALUE)
   {
       throw ECommError(ECommError::OPEN_ERROR);
   }

   
   GetDefaultCommConfig( m_CommPort.c_str(), &m_config, &size );
   SetCommConfig( m_hCom, &m_config, sizeof(m_config) );

    if(!SetupComm(m_hCom, 1024*32, 1024*9))
    {
        // something is hay wire, close the port and return
        CloseHandle(m_hCom);
        throw ECommError(ECommError::SETUPCOMM);
    }


   
   GetCommTimeouts( m_hCom, &m_TimeOuts );
   m_TimeOuts.ReadIntervalTimeout         = 15;
   m_TimeOuts.ReadTotalTimeoutMultiplier  = 1;
   m_TimeOuts.ReadTotalTimeoutConstant    = 250;
   m_TimeOuts.WriteTotalTimeoutMultiplier = 1;
   m_TimeOuts.WriteTotalTimeoutConstant   = 250;

   if(!SetCommTimeouts(m_hCom, &m_TimeOuts))
   {
       // something is hay wire, close the port and return
       CloseHandle(m_hCom);
       throw ECommError(ECommError::SETCOMMTIMEOUTS);
   }

    // if we made it to here then success
    m_CommOpen = true;
}

void  TCommPort::SetCommOptions( int iOption )
{

   DWORD Func;
   switch (iOption)
   {
   case 1:
      Func = SETXOFF;
      break;
   case 2:
      Func = SETXON;
      break;
   case 3:
		Func = SETRTS;
      break;
   case 4:
		Func = CLRRTS;
      break;
   case 5:
		Func = SETDTR;
      break;
   case 6:
      Func = CLRDTR;
      break;
   }

   if(!EscapeCommFunction(m_hCom, Func))
   {
       throw ECommError(ECommError::SETCOMMOPTIONS);
   }
 
}

void TCommPort::SetupHandshaking(int eHandshake)
{
	// Reset error state

   VerifyOpen();
	// Obtain the DCB structure for the device

   if (!GetCommState(m_hCom,&m_dcb))
	{
      throw ECommError(ECommError::GETCOMMSTATE);
   }

	// Set the handshaking flags
	switch (eHandshake)
	{
   case EHANDSHAKEOFF:
      m_dcb.fOutxCtsFlow = false;              // Disable CTS monitoring
      m_dcb.fOutxDsrFlow = false;              // Disable DSR monitoring
      m_dcb.fDtrControl = DTR_CONTROL_DISABLE;    // Disable DTR monitoring
      m_dcb.fOutX = false;                     // Disable XON/XOFF for transmission
      m_dcb.fInX = false;                   // Disable XON/XOFF for receiving
      m_dcb.fRtsControl = RTS_CONTROL_DISABLE;    // Disable RTS (Ready To Send)
		break;

   case EHANDSHAKEHARDWARE:
      m_dcb.fOutxCtsFlow = true;               // Enable CTS monitoring
      m_dcb.fOutxDsrFlow = true;               // Enable DSR monitoring
      m_dcb.fDtrControl = DTR_CONTROL_HANDSHAKE;  // Enable DTR handshaking
      m_dcb.fOutX = false;                     // Disable XON/XOFF for transmission
      m_dcb.fInX = false;                   // Disable XON/XOFF for receiving
      m_dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;  // Enable RTS handshaking
		break;

   case EHANDSHAKESOFTWARE:
      m_dcb.fOutxCtsFlow = false;              // Disable CTS (Clear To Send)
      m_dcb.fOutxDsrFlow = false;              // Disable DSR (Data Set Ready)
      m_dcb.fDtrControl = DTR_CONTROL_DISABLE;    // Disable DTR (Data Terminal Ready)
      m_dcb.fOutX = true;                   // Enable XON/XOFF for transmission
      m_dcb.fInX = true;                    // Enable XON/XOFF for receiving
      m_dcb.fRtsControl = RTS_CONTROL_DISABLE;    // Disable RTS (Ready To Send)
		break;

   case EHANDSHAKEHARDWAREDTR:
      m_dcb.fOutxCtsFlow = false;               // Enable CTS monitoring
      m_dcb.fOutxDsrFlow = true;               // Enable DSR monitoring
      m_dcb.fDtrControl = DTR_CONTROL_HANDSHAKE;  // Enable DTR handshaking
      m_dcb.fOutX = false;                     // Disable XON/XOFF for transmission
      m_dcb.fInX = false;                   // Disable XON/XOFF for receiving
      m_dcb.fRtsControl = RTS_CONTROL_DISABLE;  // Enable RTS handshaking
		break;

   case EHANDSHAKEHARDWARERTS:
      m_dcb.fOutxCtsFlow = true;               // Enable CTS monitoring
      m_dcb.fOutxDsrFlow = false;               // Enable DSR monitoring
      m_dcb.fDtrControl = DTR_CONTROL_DISABLE;  // Enable DTR handshaking
      m_dcb.fOutX = false;                     // Disable XON/XOFF for transmission
      m_dcb.fInX = false;                   // Disable XON/XOFF for receiving
      m_dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;  // Enable RTS handshaking
		break;

	}

	// Set the new DCB structure
   if (!SetCommState(m_hCom,&m_dcb))
	{
      throw ECommError(ECommError::SETCOMMSTATE);
   }

}


void TCommPort::GetPortStatus(DWORD *Stat)
{
   DWORD ModemStat =0 ;
   VerifyOpen();
   if (!GetCommModemStatus(m_hCom, &ModemStat))
	{
      *Stat = 0;
      throw ECommError(ECommError::MODEMSTATUSERROR );
   }
  *Stat = ModemStat;

}

